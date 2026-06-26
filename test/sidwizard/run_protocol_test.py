#!/usr/bin/env python3
"""SID-Wizard <-> Vessel user-port protocol tests.

Executes SID-Wizard's *real* 6502 Vessel routines (assembled from
native/sources/MIDI-C64.asm via harness.asm) on the py65 6502 simulator and
asserts the exact byte protocol it drives on the C64 user port.

This is the SID-Wizard-side mirror of the firmware-side SidWizard* tests in the
Vessel repo (test/host/vessel_test.cpp). There, the firmware is fed the bytes
SID-Wizard sends and its reaction is checked; here, SID-Wizard's code is run and
the bytes it emits / the way it reads RX are checked. Together they pin both
ends of the contract documented in PROTOCOL.md.

Usage: python3 run_protocol_test.py        (assembles harness.asm if needed)
Requires: py65 (pip install py65), 64tass on PATH.
"""

import os
import subprocess
import unittest

from py65.devices.mpu6502 import MPU
from py65.memory import ObservableMemory

HERE = os.path.dirname(os.path.abspath(__file__))
HARNESS_ASM = os.path.join(HERE, "harness.asm")
HARNESS_PRG = os.path.join(HERE, "harness.prg")
HARNESS_LABELS = os.path.join(HERE, "harness.labels")

# CIA2 user-port registers Vessel is wired to.
DD00 = 0xDD00  # CIA2 port A: VIC bank (bits0-1) + user-port PA2 (bit2) handshake
DD01 = 0xDD01  # CIA2 port B: the 8-bit user-port data bus (TX writes / RX reads)
DD03 = 0xDD03  # CIA2 port B data-direction (0x00 = input, 0xff = output)
D020 = 0xD020  # border colour (GetData pokes it as a debug aid; ignored here)

# Vessel command marker and commands (see PROTOCOL.md).
CMD = 0xFD
RESET, EN_CHANNELS, EN_STATUS, EN_COMMANDS = 0x00, 0x05, 0x06, 0x07

RETURN_SENTINEL = 0xFFFF  # RTS from an entry returns here; we stop stepping.
MIDIBUFFER_SIZE = 32      # MIDIbuffer_size in MIDI-C64.asm


def assemble():
    """(Re)assemble the harness when the prg/labels are missing or stale."""
    srcs = [HARNESS_ASM, os.path.join(HERE, "..", "..", "native", "sources",
                                      "MIDI-C64.asm")]
    fresh = os.path.exists(HARNESS_PRG) and os.path.exists(HARNESS_LABELS) and \
        all(os.path.getmtime(HARNESS_PRG) >= os.path.getmtime(s) for s in srcs)
    if fresh:
        return
    subprocess.run(
        ["64tass", "-o", HARNESS_PRG, "--vice-labels",
         "-l", HARNESS_LABELS, HARNESS_ASM],
        cwd=HERE, check=True, stdout=subprocess.DEVNULL)


def load_labels():
    """Parse 64tass VICE-label output: 'al c021 .MIDIC64:EventBuffer'."""
    labels = {}
    with open(HARNESS_LABELS) as fh:
        for line in fh:
            parts = line.split()
            if len(parts) >= 3 and parts[0] == "al":
                labels[parts[2].lstrip(".")] = int(parts[1], 16)
    return labels


class Vessel:
    """A 6502 + simulated Vessel user port; runs harness entry points."""

    ENTRY = {"open": 0xC000, "getdata": 0xC003, "put": 0xC006}

    def __init__(self):
        self.mem = ObservableMemory()
        with open(HARNESS_PRG, "rb") as fh:
            blob = fh.read()
        load = blob[0] | (blob[1] << 8)        # 2-byte PRG load address
        for i, b in enumerate(blob[2:]):
            self.mem[load + i] = b
        self.labels = load_labels()

        # User-port latches (reads return last written value, like real CIA).
        self.regs = {DD00: 0x03, DD01: 0x00, DD03: 0x00}
        self.port_writes = []   # bytes written to $dd01 (TX / command stream)
        self.ddrb_log = []      # sequence of DDRB ($dd03) values written
        self.pa2_log = []       # sequence of user-port PA2 (bit2) states
        self.rx = []            # bytes the simulated Vessel offers on RX reads

        self.mem.subscribe_to_write([DD00], self._w_dd00)
        self.mem.subscribe_to_write([DD03], self._w_dd03)
        self.mem.subscribe_to_write([DD01], self._w_dd01)
        self.mem.subscribe_to_write([D020], lambda _a, _v: None)
        self.mem.subscribe_to_read([DD00], lambda _a: self.regs[DD00])
        self.mem.subscribe_to_read([DD03], lambda _a: self.regs[DD03])
        self.mem.subscribe_to_read([DD01], self._r_dd01)

        self.mpu = MPU(memory=self.mem)

    # --- user-port behaviour ---------------------------------------------
    def _w_dd00(self, _addr, value):
        self.regs[DD00] = value
        self.pa2_log.append((value >> 2) & 1)

    def _w_dd03(self, _addr, value):
        self.regs[DD03] = value
        self.ddrb_log.append(value)

    def _w_dd01(self, _addr, value):
        self.regs[DD01] = value
        self.port_writes.append(value)

    def _r_dd01(self, _addr):
        # Vessel presents a count byte then that many MIDI bytes; model that as
        # a queue. Empty -> 0 (no bytes available), matching idle hardware.
        return self.rx.pop(0) if self.rx else 0x00

    # --- execution -------------------------------------------------------
    def call(self, entry, a=0):
        pc = self.ENTRY[entry]
        self.mpu.a, self.mpu.x, self.mpu.y = a, 0, 0
        self.mpu.sp = 0xFD
        # Push RETURN_SENTINEL-1 so the entry's RTS lands on RETURN_SENTINEL.
        ret = RETURN_SENTINEL - 1
        self.mem[0x0100 + self.mpu.sp] = (ret >> 8) & 0xFF
        self.mpu.sp -= 1
        self.mem[0x0100 + self.mpu.sp] = ret & 0xFF
        self.mpu.sp -= 1
        self.mpu.pc = pc
        for _ in range(200000):
            if self.mpu.pc == RETURN_SENTINEL:
                return
            self.mpu.step()
        raise RuntimeError("6502 routine did not return (runaway)")

    def label(self, name):
        return self.labels[name]

    def buffer(self):
        base = self.label("MIDIC64:EventBuffer")
        return [self.mem[base + i] for i in range(MIDIBUFFER_SIZE)]


def expected_open_stream():
    """The exact $dd01 byte stream SID-Wizard's OpenLgc emits for Vessel."""
    s = [CMD, RESET]                         # FD 00          reset
    s += [CMD, EN_CHANNELS, 0xFF, 0xFF]      # FD 05 FF FF    all 16 channels
    for ch in range(0x70, 0x80):             # FD 07 70..7F   all cmds/channel
        s += [CMD, EN_COMMANDS, ch]
    s += [CMD, EN_STATUS, 0xFF, 0xFF]        # FD 06 FF FF    all realtime status
    return s


class ProtocolTest(unittest.TestCase):
    def setUp(self):
        self.v = Vessel()

    # device open: full configuration handshake (mirror of vessel
    # SidWizardDeviceOpenConfiguresMasks / ReceiveExampleConfigForwardsNoteOn).
    def test_open_emits_full_config_sequence(self):
        self.v.call("open")
        self.assertEqual(self.v.port_writes, expected_open_stream())

    def test_open_enables_all_sixteen_command_channels(self):
        self.v.call("open")
        cmds = [self.v.port_writes[i + 2]
                for i in range(len(self.v.port_writes) - 2)
                if self.v.port_writes[i] == CMD
                and self.v.port_writes[i + 1] == EN_COMMANDS]
        self.assertEqual(cmds, list(range(0x70, 0x80)))

    def test_open_leaves_port_in_output_mode(self):
        self.v.call("open")
        self.assertEqual(self.v.regs[DD03], 0xFF, "DDRB should be output")
        self.assertTrue(self.v.regs[DD00] & 0x04, "PA2 should be high (output)")

    # GetData: read the count byte then that many MIDI bytes into the ring
    # buffer (mirror of the firmware forwarding bytes to the C64).
    def test_getdata_reads_available_bytes_into_buffer(self):
        self.v.call("open")
        msg = [0x90, 0x3C, 0x40]            # NoteOn that Vessel delivered
        self.v.rx = [len(msg)] + msg        # count byte first, then the bytes
        self.v.port_writes.clear()
        self.v.call("getdata")
        self.assertEqual(self.v.buffer()[:3], msg)

    def test_getdata_with_no_bytes_writes_nothing(self):
        self.v.call("open")
        self.v.rx = [0x00]                  # count = 0: nothing available
        self.v.call("getdata")
        self.assertEqual(self.v.buffer(), [0x00] * MIDIBUFFER_SIZE)

    def test_getdata_switches_to_input_then_back_to_output(self):
        self.v.call("open")
        self.v.rx = [1, 0xF8]
        self.v.ddrb_log.clear()
        self.v.call("getdata")
        # must drop to input (0x00) to read, then restore output (0xff).
        self.assertIn(0x00, self.v.ddrb_log)
        self.assertEqual(self.v.ddrb_log[-1], 0xFF)

    def test_getdata_ring_buffer_wraps(self):
        self.v.call("open")
        n = MIDIBUFFER_SIZE + 5            # overflow the 32-byte ring
        data = [(0x10 + i) & 0x7F for i in range(n)]
        self.v.rx = [n] + data
        self.v.call("getdata")
        buf = self.v.buffer()
        # last 5 bytes wrapped over the first 5 slots.
        self.assertEqual(buf[:5], data[MIDIBUFFER_SIZE:])
        self.assertEqual(buf[5:], data[5:MIDIBUFFER_SIZE])

    # transmit: SID-Wizard writes a MIDI byte straight to $dd01.
    def test_put_transmits_byte_to_dataport(self):
        self.v.call("put", a=0xF8)         # MIDI clock
        self.assertEqual(self.v.port_writes, [0xF8])


if __name__ == "__main__":
    assemble()
    unittest.main(verbosity=2)
