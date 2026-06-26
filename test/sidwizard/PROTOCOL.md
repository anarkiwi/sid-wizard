# How this SID-Wizard fork talks to Vessel

This is the **SID-Wizard side** of the Vessel user-port protocol. It is the
mirror of the Vessel repo's `test/sidwizard/PROTOCOL.md`, which documents the
same contract from the firmware's point of view.

- Vessel side (`vessel/test/host/vessel_test.cpp`, the `SidWizard*` tests):
  feeds the firmware the bytes SID-Wizard sends and checks the firmware reacts
  correctly.
- This side (`run_protocol_test.py`): assembles SID-Wizard's **real** 6502
  routines from `native/sources/MIDI-C64.asm` and runs them on a 6502 simulator
  (py65), checking they emit exactly those bytes and read RX the way the
  firmware expects.

Together the two suites pin both ends of the wire.

> Note: this fork (anarkiwi/master) is the **polled** variant — it reads MIDI by
> calling `MIDIC64.GetData` once per video frame from the editor IRQ
> (`include/irq.inc`). It does **not** use Vessel's NMI-on-MIDI-clock sync (the
> `FD 04 09` config the M64 fork uses). So the open handshake here sets the
> channel/command/status masks but never enables NMI.

## Hardware wiring

Vessel hangs off the C64 user port, reached through CIA2:

| Register | Role |
|---|---|
| `$DD00` | CIA2 port A — VIC bank select (bits 0-1) **and** user-port `PA2` (bit 2) used as the TX/RX direction handshake to Vessel |
| `$DD01` | CIA2 port B — the 8-bit user-port data bus (command/MIDI bytes both ways) |
| `$DD03` | CIA2 port B data-direction — `$00` = input (RX), `$FF` = output (TX) |

Because `$DD00` is shared with the VIC bank bits, every place SID-Wizard sets
the VIC bank is patched to read-modify-write `$DD00` so the `PA2` handshake bit
is preserved (`irq.inc`, `menu.inc`, `SID-Wizard-splashed.asm`).

## 1. Device open — full configuration

`MIDIC64.OpenDev` → `OpenLgc`/`SetAddr` (`native/sources/MIDI-C64.asm`), run when
the user selects VESSEL as the MIDI device. It first switches the port to output
(`$DD00` bit2 = 1, `$DD03` = `$FF`) then sends, all to `$DD01`:

```
FD 00              ; Reset
FD 05 FF FF        ; Channel mask = all 16 channels
FD 07 70           ; loop vEnCh, once per channel nibble $70..$7F:
 ...                ;   Command mask = all commands, every channel
FD 07 7F
FD 06 FF FF        ; Status mask = all real-time / channel-less messages
```

This is byte-for-byte the sequence the Vessel test's `sidWizardDeviceOpen()`
replays. After open the firmware has `receiveChannelMask = receiveStatusMask =
0xFFFF` and `receiveCommandMask[0..15] = 0x07`, and the port is left in output
mode. `clrBuff` zeroes the receive ring-buffer write index.

`run_protocol_test.py` pins this with:
- `test_open_emits_full_config_sequence`
- `test_open_enables_all_sixteen_command_channels`
- `test_open_leaves_port_in_output_mode`

## 2. Receiving MIDI — polled `GetData`

`MIDIC64.GetData` (called every frame from `irq.inc`) reads input like this:

```
$DD00 bit2 <- 0 ; switch user port to input
$DD03      <- 00
ldy $DD01       ; Vessel presents a COUNT byte first
beq empty       ; nothing waiting
loop: lda $DD01 ; read COUNT MIDI bytes
      store into MIDIbuffer (32-byte ring, self-modifying WrIndex)
empty:
$DD00 bit2 <- 1 ; switch back to output
$DD03      <- FF
```

So Vessel's contract is: in input mode the **first** byte read from `$DD01` is
the number of MIDI bytes available, followed by that many MIDI bytes. Pinned by:
- `test_getdata_reads_available_bytes_into_buffer`
- `test_getdata_with_no_bytes_writes_nothing`
- `test_getdata_switches_to_input_then_back_to_output`
- `test_getdata_ring_buffer_wraps` (the buffer is a 32-byte ring)

Note: filtering (channel/command/status masks) happens in the **firmware**; by
the time bytes reach `GetData` they are already filtered. That side is tested in
the Vessel repo, not here.

## 3. Transmitting MIDI

SID-Wizard sends a MIDI byte (e.g. beat-clock) by writing it straight to `$DD01`
while the port is in output mode (`midisubs.inc` `putMIDI`). Pinned by
`test_put_transmits_byte_to_dataport`.

## Firmware contract pinned (this side)

| SID-Wizard step | Bytes / port state asserted |
|---|---|
| device open | `FD 00`, `FD 05 FF FF`, `FD 07 70`..`FD 07 7F`, `FD 06 FF FF` on `$DD01`; `$DD03 == FF`, `$DD00` bit2 set |
| poll RX (n>0) | reads count then n bytes from `$DD01` into the ring buffer |
| poll RX (n=0) | reads count only, buffer untouched |
| poll RX | `$DD03` toggles `00` → `FF` (input then back to output) |
| transmit | byte written to `$DD01` |
