;===============================================================================
; Test harness: assembles SID-Wizard's real Vessel MIDI routines from
; ../../native/sources/MIDI-C64.asm in isolation and exposes them through a
; fixed jump table, so run_protocol_test.py can execute them on a 6502
; simulator (py65) and assert the exact Vessel user-port byte protocol.
;
; This is the SID-Wizard-side mirror of Vessel's firmware-side SidWizard* tests
; (vessel: test/host/vessel_test.cpp, test/sidwizard/PROTOCOL.md).
;===============================================================================

EMULATOR = 0            ; real-C64 build path (not the desktop emulator port)

; Stubs for main-program interrupt symbols referenced only by the device-close
; path (SetMIDIintA). That path is never taken for Vessel, but must assemble.
        .virtual $fe00
IRQ     .block
entry2  .byte ?
        .bend
NMI2    .byte ?
        .endv

        *=$c000
; --- entry jump table (addresses are the contract with the Python runner) ---
ENTRY_OPEN      jmp do_open     ; $c000  select Vessel + OpenDev (init handshake)
ENTRY_GETDATA   jmp do_getdata  ; $c003  GetData (poll/RX into ring buffer)
ENTRY_PUT       jmp do_put      ; $c006  transmit one MIDI byte (A) to Vessel

; OpenDev/GetData dispatch on MIDIdev (= MIDIC64.DeviceID). With HerMIDI
; disabled the single legacy device (Vessel) has ID 1 (Legacy_min).
do_open
        lda #1
        sta MIDIC64.DeviceID
        jsr MIDIC64.OpenDev
        rts
do_getdata
        lda #1
        sta MIDIC64.DeviceID
        jsr MIDIC64.GetData
        rts
; SID-Wizard transmits MIDI to Vessel by writing the byte to the user port data
; register $dd01 (midisubs.inc putMIDI); reproduce that minimal path here.
do_put
        sta $dd01
        rts

; pull in the library under test (defines the MIDIC64 block, Vessel device,
; bit0..bit7, banksel, etc.). HerMIDI is disabled so no further includes pull in.
        .include "../../native/sources/MIDI-C64.asm"
