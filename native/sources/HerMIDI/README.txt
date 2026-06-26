**************************************************************************************
 HerMIDI 1.0c - a MIDI interface for Commodore 64 Serial port and SID-Wizard by Hermit 
**************************************************************************************

Introduction:
-------------
 HerMIDI A MIDI-in hardware-interface for the Commodore personal computers.
 It connects to the Serial (IEC) port of the C64/Plus4 directly or daisy-chained
 through 1541 floppy devices...
 The main reason to create this device is to have a MIDI-input for SID-Wizard,
 but it is open-source and can be used for other (your) programs as well...
 TEDzakker for the Plus4 and STATION64 v2.4 by DJ Indikator has support for HerMIDI now.
 

Compilation:
------------
 I developed and tested HerMIDI in Linux environment with GPUTILS (gpasm) and GPSIM.
 The target PIC microchip is of PIC12F509 type, my used burner device is PICkit2.
 The 'Makefile' contains the necessary settings and sequences of commands.
 You can compile HerMIDI.hex file by typing ('make clean' then) 'make' in folder.
 (A C64-side exapmle 'receiver.prg' is also created as well as stimulus.stc file)
 Then you can transfer the HerMIDI.hex file to real PIC by 'make install' if you
 have a PICkit2 programmer (or LPT port?) and the necessary slot for the HerMIDI hardware.
 You can delete the generated files with 'make clean' if you wish so.
 
 If you want to develop and run a little simulation (testbench), just have 'gpsim' and
 type 'make sim', and it will perform the tests with the example MIDI-data provided 
 in the 'testbench.stc' gpsim startup-configuration file. (No datatransfer simulation.)

Building the device:
--------------------
 You can see the PCB layout in 'HerMIDI' folder called 'HerMIDI-PCB.pcb', which is a format
 for the X11 Linux tool called 'PCB' (found in Synaptic, or 'http://pcb.geda-project.org')
 The exported 'HerMIDI-PCB-top.png' and 'HerMIDI-PCB-bottom.png' are in 600dpi resolution.
 (The schematic diagram called 'HerMIDI-Schema.sch' is in gEDA's 'gschem' format.)
 You can easily reproduce the small PCB of HerMIDI by getting it onto a copper by various 
 techniques (toner-transfer,photolithography). The parts are easy to acquire from any 
 electronic shop, PIC12F509 must be found nearly anywhere, and is inexpensive.
 There are SMD components on the PCB beside the microrchip, so you need some good
 soldering skills to solder them onto the PCB. 
 If you can find the correct type of standard DIN plugs, the assembly doesn't re-
 quire any screws/boxes, the casings of the plugs are the cover of the whole PCB.
 (You might need to cut the unneeded wire-holder iron parts and rubbers...)

 You can find a full description in PDF format here:
 http://sourceforge.net/projects/hermidi/files/HerMIDI-Building-HowTo.pdf/download


Usage:
------
 The HerMIDI hardware should be plugged into the C64, or if there's a 1541 already
 connected, to its remaining port. So the USER port and Expansion port remain free.
 HerMIDI (mainly) takes its power from the +5V leg of the MIDI device.

 As a test if your MIDI-C64 setup and HerMIDI works well you can run 'receiver.prg'
 which can initialize HerMIDI in different transfer modes, and then it makes the
 played notes/effects audible on the C64. This is just for testing reasons though.
 (There are sound-settings to tweak but it's not a full-featured synthesizer app,
  though its source can be a good start to check out how to use 'HerMIDI-C64.asm'.)

 If you want to write your own code utilizing HerMIDI, you can find a 'library'
 (so to speak) called 'HerMIDI-C64.asm' (64tass-format) in this folder, which can 
 be included in your own project too. It contains initialization & polling subroutines.
 (They don't use the zeropage at all. Be careful with IRQ handling, not to be reentrant... )
 
 *CAUTION: Don't set the full $DD00 to switch VIC-bank between 2 'GetData' calls,
           because that would modify DATA-out and CLK-out on IEC bus and screw up
           the operation of HerMIDI. Keep this in mind when you write your code.
 
 HerMIDI has 3 kinds of data-transfer protocols, you can decide which fits best:
 -Synchron: This mode is really fast (12kbyte/s) and as such, doesn't allow lost CPU cycles, 
            so be careful when you call the 'GetData' routine. Don't be on badline,
            best to be in upper/lower border, preferably starting on $FF rasterline,
            so 'GetData' will surely have enough time before the 1st badline coming 
            in next PAL frame. Also sprites shouldn't be displayed in the rows in which
            GetData is running. (Be careful with sprite Y-coordinates that VIC repeats.)
            Btw. 'GetData' disables sprites during execution to be on the safe(r) side.
   *Note: HerMIDI has not been tested on NTSC machines, synchron mode will not work I guess.
          (I have no NTSC machine to test, so for NTSC owners the Asynchron-mode would fit.)
 -Asynchron: Much slower (1.6kbyte/s) protocol with handshaking for each bits where needed, 
             so it allows lost CPU-cycles on C64. Can be called anytime, even on badlines, but
             eats rastertime, so you might have to reduce "PacketSize" with ControlByte at init.
 -Direct: This mode doesn't do much just bypasses the MIDI-input signal on DATA-wire.

 To switch HerMIDI on, the initialization string should be sent from the C64
 to the IEC bus (serial port) with special code. When HerMIDI gets the awaited
 initialization string, it turns its LED on and works as a converter & buffer
 between the MIDI device (e.g. MIDI-keyboard/controller) and the C64.
 The ControlCmd (last byte) of the initialization string is not checked for match,
 it sets the datatransfer-protocol (synchron/asynchron/direct) and max. packet-size.
 The main advantage of the initialization string method is that fastloaders that use
 the ATN-line for data on the IEC-bus won't interfere with HerMIDI's MIDI-function.

 When C64 polls MIDI data, HerMIDI transfers the MIDI messages that arrived mean-
 while on its MIDI input, using the selected transfer-protocol (synchron/asynchron).
 You can decide in your program how frequently you want to read HerMIDI buffer:
 -The buffer can hold max. 25 MIDI-bytes, on overload the rest of data gets lost. 
 -A MIDI-byte needs 32*10 = 320 microseconds (cycles) to be sent (start,data,stop).
 -Therefore you need to call HerMIDI theoretically in every 25*320=8000 cycle (8ms)
  (2.5 times in a frame) if you want to make sure the buffer doesn't get overloaded.
  That's only possible with the fast 'synchron' protocol if screen is off ($d011=0)
 -If you know that MIDI-commands won't come so frequently, or you don't care
  about the missed notes, probably the most convenient way is to poll in 50Hz PAL
  frame frequency which means around 20ms (19687 cycles) period. Seems to be enough.

 CAUTION: -If a MIDI-controller device has non-precise timing, data-stability is in risk.
          -1.0 version of HerMIDI doesn't have an optocoupler on the MIDI-input,
           which is often supposed to prevent ground-loops and damages caused by ESD.
           In my opinion it doesn't matter with a simple MIDI-controller/keyboard setup,
           because we connect devices to each other through audio-connectors anyway,
           and that doesn't cause damage either if connected properly (before turned on).
           On the other hand, many MIDI keyboards might pull their 5V output down disabling
           HerMIDI operation and IEC bus data-transfer due to the current simple design.
           If you don't want this or to risk ESD, you may use an external fast optocoupler
           (risetime<1microsec) on the input. (HerMIDI 2.0 will have this by default.)
           (That might require altering 'HerMIDI-PIC.gpasm' code to have 'MIDIphase=1'.)
           (Upcoming HerMIDI versions will be SMD, so the optocoupler will fit on the PCB.)

 TROUBLESHOOTING: If you have a particular synthesizer/keyboard that has issues,
                  you can debug the bytes that HerMIDI receives:
                  receiver.prg now makes logging of all the received MIDI-bytes when it
                  starts up. The logging starts at memory address $2000,
                  and finishes at maximum $A000. (Not logging after reaching this maximum.)
                  It can be checked and saved from monitor-programs and can be examined.
                  ('$FD' bytes are added in the log to separate frames of buffer-receipts.)

I wish you to have luck and a good time with HerMIDI.
In case I haven't covered some aspects don't hesitate to email me for tech. support.

2015 Hermit Software Hungary (Mihaly Horvath)   email: hermitsoft@users.sourceforge.net


=================================================================================
APPENDIX:
=========

HerMIDI has been tested successfully with these equipments:
-----------------------------------------------------------
as receiver:
 -Various types of (new/old) PAL C64 machines (e.g. C64C, old brown breadbox C64, C64G)

as MIDI-source (in alphabetical order to find yours fast):
 keyboards/synths:
  -AKAI MPK25 MIDI-controller (output routed to MIDI-out by PC, thx to Chabee for testing)
  -AKAI MPK61
  -CASIO CT-640 (relies heavily on running-status -> turn on AFTER program startup or set patch.)
  -E-Mu EMAX II keyboard
  -Elka MK88 master keyboard
  (-Fatar Studio 900 keyboard)
  -KORG 707 synthesizer's DIN MIDI-output (my own equipment, used for testing)
  -Korg MS2000R
  -Korg-X50
  -M-Audio Axiom 25 (2nd gen)
  -M-Audio O2 MIDI controller
  -M-Audio Radium MIDI keyboard
  -Miditech "midistart music" USB MIDI-keyboard (thanks to Shokey for lending it to test)
  -Roland A-880 patchbay
  (-Roland E-16 Intelligent Synthesizer)
  -Roland XP-30
  -Swissonic CK490 USB midi-keyboard's DIN MIDI-output (my own equipment, used daily)
  -Yamaha PSS-790 Keyboard
  -Yamaha AN1X Keyboard
  -Yamaha RM1X
  -Yamaha DX7IID
  -Yamaha YS-200
  -Waldorf Bloefeld

 miscellaneous (computer/controller/etc. outputs):
  -Akai EIE Pro soundcard's MIDI-out controlled with MIDImyAPP commandline tool
  -Arduino serial port (used for MIDI)
  -Audiophile 2496
  -iRig MIDI  (IOS MIDI interface)
  -M-Audio M-TRACK plus USB audio interface (and PC-output of Cubase7 & Renoise)
  -Sonar X2 with M-Audio Delta 1010

Models that didn't seem to work:
  -Roland D-50 (many people report that it has quirky MIDI-handling in general)

*Many thanks go to people for testing with their keyboards, especially WitchMaster, who
 helped a lot to make HerMIDI much more compatible with the rest of the world. :)


Changelog,ToDOs,Plans:
----------------------
 changes since 1.0b version: (only in software and documentation)
 ---------------------------
  -Commodore Plus4 and C-16 support by HerMIDI-Plus4.asm, only asynchronous mode yet (TEDzakker)
  -Refreshed documentation with new tried synthesizers/keyboadrs/controllers and corrected typos
  -Enhanced safety by checking turned off MIDI-keyboard that pulls IEC lines down
  -Stabler initialization and response-check when HerMIDI is turned on (in non-badline area)
  -Improvement for asynchronous mode: if too much data was transferred (which took more than the
   18ms watchdog-timout) HerMIDI restarted itself due to missed CLRWDT (watchdog-timeout)
   (It needed restart. Now it doesn't restart, but even if it does, HerMIDI-C64.asm reinits it.)

 software (PIC/C64/PC software re-write):
 ----------------------------------------
  *Embed Tx-routines into ReadMIDIbyte so MIDI-byte doesn't delay transfer much
  -selectable MIDI-shortcode to decrease time of data-transfer if OMNI (mono.) is enough
  -Include a Synthesizer program, or at least OPEN,READ,CLOSE on the PIC's remaining memory, 
   that could be loaded from the C64 by LOAD"!",15 (15 could be the default device-number)
   (or even more: use HerMIDI as data/file-transfer interface between PC and C64)
  -if ever needed (probably not), calibrate OSCCAL (PIC-clock) to match C64 & MIDI timing
  -maybe with proper circuitry C64 could program the PIC to refresh firmware of HerMIDI :)
  -immediate-mode (a MIDI-byte gets transferred to C64 as soon it arrives, may use SRQ IN pin)

 hardware (PCB/outfit re-design):
 --------------------------------
  -device-number for drive-emulation should be selectable by 'CONFpin' capacitor value
  -take 5V power from C64 Reset-pin (too?) (with careful LED-brightness setting)
   and use opto-isolator on MIDI-input to prevent any ground-loops / ground-shifts / ESD
   (optocoupler solves when synth is switched off and pulls DATA/CLK to ground causing freeze.)
  -use SLEEP mode of the microcontroller with 'wake-up on pin' for ATN line GPIO0/1/3
  -re-design PCB to hold SMD chips (MCU,Opto) instead of normal DIP-packaked
  -make MIDI-out plug or make MIDI-plug to be configurable either input/output/both
  -USB-MIDI protocol support? (through USB plug, that most modern MIDI-keyboards have)
  -(make proper headers for in-circuit programming from PICkit,etc.  if fits on PCB)
  -(would be even better if C64 itself could update the firmware of PIC thourgh serial port)
  -(maybe a PIC with USART,IRQ,etc. like PIC12F675 could do the task easier/better.)
  -maybe better to design a MIDI-device for Amiga Clock-port of MMC64/TC64, or for 1541U


MIDI standard description: 
--------------------------
  physical: -grounded at transmit-end only (?)
            -5pin 180degrees DIN connector, max. 5mA
            -pin 4 and 5 transmit the signal, 
            -receiver is opto-isolator IC (max. 2 microseconds' rise-time)

  MIDI-data: -Asynchronous serial 31.25kbps signal in 8-N-1 format 
              (one startbit=0, 8bit data, one stopbit=1)
             -One-byte notes/messages (low-nybble: channel-number) 
              then follows 0/1/2 databytes if needed by message-type

Serial IEC description: -Open-collector outputs which are also used as inputs 
-----------------------  FALSE=5V (high-imp.), TRUE=0V (sink/ground)
                        -C64 has 1kOhm pullup-resistors. C64 sets ATN (attention) 
                         line TRUE (0V) to send control messages on DATA and CLK.
                        -The control message asks a device (x) to LISTEN ($2x) 
                         or TALK($4x). All other devices stay silent on the IEC bus. 


;************************************************************************************
