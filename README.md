```
;===============================================================================
;--- M64 update II: ext NMI SYNC ---

The first proof of concept is working! In the menu you can select "SYNC MODE".
(You need to choose/activate MIDI interface VESSEL first)
MODE 0: normal mode (default)
MODE 1: external MIDI sync in (by polling)
MODE 2: NMI sync: external MIDI sync in via NMI

Mode 0 and 1 are from the prev proof of concept, and work the same.

I added a new "display section" - on the patterns very top row, on the right
end - just before the instrument table. First row displays the SYNC MODE,
2nd row is incremented each main loop. When in NMI playback, a 3rd row is
incremented on each SYNC.

A little change in behaviour (for now, this shall change): after stopping
playback in modes 1 or 2, the mode is set back to 0.

In NMI SYNC playback also the keyboard works, you can mute/solo channels

MODE 2: NMI sync:

 - when choosing this mode, immediately waits for MIDI start message
   (border turns white)
 - the NMI mode is enabled on VESSEL, with status message filter feature enabled
 - NMI gets pointed to to MIDI sync
 - until MIDI START nothing happens (main loop "animates" 2nd row character)
 - each MIDI sync message triggers a NMI by VESSEL via /flag on CIA2. 
 - in NMI code playback is called, and follow flow is displayed, 3rd row character
   is "animated".
 - NMI SYNC can only be EXITED via MIDI STOP message ! 
 
branch "add-external MIDI sync" is usable (but w/o hardrestart correction)

;===============================================================================
;--- M64 update I: ext MIDI sync IN ---

First poc for external MIDI sync IN

 - when no MIDI device is selected, no changes
 
 - when MIDI device VESSEL is selected playback changes:

   - starting playback via F1/F2/F3 / MIDI START: 
     will start playback in VESSEL SYNC mode:
   
   - wait for incoming MIDI SYNC message 
     (24ppqn, when SW single speed tune tempo == 6: 1 row = 1/16th note)
   
     - call player routine

     - check keyboard (SHIFT 1..3 channel mute/unmute should work)

     - if keyboard F4, <-, or MIDI STOP : exit VESSEL SYNC mode 
   
     - repeat
   
   - Note: for keyboard F4 or <- to exit VESSEL SYNC mode:
     keyboard must be checked -> MIDI SYNC must be still sent
     should not be necessary, as MIDI STOP exits sync mode now.
```
![SID Wizard menu NMI mode on](http://m64.rocks/3.png "VESSEL SYNC")
```   


;===============================================================================
;--- SID-WIZARD for VESSEL ---

This is a port of SID Wizard 1.8.7, for the Vessel MIDI interface (https://github.com/anarkiwi/vessel).

Within SID Wizard, select MIDI device VESSEL.  Future versions will increase MIDI performance.

For releases, see https://github.com/anarkiwi/sid-wizard/releases.


;===============================================================================
;--- SID-WIZARD --- A native C64 music editor (tracker) tool
;-------------------------------------------------------------------------------
;by Hermit Software (Mihaly Horvath) - with much help from Soci of Singular Crew
;-------------------------------------------------------------------------------

The complete User Manual can be found in 'manuals' folder. The most recent version is
'SID-Wizard-1.8-UserManual.txt'. You can find older versions in PDF format in the
older SID-Wizard releases like 1.7 , if you need a more verbose documentation.

The latest Japanese translation is found here:
http://csdb.dk/release/?id=126819

Also, don't forget to check out WitchMaster's perfect book about SID-Wizard at CSDb.

You can find all executables in 'application' directory, including a d64 with instruments.
There's a KERNALload version of SID-Maker and SID-Wizard now which can be used with
fastloader cartridges that couldn't work with SID-Wizard, like the Final Cartridge III.
It completely turns off the cartridge fastloader and forces KERNAL loader and saver.
If there's no binary or you want to re-compile all the .prg and .d64 files,
here :


Assemble/compile/install (in Linux):
------------------------------------
 Prerequisites:
  -64tass (V1.53.1515 was used) in system-directory* to compile .asm and .inc source files
  -exomizer packer in system-directory* to compress the compiled binaries
  -c1541 in system-directory* to create .d64 diskimages via program and examples
   (it is part of VICE, while x64 is the emulator executable but not necessary)
  *system directory /usr/local/bin might be a good option for Linux
  -gcc, g++, make (should be in the system-path, part of 'build-essential' .deb package)
  (You can use mingw-gcc toolchain as well if you have M$ OS. On 64bit systems you need
   gcc-multilib and g++-multilib packages to compile SWMconvert and sng2swm. In case
   you want 64bit version, delete the -m32 gcc/g++ modifiers in the Makefile.)

 There is a 'Makefile' present in the 'sources' directory...
 Go to the 'sources' directory in shell, and simply type 'make' to create the 
 binaries into 'application' directory (one level upper in file-tree).
 
 -'make clean' : will purge all the binary/tune files created by 'make'.)
 
 -'make install' : the SWMconvert & sng2swm executables will be copied to /usr/local/bin
  system-resource directory to be executable from anywhere later.
  Also a copy of SID-Wizard, SID-Maker and example tunes & instruments will be
  copied to an '/opt/SID-Wizard' directory, and a desktop/menu entry file
  '/usr/applications/SID-Wizard.desktop' and icon will be created to point to SID-Wizard.
  (Of course 'x64' of VICE-emulator is a prerequisite for this to work.)
 
 -'make uninstall' : typing this in commandline will delete the executable & desktop 
  files that were copied to system-folders by 'make install'. (Instruments & tunes remain)
 
 *To re-compile only 2SID version of SID-Wizard, type 'make 2SID' in command prompt.



;Graphic-tools: sw-header was painted with SPRedit (.ols format) 
;               sw-splash was finalized in HermIRES (.hbm format)
;
;-------------------------------------------------------------------------------
;The PIC assembler source for the MIDI interface should be compiled separately.
;I used 'gpasm' for Linux, the syntax might be compatible with 'mpasm' as well.
;To design the little HerMIDI PCB I used gEDA's PCB Designer...

;-------------------------------------------------------------------------------
;License: You know I'm not into licensing and legal stuff but for the sake of completeness
;let's declare SID-Wizard as a WTF-licensed tool, so you can do WTF you want with it.

; 2018 Hermit Software Hungary  - contact:   by messagebox at hermit.sidrip.com
;===============================================================================
```
