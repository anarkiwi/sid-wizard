```
;==============================================================================
;--- external MIDI SYNC IN via NMI --------------------------------------------
;09/21                                                                      M64
;------------------------------------------------------------------------------

First release candidate: MIDI SYNC IN has matured and now uses VESSEL/NMI only:
in the menu you can select "SYNC MODE": ON (1) or OFF (0) to enable the playback
synchronisation via incoming MIDI SYNC messages.
Timing is only correct for tune tempo 6.

```
![Choose SYNC mode](http://m64.rocks/githubimgs/2-menu3.png "SID-Wizard VESSEL: choose sync mode")
``` 
^ menu point to switch sync mode ON ^

MODE 0: sync OFF - normal mode ( default, original VESSEL MIDI support by anarkiwi )
MODE 1: sync ON  - external MIDI sync in via NMI

sync OFF: is the default behavior of SID-Wizard (SW) with an attached MIDI device, 
where anarkiwi has implemented the original VESSEL MIDI support. 
( see https://github.com/anarkiwi/sid-wizard ) 
Playback is running in IRQ, and no sync in is done. When you have your VESSEL 
enabled (and all MIDI channels enabled, too), SW will react on MIDI messages,
also for start and stop. It will start playback, but unsynchronized.

sync ON: NMI SYNC mode! In this mode VESSEL is configured to trigger NMIs on CIA2s
/FLAG input on MIDI start/stop/sync messages. Playback is running in NMI, 
the UI stays visible and functional, except of border sprites. 
When this mode is selected, SW drops back to the editor:

```
![NMI](http://m64.rocks/githubimgs/2-empty.png "NMI")
``` 
 ^ SW in SYNC MODE (NMI SYNC) ^

To make up for the missing border sprites, a playback status window is 
displayed over the instrument window - showing SID WF registers, the 
posistion in the current pattern, currently playing instruments,
the channel 3 oscilloscope, and the pulswewidth-/filter bars.

The keyboard is checked, and you can move around the cursor, enter notes/
values, etc. This is done during the wait for start and playback as well.
You can mute tracks, and use editor functions in the pattern window.

SW now waits for a MIDI START message. When it is received, synchronized
playback starts, in follow play mode.

```
![NMI](http://m64.rocks/githubimgs/2-song.png "NMI")
```

  Have a lot of fun!  =8] 

```



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

