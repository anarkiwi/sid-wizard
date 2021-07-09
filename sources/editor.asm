;ident:8,24
;EDITOR part of SID-Wizard
;=====================================CONSTANTS===========================================
;CAUTION: SEQUENCE/PATTERN/TABLE DELIMITERS ($FE/$FF/$7E/$7F,TEMPOTABLE BIT7) ARE HARDWIRED, NEVER EVER TRY TO MODIFY THEM!!!
; $Id: editor.asm 385 2014-07-01 12:59:56Z hermitsoft $

COMPILEDAPP = 1 ;1 stands for the 'editor'
SFX_SUPPORT = 0 ;SFX-support is not needed in editor

        .include "settings.cfg";global constants to fine-tune the maximal memory occupation of parts of tune data

        .dsection declaration

;====================================== VOLATILE DATA RANGE ===========================
;don't use .byte or .fill with initial value, because it will alter the start-address of the compiled program
        *= $340 ;don't disturb $0300..$0340, they contain important IRQ/NMI vectors, etc.
zerovar                 
curptn1 .byte ?         ;selected (or played in follow-mode) pattern 1
curptn2 .byte ?         ;selected (or played in follow-mode) pattern 2
curptn3 .byte ?         ;selected (or played in follow-mode) pattern 3
.if (SID_AMOUNT>1)
        .fill 3*(SID_AMOUNT-1) ;selected (or played in follow-mode) pattern 4,5,6
.fi

zerovar1                ;variables set to be zero only when new tune is loaded and cursor-positions must be reset (saving doesn't affect them)
curwind .byte ?         ;current window (0-patterneditor,1-orderlist,2-instrum,3-chord,4-tempo)
subwpos1 .fill 5        ;subwindow/position 1 in current window
subwpos2 .fill 5        ;subwindow/position 2 in current window
subwpos3 .fill 5        ;subwindow/position 3 in current window
.if (SID_AMOUNT>=2)
trkposi .byte ?         ;first track to be displayed (makes sense in 2SID & 3SID versions)
.fi
zerovar1_end

zerovar2                ;here are the variables that are used by the program, and initially must be set zero.
                        ;They remain till program executes. (Displayer needs all these variables to be here for faster batch-indexing operations.)

prowpos .fill CHN_AMOUNT ;first display-row positions of the patterns (track1..3)
ptncurs .fill CHN_AMOUNT ;memory (note-column) index in patterns under cursor Y-position (displayer1 calculates)

ptnMpos .fill CHN_AMOUNT ;pattern display start-positions in memory - calculated and used by displayer1

ptrdysi .byte ?         ;signal that notifies key-repeater to wait for pattern-redrawing

inswXbuf .fill 4        ;instrument-subwindow X positions
inswYbuf .fill 4        ;instrument-subwindow Y positions

;olstart  .fill 3        ;orderlist F2key-playing startpositions

;!!!these 5 bytes are index-handled some times, so leave them in the order like this
wfarpos .byte ?         ;wfarp-table display-poisition
pwtbpos .byte ?         ;pwtable display-position
ctftpos .byte ?         ;ctftable display-position
chorpos .byte ?         ;chordtable display-position
temppos .byte ?         ;tempotable display-position
;---------------

songtime .word ?        ;1st byte - minutes in BCD , 2nd byte - seconds in BCD
timeroff .byte ?        ;timer ON/OFF switch

namemode .byte ?        ;if value is nonzero, keyboard routine is typing characters instead of inst./pattern/etc. editing
                           ;if value is 2, filename-typing/selection mode
menumode .byte ?        ;if value is nonzero, the menu appears above music editor (display of musicdata stops)

OSC3Buffer .fill 12     ;storage for OSC3 SID output to display oscilloscope
;ENV3Buffer .fill 12     ;storage for ENV3 SID output to display oscilloscope

.if (MIDI_support!=0)
MIDIbuffer .fill MIDIbuffer_size
.fi
zerovar2_end

;------------------------------------------Video RAM is used for upper border, disappears when opening loader/saver/dir.display
        .align $100
ordlspr .fill $200      ;orderlist-spritedata, and other volatile data can be here
orlcurs .fill $40       ;orderlist-cursor sprite - copied here from RAM unde I/O
orlcur2 .fill $40       ;alternative orderlist-sprite for NTSC mode (due to sprite-coordinate issues)
orlcur3 .fill $40       ;alternative orderlist-sprite for NTSC mode (due to sprite-coordinate issues)

seqbuffer .fill 128     ;around 128 byte buffer where

        .cerror *>$0800,"TOO MUCH DATA BEFORE EDITOR DOESN'T FIT IN MEMORY. MOVE SOME DATA TO UPPER FREE AREAS!"
;================================= BASIC area start ===========================
        *= $0801
        .word ss,Year
        .null $9e, format("%d", start) ;Sys 2064
ss        .word 0

        *= $080d        ;SYS 2061 to safe-restart the editor, without initializing patterns/instruments
        jmp SafeRestart
;================================= Code part 1 ================================
        *= $0810        ;INITIALIZATION AT PROGRAM START - set IRQ handlers, screen, and main VIC registers
start   sei
        jsr STARTUPMENU.doit ;detect PAL/NTSC, select player, etc. - volatile, overwritten by patterns at next step
        jsr menu.CINITUN     ;clear & initialize music data area in menu.inc
        jsr menu.CININST     ;clear & initialize instrument-data area

;------------------------------------------------------------------------------
started sei             ;we don't want lost cycles by IRQ calls :)
appiniter               ;(this section was contained by initer.inc before)
        jsr initIO
        jsr commonsubs.initpos
        jsr playadapter.resetune ;contains inireq+1 increasing + selects 1st patterns of subtune
        lda #1
        sta $d019
        cli

;---------------------- display-refresher main loop (outside IRQ) ------------------
mainloop
inirequ lda #1          ;self-written variable ;if not 0, signs tune initialization request for main routine
        beq +           ;check init request from IRQ
initune jsr playadapter.inisubb        ;player-adapter version of initer ($1000) of player routine
        ;jsr ptndisp     ;display selected patterns
        ;jsr stpdisp     ;display step-highlighted bars for track 1..3
        lda #0
        sta inirequ+1
+       jsr display     ;considers menu-display over patterns (based on 'menumode' variable)
        lda menu.menupoint ;check selected menupoint (value is 0 if no menupoint was launched)
        beq mainloop    ;if no menu was launched, main loop continues
        jmp menu.runmenup

SafeRestart
        sei
        lda #$35
        sta banksel
.if (MIDI_support!=0)
        lda #0
        sta MIDIdev     ;if MIDI-interface selection clashed with cartridge, prevent further clash
.fi
        ldy #$fe
        ;sty inirequ+1   ;ensure that player initialization happens at startup
        sty $dc00
        iny             ;wait for release of RETURN-key
        cpy $dc01
        bne *-3
        jmp menu.retoedi ;started
        
;--------------------------------------------
initIO  lda #$34
        sta banksel
        ldy #0
-       lda #0
        sta zerovar2,y  ;zerovar,y
        sta ordlspr,y   ;zero out orderlist sprite-bitmap area (the gaps between orderlist-rows won't be cleared afterwards)
        sta ordlspr+$100,y
        sta orlcurs,y   ;zero sprites to initialize not written parts
        iny
        bne -           ;Y=0 on exit
        sta insdelcopy.seqbufs+1 ;init sequence-buffer (set sequence-buffer size to zero)

        ldy #$3E
-       lda olcursp,y   ;copy orderlist-cursor sprite from RAM under IO area
        sta orlcurs+0,y ;and overwrite the beginning of orlcur2 with zeroes
        sta orlcur2+(1*NTSCseqdistance)*3,y ;copy alternative shifted orderlist-cursor for NTSC mode
        sta orlcur3+(2*NTSCseqdistance)*3,y ;copy alternative shifted orderlist-cursor for NTSC mode
        dey
        bpl -

        inc banksel ;$35 ;no need for ROM, just when loading/saving in menu
        jsr commonsubs.setInterrupt

        lda Config.ThemeNo
        jsr coloriz     ;based on Accumulator, select a colour-theme
        jsr iniscrn     ;initialize VIC, screen colors and sprite-pointers

        ;$01 must be $35 here

        lda #<(cursprite / $40);divided by spritesize, 256 sprites in a bank
        sta $fff8       ;cursor-sprite pointer for 26th row
        sta vidram+$3f8

.if (MIDI_support!=0)
        jmp midisubs.initMIDI
.else
        rts
.fi



;==============================================================================
        .include "include/irq.inc";call keyhandler, cursordisplay, player routine, raster-tricks, followplay
 ;(don't move this area if possible, because code boundary-crossing can affect rastertiming)
;==============================================================================
        .include "include/displayer1.inc";display pattern, step highlighting & numbering, etc.

;==============================================================================
;end of code part 1
;==============================================================================
        .cerror *>PLAYERADDR,"TOO MUCH CODE BEFORE PLAYER DOESN'T FIT IN MEMORY. MOVE SOME CODE TO UPPER FREE AREAS! ",*


;************************ MUSICAL CODE AND DATA *******************************
        .include "include/player.asm";the player/driver routine to produce the SIDDY sound
        *= PLAYERADDR	;$1000
MAX_PLAYERSIZE=size(STARTUPMENU.extraPlayer.player) ;take biggest player ;(in the editor player.asm compiles smaller due to the zeropage usage...)
        .union
         .fill MAX_PLAYERSIZE,0 ;give enough space
         .if (SID_AMOUNT>=2)
          .struct
player2SIDa #playerm PLAYERADDR, nonSID2addr, defaultSID3BASE ;an address that is not in SID-address range
          .ends
         .fi
         .if (SID_AMOUNT>=3)
          .struct
player3SIDa #playerm PLAYERADDR, defaultSID2BASE, nonSID3addr
          .ends
         .fi
         .struct
player    #playerm PLAYERADDR, defaultSID2BASE, defaultSID3BASE
         .ends
        .endu

        #FeatInstances ;macro in settings.cfg  (on fix base-address, has fix size)

.if (SID_AMOUNT>=2) ;this table will be used to set SID2 address on the fly for the selected player
SID2addTbl .fill size(STARTUPMENU.extraPlayer.SID2ADDtable),0 ;take the biggest ;#SID23ADDm, SID2ADDtable, player2SIDa, player, PLAYERADDR  ;SID2-address table
SID2regTbl .fill size(STARTUPMENU.extraPlayer.SID2ADDtable),0 ;the numbers of addressed registers will be copied here in startup-menu
.fi
.if (SID_AMOUNT>=3) ;this table will be used to set SID3 address on the fly for the selected player
SID3addTbl .fill size(STARTUPMENU.extraPlayer.SID3ADDtable),0 ;take the biggest ;#SID3ADDm, SID3ADDtable player3SIDa, player, PLAYERADDR  ;SID3-address table
SID3regTbl .fill size(STARTUPMENU.extraPlayer.SID3ADDtable),0 ;the numbers of addressed registers will be copied here in startup-menu
.fi

        .include "include/playadapter.inc";subroutines connecting player and editor (jamming, follow-play, etc.)


;===============================
MUSICDATA                ;musicdata-pointers reset relative to this address accordingly...preset music-data for editor------------------
;---------------------FIX pointer-tables-no need to save and recalculate on load-----------------------
        *= SUBTUNES     ;Subtune-orderlist-pointer table (max. 256 bytes, packer will crop unused areas)
        .for SUBTC=0, SUBTC<=maxsubtuneamount, SUBTC=SUBTC+1
        .word SEQUENCES + SUBTC * (CHN_AMOUNT) * seqbound + seqbound * range(CHN_AMOUNT)
        .if (SID_AMOUNT>1)
         .fill (SID_AMOUNT!=3)? (SID_AMOUNT*8-CHN_AMOUNT*2)-2 : 12 , $00
        .fi
        .byte defsubtempo1,defsubtempo2 ;DEFAULT FUNKTEMPO FOR SUBTUNES
        .next

-       = [PTN0]..(RESTPTN+range(maxptnamount)*PTNBOUND) ;Pattern pointer-table
        *= PPTRLO       ;Pattern lo-pointer-table (max. 128 bytes, packer will crop unused areas)
        .byte <(-)      ;LO-BYTES OF PATTERN-POINTERS
        *= PPTRHI       ;Pattern hi-pointer-table (max. 128 bytes, packer will crop unused areas)
        .byte >(-)      ;HI-BYTES OF PATTERN-POINTERS

-       = INSTRUMENTS+range(maxinstamount)*maxinstsize ;Instrument-data Pointer Table
        *= INSPTLO+1    ;Instrument-data lo-Pointer Table ;(max. 128 bytes, packer will crop unused areas), instrument 0 unused
        .byte <(-)      ;LO-BYTES OF INSTRUMENT-POINTERS
        *= INSPTHI+1    ;Instrument-data hi-Pointer Table ;(max. 128 bytes, packer will crop unused areas)
        .byte >(-)      ;HI-BYTES OF INSTRUMENT-POINTERS
;---------------------variable-pointer tables - not saved but need to be recalculated---------------------
        *= CHDPTRLO     ;Chord-pointer Table (max. 64 bytes, packer will crop unused areas)
        .byte 0,1       ;unchanged chord-pointer of chord 0 and 1
        *= TEMPTRLO     ;Pointers to specific tempo-programs in tempo-program table (max. 128 bytes, packer will crop unused areas)
        .byte 2+2*CHN_AMOUNT, 2+2*CHN_AMOUNT+1
;...ptnlength - pattern length registry for faster operations and display  - JUST FOR EDITOR ;(max. 128 bytes, packer will crop unused areas)
;...ptnsize - real size of pattern in memory - JUST FOR EDITOR
;=====================music data to save starts here - in the same order as saved file==============================
        *= TUNEHEADER   ;some basic tune-specific settings workfile/tune/subtune related settings (framespeed, etc.)
.enc screen
filetyp .text "SWM"      ;3 BYTE FILETYPE-DESCRIPTION - abbreviation of 'SID-WIZARD MODULE'
version .text SWMversion ; VERSION 1 of the module format
.enc none
framesp .byte 1         ;framespeed of the music (1 singlespeed.. to 8 multispeed)
 .if (SID_AMOUNT==1)
stptick .byte $04       ;tick amount for step-highlighting
leavemp .fill 2,$01 ;!!!LEAVE EMPTY!!! (placeholder), was used before as config but now they are in 'Config' block (Will be completely removed in SWM2)
mutesolo .byte $FF,$FF,$FF ;the mute/solo indicators for the tracks (toggled by shift+1/2/3)
 .elsif (SID_AMOUNT==2)
mutesolo .fill CHN_AMOUNT,$FF
 .fi
defptlen .byte DEFAULTPTNLEN ;default pattern-length (this info is needed for depacker to retain the status before saving)
;.............CALCULATED VALUES FOR PACKER/DEPACKER follow here (described in SWM-spec.src)
        .fill 5,0        ;bytes modified by packer/depacker (their positions are given in SWM-spec.src / settings.cfg)
 .if (SID_AMOUNT<=2)
leavem2 .fill 2,$00 ;!!!LEAVE EMPTY!!! (placeholder), was used before as config but now they are in 'Config' block (Will be completely removed in SWM2)
 .fi
DrvType .byte 0 ;<(PLAYERID) ;type of the music player/driver routine (0=NORMAL, 1=MID, 2=LIGHT, 3=EXTRA, 4=BARE), just an information, no restriction caused
TuningT .byte 0 ;tuning-type: 0:PAL normal (A=440Hz Equal-tempered), 1:PAL Verdi-tuning (A=432Hz Equal-tempered), 2: A=432Hz Just-intonation for C-Major scale
 .if (SID_AMOUNT>=2)
stptick .byte $04       ;tick amount for step-highlighting
SID2addID .byte $00     ;a copy of Config.SID2aID, used by exporter
 .fi
 .if (SID_AMOUNT>=3)
SID3addID .byte $00
mutesolo .fill CHN_AMOUNT,$FF
 .fi
;...in 1SID/2SID versions some bytes left for later additions (to keep compatibility)

        *= authorinfo
        .text "author name : tune-title...             "
;...SEQUENCES - orderlists of subtunes ;maximum $2000 bytes ( $2000/3tracks*$80 = 20 SUBTUNES )
;...PATTERNS ;maximum $8000 bytes  ($7f00/$100 = $7f patterns)
        *= PTN0         ;dummy pattern-data (in case of empty subtune-playback)
        .byte $00,$ff
        ;a little free area after PTN0, might be used for some runtime data in the future...

        ;=======================================================================
        .include "include/startupmenu.inc" ;initially at pattern-area, then overwritten by tune-initer
        ;contains player-types (normal/medium/light/extra), one-shot help, and a menu where everything can be selected (player/PAL-NTSC/etc.)
        ;=======================================================================
.cerror *>=CHORDS,"Startupmenu has too much data, (too much/big player-types)."

;...INSTRUMENTS ;maximum $1000 bytes. $1000/$3e = $42 (~64) bytes for each, but there
        *= CHORDS       ;Cordtable - dynamic memory management with aid of pointers (max. 256 bytes, packer will crop unused areas)
        .byte $7f       ;- ensure delimiter as chord0
;...TEMPOTBL, TRKTEMPO, RESTEMP - tempo program-table (max. 256 bytes, packer will crop unused areas)
        *= RESTEMP
        .byte $80       ;1st tempo if accidentally tempo-program 0 (at index 8) is selected
        ;and the rest of tempoprograms coming here
        *= endoftune


;======================== RUNTIME CONFIGURATION ========================
Config .block ;is saved to disk (in this order) if possible, and loaded at startup if possible
ChnOff=$11 ;"OFF" MIDI-channel
confbits .byte %00001001 ;ON/OFF configurations, each bit corresponds to a particular SID-Wizard ON/OFF-setting
 ;bit0 (1) - bind tracks ON/OFF (ON=1) - move track together wherever possible, or not at all
 ;bit1 (2) - rasterbar-display ON/OFF ('ON=0' to keep compatibility with previous SWM files) (some people get annoyed by player-rasterbars)
 ;bit2 (4) - followplay as default?  ('ON=1' to keep compatibility) - toggle follow-play for simple F1 play
 ;bit3 (8) - 0:monophonic/dovetailing, 1: polyphonic mode of MIDI-jamming 
 ;bit4 ($10) - dovetailing mode ON/OFF (cycling tracks on note entry/jam)
 ;bit6 ($40) - note-arp. mode ON/OFF - 0 if chord-table is based on numbers, 1 if contains notes instead (relative to last pressed/ch3 note) 
 ;bit7 ($80) - instrument auto-append ON/OFF ('ON=1' to keep compatibility) - whether instrument should be typed with note automatically
ThemeNo	.byte 2 ;colour-theme number
KeybTyp	.byte 0 ;keyboard-layout type  (0:default SID-Wizard, $2C:JANKO piano-keyboard, $44:FC/GMC, $88-SDI)
autoadv .byte 1 ;switch for autoadvance ON/OFF - saved with tune
TuningType .byte 0 ;temp. storage, will be written into saved tune's header as pre-selection for SID-Maker
MIDIdev .byte 0 ;MIDI-hardware/device: 0:OFF, 1:HerMIDI, 2:NameSoft, 3:Passport/Syntech, 4:Sequential, 5:Datel/Siel/JMS,  6:Maplin
MIDIchn .byte ChnOff ;MIDI-channel: $00=ALL(OMNI), $01..$10=1..16 (10=drumchannel), $11=OFF
SID2aID .byte 0 ;number of SID2-address (corresponding addresses can be found in menu.inc)
SID3aID .byte 6 ;number of SID3-address (corresponding addresses can be found in menu.inc)
PlayerType .byte 0 ;which player (normal/demo/light/extra/bare) to put the cursor on by default in startup-menu
confbits2 .byte 0 ;bit0: Save dialog automatic disk-directory reading
EOF .byte 0
.bend
Config_end


;================================= COPY/PASTE buffers =============================================
patternbuffer           ;around 256 byte buffer where the patterns are being copied
        .byte 0,$ff     ;initial value (empty pattern)
        .fill PTNBOUND-2,0 ;zero out copybuffer area

UndoBuffer
        .byte 0,$ff
        .fill PTNBOUND-2,0

instbuffer ;fill instrument copy-buffer with default values
defIbas .byte %00011010 ;0 CONTROL REGISTER (HR TIMER,HRTYPE ,VIBRATO-TYPE,TIED PW/CTF)
        .byte >DEFAULTHRADSR,<DEFAULTHRADSR;1 HR-ADSR
        .byte $00,$F4   ;3 ADSR ON NOTESTART (GATE-ON)
        .byte $84       ;5 VIBRATO (CALCULATED) FREQ+AMP (amplitude:high nibble)
        .byte $10       ;6 VIBRATO DELAY OR AMPLITUDE-INCREMENT SPEED
        .byte $00       ;7 ARPEGGIO AND CHORD-SPEED (0=1X)
        .byte $01       ;8 DEFAULT CHORD FOR INSTRUMENT
        .byte $00       ;9 OCTAVE SHIFT (2's complement) - it's transpose essentially but in 12 steps by editor
        .byte <(defPWba - defIbas) ;PWTBL1-INST1 ;A PW-TABLE POINTER RELATIVE TO INSTRUMENT-ADDRESS
        .byte <(defFIba - defIbas) ;FLTBL1-INST1 ;B FILTER-TABLE POINTER RELATIVE TO INSTRUMENT-ADDRESS
        .byte $00       ;C GATEOFF POINTER FOR WFARP-TABLE RELATIVE TO WF-TABLE POSITION
        .byte $00       ;D GATEOFF POINTER FOR PW-TABLE RELATIVE TO PW-TABLE POSITION
        .byte $00       ;E GATEOFF POINTER FOR FILTER-TABLE RELATIVE TO FILTER-TABLE POS.
        .byte $09       ;F 1ST FRAME WAVEFORM
        .byte $21,0,0,$ff ;WF-PROGAM ;hardwired position at $10 (relative to instrument-base)
defPWba .byte $ff       ;PW-PROGRAM
defFIba .byte $ff       ;FILTER-PROGRAM
        *=instbuffer+(maxinstsize-instnamelength)
        .text "saw-solo"

;-------------------------code that had no room elsewhere---------------------------------------
;------------------------------pack/depack/(load/save if no BASIC-ROM needed)---------------------------
        .include "include/commonsubs.inc" ;common/miscellaneous subroutines used by various parts of the program. pointer/index calculators, timer, etc.
        .include "include/displayer2.inc" ;display cursor and textual content (mainly orderlist)
        .include "include/menu.inc"       ;menu and file-dialog and file-operations
        .include "include/packdepack.inc" ;compresses/decompresses the tune to prepare for loading/saving by wiping out empty area

        .dsection data	;tables/variables being displayed and used ;all data from other sources (between '.section data' and '.send data' comes here

.if (MIDI_support!=0)
MIDIdev=Config.MIDIdev ;use this external label instead of MIDI-C64.asm's built-in label
        .include "MIDI-C64.asm" ;library with MIDI-devices' usage routines
        .include "include/midisubs.inc" ;contains SW-related MIDI routines
.fi

;.if (FINAL_CARTRIDGE_3_FIX!=0)
 ;.dsection FC3_FIX
 ;.include "include/datawriter.inc" ;common point to write value in X to cursor-position, and note in Y to cursor position
;.fi


;************************ CHARACTER SETS AND SCREEN DATA **********************
        .cerror *>$D000,"TOO MUCH MUSICDATA DOESN'T FIT IN MEMORY. DECREASE PATTERNS OR INSTRUMENTS OR SUBTUNES! ",*
;==============================================================================
        *= $d000        ;screendata - videoram and colour-ram initial GUI design, sprite pointers,sprites
        .include "graphics/graphics.inc"
;$d000 - c64 character set for SID-Wizard (binary file included)
;$d800 - extra RAM under IO used for non-graphic, or grahpic-source content
 ;seqchtbl ;reference table (5X multiplier) for 5-pixel high chardata
 ;sprtable ;precalc.table for sprite-addresses of corresponding orderlist x-coordinates
 ;seqchar  ;characters for orderlist patternvalues' display
 ;cursor-sprite in orderlist
 ;cursor-sprite
;around $dc00: Code in RAM Under IO area (section CodeUnderIO)
;$e000..$e400 VIDEO RAM initial and maintained content
;==============================================================================

;********************************** CODE PART 2 *******************************
*=$e400

;IO-bank switchers for insdelcopy.inc public procedures - needed because insdelcopy.inc was moved to RAM under IO
CopyIns    dec banksel ;$34
           jsr insdelcopy.CopyIns
           jmp returIO
PasteIn    dec banksel ;$34
           jsr insdelcopy.PasteIn
           jmp returIO
copytobuf  dec banksel ;$34
           jsr insdelcopy.copytobuf
           jmp returIO
transptn   dec banksel ;$34
           jsr insdelcopy.transptn
           jmp returIO
cutandcopy dec banksel ;$34
           jsr insdelcopy.cutandcopy
           jmp returIO
pastebuf   dec banksel ;$34
           jsr insdelcopy.pastebuf
           jmp returIO
delbackpt  dec banksel ;$34
           jsr insdelcopy.delbackpt
           jmp returIO
insertpt   dec banksel ;$34
           jsr insdelcopy.insertpt
           jmp returIO
copyseq    dec banksel ;$34
           jsr insdelcopy.copyseq
           jmp returIO
pasteseq   dec banksel ;$34
           jsr insdelcopy.pasteseq
returIO    inc banksel ;$35
           rts
        .section CodeUnderIO  ;in graphics.inc area $dc00...$dfff
        .include "include/insdelcopy.inc" ;batch-manipulators, like insertion/deletion/cut/copy/paste/transpose/etc. 
        .send CodeUnderIO

;----------------------------------------------------------------------------------
        .include "include/keyhandler.inc" ;handle the keypresses - the subroutine-names are found in the datatables.inc file ;callback subroutines for keypresses
;.if (FINAL_CARTRIDGE_3_FIX==0)
        .include "include/datawriter.inc" ;common point to write value in X to cursor-position, and note in Y to cursor position
;.fi
;==============================================================================

;...not too much bytes left here
        .cerror (*>$ffc0 || *<$e400),"TOO MUCH CODE DOESN'T FIT IN MEMORY. MOVE SOME CODE TO LOWER FREE AREAS! ",*
;==============================================================================
;$ffc0 ;row26  ;videoram for 26th row
        *= $ffc0        ;Video-RAM for 26th row
row26

;===================================================================================================================================
; vim: sw=4 ts=4 syntax=asm:
