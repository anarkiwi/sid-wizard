;ident:8,24
;SID-WIZARD MAIN MENU
;====================
; $Id: menu.inc 386 2014-07-09 13:46:05Z hermitsoft $
;------------------------------------------------------------------------------------------------------

menu .block             ;private stuff

        .if COMPILEDAPP==1 ;coming part is only needed for editor

runmenup                ;run selected menupoint (jumped here from main loop)
        lda menupoint
        asl             ;multiply by 2 to get word-jumpaddress for menupoint
        tay             
        lda menujmp,y
        sta mjumper+1
        lda menujmp+1,y
        sta mjumper+2
        lda #0
        sta menupoint
        sta defawin+1   ;filename-typer is default subwindow in filer (but changeable with TAB)
        sta editauth    ;init author-editing possibility (disable by default)
mjumper jmp selfmodA

menujmp .word dummymp,savetun,loadtun,saveins,loadins,seldriv
.if (SID_AMOUNT<3) 
	.word AutoDir
.fi
        .word SetFspd,SetDefPtLen,initunt,iniinsc 
        .word SwitchTheme,KeyLayout,AutoIns,AutoFollow,RasterBar,m64mode
.if (SID_AMOUNT==1)
        .word ChordNoteMode
.fi
.if (MIDI_support!=0)
        .word SelectMIDIdevice,SelectMIDIchannel
.fi
.if (SID_AMOUNT>=2)
        .word SetSID2address
.fi
.if (SID_AMOUNT>=3)
        .word SetSID3address
.fi
        .word SaveConfig,Helper ;bcktoe2;...
menujmpend


;-------------------------------------------------------------------------------------------------------
savetun ;save the whole tune
        .if ( COMPILEDAPP==1 && GHOSTPIXEL_ON && PTNBOUND==256 && maxptnlen<250 && maxsubtuneamount<18 )
        lda #0
        sta GHOSTPIXPOS ;correct to 0 for safety reasons, not to confuse pattern-packer
        .fi
        inc editauth    ;switch on author-editing for saver
        jsr moduledial  ;read directory, typing filename, return filename in 'namebuf', abort-signal is Carry-bit=1
        bcs retoedi     ;if Carry=1 on exit, return to editor
        lda player.PLAYER_TYPE ;store driver/player-type, just an information for SID-Maker.
        sta DrvType
        lda Config.TuningType ;store Tuning-type into the saved tune
        sta TuningT
.if (SID_AMOUNT>=2)
        lda Config.SID2aID
        sta SID2addID   ;make a copy of SID2 address ID into the saved file
.fi
.if (SID_AMOUNT>=3)
        lda Config.SID3aID
        sta SID3addID   ;make a copy of SID3 address ID into the saved file
.fi
        jsr packtun     ;pack tune, end of compressed data (to save) is stored in 'compzptr'
        lda compzptr    ;preserve end-pointer of packed data to get back tune by in-place depacking
        sta getback+1
        lda compzptr+1
        sta czptemp+1
;save tune (check and ask about existing file)
TSAVlop jsr iniwfn      ;init workfile name pointer and length, bank, and device,file,channel
        lda #<TUNEHEADER ;Set Start Address
        ldx #>TUNEHEADER
        jsr inidptr     ;set decozptr as startaddress (by A and X)
        ldx getback+1   ;compzptr ;Get compzptr as End Address
        ldy czptemp+1   ;compzptr+1
        lda #<decozptr  ;pointer of start-address
        jsr KERNAL.SAVE
        bcs +           ;if saving error (e.g. file exists)
        jsr KERNAL.READST
        ;cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        beq getback     ;if no saving error
+       jsr saverror
        jsr moduledial  ;read directory, typing filename, return filename in 'namebuf', abort-signal is Carry-bit=1
        bcs getback     ;if Carry=1 on exit, return to editor
        jmp TSAVlop     ;allow to type new name
getback lda #selfmod    ;(restore packed tune's end-address to compzptr
czptemp ldx #selfmod    ;(restore packed tune's end-address to compzptr
        jsr inicptr     ;set compzptr
        jsr depackt     ;depack tune - input needed: compzptr (end of packed tune)
        
retoedi jsr inidisp     ;restart editor (abort file-dialog)
        jsr initIO
        jsr playadapter.resetune.reseply
        jmp backtoe

inidisp sei
        inc clearmenu
        jsr dispmenu    ;clear menu area
        ldy #0
        sty $d011       ;cover GUI-drawing
        sty IRQ.fspdctr+1 ;we must start with singleplayer after getting back to IRQ
        sty clearmenu
        sty menumode
        inc inirequ+1   ;perform re-displaying all GUI
        inc display.insrefr+1 ;perform instrument-table refresh too
        rts


;----------------------------------
saveins                 ;save instrument menupoint - might be a good idea to sign version of instrument!
        lda #"i"
        jsr filedialog  ;read directory, typing filename, return filename in 'namebuf'
        bcs retoedi     ;if Carry=1 on exit, return to editor
;pack & save selected instrument (packer should check/handle different instrument-sizes between versions)
        lda #<dirvmem   ;use screen as packer packed data destination (compzptr)
        ldx #>dirvmem
        jsr inicptr     ;init pointers
        jsr setidptr    ;set starting source-address in decozptr, set bank, disable IRQ
        jsr packinst    ;pack instrument for saving, set compzptr to end of data
        jsr iniwfn      ;init workfile name pointer and length, bank, and device,file,channel
        lda #<dirvmem   ;packed data start-address (used screen as packed data storage (decozptr))
        ldx #>dirvmem
        jsr inidptr     ;set decozptr as startaddress (by A and X)
        ldx compzptr    ;Get compzptr as End Address
        ldy compzptr+1
        lda #<decozptr  ;pointer of start-address
        jsr KERNAL.SAVE
        bcs +           ;if error
        jsr KERNAL.READST
        cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        beq retoedi     ;if no error, return to editor
+       jsr saverror    ;display 'save error' and wait for keypress
;        bcc retoedi     ;if answered NO, return to editor
        jmp mjumper     ;if yes, try other name (maybe better not to start all over again if mistake by user)

;------;(To retrieve and display the device status, one could code: OPEN 1,8,15:INPUT#1,E,E$,T,S:PRINT E,E$,T,S:CLOSE 1 )
;display error based on code in Accu (A = $05 (DEVICE NOT PRESENT) ; A = $04 (FILE NOT FOUND) ; A = $1D (LOAD ERROR ; A = $00 (BREAK, RUN/STOP has been pressed during loading))


;--------------------------------------
loadtun                 ;load tune (workfile) menupoint - should check if SWM module version matches
        inc defawin+1   ;file-selector is preferred default subwindow in filer (but changeable with TAB)
        jsr moduledial  ;read directory, typing filename, return filename in 'namebuf'
        bcs retoedi     ;if Carry=1 on exit, return to editor
;load tune (notify if unsuccessful) - fastloader may fit in memory (and could be selected with Shift+Return)
        jsr iniwfn      ;(secondary address inside = 0 ensures load-address forcing)
        jsr KERNAL.OPEN ;open file for checking version in its beginning
        jsr KERNAL.READST ;read drive status
        cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        beq ChSWVer     ;check if file exists, if exists, go to check filetype/version
        jsr clsfile     ;in case of error, close to allow further trials
        jmp fnotfnd

ChSWVer jsr CheckSWM    ;check filetype version of SWM module in 1st 4 bytes, if different version, refuse the tune and init SWM1
        bcc SWverOK     ;if carry is 0, no version mismatch, go to loader
        jsr vermism     ;display version-mismatch
        jmp backfil

SWverOK jsr clsfile     ;quit testing, start fresh loading
        jsr iniwfn      ;(secondary address inside = 0 ensures load-address forcing)
        ldx #<TUNEHEADER ;set load-address
        ldy #>TUNEHEADER
        lda #$00        ;$00 means: load to memory (not verify)
        jsr KERNAL.LOAD ;call LOAD, outputs last loaded address to X/Y in case of success
        bcc +           ;if carry set, a load error has happened
fnotfnd jsr filenot     ;display file not found and wait for SPACE
backfil jmp mjumper
+       stx compzptr    ;end of data is important info for depacker to start in-place backwards-decompression
        sty compzptr+1
        jsr depackt
        ldy TUNEHEADER+PTAMOUPOS ;register the 1st empty pattern after the loaded music
        cpy #maxptnamount
        bcs +
        iny
+       sty keyhandler.firstEmptyPtn
        jsr dispaut
retsubt lda #0
        sta selsubt+1   ;point to subtune 0
        sta keyhandler.solosta+1   ;unsolo by default
        sta insdelcopy.copytobuf.copyptn+1   ;reset copy-endsignal
        ldy #CHN_AMOUNT-1
-       sta playadapter.olstart,y
        dey
        bpl -
        lda #$ff
        lda #1
        sta display.selinst+1 ;poit to 1st instrument, chord and tempo
        sta display.selchor+1
        sta display.seltemp+1
        lda #$ff
        sta keyhandler.UndoType         ;reset Undo's pattern/sequence/instrument type

initedi jsr inidisp
        jmp started

dispaut .proc
        ldy #authorilen-2 ;hardwired author-info length
-       lda authorinfo,y ;display author-info after loading tune
        sta row26,y
        dey
        bpl -
        sty display.titledi+1 ;switch on author-info/title display (till a keypress)
        lda #$bd
        sta row26+38 ;separator
        ldy DrvType
        lda drvsig,y
        sta row26+39 ;player-type abbreviation
        rts
        .enc 'screen'
drvsig .text "N","M","L","E","B","D"
        .enc 'none'
        .pend


;---------------------------------
loadins                 ;load instrument menupoint - might be a good idea to check version of instrument, if saved!
        inc defawin+1   ;file-selector is preferred default subwindow in filer (but changeable with TAB)
        lda #"i"
        jsr filedialog  ;read directory, typing filename, return filename in 'namebuf'
        bcc +
        jmp retoedi     ;if Carry=1 abort file-dialog, return to editor
;load and depack instrument to replace current instrument (depacker should handle length between versions)
+       jsr iniwfn      ;init workfile name pointer and length, bank, and device,file,channel
        ldx #<dirvmem   ;use screen for packed data (compzptr)
        ldy #>dirvmem
        lda #$00        ;$00 means: load to memory (not verify)
        jsr KERNAL.LOAD ;call LOAD, outputs last loaded address to X/Y in case of success
        bcc +
        jmp fnotfnd     ; if carry set, a load error has happened
+       stx compzptr    ;end of data is important info for depacker to start in-place backwards-decompression
        sty compzptr+1
        jsr setidptr    ;set decozptr to current instrument, set bank, disable IRQ
        jsr depkins     ;depack instrument to selected destination instrument
        jmp retoedi     ;return to editor


;---------------------------------
seldriv                 ;select disk drive (8..30)
        ldy devicen+1
        cpy #MAXDEVICE  ;max. device number
        iny
        blt +
        ldy #MINDEVICE  ;min. device-number
+       sty devicen+1
        jsr dispdev
        jmp mainloop

;--------------------------------------------
AutoDir lda Config.confbits2
	eor #1
	sta Config.confbits2
        jmp mainloop

;--------------------------------------------
SetDefPtLen
        jsr IncPtln
        jmp mainloop

IncPtln .proc ;increase default pattern-length, roll back to minimal if reached max
        ldy defptlen
        iny
        cpy #maxptnlen
        bne +
        ldy #minptnlen ;1
+       sty defptlen
        rts
        .pend

DecPtln .proc ;decrease default pattern-length, roll back to maximal if reached minimum
        ldy defptlen
        cpy #minptnlen ;1
        bne +
        ldy #maxptnlen
+       dey
        sty defptlen
        rts
        .pend


;----------------------------------------
initunt                 ;tune & tempotable init menupoint
        sei
        jsr RyouSur     ;Ask 'are you sure', wait for Y/N and set Carry accordingly (1 if Y)
        bcs +
        jmp backtoe
+       jsr CINITUN
        jmp retsubt     ;retoedi

CINITUN ldy #0          ;clear and init tune
        sty TUNEHEADER+PTAMOUPOS ;init all patterns
        sty TUNEHEADER+TMPLENPOS ;init all tempo-programs
        sty TUNEHEADER+SEQAMOPOS ;init all sequences
        jsr depkptseq
        jsr depktempo
        ;create a default subtune with pattern 1..3
        lda #<SEQUENCES
        ldx #>SEQUENCES
        jsr inidptr
        ldx #1
-       ldy #0
        txa             ;set pattern 1..3 to sequence 1..3
        sta (decozptr),y
        iny
        lda #$FE        ;sequence-endsignal for sequence 1..3
        sta (decozptr),y
        lda #seqbound
        jsr incopt2
        cpx #CHN_AMOUNT+1
        bne -
        stx keyhandler.firstEmptyPtn
        ldy #1
        jmp isttemp     ;init subtune 0 tempo


;-----------------------------------------
iniinsc                 ;instrument & chords init menupoint
        sei
        jsr RyouSur     ;Ask 'are you sure', wait for Y/N and set Carry accordingly (1 if Y)
        bcs +
        jmp backtoe
+       jsr CININST
        jmp retoedi

CININST lda #0
        sta TUNEHEADER+INSTAMPOS ;init all instruments
        sta TUNEHEADER+CHRDLEPOS ;init all chords
        jmp depkinsch   ;init instruments and chords


;--------------------------------------------
SwitchTheme             ;select editor-colour set
        inc Config.ThemeNo ;increment number of color-theme
        lda Config.ThemeNo ;number of current colour-theme
        cmp #(ColorThemesEnd-ColorThemes)/(ColorSetEnd-ColorSet)
        bne +
        lda #0
        sta Config.ThemeNo
+       jsr coloriz     ;based on Accumulator, select a colour-theme
        jsr iniscrn     ;initialize VIC, screen colors and sprite-pointers
        jsr display.insdisp        ;display instrument data
        jsr stpdisp     ;display step-highlighted bars for track 1..3
        inc display.insrefr+1
        jmp mainloop


;--------------------------------------------
KeyLayout ;switch (piano) keyboard-layout to ease migration from other trackers
        ldx #0          ;default layout in X
        lda Config.KeybTyp
        bmi setKbTy
        bne +
        ldx #$2C        ;Janko chromatic keyboard
        bne setKbTy ;jump   
+       and #$40        ;test bit6
        bne +
        ldx #$44        ;DMC layout in X
        bne setKbTy ;jump
+       ldx #$88        ;SDI layout
setKbTy stx Config.KeybTyp
        jmp mainloop


;--------------------------------------------
AutoIns jsr keyhandler.toggIns
        jmp mainloop


;--------------------------------------------
AutoFollow jsr keyhandler.autoFlw
        jmp mainloop

;--------------------------------------------
m64mode lda #1
        sta plybcol+1
        jsr keyhandler.f1er.PlayBeg
        jmp mainloop

;--------------------------------------------
RasterBar lda #2        ;rasterbar-displaying on/off
        jsr commonsubs.eorconf
        jmp mainloop

.if (SID_AMOUNT==1)
ChordNoteMode lda #$40
        jsr commonsubs.eorconf
        jmp mainloop
.fi

SetFspd jsr keyhandler.IncFspd
        jmp mainloop


.if (MIDI_support!=0)
;--------------------------------------------
SelectMIDIdevice .proc
        lda shiftsi
        bne clsMIDI ;if Shift pressed simultaneously, it always returns to 'OFF'
        ldx Config.MIDIdev
        cpx #MIDIC64.DeviceAmount
        bcs clsMIDI
NextDev lda Config.MIDIchn
        cmp #Config.ChnOff ;"OFF"?
        beq + ;if channel, switch off previous device
        jsr midisubs.closeMIDI ;close current device
+       inc Config.MIDIdev
        jmp clsChan
clsMIDI jsr midisubs.closeMIDI ;close current device
        ldx #0
        stx Config.MIDIdev
clsChan lda #Config.ChnOff
        sta Config.MIDIchn ;channel default is 'OFF'
        jmp mainloop
        .pend

;--------------------------------------------
SelectMIDIchannel .proc
        lda shiftsi ;if Shift pressed simultaneously, it always returns to 'OFF'
        beq +
        ldx #Config.ChnOff
        jmp storChn
+       ldx Config.MIDIchn
        cpx #Config.ChnOff
        bcc advChan ;if MIDIchn was 'OFF' switch on device
iniDevi jsr midisubs.initMIDI ;open selected device
allChan ldx #0
        stx Config.MIDIchn
        jmp mainloop
advChan inx
storChn stx Config.MIDIchn
        cpx #Config.ChnOff
        bne +
clsDevi jsr midisubs.closeMIDI ;close current device
+       jmp mainloop
        .pend
;--------------------------------------------
.fi


;--------------------------------------------
.if (SID_AMOUNT>=2)
SID2aID=Config.SID2aID

SetSID2address
        jsr SID23set.IncSID2address
        jmp mainloop

WrSID2add .proc
        ldx #0
-       lda SID2addTbl,x
        beq +
        sta SID2wr1+1
        sta SID2wr2+1
        inx
        lda SID2addTbl,x
        sta SID2wr1+2
        sta SID2wr2+2
        lda SID2regTbl-1,x
        clc
        adc SID2add+0
        ldy #0
SID2wr1 sta selfmodA,y ;write SID2 address lo-byte
        iny
        lda SID2add+1
SID2wr2 sta selfmodA,y ;write SID2 address hi-byte
        inx
        cpx #size(STARTUPMENU.extraPlayer.SID2ADDtable) ;safety check
        bne -
+       rts
        .pend

.fi

.if (SID_AMOUNT>=3)
SID3aID=Config.SID3aID

SetSID3address
        jsr SID23set.IncSID3address
        jmp mainloop

WrSID3add .proc
        ldx #0
-       lda SID3addTbl,x
        beq +
        sta SID3wr1+1
        sta SID3wr2+1
        inx
        lda SID3addTbl,x
        sta SID3wr1+2
        sta SID3wr2+2
        lda SID3regTbl-1,x
        clc
        adc SID3add+0
        ldy #0
SID3wr1 sta selfmodA,y ;write SID3 address lo-byte
        iny
        lda SID3add+1
SID3wr2 sta selfmodA,y ;write SID3 address hi-byte
        inx
        cpx #size(STARTUPMENU.extraPlayer.SID3ADDtable) ;safety check
        bne -
+       rts
        .pend
.fi


;--------------------------------------------
SaveConfig .proc
        lda #0
        sta SIDBASE+$18
.if (SID_AMOUNT>=2)
        jsr SID23Mute
.fi
chkconf jsr SetCfgFileHandle ;Check if config file exists already
;        lda cfgstat
;        cmp #"?"
;        beq wrconf2 ;if wasn't existing at startup, skip file-test
;        ldx #<lowvidram
;        ldy #>lowvidram
;        lda #$00        ;$00 means: load to memory (not verify)
;        jsr LOAD ;test-load, checks if config file exists already
;        bcs wrconfg     ; if carry set, a no existing config was found
;delconf ;attempt to delete existing config
;        lda #(cfgname_end-delCnam)        ;filename-length together with scratch-sign
;        ldx #<delCnam   ;lo byte pointer to file name. ("$")
;        ldy #>delCnam   ;hi byte pointer to file name. ("$")
;        jsr SETNAM
;        ldx devicen+1
;        lda #1          ;file # 1 (logical file-number)
;        ldy #15         ;channel #15 (command-channel of 1541 drive)
;        jsr SETLFS      ;call SETLFS
;        jsr OPEN
;        lda #1
;        jsr CLOSE
;wrconfg jsr SetCfg2 ;SetCfgFileHandle
wrconf2 lda #<Config
        ldx #>Config
        jsr inidptr     ;set decozptr as startaddress (by A and X)
        ldx #<Config_end ;End Address
        ldy #>Config_end
        lda #<decozptr  ;pointer of start-address
        jsr KERNAL.SAVE
        sei
        bcs +           ;if error
        jsr KERNAL.READST
        cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        beq ++          ;if no error, return to editor
+       lda #"!" ;display 'save error' in menu
        sta cfgstat
        jmp retomenu
+       lda #" "
        sta cfgstat
retomenu lda #$35
        sta banksel
keyrele lda #$fe
        sta $dc00
        lda #$80        ;otherwise bouncing noise of keyboard-buttons would cause inreliable release-detection
        cmp $d012
        bne *-3
keyrelp lda $dc01       ;wait for key-releasing
        cmp #$ff
        bne keyrelp
        lda #$80        ;otherwise bouncing noise of keyboard-buttons would cause inreliable release-detection
        cmp $d012
        bne *-3
        .if (COMPILEDAPP==1 && MIDI_support!=0)
        jsr midisubs.initMIDI
        .fi
        ldy #1
        sty $d01a       ;re-enable raster-IRQ
        cli
        jmp mainloop    ;if yes, try other name (maybe better not to start all over again if mistake by user)
        .pend

;loading config is included in startup-menu as needed only once at startup

SetCfgFileHandle
        sei
        ldy #0
        sty $d01a       ;prevent raster-IRQ
        lda $d019
        .if (COMPILEDAPP==1 && MIDI_support!=0)
        jsr midisubs.closeMIDI ;avoid NMI during saving
        .fi
SetCfg2 lda #$36
        sta banksel
        lda #size(cfgname)
        ldx #<cfgname
        ldy #>cfgname
        jsr KERNAL.SETNAM ;init filename pointer and length, and device,file,channel
        ldx devicen+1
        lda #1          ;file # 1
        ldy #0          ;channel # 0 - (in case of LOAD: if 1, load to address automatically, if 0, address can be given in X/Y)
        jmp KERNAL.SETLFS ;call SETLFS

delCnam .text "S0:"    ;Scratch-signal (OPEN 15,8,15,"S0:file name":CLOSE 15) - will preceed filename when user want to overwrite a file (Save&Replace "@filename" is buggy in old 1541 drives)
cfgname .block
        .text "@" ;replace if exists (buggy on some drives)
        .text ConfigFileName
        .bend
cfgname_end


;--------------------------------------------
helpmemp = menumemp+40*0+0 ;(menuypos+1)*40+2 ;
helpwidth = menuxsize ;27 ;width of help-screen (be careful with not-refreshed parts of the screen)
Helper ;display instant help (the most important keys and values)
helpmode lda #selfmod   ;signs if help should be displayed not menu
        eor #1
        sta helpmode+1
        jmp mainloop


;--------------------------------------------
backtoe                 ;'Back to editor' menupoint
        sei
bcktoe2 inc clearmenu
        jsr dispmenu
        jsr menuback
        cli
dummymp jmp mainloop


;---------------------------------------------------------------------------------------------------------
menuback                ;resume menuless state
        jsr mutedis     ;display all mute/solo column colourings;        jsr iniscrn        ;initialize VIC, screen colors and sprite-pointers
        ldy #0
        sty clearmenu
        sty menumode
        jsr ptndisp     ;display selected patterns
        jsr stpdisp     ;display step-highlighted bars for track 1..3
        ;jsr display.insdisp ;display instrument
        rts

RyouSur                 ;Ask 'are you sure', wait for Y/N and set Carry accordingly (1 if Y)
RyouXpos = menuxpos+1
RyouYpos = menuypos+(menuysize/2)-2
RyouCol  = 1
        ldy #16
-       lda #"*"        ;DECOR
        sta vidram+40*(RyouYpos+0)+RyouXpos,y
        sta vidram+40*(RyouYpos+3)+RyouXpos,y
        lda #RyouCol
        sta COLRPOS+40*(RyouYpos+0)+RyouXpos,y
        sta COLRPOS+40*(RyouYpos+1)+RyouXpos,y
        sta COLRPOS+40*(RyouYpos+2)+RyouXpos,y
        sta COLRPOS+40*(RyouYpos+3)+RyouXpos,y
        lda RyouSurTx,y
        sta vidram+40*(RyouYpos+1)+RyouXpos,y
        lda RyouSurT2,y
        sta vidram+40*(RyouYpos+2)+RyouXpos,y
        dey
        bpl -
        jmp YesOrNo


;============================================================================
;draw main menu
;=========================================
menumemp = menuypos*40+menuxpos

dispmenu                ;clear-flag is set by Accu (input parameter)
        lda #<menutxt
        ldx #>menutxt
        ldy helpmode+1
        beq +           ;if helpmode was called, display help instead
        lda #<helptxt
        ldx #>helptxt
+       jsr inicptr     ;init text-source poiters
        lda #<(vidram+menumemp)
        ldx #>(vidram+menumemp)
        jsr inidptr     ;inint videoram-pointers
        sta menucol+1
        lda #>(COLRPOS+menumemp)
        sta menucol+2
        ldx #0          ;counts rows of menu
menudlo ldy #menuxsize-1 ;column-counter for menu
-       lda clearmenu
        beq +
        lda #$bd        ;clear menu
        sta (decozptr),y
        lda fxcol       ;set back normal colours under menu
        jmp menucol
+       lda (compzptr),y
putmchr sta (decozptr),y
        lda helpmode+1  ;if help enabled, only use bright (menu-cursor) colour
        bne +
        lda menubgcol
        cpx menucurpos
        bne menucol     ;if in display is cursor position, use cursor colour
+       lda menucurcol
menucol sta COLRPOS,y   ;self-written code
        dey
        bpl -
        lda #40         ;increase videoram-pointer by
        ldy #menuxsize  ;increase textdata-pointer by
        jsr incopt2     ;increase pointers and and inx
        lda menucol+1
        clc
        adc #40
        sta menucol+1
        bcc +
        inc menucol+2
+       lda helpmode+1
        beq +
        cpx #helpYsize
        jmp ++
+       cpx #menuysize  ;number of rows for menu
+       bne menudlo
        lda clearmenu
        beq +           ;if exiting from menu, disable help-mode
        lda #0
        sta helpmode+1
        jmp retmdis
+       lda defptlen    ;display default pattern-length
        jsr commonsubs.hexdis2
        sta ptlentx
        stx ptlentx+1
dThName lda Config.ThemeNo ;display colour-theme name
        jsr commonsubs.getsubaddr.mul8toY ;A*=8 -> Y   - in commonsubs.inc
        dec banksel     ;$34
        ldx #0
-       lda ThemeNames,y
        sta themetx,x
        iny
        inx
        cpx #8
        bne -
        inc banksel     ;$35
dispdev lda devicen+1
        jsr decdis2
        sty drivetx     ;display device-number digit1 in menu
        sta drivetx+1   ;display device-number digit2 in menu
disKbTy lda Config.KeybTyp
        and #$1f
        tay
        ldx #0
-       lda KeyTypes,y
        sta keybttx,x
        iny
        inx
        cpx #4
        bne -
disFspd lda framesp     ;display framespeed in menu
        jsr decdis2
        sta frspdtx
disRbar lda #2          ;hide rasterbars?
        jsr diOnOff
        stx rastbtx+0
        sty rastbtx+1
.if (SID_AMOUNT==1)
disChNo lda #$40        ;note-mode for chord-table?
        jsr diOnOff
        stx notectx+0
        sty notectx+1
.fi
.if (SID_AMOUNT<3)
disAdir	lda #1
	jsr diOnOf2
	stx audirtx+0
	sty audirtx+1
.fi
disFoll lda #4          ;auto-followplay?
        jsr diOnOff
        stx autfltx+0
        sty autfltx+1
disAins lda #$80        ;auto-instrument?
        jsr diOnOff
        stx autistx+0
        sty autistx+1
.if (MIDI_support!=0)
       .enc 'screen'
disMIDI ldx #"?"
        lda Config.MIDIdev
        beq +           ;if MIDI-device is not switched on, don't have 'Status' of it
        ldx #":"        ;pre-selection for "found OK"
        ldy MIDIC64.Status
        beq +           ;if status is zero, "found OK"
        ldx #"!"        ;if status is nonzero, we have error
+       stx MIDItxt+0
        asl ;*2
        asl ;*4
        asl ;*8
        tax
        ldy #0
-       lda MIDIC64.DevName,x
        sta MIDItxt+1,y
        inx
        iny
        cpy #8
        bne -
        ldx #"A"
        ldy #"L"
        lda Config.MIDIchn
        bne +
SpecChn tya ;"L" (/"F")
        bne showChn ;jump
+       cmp #$11 ;OFF?
        bcc chnNumb
        ldx #"O"
        ldy #"F"
        bne SpecChn ;jump
chnNumb jsr decdis2
        ldx #" "
showChn stx MIDCtxt+0
        sty MIDCtxt+1
        sta MIDCtxt+2
       .enc 'none'
.fi
.if (SID_AMOUNT>=2)
        jsr SID23set.CalcSID2addID
        lda SID2add+1
        jsr commonsubs.hexdis2
        sta SID2Atx+0
        stx SID2Atx+1
        lda SID2add+0
        jsr commonsubs.hexdis2
        sta SID2Atx+2
        stx SID2Atx+3
.fi
.if (SID_AMOUNT>=3)
        jsr SID23set.CalcSID3addID
        lda SID3add+1
        jsr commonsubs.hexdis2
        sta SID3Atx+0
        stx SID3Atx+1
        lda SID3add+0
        jsr commonsubs.hexdis2
        sta SID3Atx+2
        stx SID3Atx+3
.fi
retmdis rts

        .enc 'screen'
diOnOff ldx #"N" ;"N "  ;on/off text endings
        ldy #" "
        and Config.confbits ;display confbit ON/OFF status in X and Y based on value of A
        bne +
-       ldx #"F" ;"FF"  ;on/off text endings
        ldy #"F"
+       rts
diOnOf2 ldx #"N" ;"N "  ;on/off text endings
        ldy #" "
        and Config.confbits2 ;display confbit ON/OFF status in X and Y based on value of A
        beq -
        rts
        .enc 'none'


;------------------------------------------------
        .enc 'screen'
menutxt .text "*******************"
menurow .text "*  - MAIN MENU -  *"
firstmp .text "*    SAVE TUNE    *"
        .text "*    LOAD TUNE    *"
        .text "* SAVE INSTRUMENT *"
        .text "* LOAD INSTRUMENT *"
        .text "* SELECT DRIVE:"
drivetx .text "08 *"
.if (SID_AMOUNT<3)
        .text "* SAV.AUT.DIR:O"
audirtx .text "FF *"
.fi
        .text "* FRAMESPEED: "
frspdtx .text "1X  *"
ptlnrow .text "* DEF.PATT.LEN:"
ptlentx .text "00 *"
        .text "* KILL TUNE&TEMPO *"
        .text "* KILL INST&CHORD *"
        .text "* COLOR: "
themetx .text "ORIGINAL *"
        .text "* KEY-LAYOUT:"
keybttx .text "SIDW *"
        .text "* AUTO-INSTR: O"
autistx .text "FF *"
        .text "* AUTOFOLLOW: O"
autfltx .text "FF *"
        .text "* HIDE RBARS: O"
rastbtx .text "N  *"
        .text "* NMI MODE  : O"
        m64btx .text "N  *"
.if (SID_AMOUNT==1)
        .text "* CHORD-NOTE: O"
notectx .text "N  *"
.fi
.if (MIDI_support!=0)
        .text "* MIDIDEV"
MIDItxt .text ":  OFF   *"
        .text "* MIDICHANNEL:"
MIDCtxt .text "OFF *"
.fi
.if (SID_AMOUNT>=2)
SID2row .text "* SID2 ADDR:$"
SID2Atx .text "D420 *"
.fi
.if (SID_AMOUNT>=3)
SID3row .text "* SID3 ADDR:$"
SID3Atx .text "D420 *"
.fi
        .text "*   SAVE CONFIG "
cfgstat .text "  *" ;will be used to display if config-saving was unsuccessful
lastmpt .text "*  INSTANT HELP!  *"
        .text "*******************"
menutxtend
        .enc 'none'

menuxsize = menurow-menutxt ;menu X size
menuysize = (menutxtend-menutxt)/menuxsize ;menu Y size
firstcpos = (firstmp-menutxt)/menuxsize ;first option's Y position in menu display area
lastcrpos = (lastmpt-menutxt)/menuxsize ;last option's Y position in menu display area
ptlenmenupt= (ptlnrow-menutxt)/menuxsize ;number or menupoint that sets default pattern-length
.if (SID_AMOUNT>=2)
SID2Amenupt= (SID2row-menutxt)/menuxsize ;number or menupoint that sets SID2 address
.fi
.if (SID_AMOUNT>=3)
SID3Amenupt= (SID3row-menutxt)/menuxsize ;number or menupoint that sets SID3 address
.fi

clearmenu .byte 0       ;flag - signs for main loop if menu to be deleted
menucurpos .byte firstcpos+0 ;cursorposition in menu
menupoint .byte 0       ;selected menupoint - for main loop - if 0, nothing done, otherwise jump depending on value

;-----------text for help------------------------------------
        .enc 'screen'
helptxt .text "F1/F2 PLAY ALL/MARK"
helprow .text "F3/F4 PLAYPATT/STOP"
        .text " <-   FAST-FORWARD "
        .text "F5/F6 PATT/ORDERLST"
        .text "SPACE EDIT/JAM MODE"
        .text " F7   EDIT INSTRUM."
        .text "+ / - SELECT INSTR."
        .text "C=+,/. SET SUBTUNE "
        .text "RETURN NOTEOFF/SEL."
        .text "SH+1..",SID_AMOUNT*3+"0"," MUTE/UNMUTE"
        .text "C=+1..8 SET OCTAVE "
        .text "SHIFT+N NAME INSTR."
        .text "C=F5/F7 CHORD/TEMPO"
        .text "C=X/C/V COPY/PASTE "
        .text "SH+SPACE PLACE MARK"
        .text "SHIFT+F/G FRAMESPD."
        .text "SHIFT+Q/W TRANSPOSE"
        .text "FX1,2,3:SLIDE,PORTA"
.if (MIDI_support!=0)
        .text "FX5,6,7:ADSR, CHORD"
        .text "FX8,$10:VIBRA,TEMPO"
.fi
        .text "-------------------"
        .text " ", format("%d", Year)," HERMIT SOFT. "
        .enc 'none'
helptxend
helpYsize = (helptxend-helptxt)/(helprow-helptxt) ;help-text vertical size

;-----------------------------------------------
        .enc 'screen'
KeyTypes .text "SIDW"   ;default SID-Wizard piano key-layout (2 octaves: Z,X,C,V,.. and Q,W,E,R,T,Y,.. , octave: C=+1..8)
         .text " DMC"   ;FC/GMC/DMC piano key-layout (1 octave: A,S,D,F,..., select octave: 1..8 numerical keys)
         .text " SDI"   ;'SID DUZZ IT' like keyboard-layout (not only piano keys)
         .text "JANK"   ;Janko-keyboard (chromatic approach, invented by Paul von Janko at the end of 1800s)
        .enc 'none'


        .fi             ;end of part that's not included in SID-Maker

;--------------------------------------------------------------------------------------------------------------------------
YesOrNo                 ;wait for Y/N and set Carry accordingly (1 if Y)
        sei
        ldy #0
        sty SIDBASE+$18 ;mute for now
        lda #$e7
        sta $dc00
YNloop  lda $dc01
        cmp #($ff-2)    ;Y key (or I key)?
        bne +
-       cmp $dc01       ;wait to release key
        beq -
        sec
        rts
+       cmp #($ff-$80)  ;N (or V) key?
        bne YNloop      ;key-testing loop
-       cmp $dc01       ;wait to release key
        beq -
        clc
        rts
;----------
CheckSWM                ;check filetype version of SWM module in 1st 4 bytes, if different version, refuse the tune and init SWM1
        ;setting FILE-descriptor (SETLFS in 'iniwfn') and OPEN must have happened before
        ldx #$01        ;file #1
        jsr KERNAL.CHKIN ;call CHKIN to set input file #.
        jsr KERNAL.CHRIN ;skip load-address lo-byte
        jsr KERNAL.CHRIN ;skip load-address hi-byte
        ldx #0
-       jsr KERNAL.CHRIN
        cmp filetyp,x   ;compare opened filetype and version to current tracker's version
        beq +           ;if character matches
        sta readver     ;the mismatching character/version-number of the file
        sec             ;if there was character-mismatch in filetype/version field
        rts
+       inx
        cpx #4          ;summed length of filetype-string and version-character
        bne -
        clc             ;all characters matched in filetype/version field
retchkv rts             ;Carry bit is set to 1 if there's a mismatch

;=======================================DISK OPERATIONS========================================================
        .enc 'screen'
filetx1 .text "FILENAME:             "
        .byte $75       ;the ruler between filename and extension display
        .text "(FILETYPE:*";....
.if (SID_AMOUNT==1)
swftype .text ".SWM)"   ;SW-file type - string to search extension in filenames to filter on tune/instrument
.elsif (SID_AMOUNT==2)
swftype .text ".SWS)" ;'SWS' stands for 'SID-Wizard Stereo'
.elsif (SID_AMOUNT==3)
swftype .text ".SWT)" ;'SWT' stands for 'SID-Wizard Trio'
.fi
        .text SWMversion ;show current module-version in file-dialog
readver .text SWMversion ;the read version-number of selected tune (for loading) - preset to current version
filetx2 .text "DR:   F1,F3:DIR, CTRL:FIELD   FREE:...KB"
        .enc 'none'
dirname .byte "$"       ;$ is the name of disk-directory

;scratch .text "S0:"    ;Scratch-signal (OPEN 15,8,15,"S0:file name":CLOSE 15) - will preceed filename when user want to overwrite a file (Save&Replace "@filename" is buggy in old 1541 drives)
namebuf .fill 16        ;filename-buffer (for extension-checking and for disk-operations)
        .fill 10        ;for safety reasons, if filename gets appended by commands

sizebuf .fill 4         ;(byte 0 and 1) filesize-temp-storage, after reading dir., free blocks (last 2 bytes (row-pointers) not needed)
colutab .byte maxnamelen+2,maxnamelen+1,maxnamelen+1 ;column-tabbing values (to get screenwidth as sum) (14+13+13=40)
filecnt     .byte 0     ;amount of displayed files in directory
dirready    .byte 0     ;value is 1 if directory is fully read
editauth    .byte 0     ;0 - no author info display $ edition, 1 - autor info displayed & editable
filerwindow .byte 0     ;window in file-menu: 0-filename-typer , 1-authorinfo-typer, 2-file-selector
fkeybout    .byte 0     ;keyboard-routine output of filer's keyboard handler - used by main loop to take some actions

;error messages in filer window, 29 character long each
        .enc 'screen'
nodevicep   .text " NO DISK OR DRIVE PRESENT!!! "
fnotfound   .text " FILE NOT FOUND! PRESS SPACE."
;filexists  .text "ERROR! (EXISTS?) PRESS SPACE."
vermismatch .text " WRONG VERSION0! PRESS SPACE."
saveerror   .text " SAVING ERROR 0! PRESS SPACE. "
RyouSurTx   .text "* ARE YOU SURE? *"
RyouSurT2   .text "*  (Y/N) (I/N)  *"
        .enc 'none'

        .if COMPILEDAPP==1
moduledial              ;start file-dialog for .SWM module (Accu preset for filedialog)
        .enc 'screen'
.if (SID_AMOUNT==1)
        lda #"M"        ;for modules
.elsif (SID_AMOUNT==2)
        lda #"S"        ;for Stereo (2SID) modules
.elsif (SID_AMOUNT==3)
        lda #"T"        ;for Trio (3SID) modules
.fi
        .enc 'none'
filedialog              ;input:Accu - extension read directory, typing filename, return filename in 'namebuf'
        sta swftype+3   ;set .SWM (SID-Wizard module) or .SWI (instrument) extension
	lda #0
	sta filecnt
	sta not1st+1
        .fi

reredir sei             ;init ROM $ VICbank and read disk directory to the screen
        .if (COMPILEDAPP==1 && MIDI_support!=0)
        jsr midisubs.closeMIDI
        .fi
        lda #$36        ;enable KERNAL ROM (only)
        sta banksel
        lda $dd00
        ora #%00000011
        sta $dd00
        lda #$17        ;small character set
        sta $d018
        lda #$1b
        sta $d011
        lda #fileBGcolor
        sta $d021
        .if (COMPILEDAPP==1 && SID_AMOUNT>=2)
        jsr SID23Mute
        .fi
        ldy #0
        sty $d020
        sty $d015
        sty $d01a       ;prevent raster-IRQ
        lda $d019
        sty SIDBASE+$18 ;mute for now
-       lda #" "
        sta lowvidram,y
        sta lowvidram+$100,y
        sta lowvidram+$200,y
        sta lowvidram+$300,y
        lda #fileFGcolor
        sta $d800,y
        sta $d900,y
        sta $da00,y
        sta $db00,y
        iny
        bne -
        ldy #39         ;draw UI base
-       lda filetx1,y
        sta lowvidram+0,y ;filename and type (extension)
        lda #$62+$80 ;$78  ;$71 ;$68
        sta lowvidram+40,y ;separator line 1
        sta lowvidram+40*3,y ;separator line 2
        sta lowvidram+40*23,y ;separator line 3
        .if COMPILEDAPP==1
        lda swftype+3   ;extension 3rd letter
        cmp #"i"        ;instrument?
        beq +           ;don't display author info in instrument (might display instrument-name instead)
        .fi
        lda authorinfo,y
        .if COMPILEDAPP==2 ;if SID-Maker is being compiled
        ldx outputformat
        cpx #$ff         ;if starting importer, display import-text
        bne skipaut
        lda importxt,y
        .fi
skipaut sta authorip,y  ;author info and title
+       lda filetx2,y
        sta lowvidram+40*24,y ;diskname and free space
        dey
        bpl -

.if (COMPILEDAPP==1)
	lda defawin+1 ;don't read disk-directory automatically for save-dialog, but by pressing F1 if wished
not1st	ora #selfmod
	ora Config.confbits2 ;Save_AutoDiskDir
	bne readdir
	jmp typesel
.fi

readdir lda #$01        ;filename-length is 1
        ldx #<dirname   ;lo byte pointer to file name. ("$")
        ldy #>dirname   ;hi byte pointer to file name. ("$")
        jsr inifnam     ;init filename memory position and length
        jsr KERNAL.OPEN ;call OPEN
        bcs NOredir     ;if error
        jsr KERNAL.READST
        cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        bne NOredir     ;if error
        ldx #$01        ;file #1 ;if no error (device is present), just go forward
        jsr KERNAL.CHKIN ;call CHKIN to set input file #.
;read and display disk-header
        jsr KERNAL.CHRIN
        cmp #$C3        ;normally first byte is $01, VICE gives $C3 if device is not present
        bne +           ;if Carry remained set, no device is present
NOredir jsr nodevic     ;if error opening device, notify user
        jmp typesel     ;and skip filelist-making, still give a chance to return to e.g. editor and change drive-number
+       ldx #6
-       jsr KERNAL.CHRIN ;read through some unneeded characters (amount given in X)
        dex
        bne -
        ldx #0          ;read diskname & ID
        stx dirready    ;signs if directory fully read
-       jsr KERNAL.CHRIN
        beq listfil     ;check for closing 0
        ora #$80
        cpx #24         ;don't allow too long diskheader to be displayed (crop beginning to max. length)
        bpl +
        sta disknam,x   ;standard 'jsr CHROUT' is not good for special display modes
+       inx
        bne -
;parse and display directory for filesizes and names
listfil jsr clrfsel     ;clear file-selector area, init file-listing variables
flistlo lda filecnt
        cmp #filelstmax ;check displayable filelist max. length
        bcc +
        jmp typesel     ;if reached all displayable content, go to type/select name or read next dir. page
+       ldx #4-1        ;file-lister loop
-       jsr KERNAL.CHRIN ;get filesize (last one will be filesize in blocks)
        sta sizebuf,x   ;sizebuf might be displayed or filesizes stored in a filesize-list to help user calculate
        dex
        bpl -
-       jsr KERNAL.CHRIN ;check for beginning of filename (1st " delimiter)
        cmp #RETURNcode ;return/enter signal - check for end of dir/filelist
        beq closdir     ;if end of filelist, close device
        cmp #namesep
        bne -           ;search for left " filename-delimiter
        ldy #0          ;start checking&copying filename
-       jsr KERNAL.CHRIN
        cmp #RETURNcode ;return/enter signal - check for end of dir/filelist
        beq closdir     ;if end of filelist, close device
        cmp #namesep    ;check for filename end (right " filename-delimiter)
        beq frowend     ;find $00 row-delimiter
        sta namebuf,y   ;loads current filename to buffer
        iny
        cpy #16+1       ;maximum allowed filename-length for C64 devices
        bne -
frowend jsr KERNAL.CHRIN ;check for $00 - end of line of filename, then comes beginning of next line pointer and filesize/freeblocks info
        bne frowend
        dey             ;check name for .SWM/.SWI extension
        ldx #extlen-1   ;check 4 character long (with dot) filename-extension to filter on filetypes (.SWM/.SWI)
-       lda namebuf,y
        cmp swftype,x
        bne flistlo     ;if there was any difference between extension and end of filename-string, don't display filename
        dey
        bmi flistlo     ;if filename is empty (only extension), no point to display, go to next filename
        dex
        bpl -
dispnam lda namebuf,y
        cmp #$20        ;is last character SPACE? -then sign it with a delimiter character
        bne dispnlp     ;if not SPACE, simply display filename
        lda #$20+$40    ;the invisible space will sign real end of 'space-ended' filename to loader/saver routine
        bne +           ;jump
dispnlp lda namebuf,y   ;filename-displayer loop
+       sta (compzptr),y
        dey
        bpl dispnlp
        inc filecnt     ;count displayed filenames
nextnam jsr cycfcol     ;cycle filer-column counter, Y output is tabbing amount ;go to next screenplace for next filename
        jmp flistlo     ;back to read next line

closdir inc dirready    ;sign that directory fully read
        lda sizebuf+1   ;display free kilobytes on disk in decimal format (divide blocks by 4)
        lsr sizebuf+0   ;divide by 2
        ror
        lsr sizebuf+0   ;divide by 2 (4 in sequence)
        ror
        ldx #100        ;convert
        jsr divideX
        sty diskfree+0
        ldx #10
        jsr divideX
        sty diskfree+1
        tax
        .if COMPILEDAPP==1
        lda commonsubs.hexchar,x
        .else
        lda hexchar,x
        .fi
        sta diskfree+2
        jsr clsfile

;-------type filename or author info / select file to load/save--------------
typesel sei             ;type/select filename & description
        lda #$35
        sta banksel
        lda #$fe
        sta $dc00
-       lda $dc01       ;wait for keys to be released, especially RETURN
        cmp #$ff
        bne -
        lda #2          ;set filename-typer mode in key-handler
        sta namemode
defawin lda #selfmod    ;default filer-window (selector is preferred in case of loader)
        beq +
        jsr setsele     ;set filename-selector subwindow as default for loader
        jmp fkeylop
+       jsr setfnam     ;set filename-typer subwindow by default
fkeylop lda #$ff
        cmp $d012       ;constant 50Hz timing loop for keyboard-handler and cursor-displayer
        bne *-3
        lda #0
        sta fkeybout    ;reset keyhandler output
        jsr keyhandler.keyer ;handles key-repeating and name-mode
        jsr disfcur     ;display filer-cursors
        lda fkeybout    ;keyhandler-output
        cmp #$f8        ;F1 key check
        bne +           ;if F1 pressed, re-read directory
        .if (COMPILEDAPP==1)
        sta not1st+1
        .fi
        jsr regauth     ;clsfile ;close dir., save typed author-info to re-read filelist
        jmp reredir     ;readdir ;re-read directory/file-list
+       cmp #$f9        ;F3/UP(PageDown) key check
        bne +           ;if F3/Pagedown pressed, flip to next page of disk-directory (if exists)
        lda dirready    ;check, if directory fully read
        bne lopover     ;if directory read entirely, no further paging
        jmp listfil     ;further read filelist
+       cmp #$fe        ;if stop/esc pressed, returning to editor (abort file-operation)
        bne +           ;$fe/$ff
        jsr regauth     ;clsfile ;close directory-file - register author-text
        sec             ;cause file-dialog abort in main routine (restart editor)
        rts
+       cmp #$ff        ;if RETURN/Enter pressed, take action (load/save)
        bne lopover
        ldy filerwindow
        cpy #1
        beq +
        jsr regname     ;register filename in 'namebuf' and author-info
        clc             ;cause load/save in main routine, if return pressed window 1 or 2 (filename/file-selector)
        rts
+       jsr setfnam     ;if window 1, go back to filename-typer
lopover jmp fkeylop

;----------------------------------file-dialog subroutines----------------------------------
iniwfn  lda #$36        ;init workfile name pointer and length, bank, and device,file,channel
        sta banksel
bufnlen lda #selfmod    ;self-written code - length of selected/typed filename
        ldx #<namebuf
        ldy #>namebuf
inifnam jsr KERNAL.SETNAM ;init filename pointer and length, and device,file,channel
devicen ldx #ddevice    ;self-written, device # 8 is default (modifiable by user)
        txa
        jsr decdis2
        sty $07c3
        sta $07c4
        lda #1          ;file # 1
        ldy #0          ;channel # 0 - (in case of LOAD: if 1, load to address automatically, if 0, address can be given in X/Y)
        jmp KERNAL.SETLFS

;---------------------------------------------------------------
clrfsel                 ;clear file-selector area, init file-listing variables
        lda #$36
        sta banksel
        lda #<dirvmem
        ldx #>dirvmem
        jsr inicptr     ;set video-pointers
        ldx #0          ;clear filelist-area
        stx filecnt     ;counts displayed filenames
        stx compzvar3   ;counts columns
fsclrlp ldy #40-2
        lda #$20        ;Space character
-       sta (decozptr),y
        dey
        bpl -
        lda #40         ;one row forward
        jsr incptrs     ;increase decozptr  ;iny -> Y=0 -> preserve compzptr
        cpx #filelistheight
        bne fsclrlp
        rts
;-----------------------------------------------------------------------------
filenamer               ;handles filename-typing/selection (called from keyer/nametyper routine in keyhandler.inc, char passed in Accu, keycode in Y)
        sta fkeybout    ;for main routine
        cmp #$fa        ;Control/TAB keys - cycle filer sub-windows
        bne filkeys
        ldy filerwindow
        bne +           ;if filename-subwindow
        iny             ;set Y to 1
        lda editauth    ;if loader, no author-info editing, skipping that
        beq setsele     ;if authorinfo editing disabled (=0) go straight to selector
        lda #authorilen-1
        sta compzvar4   ;length of input-field
        lda #<authorip
        ldx #>authorip
        jmp setyfld     ;set typer field
+       cpy #1
        beq setsele     ;if 1 set filename-selector subwindow
        clc
        jsr diselcu     ;first clear selector-cursor
        jmp setfnam     ;then cycle back to filename-typer window
setsele lda filecnt     ;set file-selector
        beq setfnam     ;if filelist is empty, don't go to selector, go back to / stay at filename-typer instead
        lda #<dirvmem
        ldx #>dirvmem
        jsr inicptr     ;set video-pointers
        sec             ;set display-mode for cursor
        jsr diselcu     ;display file-selector cursor
        ldy #2          ;set to 2
        bne setyfld     ;jump
setfnam lda #maxnamelen-1
        sta compzvar4   ;length of input-field
        lda #<filenamp
        ldx #>filenamp
        ldy #0
setyfld sty filerwindow ;branched to here if filer-window was 2
setyfl2 jsr inidptr     ;takes A , X
        lda #0
        sta compzvar2   ;reset cursor-position
        sta compzvar3   ;counts filename-selector columns
        rts

filkeys ldx filerwindow ;handle keys depending on subwindow---------------------------------
        cpx #2
        bne typefld     ;if not file-selector window, go to typer ;if file-selector subwindow, use cursor keys and Return only
        cmp #$fb        ;cursor up/down
        bne +
        jsr fiselLR     ;3 columns
        jsr fiselLR
        jmp fiselLR
+       cmp #$fc        ;cursor left/right
        bne endfiln
fiselLR clc             ;clear cursor
        jsr diselcu     ;clear file-selector-cursor
        jsr fiselec     ;change cursor-pointer if possible
        sec             ;draw cursor
        jmp diselcu     ;draw file-selector-cursor
fiselec lda shiftsi     ;the effective algorithm itself to select file
        bne fselbwd
fselfwd ldy compzvar2
        iny
        cpy filecnt
        bcs endfiln
        inc compzvar2
cycfcol inc compzvar3   ;column-tabbing counter
        ldx compzvar3
        cpx #namecolums
        bne +
        ldx #0
        stx compzvar3
+       ldy colutab,x
        jmp incompt
fselbwd lda compzvar2   ;filename-count
        beq endfiln
        dec compzvar2
        ldx compzvar3
        ldy colutab,x
        dec compzvar3   ;cycle filer-column counter
        bpl +           ;if column-turnaround, compensate non-integer divisibility of screen width per filename-columns
        ldx #namecolums-1
        stx compzvar3
+       jmp decptrs

typefld ldy compzvar2   ;handle typing fields------------------------------------------------
        cmp #$f5
        bne +           ;Home-key?
        lda #0
        sta compzvar2
        rts
+       cmp #$f7        ;insert?
        beq +
        cmp #$fd        ;backspace?
        bne cursidw
        lda shiftsi     ;Insert? (Shift+Backspace)
        beq bkspclp
+       ldy compzvar4
-       cpy compzvar2
        beq clrcurp
        dey
        lda (decozptr),y ;shift letters forward
        iny
        sta (decozptr),y
        dey
        bpl -
clrcurp lda #$20
        sta (decozptr),y
endfiln rts
bkspclp jsr fcurbwd
        ldy compzvar2
-       cpy compzvar4
        beq clrcurp     ;BACKSPACE character to last cursorplace
        iny
        lda (decozptr),y ;shift letters back
        dey
        sta (decozptr),y
        iny
        jmp -
cursidw cmp #$fc        ;cursor-sideways keys?
        bne norchar
        lda shiftsi
        beq fcurfwd
fcurbwd cpy #0          ;move cursor backwards in name-field
        beq endfiln
        dec compzvar2
        rts
norchar cmp #0
        bmi endfiln     ;avoid typing characters for remaining control characters
        sta (decozptr),y ;normal character branch
fcurfwd cpy compzvar4   ;move cursor forward (if possible)
        bpl endfiln
        inc compzvar2
        rts

;--------------------------------------------------------------------------------
disfcur                 ;display cursor over typed data
        jsr clrcurs     ;clear type-fields from cursor invertions
        lda filerwindow
        cmp #2          ;selector?
        beq +
        ldy compzvar2   ;display text-typer cursor
        lda (decozptr),y
        ora #$80
        sta (decozptr),y
+        rts
diselcu ldy #maxnamelen-1 ;display/clear file-selector cursor (action depends on Carry-bit, displays if Carry=1, clears if Carry=0)
-       lda (compzptr),y
        and #$7f        ;clear cursor
        bcc +           ;if Carry bit set
        sta filenamp,y
        ora #$80        ;set cursor
+       sta (compzptr),y
        dey
        bpl -
        rts
clrcurs ldy #maxnamelen-1 ;clear type-fields from cursor invertions
-       lda filenamp,y
        and #$7f
        sta filenamp,y
        dey
        bpl -
        ldy #authorilen-1 ;clear cursor
-       lda authorip,y
        and #$7f
        sta authorip,y
        dey
        bpl -
        rts

;--------------------------
regname jsr clrcurs     ;start by clearing type-fields from cursor invertions
        ldy #maxnamelen-1 ;copy/register typed/selected filename and author-info (latter only if editauth is on
-       lda filenamp,y  ;loop search back to first non-Space character
        cmp #$20        ;check for spaces (if filename is 'spaced;, $60 Space will be noticed)
        bne +           ;if not space coming, we found end of filename (without extension), and we can add the extension to it
        dey
        bpl -           ;if not looped here, empty filename
+       iny
        sty compzvar3   ;copy filename before last space
        beq +           ;zero filename
        ldy #0
-       lda filenamp,y
        sta namebuf,y
        iny
        cpy compzvar3   ;stored length of filename
        bne -
+       ldx #0          ;add extension
-       lda swftype,x   ;read extension-string
        sta namebuf,y   ;add it to namebuf.
        iny
        inx
        cpx #extlen
        bne -
        sty bufnlen+1   ;register buffered name's length
regauth lda editauth
        beq clsfile     ;if author-info being edited, register it in tune-header
        ldy #authorilen-1 ;hardwired author-info length
-       lda authorip,y
        sta authorinfo,y
        dey
        bpl -
clsfile lda #$36
        sta banksel
        lda #1          ;file # to close
        jsr KERNAL.CLOSE ;so close it
        jmp KERNAL.CLRCHN ;clear all channels

nodevic                 ;if OPEN failed, warn user and ask for retrial
        ldy #28         ;display 'device not present' (and later ask for overwrite-confirmation)
-       lda nodevicep,y
        sta lowvidram+40*24,y
        dey
        bpl -
        rts             ;jmp waitspace

saverror .proc
        ldy #28         ;Display 'save error' message and wait for SPACE
-       lda saveerror,y
        sta lowvidram+40*24,y
        dey
        bpl -
        jsr KERNAL.READST ;read status error-code
        clc
        adc #"0"        ;shift numbering for status-number display to be PETSCII compatible
        sta lowvidram+40*24+14 ;display status error-code
        jmp waitspace ;wait for SPACE key press/release
        .pend

;fexists .proc          ;display 'file exists' (and later ask for overwrite-confirmation)
;        ldy #28
;-        lda filexists,y
;        sta lowvidram+40*24,y
;        dey
;        bpl -
;        jmp waitspace
;        .pend

vermism .proc           ;display 'version mismatch' message 
        lda readver
        cmp #$0d        ;if $0d (enter) is read it's not found file instead
        beq filenot
        jsr clsfile     ;in case of version-mismatch, close to allow further trials
        ldy #28         ;if carry was 1, filetype or version is not OK!
-       lda vermismatch,y
        sta lowvidram+40*24,y
        dey
        bpl -
        lda readver     ;display selected file's version or 1st mismatching character
        sta lowvidram+40*24+14
        jmp waitspace
        .pend

filenot .proc
        jsr clsfile     ;in case of error, close to allow further trials
        ldy #28         ;Display 'file not found' message and wait for SPACE
-       lda fnotfound,y
        sta lowvidram+40*24,y
        dey
        bpl -
        jmp waitspace
        .pend

waitspace .proc         ;wait for SPACE press and release
        sei
        lda #$7f
        sta $dc00
-       lda $dc01       ;wait for pressing SPACE
        cmp #$ff
        beq -
-       lda $dc01       ;wait to release SPACE
        cmp #$ff
        bne -
        rts
        .pend

decdis2 .proc           ;decimal display - input:accu, output: in Accu and Y-register
        ldy #$2f
        sec
-       iny
        sbc #10
        bcs -
        adc #$3a
        rts
        .pend
;=============================================================================
;----------------------------------------------------------------------------------
nametyper               ;subroutine which handles nametyping (instrument-naming or file-naming)
        lsr
        tay
        jsr keyhandler.repeatex
        lda keychar,y
        beq inmret1     ;no process if 0

        cpx #2          ;check if file-dialog is opened
        bne +
        jmp filenamer   ;jump filename-typer (in menu.inc)
+
        .if COMPILEDAPP==1
        cmp #$fb        ;cursor up/down (down to instrument-settings in editor)
        bne +
        ldx shiftsi
        beq ++
+       cmp #$fe        ;signs if stop/esc pressed, returning to normal editing mode
        bcs +
        jmp typefld     ;typing instrument-name
+       ldx #0
        stx namemode    ;escape instrument-name typing without saving the result
        inc display.insrefr+1 ;display original instrument name
        cmp #$ff        ;if this is Return, store the name
        bne inmret1
storina ;store to instrument
        jsr commonsubs.setinsptr ;load selected instrumet's basepointer to zeropage pointers
        ldy #maxinstsize-instnamelength ;display instrument-name
        ldx #0
-       lda vidram+$20,x
        sta (inszptr),y
        iny
        inx
        cpx #instnamelength
        bne -
        .fi
inmret1 rts

;keyboard-charactertable for naming                             ;- 01 - 02 - 04 - 08 - 10 - 20 - 40 - 80 ---
keychar .section data
        .byte $fd,$ff   ,$fc   ,$fe  ,$f8   ,$f9  ,$fe    ,$fb;;; del |retu|crri| F7 | F1 | F3 | F5 |crdn|fe
        .byte 51 ,23+64 ,1+64  ,52   ,26+64 ,19+64 ,5+64  ,$f6;;;  3  | W  | A  | 4  | Z  | S  | E  |lSH |fd
        .byte 53 ,18+64 ,4+64  ,54   ,3+64  ,6+64  ,20+64 ,24+64;  5  | R  | D  | 6  | C  | F  | T  | X  |fb
        .byte 55 ,25+64 ,7+64  ,56   ,2+64  ,8+64  ,21+64 ,22+64;  7  | Y  | G  | 8  | B  | H  | U  | V  |f7
        .byte 57 ,9+64  ,10+64 ,48   ,13+64 ,11+64 ,15+64 ,14+64;  9  | I  | J  | 0  | M  | K  | O  | N  |ef
        .byte 43 ,16+64 ,12+64 ,45   ,46    ,$3a   ,$00   ,$2c;;;  +  | P  | L  | -  | .  | :  | @  | ,  |df
        .byte $f7,0     ,$3b   ,$f5  ,$f6   ,$3d   ,$f9   ,$2f;;;pound| *  | ;  |HOME|rSH | =  | up | /  |bf
        .byte 49 ,$fe   ,$fa   ,50   ,32    ,0     ,17+64 ,$fe;;;  1  |back|CTRL| 2  |SPC | C= | Q  |stop|7f
        .send data

;------------------------------------------------------
.if (SID_AMOUNT>=2)

 .if (COMPILEDAPP==1)
SID2add .word defaultSID2BASE ;$d420/$d500/etc. ;2nd SID's base-address
SID23set #SID23setter
  .if (SID_AMOUNT>=3)
SID3add .word defaultSID3BASE ;$d420/$d500/etc. ;3rd SID's base-address        
  .fi
 .fi

SID23Mute lda SID2add+0
        sta SID2Mut+1
        lda SID2add+1
        sta SID2Mut+2
 .if (SID_AMOUNT>=3)
        lda SID3add+0
        sta SID3Mut+1
        lda SID3add+1
        sta SID3Mut+2
 .fi
        ldy #$18
        lda #0
SID2Mut sta selfmodA,y
 .if (SID_AMOUNT>=3)
SID3Mut sta selfmodA,y
 .fi
        rts

.fi


;----------------------------------------------------------
        .bend ;end of 'menu' block

;=====================================================================================
; vim: sw=4 ts=4 syntax=asm:
