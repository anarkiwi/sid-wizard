;SID-WIZARD SID-Maker (makes PRG/SID from SWM workfile)
;----------------------------------------------------------

;=====================================CONSTANTS==========================================
;CAUTION: SEQUENCE/PATTERN/TABLE DELIMITERS ($FE/$FF/$7E/$7F,TEMPOTABLE BIT7) ARE HARDWIRED, NEVER EVER MODIFY THEM!!!

COMPILEDAPP = 2

        .include "settings.cfg";global constants to fine-tune the maximal memory occupation of parts of tune data

.if (SID_AMOUNT>=2)
SID2aID=normalPlayer.player.SID2ADD_ID
SID2add=$03fe ;menu.SID2add
.fi
.if (SID_AMOUNT>=3)
SID3aID=normalPlayer.player.SID3ADD_ID
SID3add=$03fc ;menu.SID3add
.fi
.if (SID_AMOUNT>=4)
SID4aID=normalPlayer.player.SID4ADD_ID
SID4add=$03fa ;menu.SID4add
.fi
hexdisp=(hexdis0-BASICbe)+EXEstarter
hexchar=(hexcha0-BASICbe)+EXEstarter

        .dsection declaration

plusminus .segment
        lda #$80
        cmp $d012       ;wait for retrace
        bne *-3
        lda #$ff-($01+$20+$80)
        sta $dc00
        lda $dc01
chkplus cmp #$ff-$01    ;plus-key?
        bne chminus
        lda repecnt     ;keyrepeat-counter
        bne +           ;check, if repeat counter is 0
        lda #\5         ;default repeat-speed 2 for relocation-setting
        sta repecnt     ;if 0, reset counter to repeatspeed 2
        rts
+       jsr \4
        jsr \1
        jmp dispval
chminus cmp #$ff-$08    ;minus-key?
        bne chkret2
        lda repecnt     ;keyrepeat-counter
        bne +           ;check, if repeat counter is 0
        lda #\5         ;default repeat-speed 2 for relocation-setting
        sta repecnt     ;if 0, reset counter to repeatspeed 2
        rts
+       jsr \4
        jsr \2
dispval jmp \3          ;display hexa relocation target address (or SID2/SID3 address)
chkret2 lda $dc01
        cmp #$ff-$02
        beq +
chspace lda $dc01
        cmp #$ff-$10
        bne ++
+       pla  ;added by Soci - retrieve stack-pointer after subroutine-call - jump one level back
        pla
        jmp endplmi
+
.endm

plmiret .macro ;plus/minus/ return/space handler macro
        #plusminus \1,\2,\3,\4,\5
nokeypr lda #repspd1    ;no pressed key, repeatcounter resets
        sta repecnt
        rts
endplmi ;end of 'plmiret' macro
.endm

plmiret2 .macro ;plus/minus/left/right return/space handler macro
        #plusminus \1,\2,\3,\4,\5
        ;.if (SID3addInc!=selfmodA) ;for SID3 address-setting in exported executable
chcurri lda #$ff-($01+$02+$40)
        sta $dc00
        lda $dc01
        and #$04
        bne nokeypr
        lda repecnt     ;keyrepeat-counter
        bne +           ;check, if repeat counter is 0
        lda #\5         ;default repeat-speed 2 for relocation-setting
        sta repecnt     ;if 0, reset counter to repeatspeed 2
        rts
+       jsr \4
        lda $dc01
        and #$10+$80
        cmp #$90
        bne +
        jsr \6
        jmp dispval
+       jsr \7
        jmp dispval
        ;.fi
nokeypr lda #repspd1    ;no pressed key, repeatcounter resets
        sta repecnt
        rts
endplmi ;end of 'plmiret2' macro
.endm

plmiret3 .macro ;plus/minus/left/right/up/down return/space handler macro
        #plusminus \1,\2,\3,\4,\5
        ;.if (SID3addInc!=selfmodA) ;for SID3 address-setting in exported executable
chcurrd lda #$ff-($01+$02+$40) ;/right/down?
        sta $dc00
        lda $dc01
        and #$84
        cmp #$84
        beq nokeypr
        lda repecnt     ;keyrepeat-counter
        bne +           ;check, if repeat counter is 0
        lda #\5         ;default repeat-speed 2 for relocation-setting
        sta repecnt     ;if 0, reset counter to repeatspeed 2
        rts
+       jsr \4
        lda $dc01
        and #$04
        bne updown
lftrght lda $dc01
        and #$10 ;+$80
        ;cmp #$90
        beq + ;bne +
        jsr \6
        jmp dispval
+       jsr \7
        jmp dispval
updown  lda $dc01
        and #$10
        bne +
        jsr \8
        jmp dispval
+       jsr \9
        jmp dispval
        ;.fi
nokeypr lda #repspd1    ;no pressed key, repeatcounter resets
        sta repecnt
        rts
endplmi ;end of 'plmiret2' macro
.endm


;================================= BASIC area start ===============================================================
        *= BASIC_STARTADDRESS ;$0801
        .word ss,Year
        .null $9e,^start;Sys 2064
ss      .word 0

;================================= Code part 1 ================================
        *= $0810        ;INITIALIZATION AT PROGRAM START - set IRQ handlers, screen, and main VIC registers
start   sei
        lda $ba         ;the default device might be where SID-Maker was loaded from
        cmp #13+1       ;if above 13, force 8 as default
        bcs +
        cmp #8
        bcs ++          ;if below 16 and greater than / equal to 8, no forcing, leave as it is
+       lda #ddevice    ;default device number if exporter was loaded from non-disk drive
+       sta menu.devicen+1
        lda #$35
        sta banksel
.if (KERNAL_LOADSAVE!=0)
        lda #<KERNAL.LOAD2 ;disable turbo for Final Cartridge III as it seems to clash with programs above $F000
        ldx #>KERNAL.LOAD2
        sta KERNAL.ILOAD+0
        stx KERNAL.ILOAD+1
        lda #<KERNAL.SAVE2
        ldx #>KERNAL.SAVE2
        sta KERNAL.ISAVE+0
        stx KERNAL.ISAVE+1
.fi
imports lda #$ff        ;initialize output-format to a unique value
        sta outputformat
        lda #>PLAYERADDR
        sta relocaddr   ;reset reloc.address (to be safe about next tune being in range
        ldx #2          ;set SID-Wizard module extension to import
-       lda SWMext,x
        sta menu.swftype+1,x
        dex
        bpl -
        lda #2
        sta menu.defawin+1 ;file-selector is preferred default subwindow in filer (but changeable with TAB)
        jsr menu.reredir   ;read directory, typing filename, return filename in 'namebuf'
        bcs imports        ;if Carry=1 on exit, restart file-handler (no returning to anywhere, like in tracker)
;load tune (notify if unsuccessful) - fastloader may fit in memory (and could be selected with Shift+Return)
        jsr menu.iniwfn ;(secondary address inside = 0 ensures load-address forcing)
        jsr KERNAL.OPEN ;open file for checking version in its beginning
        jsr KERNAL.READST ;read drive status
        cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        beq ChSWVer     ;check if file exists, if exists, go to check filetype/version
        bne fnotfnd ;=jmp fnotfnd
ChSWVer jsr menu.CheckSWM ;check filetype version of SWM module in 1st 4 bytes, if different version, refuse the tune and init SWM1
        bcc SWverOK     ;if carry is 0, no version mismatch, go to loader
        jsr menu.vermism ;display version-mismatch
        jmp backfil
SWverOK jsr menu.clsfile ;quit testing, start fresh loading
        jsr menu.iniwfn ;(secondary address inside = 0 ensures load-address forcing)
+       ldx #<MUSICDATA ;set musicdata base load-address just after player-code
        ldy #>MUSICDATA
        lda #$00        ; $00 means: load to memory (not verify)
        jsr KERNAL.LOAD ; call LOAD, outputs last loaded address to X/Y in case of success
        bcc +           ; if carry set, a load error has happened
fnotfnd jsr menu.filenot ;display file not found and wait for SPACE
backfil jmp imports
+       stx compzptr    ;end of data is important info for depacker to start in-place backwards-decompression
        sty compzptr+1

;prepare tunedata (crop unneeded module-data) and set player absolute addresses accordingly, set SID header
preptune                ;tune-data loaded, compzptr was set to end of loaded data
        sei
        lda #$35
        sta banksel     ;ensure consistent variable and tune-data area

        jsr setexport1  ;ask user for input (export-type, player-type, machine-type)

        jsr setEXEdetails

.if(SWP_EXPORT!=0) ;turn zeropage-pointer values of subtune/pattern/instrument/chordtable/temptable into SWP-offset-positions
        lda MUSICDATA+DRIVERTYPE_POS ;set player-setter table addresses
        asl
        tax
        lda altplayers.PlrValP_SWP+0,x
        sta datzptr+0
        lda altplayers.PlrValP_SWP+1,x
        sta datzptr+1
        ldy #0
-       lda (datzptr),y
        beq doneSWP ;end at value 0
        ldx #$FF ;default (invalid)
        cmp #subtuneadd
        bne +
        ldx #SWP_SUBTUNES_OFFS
+       cmp #ptnptloadd
        bne +
        ldx #SWP_PTNPTL_OFFS
+       cmp #ptnpthiadd
        bne +
        ldx #SWP_PTNPTH_OFFS
+       cmp #insptloadd
        bne +
        ldx #SWP_INSPTL_OFFS
+       cmp #inspthiadd
        bne +
        ldx #SWP_INSPTH_OFFS
+       cmp #chordtbadd
        bne +
        ldx #SWP_CHORDTB_OFFS
+       cmp #chordptadd
        bne +
        ldx #SWP_CHORDPT_OFFS
+       cmp #tempotbadd
        bne +
        ldx #SWP_TEMPOTB_OFFS
+       cmp #tempoptadd
        bne +
        ldx #SWP_TEMPOPT_OFFS
+       txa
        sta (datzptr),y
        iny
        iny
        bne -
doneSWP
.fi

        jsr preparetunedata ;exits with compzptr pointing to end of data

        jsr setexport2  ;ask user about relocation-address and SID-type if needed (for SID export)

        .if (ECHO_SUPPORT==1)
         jsr setEXEdetails2  ;apply user-modified echo-delay/etc to EXEstarter
        .fi

        jsr setplayer   ;adjust player routine's absolute addresses

        jsr SetSIDply   ;set SID player-pointers and player-type (CIA/video depending on multi-speed)
        jsr SetSIDt     ;set SID-text

        lda outputformat
        cmp #2          ;if EXEcutable, no need for tune-relocation
        beq +
        lda relocaddr   ;if no relocation needed
        cmp #>PLAYERADDR
        beq +
        jsr relocator   ;RELOCATE logical address of player!!!
+

exporter                ;convert/export (save) tune
        lda outputformat;set extension for output-format
        asl
        adc outputformat
        tax             ;accu multiplied by 3
        ldy #0
-       lda exportext,x
        sta menu.swftype+1,y
        inx
        iny
        cpy #3          ;extension-length is 3
        bne -


;----------------------------------------
savexpo sei
        ;lda #$35
        ;sta banksel
        lda #0
        sta menu.defawin+1 ;filename-typer is default subwindow in filer (but changeable with TAB)
        .if (EMULATOR!=0)
         sta menu.not1st+1
        .fi
-       jsr menu.reredir ;read directory, typing filename, return filename in 'namebuf'
        bcc ++          ;if user is exiting from export-filedialog
        lda relocaddr   ;check if there was no relocation
        cmp #>PLAYERADDR
        beq +           ;if it was normal save
        jsr unrelocate  ;always a new copy of player so unrelocate is not needed
+       jmp imports     ;start all over again (importing) if user exits exporter
+
.if (SID_AMOUNT>=4)
        jmp setsaveparam
.fi

SID4upper .segment
;.if (SID_AMOUNT>=4)
; SID3upper .segment
;.fi

;save tune (check and ask about existing file or any saving error?)
setsaveparam            ;set saving range and filetype
        lda outputformat
        bne +           ;C64 native code?

;0.Saving in native C64 PRG format (with load-address)
ExpoC64
.if (SWP_EXPORT != 0)
        lda outputformat
        ;cmp #0 ;SWP-player (driver) ?
        bne +
        lda SWPdatL+1
        sta expoendadd+0
        lda SWPdatH+1
        sta expoendadd+1
.fi
        lda relocaddr   ;if no relocation needed
        cmp #>PLAYERADDR
        bne reloced     ;if tune is to be relocated
        jmp NorSave     ;normal save (compatible mode for saving - without relocation
reloced jsr appendPRGW  ;append PRG file Writing string to filename
        lda #<PLAYERADDR ;Set Start Address
        ldx #>PLAYERADDR
        bne SaveExp ;=jmp SaveExp

+       cmp #1          ;Binary native code without 2-byte load-address?
        bne +



;1.Saving in native C64 RAW (BIN) binary format (raw file without load-address)
ExpoBIN jsr appendSEQW  ;append SEQ file Writing string to filename
.if (SWP_EXPORT == 0)
        lda #<PLAYERADDR ;Set Start Address
        ldx #>PLAYERADDR
        bne SaveExp ;=jmp SaveExp
.else
         jmp NorSave
.fi
+
.if (ECHO_SUPPORT!=1)
        cmp #2          ;Executable file for C64 command prompt?
        bne +
ExpoEXE ;2.Saving in runnable C64 PRG file format
.fi
        lda expoendadd+0 ;set EXEcutable code's pointers for copying tune-data
        sta (EXEtgtL+1-BASICbe)+EXEstarter
        sec
        sbc #<(SIDHEADER-(BASICbe+EXEstarterSize))
        sta (EXEsrcL+1-BASICbe)+EXEstarter
        lda expoendadd+1
        sta (EXEtgtH+1-BASICbe)+EXEstarter
        sbc #>(SIDHEADER-(BASICbe+EXEstarterSize)) ;involve Carry-flag
        sta (EXEsrcH+1-BASICbe)+EXEstarter
.if (ECHO_SUPPORT==1)
        lda outputformat
        cmp #2          ;Executable file for C64 command prompt?
        bne +
.fi
.if (SID_AMOUNT<4 && ECHO_SUPPORT!=1)
        ldy normalPlayer.player.FRAME_SPD ;TUNEHEADER+FSPEEDPOS;framespeed of loaded tune (in header)
        lda frmspds,y   ;check framespeed-program lookup table in datatables.inc
        sta fspdsto+1
        dey
        beq appendn     ;if 1x single-speed, don't disturb table
-       tya
        clc
fspdsto adc #0          ;self-written - add base-position of actual rasterspeed
        tax
        lda PALfrRow,x  ;get actual frameraster from table
        sta (EXEframesPAL-BASICbe)+EXEstarter,y;graving into the executable-code
        lda NTSCfrRow,x
        sta (EXEframesNTSC-BASICbe)+EXEstarter,y
        dey
        bne -           ;place 0 never changes (basic position of single-speed player routine)
.fi
appendn jsr appendPRGW  ;append PRG file Writing string to filename
        lda #<EXEstarter ;Set Start Address
        ldx #>EXEstarter
        bne SaveExp ;=jmp SaveExp

+       cmp #3          ;SID binary file? With/without load-address?
        bne +
;3.Saving in SID binary (SEQ) format
ExpoSID jsr appendSEQW  ;append SEQ file Writing string to filename
        lda #<SIDHEADER ;Set Start Address
        ldx #>SIDHEADER
        bne SaveExp ;=jmp SaveExp

+       jmp imports     ;if illegal format was selected somehow

;Common point for saving
SaveExp jsr inidptr     ;set decozptr as startaddress (by A and X)
        ;init workfile name pointer and length, bank, and device,file,channel
        lda #$36        ;init workfile name pointer and length, bank, and device,file,channel
        sta banksel
        .if (SID_AMOUNT>=3)
         jmp SaveEx2
        .fi
.endm ;end of exporter macro 'SID4upper'

.if (SID_AMOUNT<4)
        #SID4upper
.fi


SID3upper .segment

SaveEx2 lda bufnln2     ;appended filename-length
        ldx #<menu.namebuf ;point to appended (,S,W or ,P,W) filename
        ldy #>menu.namebuf
        jsr KERNAL.SETNAM ;init filename pointer and length, and device,file,channel
        lda #1          ; file # 1
        ldx menu.devicen+1 ;set in the beginning of the program, device # 8 is default (modifiable by user)
        ldy #2          ; channel # 2 ;this can be used for SEQ file formats
        jsr KERNAL.SETLFS ; - call setlfs
        jsr KERNAL.OPEN ;open the file for writing
        bcc OKtoSav     ;check if file exists, if exists, go forward with saving

SavErr  lda outputformat
        bne +           ;if native output, try normal SAVE to keep compatibility
        jsr menu.clsfile ;need a fresh start
        jmp NorSave     ;compatible saving mode
+       cmp #3          ;if SID-saving, tolarate error by saving SID with load-address
        bne +
        jsr menu.clsfile ;need a fresh start
        jsr menu.iniwfn ;init workfile name pointer and length, bank, and device,file,channel
        lda #<SIDHEADER ;Set Start Address
        ldx #>SIDHEADER
        jmp NorSav2     ;try with compatible SAVE
+       jsr menu.saverror ;display save-error and wait for keypress
        jsr menu.clsfile ;in case of error, close to allow further trials
        jmp savexpo     ;try again when saving error occurs

        ;perform the saving if device is present and file could be opened for writing
OKtoSav ldx #$01        ; file #1
        jsr KERNAL.CHKOUT ; - call chkout to set output file #.
        lda outputformat ;check if load-address should be written into the file (PRG)
        bne +           ;C64.PRG?
        lda #<PLAYERADDR
        jsr KERNAL.CHROUT ;address lo-byte
        lda relocaddr
        jsr KERNAL.CHROUT ;address hi-byte
        jmp SaveDat
+       cmp #2          ;EXE.PRG?
        bne chkfsp1
        lda #<BASIC_STARTADDRESS ;$01        ;set C64 BASIC-program start address $0801 to run standalone
        jsr KERNAL.CHROUT
        lda #>BASIC_STARTADDRESS ;$08
        jsr KERNAL.CHROUT
        jmp SaveDat
chkfsp1 cmp #3          ;SID.SEQ?
        bne SaveDat
        ldy normalPlayer.player.FRAME_SPD ;framespeed of loaded tune (in header)
        cpy #1          ;if framespeed is 1 don't use CIA starter code
        bne SaveDat     ;if framespeed is not 1, save normally
        ;1x SID is handled differently here (header modified & CIA-starter code left out from saving)
        ldy #0          ;write SID-header to disk ($7C bytes)
-       jsr KERNAL.READST
        ;cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        bne SavErr
        lda SIDHEADER,y
        jsr KERNAL.CHROUT ;write byte to disk
        iny
        cpy #SIDHEADERSIZE
        bne -
        lda #<(ECHO_SUPPORT==1? BASIC_STARTADDRESS : PLAYERADDR) ;write load-address to SID
        jsr KERNAL.CHROUT ;address lo-byte
        .if (ECHO_SUPPORT!=1)
         lda relocaddr
        .else
         lda #>BASIC_STARTADDRESS ;$08
        .fi
        jsr KERNAL.CHROUT ;address hi-byte
        lda #<(ECHO_SUPPORT==1? EXEstarter : PLAYERADDR) ;Set Start Address  (skip CIA-starter code, it's not needed for 1x speed)
        ldx #>(ECHO_SUPPORT==1? EXEstarter : PLAYERADDR)
        jsr inidptr     ;set decozptr as startaddress (by A and X)

SaveDat ldy #0          ;reset Y to always be 0 while saving (no need to index, zp-pointers increase instead)
saveloop                ;save effective file-content byte-by-byte
        jsr KERNAL.READST
        ;cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        beq +
        jmp SavErr ;bne SavErr ;=jmp SavErr      ;if inbetween error occurs, handle it
+       lda (decozptr),y
        jsr KERNAL.CHROUT ;write byte to disk
        inc decozptr+0
        bne +
        inc decozptr+1
+       lda decozptr+0  ;check if end of file
        cmp expoendadd+0
        lda decozptr+1
        sbc expoendadd+1
        bcc saveloop
        ldx #$00        ; filenumber 0
        jsr KERNAL.CHKOUT ; call CHKOUT (reset output device to prevent keyboard-typings being written to disk)

        jsr menu.clsfile ;close file #1

expdone jmp savexpo     ;export done, return back to importer (or export-setter?)

        .enc "screen"
.if (SID_AMOUNT==1)
SWMext  .text "SWM"     ;importable SID-Wizard module extension (on the disk)
.elsif (SID_AMOUNT==2)
SWMext  .text "SWS"     ;'SWS' stands for 'SID-Wizard Stereo'
.elsif (SID_AMOUNT==3)
SWMext  .text "SWT"     ;'SWT' stands for 'SID-Wizard Trio'
.elsif (SID_AMOUNT==4)
SWMext  .text "SWQ"     ;'SWQ' stands for 'SID-Wizard Quartett'
.fi
        .enc "none"
.enc "screen"
filetyp .text "SWM"     ;3 BYTE FILETYPE-DESCRIPTION (in the workfile)- abbreviation of 'SID-WIZARD MODULE'
        .text SWMversion ; VERSION 1 of the module format
.enc "none"
outputformat .byte (ECHO_SUPPORT!=1 ? 0 : 2)    ;the desired exporting output format (PRG/SEQ/EXE/SID)
        .enc "screen"
exportext               ;extensions for export-types
        .text (SWP_EXPORT==0) ? "C64" : "DRV"     ;0
        .text (SWP_EXPORT==0) ? "BIN" : "SWP"    ;1
        .text "EXE"     ;2
        .text "SID"     ;3
        .enc "none"

.endm ;end of exporter macro 'SID3upper'

.if (SID_AMOUNT<=2)
        #SID3upper
.fi

;===============================================================================================================
        .cerror *>EXEstarter,"TOO MUCH CODE BEFORE EXE-STARTER DOESN'T FIT IN MEMORY. MOVE SOME CODE TO UPPER FREE AREAS! ",*,",",*-EXEstarter

;FROM HERE TILL END OF PLAYER DATA DO NOT INSERT ANY CODE!!! ALL SPACE IS KEPT FOR SID TUNE ITSELF
;================================= BASIC-starter for executable export ==========================================
EXEstarterSize = EXEstarter_end - EXEstarter  ;(SID_AMOUNT==1)? $340 : (SID_AMOUNT==2)? $4B2 : (SID_AMOUNT==3)? (SWP_EXPORT? $599:$5C4) : (SWP_EXPORT? $61E:$64C)
        * = SIDHEADER-EXEstarterSize ;EXEfillerData - EXEstarterSize
titlerow=3  ;row of tune-title
subturow=8  ;row of subtune-info
restarow=11 ;row ow restart-info
timesrow=17 ;row of playtime/rastertime info
editorow=24 ;row of editor-info
EXEstarter              ;executable-SID startercode
        .logical BASIC_STARTADDRESS ;$0801  ;set logical compilation address
BASICbe .word ss2,Year
        .null $9e,^start2 ;Sys 2064
ss2     .word 0
        ;.if (!SWP_EXPORT)
         ;.byte 0,0,0     ;to align to $0810
        ;.fi
;address 2061 ($080D) - INITIALIZATION AT PROGRAM START - set IRQ handlers, screen, and main VIC registers
start2  sei
        lda #$35
        sta 1
-       lda $d012       ;detect if machine is PAL/NTSC
-       cmp $d012
        beq -           ;waiting 1 rasterline's time (no matter CPU speed)
        bmi --
        ldx #0          ;0 for NTSC
        cmp #<(PALfullrast-1) ;$37 ;PAL/Drean machine?
        bne +
        inx             ;1 for PAL
+       stx MachTyp     ;set to 0 if NTSC, 1 if PAL/Drean (Drean is actually PAL-N)

        lda #$17        ;small character set
        sta $d018
        lda #$1B
        sta $d011
        jsr clrscrn
        ;copy SID-tune to correct location
EXEsrcL lda #selfmod    ;set source-address
        sta compzptr
EXEsrcH lda #selfmod
        sta compzptr+1
EXEtgtL lda #selfmod    ;set target-address
        sta decozptr
EXEtgtH lda #selfmod
        sta decozptr+1
        ldy #0
EXEcopy lda (compzptr),y ;read data from source
        sta (decozptr),y ;write data to destination
        lda compzptr+0
        bne +
        dec compzptr+1
+       dec compzptr+0
        lda decozptr+0
        bne +
        dec decozptr+1
+       dec decozptr+0
        lda compzptr+0
        cmp #<(BASICbe+EXEstarterSize)
        lda compzptr+1
        sbc #>(BASICbe+EXEstarterSize)
        bcs EXEcopy
.if (SID_AMOUNT>=2) ;user can set SID2 base address if needed
        ldy #40-1
-       lda SID2txt,y
        sta $0400+12*40,y
        .if (SID_AMOUNT>=3)
        lda SID3txt,y
        sta $0400+16*40,y
        .fi
        .if (SID_AMOUNT>=4)
        lda SID4txt,y
        sta $0400+20*40,y
        .fi
        dey
        bpl -
        jsr diSID23aExe
        jsr SetSID2address
        jsr ModifySID2addresses
 .if (SID_AMOUNT>=3)
        jsr ModifySID3addresses
 .fi
 .if (SID_AMOUNT>=4)
        jsr ModifySID4addresses
 .fi
        jsr clrscrn
.fi

        lda #EXEBGROUND ;init screen
        ;sta $d020
        sta $d021
.if (ECHO_SUPPORT==1)
            lda #$00  ;not needed while background is $00
InitEchoDat sta $D000-$100 ;EchoDataLastPage
            inc InitEchoDat+1
            bne InitEchoDat
            dec InitEchoDat+2
            bmi InitEchoDat
.fi

;play the tune
        sei
        ldy #40-1       ;display author-info
-       lda #$40 ;"*"   ;decorate title
        sta $0400+(titlerow-1)*40,y
        sta $0400+(titlerow+1)*40,y
        lda SIDstamount+1
        cmp #1
        beq only1st
.if (SID_AMOUNT<3 || SWP_EXPORT<1)
         lda subttxt,y   ;subtune-selector info
         sta $0400+subturow*40,y
.fi
only1st .if (SID_AMOUNT<3 || SWP_EXPORT<1)
         lda resttxt,y   ;tune-restart info
         sta $0400+restarow*40,y
        .fi
        ;.if (SID_AMOUNT<3 || SWP_EXPORT<1)
         lda timetxt,y   ;playtime/rastertime info
         sta $0400+timesrow*40,y
        ;.fi
        lda madewith,y
        sta $0400+editorow*40,y
        dey
        bpl -
        jsr dispAuthorInfo
plysubt lda SIDstamount+1 ;display maximum amount of subtunes
        cmp #1
        beq subtune
        jsr decimdi     ;input:A, output: decimal digits in Y and A
        sty $0400+subturow*40+38
        sta $0400+subturow*40+39
        lda subtune+1   ;display subtune-number/letter (using Soci's display routine)
        clc
        adc #1          ;more human-centrict numbering - for listeners the 1st subtune is 01 (not 00)
        jsr decimdi     ;input:A, output: decimal digits in Y and A
        sty $0400+subturow*40+35
        sta $0400+subturow*40+36
subtune lda #selfmod    ;number of subtune to init
.if (SWP_EXPORT!=0)
SWPaddL ldx #selfmod
SWPaddH ldy #selfmod
.fi
        jsr normalPlayer.player.inisub ;overwrites TUNE_HEADER
        lda #$80        ;wait a bit (for key-debounce on the real machine)
        cmp $d012
        bne *-3
        lda #$ff        ;wait for key-release
        cmp $dc01
        bne *-3
        lda #0
        sta rastmax     ;reset max. rastertime-usage
        sta minute
        sta second
        ldx MachTyp     ;PAL/NTSC timing?
        lda framesec,x
        sta framect

.if (ECHO_SUPPORT==1)
 ECHO_OSC3_RANGE = 64.0 ;$40
  ECHO_OSC3_MAX = (ECHO_OSC3_RANGE - 1.0) ;63.0
 ECHO_ENV3_RANGE = (256.0 / ECHO_OSC3_RANGE) ;4.0  ;ECHO_OSC3_DIVSHIFTS = 2 ;log2(ECHO_ENV3_RANGE)
  ECHO_ENV3_MAX = (ECHO_ENV3_RANGE - 1.0) ;3.0
   ECHO_ENV3_MASK = (256.0 - ECHO_OSC3_RANGE) ;192.0 = $C0
            ;sei
            lda #<digiEchoIRQ
            ldx #>digiEchoIRQ
            sta $FFFE
            stx $FFFF
            lda #<dummyNMI
            ldx #>dummyNMI
            sta $FFFA
            stx $FFFB
            lda #$00
            sta $d01A
            lda #$01
            sta $d019

            lda #$10
            sta $DC0E
            lda #$81
            sta $DC0D
            lda #$7F
            sta $DD0D
            lda $DC0D
            lda $DD0D
            lda #$40 ;RTI
            sta $DC0C
            lda #0
            sta $d015
            lda #$00
            sta $DC05
            ldx MachTyp
            lda EchoCIAperiods,x
            sta $DC04
 ;.page ;older 64tass doesn't support this
            ldy #1
            lda #$80
            cmp $d012
            bne *-3
            lda #$FE
            cmp $d012
            bne *-3
            ldx #12
            dex
            bpl *-1
            bit 0 ;nop
            sty $DC0E
            cli
 ;.endpage
.fi

rast1st .if (SID_AMOUNT<4 && ECHO_SUPPORT!=1)
        lda #0
        sta fspdctr+1   ;reset framespeed-counter
        lda EXEframesPAL+0
        ldx MachTyp
        bne +
        lda EXEframesNTSC+0
+       ldx #EXEBARCOL1
        .else
        lda #(ECHO_SUPPORT!=1 ? PALrastcomm : $FC)
        ldx MachTyp
        bne +
        lda #(ECHO_SUPPORT!=1 ? NTSCrastcomm : $FC)
+       ldx #EXEBARCOL1
        .fi
        cmp $d012
        .if (ECHO_SUPPORT!=1)
         bne *-3
        .else
         bcs *-3
        .fi
        stx $d020
        jsr normalPlayer.player.playsub

.if (ECHO_SUPPORT!=1)
 calcRas lda $d012       ;calculate rastertime
         ldx #EXEBORDCOL
         stx $d020
         sec
         .if (SID_AMOUNT<4)
          sbc EXEframesPAL+0
         .else
          sbc #PALrastcomm
         .fi
         ldx MachTyp     ;(PAL=1, NTSC=0)
         .if (SID_AMOUNT>=2) ;if PAL-rastertime is over 64 ($d011 bit9=0), add 64
         bne +           ;PAL/NTSC - framespeed $07 should be added in NTSC
         adc #(NTSCfullrast-$100)
         jmp chkrmax
 +       cmp #57 ;except we're in the 312.5th last row
         beq chkrmax
         bit $d011
         bmi chkrmax
         clc
         adc #57
         .else
         bne chkrmax     ;PAL/NTSC - rastertime $07 should be added in NTSC
         adc #(NTSCfullrast-$100)
         .fi
 chkrmax cmp rastmax
         bcc dispRas
         sta rastmax     ;set max. rastertime if bigger than previous
 dispRas .if (SID_AMOUNT>=3)
         jsr decimd0     ;display current rastertime usage
         stx $0400+timesrow*40+29
         sty $0400+timesrow*40+30
         sta $0400+timesrow*40+31
         .else
         jsr decimdi     ;display current rastertime usage
         sty $0400+timesrow*40+29
         sta $0400+timesrow*40+30
         .fi
         lda rastmax     ;diplay max. rastertime usage
         .if (SID_AMOUNT>=3)
         jsr decimd0
         stx $0400+timesrow*40+36
         .else
         jsr decimdi
         .fi
         sty $0400+timesrow*40+37
         sta $0400+timesrow*40+38

.else ;.if (ECHO_SUPPORT==1)
 SID_VOLUME_RANGE = 16
 SID_DIGI_RANGE = SID_VOLUME_RANGE
 ECHO_AMPL = int(SID_DIGI_RANGE / ECHO_DIVISOR)
 ECHO_VOL_RANGE = 1.0
 ECHO_VOL_STEP = 0.25
  ECHO_AUDIBLE_VOLUME_MIN = SID_VOLUME_RANGE * (ECHO_VOL_STEP/ECHO_VOL_RANGE) ;4  ;nonzero multiplier table
 ECHO_ACTIVATION_MINIMUM_MAINVOLUME = 2 ;ECHO_AUDIBLE_VOLUME_MIN ;+1 needed because of SEC in digi-code before SBC/ADC
 ;ECHOFORCE_MAXIMUM_MAINVOLUME = 3  ;no tweak above this volume
 SBC_ABS_opcode = $ED
 ADC_ABS_opcode = $6D
 BIT_ABS_opcode = $2C
 SEC_opcode = $38
 CLC_opcode = $18
            ldx #EXEBORDCOL
            stx $d020
            ldx #SEC_opcode
            ldy #SBC_ABS_opcode  ;by default subtract digi from MainVolume value (better utilizing 0..15 range this way and echo is inversed in nature anyway)
 EXEmainVol lda normalPlayer.player.MAINVOL+1  ;modified for player-type by exe-exporter code
            pha ;tay
            cmp #ECHO_AMPL ;cmp #ECHO_AUDIBLE_VOLUME_MIN ;cmp #(ECHOFORCE_MAXIMUM_MAINVOLUME+1) ;4 ;volume is greater than or equal to maximum echo-value?
            bcs +  ;beq silentEcho ;setMulIndex
            ldx #CLC_opcode             ;volume was smaller, so change SEC-SBC to CLC-ADC in digi-player code
            ldy #ADC_ABS_opcode ;ADC_ABS_opcode  ;for low MainVolume values add the digi instead of subtracting it
            cmp #ECHO_ACTIVATION_MINIMUM_MAINVOLUME ;2
            bcc +
            lda #ECHO_AUDIBLE_VOLUME_MIN ;4 ;give a chance to low volumes to have silent digi
 +          lsr  ;amount of division-shifts: log2(ECHO_VOL_RANGE/ECHO_VOL_STEP)
            lsr
setMulIndex clc
            adc #(>EchoMulTables)
            sta EchoMultiply+2
            stx EchoCarryPt+0  ;(if a digi-interrupt happens here, there's a rare chance is for a little click...)
            sty EchoReadPtr+0  ;by default it's SBC, but for low MainVolume values add the digi instead of subtracting it; problem: mainvolume should be in the delayed saved signal instead
            pla ;tya ;lda EchoVolumes,y
 EXEfiltBand ora normalPlayer.player.FLTBAND+1  ;modified for player-type by exe-exporter code
            sta VolumeBand+1
            lda $D41C
            and #ECHO_ENV3_MASK ;$C0
            sta EchoEnvelope+1
.fi

        jsr counTim
dispTim lda minute      ;display playback-timetxt
        jsr decimdi
        sty $0400+timesrow*40 + (ECHO_SUPPORT==1 ? 22 : 10)
        sta $0400+timesrow*40 + (ECHO_SUPPORT==1 ? 23 : 11)
        lda second
        jsr decimdi
        sty $0400+timesrow*40 + (ECHO_SUPPORT==1 ? 25 : 13)
        sta $0400+timesrow*40 + (ECHO_SUPPORT==1 ? 26 : 14)

.if (ECHO_SUPPORT!=1)
keyTest lda #$df        ;handle keypresses
        sta $dc00
        lda $dc01
chPosit cmp #$fe        ;plus-key?
        bne chNegat
        ldy subtune+1
        iny
        cpy SIDstamount+1
        bcc +
        ldy #0          ;roll over if max. reached
+       sty subtune+1
        jmp plysubt
chNegat cmp #$f7        ;minus-key?
        bne chSpace
        ldy subtune+1
        bne +
        ldy SIDstamount+1 ;subtamo ;roll over if min. reached
+       dey
        sty subtune+1
        jmp plysubt
.fi
chSpace lda #$7F
        sta $dc00
        lda $dc01
        cmp #$ef
        bne chArrow
        .if (ECHO_SUPPORT==1)
         sei
        .fi
        jmp plysubt
chArrow .if (SID_AMOUNT<4 && (SID_AMOUNT<3 || SWP_EXPORT<1) && ECHO_SUPPORT!=1)
        cmp #$fd
        bne fsploop
        jsr fwdplay ;start 4x fast-forward
        jsr fwdplay
        jsr fwdplay
        .fi
        jmp rast1st


.if (SID_AMOUNT<4 && ECHO_SUPPORT!=1)

fsploop inc fspdctr+1 ;go to next multispeed frame
fspdctr ldy #selfmod   ;framespeed-counter
        cpy normalPlayer.player.FRAME_SPD ;TUNEHEADER+FSPEEDPOS;check if reached selected framespeed
        bne +
        jmp rast1st
+       lda EXEframesPAL,y
        ldx MachTyp
        bne +
        lda EXEframesNTSC,y
+       asl ;*2 - Carry holds bit9 of rasterline
        ldx #EXEBARCOL2
        bcs +           ;action depending on Carry set previously based on rastercounter-bit9 info
wait9ra bit $d011       ;wait for bit9 of raster-counter to be reset (arrive to lower rasterline-area)
        bmi *-3
+       cmp $d012
        bne *-3
        stx $d020
        jsr normalPlayer.player.mulpsub ;multiplay part of the player routine ($1006)
        lda #EXEBORDCOL
        sta $d020
        .if (EXEBORDCOL != 0)
         bne fsploop ;=jmp fsploop
        .else
         beq fsploop ;=jmp fsploop
        .endif

fwdplay jsr normalPlayer.player.playsub

.fi


counTim dec framect
        bpl +
        ldx MachTyp     ;PAL/NTSC timing?
        lda framesec,x
        sta framect
        inc second
        lda second
        cmp #60
        bne +
        lda #0
        sta second
        inc minute
+       rts

clrscrn ldy #0
        sty $d015       ;switch off sprites to prevent rastertime-cycles being stolen from CPU
-       lda #$20        ;clear screen
        sta $0400,y
        sta $0500,y
        sta $0600,y
        sta $0700,y
        lda #EXETEXTCOL
        sta $d800,y
        sta $d900,y
        sta $da00,y
        sta $db00,y
        iny
        bne -
        rts

        .if (SID_AMOUNT>=3)
decimd0 ldx #"0" ;X is digit 1
        cmp #100
        bcc decimdi
        sec
        sbc #100
        inx ;ldx #"1"
        .fi
decimdi ldy #$2f        ;input:A, output: decimal digits in Y and A
        sec
-       iny
        sbc #10
        bcs -
        adc #$3a
        rts

hexdis0 pha             ;input:accu, output: nybbles in Accu and X-register
        and #$0f
        tax
        pla
div16   lsr
        lsr
        lsr
        lsr
        rts

        .enc "screen"
hexcha0 .text "0123456789ABCDEF"
        .enc "none"

dispAuthorInfo
        ldy #39
-       lda #" "
        sta $0400+titlerow*40,y
        dey
        bpl -
        ldy #39
-       lda normalPlayer.player.TUNE_HEADER+AUTHORPOS,y ;SIDtitletx,y
        cmp #" "
        bne +           ;find character before last space
        dey
        bne -
+       sty $02
        lda #39
        sec
        sbc $02
        tay
        lsr             ;halve the distance -> center text
        tay
        ldx #0
-       lda normalPlayer.player.TUNE_HEADER+AUTHORPOS,x ;SIDtitletx,y
        sta $0400+titlerow*40,y
        inx
        iny
        cpy #40
        bne -
        rts


.if (ECHO_SUPPORT==1)
 ;.page ;older 64tass doesn't support this

 digiEchoIRQ:  ;58 cycles max for full echo digi-code! (to avoid running into a badline before setting $D418 in next row with enabled display)
             pha   ;3 cycles

 EchoOscWave lda $D41B            ;4 summed:
             lsr                  ;2  6  ;ECHO_OSC3_DIVSHIFTS amount of divisor-shifts to OSC3 value ($00..$FF)
             lsr                  ;2  8
 EchoEnvelope ora #ECHO_ENV3_MASK ;$C0 ;selfmod    ;2  10
             sta EchoMultiply+1   ;4  14
 EchoMultiply lda EchoMulTables+3*$100 ;selfmod  ;4  18
 EchoWritPtr sta EchoDataLastPage ;selfmod ;4  22

 VolumeBand  lda #$0F ;selfmod    ;2  24
 EchoCarryPt sec ;selfmod         ;2  26
 EchoReadPtr sbc EchoDataLastPage+1 ;selfmod ;4  30
             sta $D418            ;4  34

 countWrite  inc EchoWritPtr+1     ;6     summed:   ;pointer-handler should fit into 58-34 = 24 cycles
             bne countRead         ;3/2    9/8    ;read-pointer and write-pointer shouldn't cross page-bounds at the same time (that's why the shift by 1 in lowbyte init value for one pointer)
             inc EchoReadPtr+1     ;0/6      14
             dec EchoWritPtr+2     ;0/6      20
             bmi returnIRQ1        ;0/3/2    23/22  ;23 cycles here is below 24, fine!
 ECHOlastPg1 lda #(>EchoDataLastPage) ;0/0/2       24
             sta EchoWritPtr+2     ;0/0/4       28  ;28 cycles: happens rarely (only at hi-wraparound)
 returnIRQ1: pla          ;3                                    ;(so if sample is skipped it's inaudible)
             jmp $DC0C    ;3 (+ RTI)

 countRead   inc EchoReadPtr+1     ;6     15
             bne returnIRQ2        ;3/2   18/17     ;18 cycles is well below 24 cycles, fine!
             dec EchoReadPtr+2     ;0/6      23
             bmi returnIRQ2        ;0/3/2    26/25  ;26 cycles is above 24 a bit but seems not to cause audible issue!
 ECHOlastPg2 lda #(>EchoDataLastPage) ;0/0/2       27             (happens every 32ms aka 30Hz rate)
             sta EchoReadPtr+2     ;0/0/4       31  ;31 cycles: happens rarely (only at hi-wraparound)
returnIRQ2:  pla          ;3                                    ;(so if sample is skipped it's inaudible)
dummyNMI:    jmp $DC0C    ;3 (+ RTI)

 ;.endpage
.fi


.if (SID_AMOUNT>=2)
diSID23aExe jsr SID23setExe.CalcSID2addID
        lda SID2add+1
        jsr hexdis0
        tay
        lda hexcha0,y
        sta lowvidram+14*40+18
        lda hexcha0,x
        sta lowvidram+14*40+19
        lda SID2add+0
        jsr hexdis0
        tay
        lda hexcha0,y
        sta lowvidram+14*40+20
        lda hexcha0,x
        sta lowvidram+14*40+21
 .if (SID_AMOUNT>=3)
        jsr SID23setExe.CalcSID3addID
        lda SID3add+1
        jsr hexdis0
        tay
        lda hexcha0,y
        sta lowvidram+18*40+18
        lda hexcha0,x
        sta lowvidram+18*40+19
        lda SID3add+0
        jsr hexdis0
        tay
        lda hexcha0,y
        sta lowvidram+18*40+20
        lda hexcha0,x
        sta lowvidram+18*40+21
 .fi
 .if (SID_AMOUNT>=4)
        jsr SID23setExe.CalcSID4addID
        lda SID4add+1
        jsr hexdis0
        tay
        lda hexcha0,y
        sta lowvidram+22*40+18
        lda hexcha0,x
        sta lowvidram+22*40+19
        lda SID4add+0
        jsr hexdis0
        tay
        lda hexcha0,y
        sta lowvidram+22*40+20
        lda hexcha0,x
        sta lowvidram+22*40+21
 .fi
        rts
SID23setExe #SID23setter
repeatex .proc          ;repeat-counter routine, return only if repeat can be done (shouldn't affect X or Y registers)
        lda repecnt     ;keyrepeat-counter
        bne norsrep     ;check, if repeat counter is 0
        lda #repspd2    ;default repeat-speed 2
        sta repecnt     ;if 0, reset counter to repeatspeed 2
noret   pla             ;getting back Program-counter from stack? (injected by Soci)
        pla             ;causing '2-level rts'?
        rts             ;repeat not allowed here
norsrep cmp #repspd1    ;check, of repeat counter is initial value (repeatspeed 1)
        bne repcan      ;if no, then just decrease the repeat counter
        dec repecnt     ;if yes, decrease by one, and reset Z-flag - repeat possible
        rts             ;repeat allowed here
repcan  dec repecnt
        bne noret       ;if reaches 0, a repeat occurs, next round will reset to speed 2
        rts             ;repeat allowed here
        .pend
SetSID2address
-       jsr SID2keyExe
        jmp -
ModifySID2addresses
        ldx #0
-       lda SID2ptrExe+1,x
        beq +
        sta decozptr+1
        lda SID2ptrExe,x
        sta decozptr+0
        txa
        lsr
        tay
        lda SID2add+0
        clc
        adc SID2regExe,y
        ldy #0
        sta (decozptr),y
        lda SID2add+1
        iny
        sta (decozptr),y
        inx
        inx
        cpx #size(extraPlayer.SID2ADDtable) ;safety check
        bne -
+       rts
SID2ptrExe .fill size(extraPlayer.SID2ADDtable),$00 ;take the biggest to ensure place for SID2 addressing commands
SID2regExe .fill size(extraPlayer.SID2ADDtable)/2,$00 ;take the biggest to ensure place for SID2 (register-numbers)
 .if (SID_AMOUNT>=3)
ModifySID3addresses
        ldx #0
-       lda SID3ptrExe+1,x
        beq +
        sta decozptr+1
        lda SID3ptrExe,x
        sta decozptr+0
        txa
        lsr
        tay
        lda SID3add+0
        clc
        adc SID3regExe,y
        ldy #0
        sta (decozptr),y
        lda SID3add+1
        iny
        sta (decozptr),y
        inx
        inx
        cpx #size(extraPlayer.SID3ADDtable) ;safety check
        bne -
+       rts
SID3ptrExe .fill size(extraPlayer.SID3ADDtable),$00 ;take the biggest to ensure place for SID3 addressing commands
SID3regExe .fill size(extraPlayer.SID3ADDtable)/2,$00 ;take the biggest to ensure place for SID3 (register-numbers)
 .fi
 .if (SID_AMOUNT>=4)
ModifySID4addresses
        ldx #0
-       lda SID4ptrExe+1,x
        beq +
        sta decozptr+1
        lda SID4ptrExe,x
        sta decozptr+0
        txa
        lsr
        tay
        lda SID4add+0
        clc
        adc SID4regExe,y
        ldy #0
        sta (decozptr),y
        lda SID4add+1
        iny
        sta (decozptr),y
        inx
        inx
        cpx #size(extraPlayer.SID4ADDtable) ;safety check
        bne -
+       rts
SID4ptrExe .fill size(extraPlayer.SID4ADDtable),$00 ;take the biggest to ensure place for SID4 addressing commands
SID4regExe .fill size(extraPlayer.SID4ADDtable)/2,$00 ;take the biggest to ensure place for SID4 (register-numbers)
 .fi
.fi

.if (SID_AMOUNT==2)
SID2keyExe #plmiret SID23setExe.IncSID2address, SID23setExe.DecSID2address, diSID23aExe, repeatex, repspd2
.elsif (SID_AMOUNT==3)
SID2keyExe #plmiret2 SID23setExe.IncSID2address, SID23setExe.DecSID2address, diSID23aExe, repeatex, repspd2, SID23setExe.IncSID3address,SID23setExe.DecSID3address
.elsif (SID_AMOUNT>=4)
SID2keyExe #plmiret3 SID23setExe.IncSID2address, SID23setExe.DecSID2address, diSID23aExe, repeatex, repspd2, SID23setExe.IncSID3address,SID23setExe.DecSID3address, SID23setExe.IncSID4address,SID23setExe.DecSID4address
.fi
        rts


        .enc "screen"
.if (SID_AMOUNT>=2)
SID2txt  .text "Set SID2-address via +/- & press RET/SPC"
.fi
.if (SID_AMOUNT>=3)
SID3txt  .text "Set SID3-address with cursor left/right."
.fi
.if (SID_AMOUNT>=4)
SID4txt  .text "  Set SID4-address with cursor up/down. "
.fi
.if (SID_AMOUNT<3 || SWP_EXPORT<1)
subttxt  .text "Select subtune with +/- . Current: 00/00"
.fi
.if (SID_AMOUNT<3 || SWP_EXPORT<1)
 resttxt  .if (SID_AMOUNT<4 && ECHO_SUPPORT!=1)
          .text "  Space: restart tune, ",$1f,": fast-forward  "
          .else
           .text "          Space: restart tune           "
          .fi
 resttxt_end
.fi
;.if (SID_AMOUNT<3 || SWP_EXPORT<1)
.if (ECHO_SUPPORT!=1)
 timetxt  .text "PlayTime: 00:00 , Rastertime:00 (max.00)"
.else
 timetxt  .text "            PlayTime: 00:00             "
.fi
;.fi
        .enc "none"
minute   .byte 00 ;counts minutes
second   .byte 00 ;counts seconds
framect  .byte PALframes1second ;counts frames for timer - initial value is set based on machine-type
rastmax  .byte 0 ;stores maximum reached rastertime
MachTyp  .byte 1 ;PAL/Drean=1, NTSC=0
framesec .byte NTSCframes1second, PALframes1second ;the same for Drean as PAL

        .enc "screen"
madewith .text "     -- Composed with SID-Wizard v" ;editor-info row, can be overwritten by different player-types
         .text SWversion," -- "
        .enc "none"

.if (SID_AMOUNT<4 && ECHO_SUPPORT!=1)
;framespeed-program - overwritten/generated by exporter for specific rasterspeeds
EXEframesPAL  .byte PALrastcomm,1,2,3,4,5,6,7 ;TABLE LOADED BY OTHER PARTS OF THE CODE
EXEframesNTSC .byte NTSCrastcomm,1,2,3,4,5,6,7 ;TABLE LOADED BY OTHER PARTS OF THE CODE
.fi

.if (ECHO_SUPPORT==1)
 EchoCIAperiods .byte (NTSCrastercycles*2)-1, (PALrastercycles*2)-1
 ;EchoVolumes .byte 0, (ECHO_MINIMUM_MAINVOLUME>1? 1:2) ,2,3,4,5,6,7,8,9,$A,$B,$C,$D,$E,$F  ;lowest volumes should be compensated for subtracted digi

 ECHO_MID = int(ECHO_AMPL / 2)
 ECHO_FLOATMAX = (ECHO_AMPL - 0.51)
 ECHO_VOL_MIN = 0 ;ECHO_VOL_STEP
 ECHO_VOL_MAX = (ECHO_VOL_RANGE - ECHO_VOL_STEP)
 .align $100
 EchoMulTables:
 .for vol=ECHO_VOL_MIN, vol<ECHO_VOL_RANGE, vol=vol+ECHO_VOL_STEP ;.for vol=0.0, vol<1.0, vol=vol+0.25  ;SID MainVolume scaled down
  .for i=0, i<ECHO_ENV3_RANGE, i=i+1  ;SID ENV3 scaled down
   .for j=0, j<ECHO_OSC3_RANGE, j=j+1  ;SID OSC3 scaled down
    .byte round( (i*ECHO_ENV3_RANGE/ECHO_ENV3_MAX) * (vol*ECHO_VOL_RANGE/ECHO_VOL_MAX) * (j*ECHO_OSC3_RANGE/ECHO_OSC3_MAX) * (ECHO_FLOATMAX/256.0) ) + round( ECHO_MID * (vol*ECHO_VOL_RANGE/ECHO_VOL_MAX) ) - round( (i*ECHO_ENV3_RANGE/ECHO_ENV3_MAX) * (vol*ECHO_VOL_RANGE/ECHO_VOL_MAX) * ECHO_OSC3_RANGE * (ECHO_FLOATMAX/256.0) / 2.0 )
   .next
  .next
 .next

 ROW_FREQ = 15625 ;Hz
 DIGI_FREQ = (ROW_FREQ / 2)
 EchoData = $8000  ;EchoDataFirstPage = EchoData + $0000  ;$8000, so tight loop can detect bottom by 'BMI'
 ECHO_PAGES = (DIGI_FREQ * ECHO_MILLISECONDS / 1000) / $100  ;default amount of echo-pages (1 page is ~33ms, 100ms is ~3 pages)
 ECHO_SIZE = ECHO_PAGES * $100  ;default echo-size  ;EchoData_end = EchoData + ECHO_SIZE
 EchoDataLastPage = EchoData + ECHO_SIZE - $100
.fi

        .here         ;restore physical address
EXEstarter_end = *
        ;.cerror *>(EXEstarter+EXEstarterSize),"YOU MUST ENSURE MORE PLACE ('EXESTARTERSIZE') FOR RUNNABLE-SID STARTER CODE! ", *-(EXEstarter+EXEstarterSize)

;---------------- framespeed-raster table - exporter's version -------------------
;EXEfillerDataSize = EXEfillerData_end - EXEfillerData
;* = SIDHEADER - EXEfillerDataSize
;EXEfillerData

.if (ECHO_SUPPORT!=1)
PALrastcomm = lastrast+4 ;($fe) rasterrow for single-speed playbar
PALsubrast = PALfullrast-PALrastcomm ;($3a) used for rasterlines between 0...$100 region (bit9=0)
NTSCrastcomm = lastrast+4 ;($fe) rasterrow for single-speed playbar
NTSCsubrast = NTSCfullrast-NTSCrastcomm ;($3a) used for rasterlines between 0...$100 region (bit9=0)
.fi

.if (SID_AMOUNT<4 && ECHO_SUPPORT!=1)
;multispeed frame-position/timing values
        .section data

frmspds ;framespeed-program positions ;couldn't calculate by iteration (accumulation caused double defines)
        .byte 0,   0, 1, 1+2, 1+2+3, 1+2+3+4, 1+2+3+4+5, 1+2+3+4+5+6, 1+2+3+4+5+6+7

PALfrRow ;framespeed-rasterrows didived by 2 (equal sharing among the $138 rasterrows) - avoid badline (modulo8=0) if possible
        .for i=1,i<=PALmaxframespeed,i=i+1 ;calculate rasterrows for framespeed 1...8
         .byte PALrastcomm ;1st non-modified position for single-part of the player (no need to divide by 2 because handled by code)
         .for j=1,j<i,j=j+1 ;multispeed-rows
          .if ( j*PALfullrast/i >= PALsubrast )
           .byte (j*PALfullrast/i-PALsubrast)/2 ;result is rasterlines in 0...$100 region (bit9=0)
          .else
           .byte j*(PALfullrast/i)/2 | $80 ;sign bit9 of rasterline
          .fi
         .next
        .next

NTSCfrRow
        .for i=1,i<=PALmaxframespeed,i=i+1 ;calculate rasterrows for framespeed 1...8
         .byte NTSCrastcomm ;1st non-modified position for single-part of the player (no need to divide by 2 because handled by code)
         .for j=1,j<i,j=j+1 ;multispeed-rows
          .if ( j*NTSCfullrast/i >= NTSCsubrast )
           .byte (j*NTSCfullrast/i-NTSCsubrast)/2 ;result is rasterlines in 0...$100 region (bit9=0)
          .else
           .byte j*(NTSCfullrast/i)/2 | $80 ;sign bit9 of rasterline
          .fi
         .next
        .next

        .send data
.fi
;EXEfillerData_end


;============================ SID-HEADER =======================================
WEBSID_FORMAT = (SID_AMOUNT>=4) ? 1:0
SIDHEADERSIZE = (WEBSID_FORMAT) ? $82 : $7c
        *= (ECHO_SUPPORT==1? LOADADDR:CIAADDR) - SIDHEADERSIZE;...SID file-format header
SIDHEADER
;SID v1 header - big endian (!!!) WORD (byte-pair) values most of the time
;--------------
        .text ( ECHO_SUPPORT==1? "rsid" : "psid" ) ;+00 magicID: 'PSID' or 'RSID' (RSID stands for strict SID that requires full, real C64 environment or full emulation
        .byte $00,(SID_AMOUNT==1)? $02 : (WEBSID_FORMAT==0? $03:$4E) ;+04 version ;can either be 0001 or 0002. Headers of version 2 provide additional fields. RSID and PSID v2NG files must have 0002 here., WebSID format:$4E (for 4SID)
        .byte $00, SIDHEADERSIZE ;+06 dataOffset ;offset from the start of the file to the C64 binary data area. fixed size of the header, this is either 0x0076 for version 1 and 0x007C for version 2.
        .byte $00,$00 ;+08 loadAddress ;C64 memory location where to put the C64 data. 0 means the data are in original C64 binary file format, must NOT be less than $07E8 in RSID files.
SIDinitadd .byte >(ECHO_SUPPORT==1? start2:CIAINIT), <(ECHO_SUPPORT==1? start2:CIAINIT) ;+0A initAddress ;start address of the machine code subroutine that initializes a song,accepting the contents of the 8-bit 6510 Accumulator as the song number parameter. 0 means the address is equal to the effective load address.
SIDplayadd .byte (ECHO_SUPPORT==1? $00:>CIAPLAY), (ECHO_SUPPORT==1? $00:<CIAPLAY) ;+0C playAddress ;start address of the machine code subroutine that can be called frequently to produce a continuous sound. 0 means the initialization subroutine is expected to install an interrupt handler, which then calls the music player at some place. This must always be true for RSID files.
SIDstamount .byte $00,$01 ;+$0E songs ;The number of songs (or sound effects) that can be initialized by calling the init address. The minimum is 1. The maximum is 256.
SIDdefasubt .byte $00,$01 ;+$10 startSong ;The song number to be played by default. This value is optional. It often specifies the first song you would hear upon starting the program is has been taken from. It has a default of 1.
SIDtimerTyp .byte !ECHO_SUPPORT*$FF,!ECHO_SUPPORT*$FF,!ECHO_SUPPORT*$FF,!ECHO_SUPPORT*$FF ;+12 LONGWORD speed ;32 bit big endian number. Each bit in 'speed' specifies the speed for the corresponding tune number, i.e. bit 0 specifies the speed for tune 1. 0 bit specifies vertical blank interrupt (50Hz PAL, 60Hz NTSC), and a 1 bit specifies CIA 1 timer interrupt (default 60Hz).
SIDtitletx .fill $20,0 ;+$16 <title> ;32 byte long zero terminated ISO 8859-1 ASCII character strings. zero byte will always be put into the last byte of each string. So the maximum number of available free characters is 31.
SIDauthrtx .fill $20,0 ;+$36 <author>
SIDreleatx .fill $20,0 ;+$56 <released> (also known as <copyright>)
;SID v2 header extensions:
;-------------------------
SIDflags .byte %00000000 ;+$76 flags ;16 bit big endian number containing the following bitfields:
           .byte (SID_AMOUNT==1 || WEBSID_FORMAT!=0)? %00100100 : %10100100 ;bit0-binary data format (0-built-in player,1-Compute MUS player must be merged), bit1-(0-C64 compatible,1-PlySID specific/C64 BASIC flag for RSID), bit2..3-video standard(01-PAL,10-NTSC), bit4..5-SID model (00-?,01-6581,10-8580,11-both), bit6..7:SID2 model (not set at all in mono version, bit8..9:SID3 model)
        .byte $00       ;+$78 startPage (relocStartPage) ;8 bit number. If 'startPage' is 0, the SID file is clean, i.e. it does not write outside its data range within the driver ranges. If 'startPage' is 0xFF, there is not even a single free page, and driver relocation is impossible.if 'startPage' is 0x1E, this free memory range starts at $1E00.
        .byte $00       ;+$79 pageLength (relocPages) ;8 bit number indicating the number of free pages after 'startPage'. If 'startPage' is not 0 or 0xFF, 'pageLength' is set to the number of free pages starting at 'startPage'. If 'startPage' is 0 or 0xFF, 'pageLength' must be set to 0.

        .if (SID_AMOUNT>=2)
SID2sidAdd .byte (defaultSID2BASE-$d000)/16 ;+$7A

         .if (SID_AMOUNT>=3)
          .if (WEBSID_FORMAT==0)
SID3sidAdd .byte (defaultSID3BASE-$d000)/16 ;+$7B (3SID)
          .else
SID2flags  .byte %01100000                  ;+$7B (WebSID-format) ;bit6:left/right-channel  ;bit5..4:model, 3..0:unused
          .fi                                       ;(I propose bit7:both channel)
         .else
          .byte $00                         ;+$7B (1SID/2SID)
         .fi

         .if (WEBSID_FORMAT!=0)
          .if (SID_AMOUNT>=3)
SID3sidAdd .byte (defaultSID3BASE-$d000)/16 ;+$7C
SID3flags  .byte %10100000                  ;+$7D
          .fi
          .if (SID_AMOUNT>=4)
SID4sidAdd .byte (defaultSID4BASE-$d000)/16 ;+$7E
SID4flags  .byte %11100000                  ;+$7F
          .fi
         .fi

         .byte $00,$00  ;(+$7A?) reserved ;This is a 16 bit number and is reserved and should be set to 0 for single SID.
                        ;(+7C?) <data> ;Version 2 of the SID header ends here. This offset is the start of the binary C64 data.
        .fi

;--------------------------------------------------------------------------------------------------------



;========================= CIA STARTER CODE ====================================
.if (ECHO_SUPPORT!=1)
        *= CIAINIT-2 ;load-address of the tune - will be modified by relocator
CIAADDR .byte <CIAINIT,>CIAINIT ;relocator won't modify this
;------ Multispeed SID CIA-based player routine
        *= PLAYERADDR-$48
CIAINIT ldy normalPlayer.player.FRAME_SPD ;TUNEHEADER+FSPEEDPOS ;leave Accumulator untouched till 'jmp inisub'!
;        ldx #$35        ;enable SID-playing in ROMs too -- !!! Ian Coog informed me that it's not necessary - SID-player sets bank automatically
;        stx banksel
p_SIDa1 ldx CiaFrameHi,y ;$13 for 4X  - SID CIA playing initialization
        stx $dc05
p_SIDa2 ldx CiaFrameLo,y ;$31 for 4X
        stx $dc04
        ldx #$00
p_SIDa3 stx frCount+1
        ;lda #0 ;SIDdefasubt-1 - Don't init here, otherwise not possible to switch subtune from player!
p_SIDa4 jmp normalPlayer.player.inisub
CIAPLAY inc frCount+1   ;SID CIA playing routine called by IRQ in player
frCount lda #selfmod
p_SIDa5 cmp normalPlayer.player.FRAME_SPD ;TUNEHEADER+FSPEEDPOS;framespeed
        beq +
p_SIDa6 jmp normalPlayer.player.mulpsub
+       lda #0
p_SIDa7 sta frCount+1
p_SIDa8 jmp normalPlayer.player.playsub

;CIA-timing values for multispeed. ;Ian Coog told this formula: ((screen lines) * (cycles per line) / speed) - 1
CiaFrameHi .byte $99, $4c,$26,$19,$13, $0f,$0c,$0a,$09 ;replaced by exporter to PAL or NTSC values
CiaFrameLo .byte $8f, $c7,$63,$97,$31, $5a,$cb,$f7,$98 ;replaced by exporter to PAL or NTSC values

        .section data
CiaFrameHiPAL .byte $99, $4c,$26,$19,$13, $0f,$0c,$0a,$09  ;framespeed '0' is treated as half-speed (25Hz)
CiaFrameLoPAL .byte $8f, $c7,$63,$97,$31, $5a,$cb,$f7,$98  ;framespeed '1' for PAL is 312*63-1=$4CC7
CiaFrameHiNTSC .byte $85, $42,$21,$16,$10, $0d,$0b,$09,$08 ;framespeed '0' is treated as half-speed (30Hz)
CiaFrameLoNTSC .byte $8d, $c6,$62,$41,$b0, $5a,$20,$89,$57 ;framespeed '1' for NTSC is 263*65-1=$42C6
CiaFrameHiDrean .byte $9e, $4f,$27,$1a,$13, $0f,$0d,$0b,$09 ;framespeed '0' is treated as half-speed (25Hz)
CiaFrameLoDrean .byte $6f, $37,$9b,$67,$cd, $d7,$33,$50,$e6 ;framespeed '1' for Drean is 312*65-1=$4F37
        .send data
.fi ;end of (ECHO_SUPPORT!=1)

        .cerror *>LOADADDR,"YOU MUST ENSURE MORE PLACE FOR CIA-BASED SID INIT/PLAY CODE!",*-LOADADDR


;************************ MUSIC CODE AND DATA *****************************************************************
        *= PLAYERADDR-2
LOADADDR .byte <PLAYERADDR,>PLAYERADDR ;relocator will NOT modify this - used for safety-saving
;-------------------------------
        .include "include/player.asm" ;the player/driver routine to produce the SIDDY sound
        *= PLAYERADDR ;($1000 if no echo-support)
.if (SWP_EXPORT==0)
MAX_PLAYERSIZE = size(extraPlayer.player) ;take the biggest player to tell how much room is needed
.else
MAX_PLAYERSIZE = (extraPlayer.SWPplayer_top - extraPlayer.player.playercode)
.fi
        .fill MAX_PLAYERSIZE,0 ;player=normalPlayer.player ;player #playerm PLAYERADDR

;-------------------------------------------------------------------------------------------
*=PLAYERADDR+MAX_PLAYERSIZE ;give enough space
MUSICDATA ;depends on the player-type, must reside immediately after the player-code
;.........




;==================================== Code part 2 ========================================================
        ;*= $fff8-(EXPORTER_CODE_END-MUSICDATA_END)
SFX_SIZEINC=(SFX_SUPPORT!=0)? $400 : 0
SWP_SIZEINC=(SWP_EXPORT!=0)? $680 : 0
SLOWDN_SIZEINC=(SLOWDOWN_SUPP!=0)? $800 : 0
SID2_SIZEINC=(SID_AMOUNT>=2)? $1500 : 0
SID3_SIZEINC=(SID_AMOUNT>=3)? (SWP_EXPORT? $1200:$1100) : 0
SID4_SIZEINC=(SID_AMOUNT>=4)? (SWP_EXPORT? $12A0:$1200) : 0
;BEFORE_IO_AREA_size = BEFORE_IO_AREA_end - BEFORE_IO_AREA
;LAST_CODE_AREA_size = LAST_CODE_AREA_end - LAST_CODE_AREA
;* = IO_AREA_ADDRESS - BEFORE_IO_AREA_size ;EXPORTER_CODE_END - *
;.cwarn (1), "MUSICDATA_END = ", *
;if ( * >= MAX_ALLOWED_ADDRESS - (BEFORE_IO_AREA_size + LAST_CODE_AREA_size) )
;* = MAX_ALLOWED_ADDRESS - (BEFORE_IO_AREA_size + LAST_CODE_AREA_size)
* = $A100-SFX_SIZEINC-SWP_SIZEINC-SLOWDN_SIZEINC-SID2_SIZEINC-SID3_SIZEINC-SID4_SIZEINC - (FINAL_CARTRIDGE_3_FIX!=0) * $1000
MUSICDATA_END
;.cwarn (1), "MUSICDATA_END = ", *

BEFORE_IO_AREA

;subroutines to create PRG/SID tune
;-----------------------------------
        .include "include/packdepack.inc" ;only altered depacker is used for SID-Maker

;=======================================================================================
.if (SID_AMOUNT>=4)
        #SID4upper
.fi
.if (SID_AMOUNT>=3)
        #SID3upper
.fi

NorSave                 ;normal save (compatible mode for saving - without relocation
        jsr menu.iniwfn ;init workfile name pointer and length, bank, and device,file,channel
        lda #<PLAYERADDR ;Set Start Address
        ldx #>PLAYERADDR
.if (SWP_EXPORT!=0)
        lda outputformat
        cmp #1
        bne NorSav2
SWPdatL lda #selfmod
SWPdatH ldx #selfmod
.fi
NorSav2 jsr inidptr     ;set decozptr as startaddress (by A and X)
        lda #<decozptr  ;pointer of start-address
        ldx expoendadd+0 ;Get End Address of tunedata to export/save
        ldy expoendadd+1
        jsr KERNAL.SAVE
        bcs +           ;if error
        jsr KERNAL.READST
        cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        bne +           ;if error
        jmp expdone     ;if no error, export is finished
+       jsr menu.saverror ;display error and wait for space
        jmp savexpo


;--------------------------------------------------------------------------------
preparetunedata         ;preparing tune-data for the player from saved workfile

;copy & crop tune-header from music data and move music data to the end of the selected player (light/mid/extra differ in size)
        ldy #tuneheadersize-1 ;copy workfile & author-info from SWM data to beginning of the player (on the variables' section, will be overwritten on init)
-       lda MUSICDATA,y ;because SWM header will be cropped when not needed to save some more space
        sta normalPlayer.player.TUNE_HEADER,y
        dey
        bpl -
.if(SWP_EXPORT!=0) ;register player-info
        ldy #(tuneheadersize-AUTHORPOS)-1
-       lda SWPplayer_txt,y
        sta normalPlayer.player.TUNE_HEADER+AUTHORPOS,y
        dey
        bpl -
.fi

        lda TUNEHEADER+FSPEEDPOS ;store framespeed, because it will be overwritten by initer but used in players
        sta normalPlayer.player.FRAME_SPD
.if (SID_AMOUNT>=2)
        lda TUNEHEADER+SID2ADD_ID_POS
        sta normalPlayer.player.SID2ADD_ID
.fi
.if (SID_AMOUNT>=3)
        lda TUNEHEADER+SID3ADD_ID_POS
        sta normalPlayer.player.SID3ADD_ID
.fi
.if (SID_AMOUNT>=4)
        lda TUNEHEADER+SID4ADD_ID_POS
        sta normalPlayer.player.SID4ADD_ID
.fi
        ;move all music data lower in memory to crop/overwrite the header-info not needed anymore (because just copied to player)
        jsr ctotmpz     ;store current compzptr (pointing right after musicdata)
        lda #(SWP_EXPORT=0)? <(MUSICDATA+tuneheadersize) : <MUSICDATA ;set source-address for move
        ldx #(SWP_EXPORT=0)? >(MUSICDATA+tuneheadersize) : >MUSICDATA
        sta decozptr
        stx decozptr+1
        lda normalPlayer.player.TUNE_HEADER+DRIVERTYPE_POS ;set target address for move
        asl
        tax
.if (SWP_EXPORT==0)
        lda altplayers.PlrEnds+0,x ;check end-address of already selected & copied player-code
        sta compzptr+0
        ldy altplayers.PlrEnds+1,x
        sty compzptr+1
.else
        lda altplayers.SWPends+0,x ;check end-address of already selected & copied player-code
        sta compzptr+0
        ldy altplayers.SWPends+1,x
        sty compzptr+1
        sta SWPdatL+1
        sty SWPdatH+1
        sta (SWPaddL+1-BASICbe)+EXEstarter
        sty (SWPaddH+1-BASICbe)+EXEstarter
.fi

        ldy #0          ;forward-motion to prevent overwriting source with target if they overlap (most probably)
movPlop lda (decozptr),y
        sta (compzptr),y
        inc decozptr+0
        bne +
        inc decozptr+1
+       inc compzptr+0
        bne +
        inc compzptr+1
+       lda decozptr+0
        cmp compztmp+0  ;check if copied the entire musicdata - low-byte part
        bne movPlop
        lda decozptr+1  ;check if copied the entire musicdata - high-byte part
        cmp compztmp+1  ;until end of musicdata is reached
        bne movPlop


;!!order of these upcoming calls is important!! (depends on SWM format) don't modify!

        jsr depktempo   ;depack/init subtune-tempos and tempotable
        ldy #1
        lda #(2+2*CHN_AMOUNT+1)-1        ;-1 to compensate missing tempoprogram 0
        sta (tempoptadd),y ;set tempo 1 pointer (not done by depacker)
        inx             ;make 1 byte free after tempoprogram (for pointer of chord 1)
        txa             ;X is amount of tempoprograms, depktempo exited with this result in X
        clc
        adc tempoptadd+0 ;calculate chord-pointer table base address
        sta chordptadd+0
        php
        adc #1
        sta chdptad+1
        lda tempoptadd+1
        adc #0          ;add Carry flag
        sta chdptad+2
        plp
        lda tempoptadd+1
        adc #0          ;add Carry flag
        sta chordptadd+1

        ldy TUNEHEADER+TMPLENPOS ;SHIFT TEMPOTBLE FORWARD BY 8 (or 14 for 2SID)
        beq +
        dey
        tya
        sta compzvar2
        clc
        adc #(RESTEMP-TEMPOTBL)
        sta compzvar3
-       ldy compzvar2
        lda (tempotbadd),y
        ldy compzvar3
        sta (tempotbadd),y
        dec compzvar3
        dec compzvar2
        bpl -
+
;------------------------------------------------------------------
        jsr depkinsch   ;depack/init chords and instruments
        jsr ctotmpz     ;store actual compzptr (pointing just after patterndata)
        ;decozptr now points to beginning of 1st 'unnamed' instrument

        ;set end-pointer for the moving in ptnptloadd
        lda TUNEHEADER+INSTAMPOS;amount of instruments
        clc
        adc inspthiadd+0 ;set pattern lo-pointer table base address, also signs end of data to move
        sta ptnptloadd+0
        lda inspthiadd+1
        adc #0          ;add Carry flag
        sta ptnptloadd+1

;---------------------
;calculate the amount of move, the difference to be set in 'relocamount'
        lda compzptr+0
        sec
        sbc decozptr+0
        sta relocamount+0
        lda compzptr+1
        sbc decozptr+1
        sta relocamount+1

;move all previously processed data to end of patterns to crop space freed by instrument-name omission
        ;and then adjust all necessary table-/instrument-pointers to this move (difference of compzptr-decozptr)
        ldy #0
-       lda (decozptr),y
        sta (compzptr),y
        iny
        bne -
        inc decozptr+1  ;increase page
        inc compzptr+1
        ldx decozptr+1
        dex             ;to check at $100 bytes until end of musicdata, makes sure all $100 pages get copied
        cpx ptnptloadd+1 ;until end of musicdata is reached
        bne -
;adjust table-pointers: subtuneadd,tempoptadd,tempotbadd,chordptadd,chordtbadd,insptloadd,inspthiadd,ptnploadd,
        ldy #subtuneadd
        jsr relozptr
        ldy #tempoptadd
        jsr relozptr
        ldy #tempotbadd
        jsr relozptr
        ldy #chordptadd
        jsr relozptr
        ldy #chordtbadd
        jsr relozptr
        ldy #insptloadd
        jsr relozptr
        ldy #inspthiadd
        jsr relozptr
        ldy #ptnptloadd
        jsr relozptr
;adjust pointers in pointer-tables: insptloadd, inspthiadd
        ldy TUNEHEADER+INSTAMPOS ;amount of instruments
-       lda (insptloadd),y
        clc
        adc relocamount+0
        sta (insptloadd),y
        lda (inspthiadd),y
        adc relocamount+1
        sta (inspthiadd),y
        dey
        bpl -

        jsr ctempzp     ;restore value of compzptr to end of patterndata
;------------------------

;-----------------------------------------------------------------------------
        lda ptnptloadd+0
        clc
        adc TUNEHEADER+PTAMOUPOS
        sta ptnpthiadd+0
        lda ptnptloadd+1
        adc #0          ;add carry bit
        sta ptnpthiadd+1

        jsr depkptseq   ;depack patterns and set sequence-pointers at generated 'subtunes' section (subtuneadd)

;set end-of-data pointer
        ldy TUNEHEADER+PTAMOUPOS
        iny             ;point after last byte
        tya
        clc
        adc ptnpthiadd+0
        sta expoendadd+0
        lda ptnpthiadd+1
        adc #0          ;add carry
        sta expoendadd+1

.if(SWP_EXPORT!=0)
        lda SWPdatL+1
        ldy SWPdatH+1
        sta datzptr+0
        sty datzptr+1
        ldy #2
        .enc "screen"
        lda #"P" ;modify 'SWM1' to "SWP1" in header
        .enc "none"
        sta (datzptr),y
        ldx #subtuneadd
        ldy #SWP_SUBTUNES_OFFS
        jsr setSWPoffs
        ldx #ptnptloadd
        ldy #SWP_PTNPTL_OFFS
        jsr setSWPoffs
        ldx #ptnpthiadd
        ldy #SWP_PTNPTH_OFFS
        jsr setSWPoffs
        ldx #insptloadd
        ldy #SWP_INSPTL_OFFS
        jsr setSWPoffs
        ldx #inspthiadd
        ldy #SWP_INSPTH_OFFS
        jsr setSWPoffs
        ldx #chordtbadd
        ldy #SWP_CHORDTB_OFFS
        jsr setSWPoffs
        ldx #chordptadd
        ldy #SWP_CHORDPT_OFFS
        jsr setSWPoffs
        ldx #tempotbadd
        ldy #SWP_TEMPOTB_OFFS
        jsr setSWPoffs
        ldx #tempoptadd
        ldy #SWP_TEMPOPT_OFFS
        jsr setSWPoffs
.fi

        rts  ;exits with compzptr pointing to end of data

.if (SWP_EXPORT!=0)
setSWPoffs .proc
        stx ptrL+1
        inx
        stx ptrH+1
ptrL    lda selfmodZPA
        sec
        sbc SWPdatL+1
        sta (datzptr),y
        iny
ptrH    lda selfmodZPA
        sbc SWPdatH+1
        sta (datzptr),y
        rts
        .pend
.fi


;===================================================================================================================
setplayer                ;adjust player routine's absolute addresses which are pointing to music data & SID2 & SID3
        lda #$34
        sta banksel

        lda normalPlayer.player.TUNE_HEADER+DRIVERTYPE_POS ;set player-setter table addresses
        asl
        tax
        lda altplayers.PlrDatP+0,x
        sta RplrDat+1
        lda altplayers.PlrDatP+1,x
        sta RplrDat+2
        lda altplayers.PlrValP+0,x
        sta RplrVal+1
        lda altplayers.PlrValP+1,x
        sta RplrVal+2

.if (PLAYER_FOR_ROM==0 || SWP_EXPORT==0)  ;don't modify ROM-based SWP as it's based on zeropage-pointers

        ldx #0
setPlLp jsr RplrDat
        sta datzptr+0   ;set target-address to set
        jsr RplrVal
        beq endPset     ;if zero read, end of table reached
        tay
        sty MusDptr+1   ;set zeropage-index
        iny
        sty MusDpt2+1   ;-||-
        inx ;X+=1
        jsr RplrDat
        sta datzptr+1   ;-||-
        jsr RplrVal     ;read additional offset for the pointer
        clc
MusDptr adc selfmodZPA  ;reading proper zeropage-value
        ldy #1          ;index to operand lo-byte of absolute-addressing instruction in player.asm
        sta (datzptr),y
MusDpt2 lda selfmodZPA  ;reading proper zeropage-value
        adc #0          ;add carry
        iny ;Y=2        ;index to operand hi-byte of absolute-addressing instruction in player.asm
        sta (datzptr),y
        jsr RplrVal
        bpl +
        lda (datzptr),y ;handle signed negative values ($80..$FF)
        sec
        sbc #1
        sta (datzptr),y
+       inx
        bne setPlLp     ;jump

endPset lda normalPlayer.player.SUBTUNESUPP ;if no subtune-support, modify SEQSUB pointers direcly from 'pointer-pointers' to 'pointers'
        bne endSetP
        ldx #CHN_AMOUNT*2-1
-       jsr RplrDat     ;get 'pointer-pointer' values and read to A
        sta datzptr+1
        dex
        jsr RplrDat
        sta datzptr+0
        ldy #2
        lda (datzptr),y ;put pointer-pointers to 'vidzptr' (datzptr points to command at p_seqtX in player)
        pha             ;(and command at p_seqtX in player now temporarily points to sequenceX-pointer in subtune-data)
        dey ;y=1
        lda (datzptr),y
        sta vidzptr+0
        pla
        sta vidzptr+1
        ldy #0          ;(vidzptr now points to sequenceX-pointer)
        lda (vidzptr),y ;write back resulted pointer-values to the original addresses
        iny ;y=1
        sta (datzptr),y
        lda (vidzptr),y
        iny ;y=2
        sta (datzptr),y ;(command at p_seqtX now points directly to sequenceX)
        dex
        bpl -
endSetP

.fi ; end of condition: (PLAYER_FOR_ROM==0 || SWP_EXPORT==0)


.if (SID_AMOUNT>=2)
WrSID2add
        lda normalPlayer.player.TUNE_HEADER+DRIVERTYPE_POS ;set player-setter table addresses
        asl
        tay
        lda altplayers.SID2Aadd+0,y
        sta datzptr+0
        lda altplayers.SID2Aadd+1,y
        sta datzptr+1
        ldy #0
-       lda (datzptr),y
        sta (SID2ptrExe-BASICbe)+EXEstarter,y ;copy to EXE export
        sta SIDaPtr+1
        sta SIDaPt1+1
        iny
        lda (datzptr),y
        sta (SID2ptrExe-BASICbe)+EXEstarter,y ;copy to EXE export
        beq plyrSet      ;last value?
        sta SIDaPtr+2
        sta SIDaPt1+2
        tya
        lsr
        tax
SIDaPt1 lda selfmodA
        sec
        sbc #<defaultSID2BASE
        sta (SID2regExe-BASICbe)+EXEstarter,x ;copy to EXE export
        clc
        adc SID2add+0
        ldx #0
        jsr SIDaPtr
        inx
        lda SID2add+1
        jsr SIDaPtr
        iny
        cpy #size(extraPlayer.SID2ADDtable) ;safety check
        bne -
plyrSet lda SID2add+1
        sta datzptr+1
        lda SID2add+0
        jsr divsida
        sta SID2sidAdd
.fi
.if (SID_AMOUNT>=3)
WrSID3add lda normalPlayer.player.TUNE_HEADER+DRIVERTYPE_POS ;set player-setter table addresses
        asl
        tay
        lda altplayers.SID3Aadd+0,y
        sta datzptr+0
        lda altplayers.SID3Aadd+1,y
        sta datzptr+1
        ldy #0
-       lda (datzptr),y
        sta (SID3ptrExe-BASICbe)+EXEstarter,y ;copy to EXE export
        sta SIDaPtr+1
        sta SIDaPt2+1
        iny
        lda (datzptr),y
        sta (SID3ptrExe-BASICbe)+EXEstarter,y ;copy to EXE export
        beq plyrSe2      ;last value?
        sta SIDaPtr+2
        sta SIDaPt2+2
        tya
        lsr
        tax
SIDaPt2 lda selfmodA
        sec
        sbc #<defaultSID3BASE
        sta (SID3regExe-BASICbe)+EXEstarter,x ;copy to EXE export
        clc
        adc SID3add+0
        ldx #0
        jsr SIDaPtr
        inx
        lda SID3add+1
        jsr SIDaPtr
        iny
        cpy #size(extraPlayer.SID3ADDtable) ;safety check
        bne -
plyrSe2 lda SID3add+1
        sta datzptr+1
        lda SID3add+0
        jsr divsida
        sta SID3sidAdd
.fi
.if (SID_AMOUNT>=4)
WrSID4add lda normalPlayer.player.TUNE_HEADER+DRIVERTYPE_POS ;set player-setter table addresses
        asl
        tay
        lda altplayers.SID4Aadd+0,y
        sta datzptr+0
        lda altplayers.SID4Aadd+1,y
        sta datzptr+1
        ldy #0
-       lda (datzptr),y
        sta (SID4ptrExe-BASICbe)+EXEstarter,y ;copy to EXE export
        sta SIDaPtr+1
        sta SIDaPt3+1
        iny
        lda (datzptr),y
        sta (SID4ptrExe-BASICbe)+EXEstarter,y ;copy to EXE export
        beq plyrSe3      ;last value?
        sta SIDaPtr+2
        sta SIDaPt3+2
        tya
        lsr
        tax
SIDaPt3 lda selfmodA
        sec
        sbc #<defaultSID4BASE
        sta (SID4regExe-BASICbe)+EXEstarter,x ;copy to EXE export
        clc
        adc SID4add+0
        ldx #0
        jsr SIDaPtr
        inx
        lda SID4add+1
        jsr SIDaPtr
        iny
        cpy #size(extraPlayer.SID4ADDtable) ;safety check
        bne -
plyrSe3 lda SID4add+1
        sta datzptr+1
        lda SID4add+0
        jsr divsida
        sta SID4sidAdd
.fi
        lda #$35
        sta banksel
        rts

.if (SID_AMOUNT>=2)
divsida lsr datzptr+1
        ror
        lsr datzptr+1
        ror
        lsr datzptr+1
        ror
        lsr datzptr+1
        ror
        rts
.fi

RplrDat lda normalPlayer.DataPtr,x ;self-written, read address of absolute-addressing player-instructions which point to music-data
        rts

RplrVal lda normalPlayer.PtrValu,x ;self-written, read of modification-values for the musicdata-addresses (+1, -1, etc.)
        rts

.if (ECHO_SUPPORT!=1)
disrelo lda relocaddr ;display hexa relocation target address
        jsr hexdisp
        tay
        lda hexchar,y
        sta lowvidram+16*40+35
        lda hexchar,x
        sta lowvidram+16*40+36
        ;handle if player-address is not on boundaries
        .if ( <PLAYERADDR != 0 )
         lda #<PLAYERADDR
         jsr hexdisp
         tay
         lda hexchar,y
         sta lowvidram+16*40+37
         lda hexchar,x
         sta lowvidram+16*40+38
        .fi
        rts
.else ;echo-support
disdelay lda EchoDelayTime+1
         jsr decidisp
         sty lowvidram+16*40+31
         sta lowvidram+16*40+32
         rts
decidisp ldy #$2f        ;input:A, output: decimal digits in Y and A
         sec
 -       iny
         sbc #10
         bcs -
         adc #$3a
         rts
.fi

        .if (SID_AMOUNT>=2)
SIDaPtr sta normalPlayer.SID2ADDtable,x ;self-written, write operands of SID2 writing commands
        rts
diSID2a lda SID2add+1
        jsr hexdisp
        tay
        lda hexchar,y
        sta lowvidram+22*40+35
        lda hexchar,x
        sta lowvidram+22*40+36
        lda SID2add+0
        jsr hexdisp
        tay
        lda hexchar,y
        sta lowvidram+22*40+37
        lda hexchar,x
        sta lowvidram+22*40+38
        rts
        .fi
        .if (SID_AMOUNT>=3)
diSID3a lda SID3add+1
        jsr hexdisp
        tay
        lda hexchar,y
        sta lowvidram + (SID_AMOUNT<4?24:23) * 40 + 35
        lda hexchar,x
        sta lowvidram + (SID_AMOUNT<4?24:23) * 40 + 36
        lda SID3add+0
        jsr hexdisp
        tay
        lda hexchar,y
        sta lowvidram + (SID_AMOUNT<4?24:23) * 40 + 37
        lda hexchar,x
        sta lowvidram + (SID_AMOUNT<4?24:23) * 40 + 38
        rts
        .fi
        .if (SID_AMOUNT>=4)
diSID4a lda SID4add+1
        jsr hexdisp
        tay
        lda hexchar,y
        sta lowvidram+24*40+35
        lda hexchar,x
        sta lowvidram+24*40+36
        lda SID4add+0
        jsr hexdisp
        tay
        lda hexchar,y
        sta lowvidram+24*40+37
        lda hexchar,x
        sta lowvidram+24*40+38
        rts
        .fi

inveblk                 ;invert display-block
        ldx outputformat
        lda blockdelim,x
        sta vidzptr
        lda blockdeli2,x
        sta vidzptr+1
        ldy #40-1
-       lda $04c8,y
        and #$7f
        cpy vidzptr
        bmi +
        cpy vidzptr+1
        bpl +
        ora #$80
+       sta $04c8,y
        dey
        bpl -
        rts
blockdelim .byte 1,10,21,32
blockdeli2 .byte 9,20,31,40

disPtyp lda MUSICDATA+DRIVERTYPE_POS ;display selected player-type and video-standard
        asl
        asl
        asl
        tay
        ldx #0
-       lda PlayerTypTx,y
        sta lowvidram+10*40+34,x
        iny
        inx
        cpx #8
        bne -
        ldx TuningType
        beq +
        inx
        bne ++ ;jmp ++
+       ldx PALNTSC
        lda SIDflags+1  ;sign PAL/NTSC in SID-header
        and #%11110011
        ora PALNTSCor,x
        sta SIDflags+1
+       txa
        asl
        asl
        asl
        tay
        ldx #0
-       lda TuningTypeTx,y
        sta lowvidram+12*40+32,x
        iny
        inx
        cpx #8
        bne -
        rts
        .enc "screen"
PlayerTypTx .text "NORMAL  ","MEDIUM  "," LIGHT  "," EXTRA  "," BARE   "," DEMO   "
TuningTypeTx .text "  NTSC  ","  PAL   ","PALVERDI","PAL-JUST"," DREAN  "
        .enc "none"
PALNTSC   .byte 0       ;0=NTSC, 1=PAL, 3=Drean
PALNTSCor .byte %1000,%0100,%0100,%0100 ;PAL/NTSC/Drean values in SID-header
TuningType = MUSICDATA+TUNINGTYPE_POS

;========================== SMALLER SUBROUTINES ===============================
appendSEQW .proc        ;append SEQ file Writing string to filename
        lda #"s"
        .byte $2c       ;jump over lda #
        .cerror appendPRGW & 0
        .pend

appendPRGW .proc        ;append PRG file Writing string to filename
        lda #"p"
        sta PRGwriTxt+1

        ldx menu.bufnlen+1 ;length of selected/typed filename with extension
        ldy #0
-       lda PRGwriTxt,y
        beq +           ;end of string?
        sta menu.namebuf,x
        inx
        iny
        bne -
+       stx bufnln2     ;adjust filename-length to be expanded with ",P,W"
        rts

PRGwriTxt .null ",p,w"  ;the appended modifiers after the filename for PRG-type outputs
        .pend


;------------------------------------------------------------------
;SID DETECTION ROUTINE ;By SounDemon - Based on a tip from Dag Lem.
;Put together by FTC after SounDemons instructions ;...and tested by Rambones and Jeff.
;Idea: there is a one cycle delay in the oscillator on 8580 compared to 6581 when turned on.
; - Won't work in VICE (always detects 6581) unless resid-fp emulation is enabled
detectSIDtype           ;X is 0 at return if 6581, 1 if 8580
        sei             ;No disturbing interrupts
        lda #$ff
        cmp $d012       ;Don't run it on a badline.
        bne *-3
      .if (EMULATOR!=0)
        ldx SIDBASE+$1F ;used by emulator to tell the SID-model
      .else
        lda #$ff        ;Set frequency in voice 3 to $ffff
        sta SIDBASE+$12 ;...and set testbit (other bits doesn't matter) in $d012 to disable oscillator
        sta SIDBASE+$0e
        sta SIDBASE+$0f
        lda #$20        ;Sawtooth wave and gatebit OFF to start oscillator again.
       .page ;strict timing, avoid page-crossings
        sta SIDBASE+$12
        lda SIDBASE+$1b ;Accu now has different value depending on sid model (6581=3/8580=2)
       .endp
        lsr             ;...that is: Carry flag is set for 6581, and clear for 8580.
        ldx #1          ;transfer Carry-flag to X
        bcc +
        dex             ;X=0
        +
      .fi
       jsr dispMod     ;display and set SID-Model in SID header, based on X register
        rts

dispMod ldy #9          ;display and set SID-model based on X register
-       lda NewSIDtxt,y
        cpx #1          ;check if 8580
        beq +           ;branch if 8580
        lda OldSIDtxt,y
+       sta lowvidram+19*40+30,y
        .if (WEBSID_FORMAT==0)
         lda SIDflags+1
         and #(SID_AMOUNT==1)? %11001111 : %00001111
         ora ModelFlag,x ;SID-model
         sta SIDflags+1
         .if (SID_AMOUNT>=3)
          lda SIDflags+0
          and #%11111100
          ora ModelFlag2,x
          sta SIDflags+0
         .fi
        .else
         lda SIDflags+1
         and #%00001111
         ora ModelFlag,x ;SID-model
         sta SIDflags+1
         .if (SID_AMOUNT>=2)
          lda SID2flags+0
          and #%01001111
          ora ModelFlag,x
          sta SID2flags+0
         .fi
         .if (SID_AMOUNT>=3)
          lda SID3flags+0
          and #%10001111
          ora ModelFlag,x
          sta SID3flags+0
         .fi
         .if (SID_AMOUNT>=4)
          lda SID4flags+0
          and #%11001111
          ora ModelFlag,x
          sta SID4flags+0
         .fi
        .fi

        dey
        bpl -
        rts

ModelFlag .if (SID_AMOUNT==1 || WEBSID_FORMAT!=0)
           .byte $10, $20 ;SID-model bits to SIDflags
          .else
           .byte %01010000, %10100000
          .fi
.if (SID_AMOUNT>=3 && WEBSID_FORMAT==0)
ModelFlag2 .byte %00000001, %00000010
.fi

        .enc "screen"
OldSIDtxt .text "6581 (OLD)"
NewSIDtxt .text "8580 (NEW)"
        .enc "none"

;-------------------------------------------------------------------------------
setexport1              ;ask user for input (export-type, player-type, machine-type)
initscr lda #0
        sta $d020
        sta $d021
        ldy #0
-       lda #$20        ;clear screen
        sta $0400,y
        sta $0500,y
        sta $0600,y
        sta $0700,y
        lda #3          ;reset color
        sta $d800,y
        sta $d900,y
        sta $da00,y
        sta $db00,y
        iny
        bne -
txtout1 ldy #39         ;print text out
-       lda makertxt1,y
        sta lowvidram,y
        lda makertxt2,y
        sta lowvidram+40,y
        lda makertxt3,y
        sta lowvidram+3*40,y
        lda makertxt4,y
        sta lowvidram+5*40,y
        lda makertxt5,y
        sta lowvidram+6*40,y
        dey
        bpl -

        lda #(ECHO_SUPPORT!=1? 0 : 2)
        sta outputformat
seloutp jsr inveblk     ;invert corresponding text-block
        jsr keyrele     ;wait to release key  (or possibly still pressed return-key)
soutplp jsr chCurLR
        bne chkretu
        jsr keyhandler.shcbget ;check shift/cbm keys
        lda shiftsi
        beq +           ;IF shift not pressed
        lda outputformat
        .if (ECHO_SUPPORT==1)
         cmp #2
        .fi
        beq ++
        dec outputformat
        jmp ++
+       lda outputformat
        cmp #(SWP_EXPORT==0)? 3 : 2 ;SID not supported (yet) for SWP due to lack of additional LDX+LDY code
        beq +
        inc outputformat
+       jmp seloutp
chkretu cmp #$fd        ;return key?
        bne soutplp
selected                ;output format is selected here
        jsr keyrele     ;wait to release possibly still pressed return-key

;select player-type and machine-type
.if (SWP_EXPORT!=0)
        lda outputformat
        cmp #1 ;Music-data as SWP?
        bne +
        rts
+
.fi
        ldy #39
-       lda makertx5b,y
        sta lowvidram+10*40,y
        lda makertx5c,y
        sta lowvidram+12*40,y
        dey
        bpl -
l1      lda $d012       ;detect video-standard
l2      cmp $d012
        beq l2          ;waiting 1 rasterline's time (no matter CPU speed)
        bmi l1
;the Accumulator here contains the last rasterline that was before $100 ($37:PAL/Drean, $06:NTSC, $05:oldNTSC)
        ldx #0          ;0 for NTSC
        cmp #<(PALfullrast-1) ;$37 ;PAL machine?
        beq dtDrean ;detect Drean?
        ldy #0 ;don't set Verdi/Just tuning for NTSC/Drean (not included due to memory limits)
        beq iniTuni ;jmp iniTuni
dtDrean ldy #$A1 ;have enough loops (63/(65-63)=32 rows) to differentiate PAL vs Drean (they arrive on different lines)
.page ;avoid added cycles by page-boundary, we measure the 2 extra cycles per line of Drean
-       stx $d015    ;4 cycles delay (plus avoid cycles stolen by sprites (badlines are not an issue in border area))
        lda $d012    ;4 cycles ;this is safest if we're at the middle of the rasterrow here
        dey          ;2 cycles
        bne -        ;2 cycles (3 at exit)
.endp
        inx             ;machinetype 1 for PAL/Drean
        cmp #$20    ;rasterline $20 if Drean, $31 if PAL
        bne iniMacT
        ldy #3 ;$81     ;tuningtype 3 for Drean
iniTuni sty TuningType
iniMacT stx PALNTSC     ;set to 0 if NTSC, 1 if PAL/Drean
selPlay jsr disPtyp     ;display selected player-type and video-standard
        jsr keyrele     ;wait to release key  (or possibly still pressed return-key)
sPlayLp jsr chCurLR
        bne chcurdn
        jsr keyhandler.shcbget ;check shift/cbm keys
        ldy MUSICDATA+DRIVERTYPE_POS
        ldx shiftsi
        bne decType
incType iny
        cpy #NumberOfPlayerTypes ;limit range to 0..4
        bcc wriType
        ldy #0
        beq wriType ;jump
decType dey
        bpl wriType
        ldy #NumberOfPlayerTypes-1
wriType sty MUSICDATA+DRIVERTYPE_POS
        jmp selPlay
chcurdn cmp #$7F ;check cursor-down
        bne chkret1
        jsr keyhandler.shcbget ;check shift/cbm keys
        ldx PALNTSC
        ldy TuningType
        lda shiftsi
        bne decTuni
incTuni cpx #0 ;NTSC?
        beq setPAL ;set PAL machinetype first
        iny
        cpy #3+1
        bne wriTuni
setNTSC ldx #0
        ldy #0
        beq wriMach ;jmp wriMach
decTuni cpx #0 ;NTSC?
        beq setDrea ;if NTSC, wrap to Drean
        cpy #0 ;PAL with Normal Tuning?
        beq setNTSC ;set NTSC machinetype
        dey
        bpl wriTuni ;jmp wriTuni
setDrea ldy #3 ;Drean
setPAL  ldx #1 ;PAL
wriMach stx PALNTSC
wriTuni sty TuningType
        jmp selPlay
chkret1 cmp #$fd        ;return key?
        bne sPlayLp
typeset
        jmp keyrele     ;wait to release possibly still pressed return-key

chCurLR lda #$fe        ;select output format (PRG/SEQ/EXE/SID)
        sta $dc00
        lda $dc01
chright cmp #$fb        ;cursor-right key?
        rts

;--------------------------
        .enc "screen"
.if (SID_AMOUNT==1)
 .if (SFX_SUPPORT==0 && SWP_EXPORT==0)
makertxt1 .text "* SID-MAKER FOR  HERMIT SID-WIZARD ",SWversion," *"
 .elsif (SWP_EXPORT!=0)
makertxt1 .text "* SID-MAKER FOR SW",SWversion," WITH SWP-EXPORTS *"
 .elsif (SFX_SUPPORT!=0)
makertxt1 .text "* SID-MAKER FOR SW",SWversion," WITH SFX-SUPPORT *"
 .elsif (ECHO_SUPPORT!=0)
makertxt1 .text "* SID-MAKER FOR SW",SWversion," WITH ECHO-SUPPORT*"
 .fi
.elsif (SID_AMOUNT==2)
makertxt1 .text "*  SID-MAKER FOR  SID-WIZARD ",SWversion," 2SID  *"
.elsif (SID_AMOUNT==3)
makertxt1 .text "*  SID-MAKER FOR  SID-WIZARD ",SWversion," 3SID  *"
.elsif (SID_AMOUNT==4)
makertxt1 .text "*  SID-MAKER FOR  SID-WIZARD ",SWversion," 4SID  *"
.fi
makertxt2 .text "----------------------------------------"
makertxt3 .text "PLEASE SELECT OUTPUT FORMAT WITH CURSOR:"
.if (SWP_EXPORT==0)
 .if (ECHO_SUPPORT!=1)
  makertxt4 .text " C64/PRG   BIN/SEQ    EXE/PRG   SID/SEQ "
  makertxt5 .text " (NORMAL) (RAW DATA) (RUNNABLE) (PC-SID)"
 .else
  makertxt4 .text "                      EXE/PRG   SID/SEQ "
  makertxt5 .text "                     (RUNNABLE) (PC-SID)"
 .fi
.else ;if (SWP_EXPORT!=0)
makertxt4 .text "  PLAYER  MUSICDATA   RUNNABLE    ----- " ;SID export of SWP not (yet) supported
makertxt5 .text "(.DRV.PRG)(.SWP.PRG) (.EXE.PRG)         "
SWPplayer_txt .text "SIW-WIZARD ",SWversion," SWP-MUSICDATA PLAYER     "
.fi
makertx5b .text "PLAYER-TYPE VIA CURSOR LEFT/RIGHT:NORMAL"
makertx5c .text "MODEL&TUNING WITH CURSOR UP/DN:   ....  "
.if (ECHO_SUPPORT!=1)
 makertxt6 .text " RELOCATION ADDRESS VIA +/- KEYS: $0000 "
.else
 makertxt6 .text " ECHO DELAYTIME WITH +/- KEYS: 00 *100ms" ;,ECHO_MILLISECONDS/100," * 100ms"
.fi
makertxt8 .text "SELECT SID-MODEL WITH CURSOR: DETECTING!"
.if (SID_AMOUNT>=2)
makertxt9 .text "SET SID2 BASE-ADDRESS (WITH +/-): $D420 "
.fi
.if (SID_AMOUNT>=3)
makertxt10 .text "SET SID3 BASE-ADDRESS (WITH +/-): $D440 "
.fi
.if (SID_AMOUNT>=4)
makertxt11 .text "SET SID4 BASE-ADDRESS (WITH +/-): $D460 "
.fi
        .enc "none"

;---------------------------------------------------------------------------------
setexport2 ;ask user about relocation-address and SID-type if needed (for SID export)
        lda outputformat
.if (SWP_EXPORT!=0)
        cmp #1 ;Music-data as SWP?
        beq skiprel
.fi
.if (ECHO_SUPPORT!=1)
        cmp #2          ;EXEcutable format? no need for relocation
        bne txtout2
skiprel jmp relocset    ;skip relocation-setting
.fi

txtout2  ldy #39
 -       lda makertxt6,y
         sta lowvidram+16*40,y
         dey
         bpl -

.if (ECHO_SUPPORT!=1)
         jsr disrelo     ;display hexa relocation target address
 setrelo jsr ReloKey     ;modified by Soci - possibly for code-reuse
         jmp setrelo

 ;---------------
 increlo .if (SWP_EXPORT==0)
          lda expoendadd+1   ;end of area to be exported - hi-byte
          sec             ;injected by tlr at CSDB
          sbc #>PLAYERADDR ;injected by tlr at CSDB - this checks if the tune fits into the remaining range below top of memory (RELOC_MAX_HI)
         .else
          lda #>MAX_PLAYERSIZE
         .fi
         clc
         adc #1          ;add 1 for safety to be in range
         adc relocaddr
         bcs +           ;if range passes over top of memory, don't allow further change
         cmp #RELOC_MAX_HI ;max relocation-address - check tunelength!!!
         beq +
         inc relocaddr
 +       rts
 decrelo lda relocaddr
         cmp #RELOC_MIN_HI ;min. reloc. address
         beq +
         dec relocaddr
 +       rts

 ReloKey #plmiret increlo, decrelo, disrelo, keyhandler.repeatex, relocRepSpd2

.else ;echo-support
         jsr disdelay    ;display echo-delay
 setdelay jsr EchoDelayKey
         jmp setdelay

 incdelay ldy EchoDelayTime+1
          cpy #(ECHO_DELAY_MAX/100)
          bcs +
          iny
          sty EchoDelayTime+1
 +        rts
 decdelay ldy EchoDelayTime+1
          beq +
          dey
          sty EchoDelayTime+1
 +        rts

 EchoDelayKey #plmiret incdelay, decdelay, disdelay, keyhandler.repeatex, repspd2

.fi

relocset                ;relocation address is set here
        jsr keyrele     ;now wait to release RETURN key


;detect and set SID-type (old/new) in SID format
        lda outputformat
        cmp #3          ;SID-format?
        bne endSIDset   ;if not, no SID-model setting supported
txtout3 ldy #40-1
-       lda makertxt8,y
        sta lowvidram+19*40,y
        dey
        bpl -
        jsr detectSIDtype ;X is 0 at return if 6581, 1 if 8580 - display included
selSIDm lda $dc01 ;$dc00 was set previously
        cmp #$fb        ;cursor-right key?
        bne chkret3
        txa
        eor #1
        tax
        jsr dispMod     ;display and set SID-Model in SID header, based on X register
        jsr keyrele     ;wait to release key
        jmp selSIDm
chkret3 cmp #$fd        ;return key?
        bne selSIDm
endSIDset               ;SID-model is selected here and set in SID

.if (SID_AMOUNT>=2)
        jsr keyrele
setSID2 ldy #40-1
-       lda makertxt9,y
        sta lowvidram + 22 * 40, y
        dey
        bpl -
        jsr SID23set.CalcSID2addID
        jsr diSID2a
-       jsr SID2key
        jmp -
SID2key #plmiret SID23set.IncSID2address, SID23set.DecSID2address, diSID2a, keyhandler.repeatex, repspd2
.fi

.if (SID_AMOUNT>=3)
        jsr keyrele
setSID3 ldy #40-1
-       lda makertxt10,y
        sta lowvidram + (SID_AMOUNT<4?24:23) * 40, y
        dey
        bpl -
        jsr SID23set.CalcSID3addID
        jsr diSID3a
-       jsr SID3key
        jmp -
SID3key #plmiret SID23set.IncSID3address, SID23set.DecSID3address, diSID3a, keyhandler.repeatex, repspd2
.fi

.if (SID_AMOUNT>=4)
        jsr keyrele
setSID4 ldy #40-1
-       lda makertxt11,y
        sta lowvidram + 24 * 40, y
        dey
        bpl -
        jsr SID23set.CalcSID4addID
        jsr diSID4a
-       jsr SID4key
        jmp -
SID4key #plmiret SID23set.IncSID4address, SID23set.DecSID4address, diSID4a, keyhandler.repeatex, repspd2
.fi

        ;jmp keyrele     ;now wait to release RETURN key

keyrele lda #$ff-($01+$80)
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
        rts

.if (SID_AMOUNT>=2)
SID23set #SID23setter
.fi

setEXEdetails
        lda #$34
        sta banksel
        lda MUSICDATA+DRIVERTYPE_POS ;driver-ID (player-type) into Accumulator
        jsr altplayers.CopyPlayer    ;copy selected player (normal/medium/light/extra) and frequency-table (PAL/NTSC) to 'PLAYERADDR'
        lda MUSICDATA+DRIVERTYPE_POS ;driver-ID (player-type) into Accumulator
        jsr altplayers.SetFreqTb     ;if needed, change to NTSC table
CedInfo ldx MUSICDATA+DRIVERTYPE_POS ;driver-ID (player-type) into Accumulator
        lda madewiX,x
        tax
        ldy #0          ;copy editor-info into exe-starter
-       lda madewi0,x
        sta (madewith-BASICbe)+EXEstarter,y
        inx
        iny
        cpy #(madewi1-madewi0)
        bne -
.if (ECHO_SUPPORT!=1)
        ldy #((1+PALmaxframespeed)*2)-1  ;(*2 to copy both hi and lo at once)
-       lda CiaFrameHiNTSC,y         ;copy suitable CIA-timing for multi-speed SIDs
        ldx PALNTSC
        beq +
        lda CiaFrameHiPAL,y
        ldx TuningType
        cpx #3 ;Drean?
        bne +
        lda CiaFrameHiDrean,y
+       sta CiaFrameHi,y             ;target in CIA-player
        dey
        bpl -
.else ;.if (ECHO_SUPPORT==1)  ;.cwarn 1, format("$%X",altplayers.PlrVolPt[0])
        lda MUSICDATA+DRIVERTYPE_POS
        asl
        tay
        lda altplayers.PlrVolPt+0,y
        ldx altplayers.PlrVolPt+1,y
        sta (EXEmainVol+1-BASICbe)+EXEstarter
        stx (EXEmainVol+2-BASICbe)+EXEstarter
        lda altplayers.PlrFilPt+0,y
        ldx altplayers.PlrFilPt+1,y
        sta (EXEfiltBand+1-BASICbe)+EXEstarter
        stx (EXEfiltBand+2-BASICbe)+EXEstarter
.fi
        lda #$35
        sta banksel
        rts

.if (ECHO_SUPPORT==1)
setEXEdetails2  ;user-made setexport2 modifications to EXE-starter
EchoDelayTime lda #(ECHO_MILLISECONDS/100) ;selfmod  ;in 100ms steps
        asl                ;*2
        ;clc  ;Carry was set to 0 by asl above as delaytime is below $80
        adc EchoDelayTime+1  ;*3 ;echo delay-time is multiplied by 3 here so the setting gives 33*3=100ms granularity
        adc #(>EchoData)
        sta (EchoWritPtr+2-BASICbe)+EXEstarter
        sta (EchoReadPtr+2-BASICbe)+EXEstarter
        sta (ECHOlastPg1+1-BASICbe)+EXEstarter
        sta (ECHOlastPg2+1-BASICbe)+EXEstarter
        rts
.fi

;set SID text (author name and tune-title, and creation-date)
SetSIDt .if (EMULATOR!=0)
.cerror (*>$E000), *
        lda banksel
        pha
        lda #$36
        sta banksel
        ldx #0
-       lda $E009,x ;MIDIbuf1,x ;from fake KERNAL, in SIDmaker it's filled continuously from PC RTC-date
        beq +
        sta SIDreleatx,x
        inx
        cpx #$1F
        bcc -
+       lda #0
        sta SIDreleatx,x ;terminating zero
        pla
        sta banksel
        .fi
        ldx #0          ;will show the position of ':' - if no separator, value will stay 0
        ldy #39
-       lda authorinfo,y
        cmp #":"        ;if there's a ':' separator, the string before will be handled as author-name
        bne +
        tya             ;only the 1st ':' will be used as split-separator
        tax
+       dey
        bpl -
        cpx #0
        bne splitTX     ;if ':' was found
noSplit ldy #31         ;SID-header text field max. size
-       lda #0          ;reset author-info text
        sta SIDauthrtx,y
        lda authorinfo,y
        sta SIDtitletx,y
        dey
        bpl -
        rts
splitTX cpx #32-1
        bpl noSplit     ;if the separator is at illegal place (authorname would be longer than allowed 32
        lda #0
        sta SIDauthrtx,x ;put '0' string-termination
        txa
        tay
-       lda authorinfo-1,y ;right before ':'
        sta SIDauthrtx-1,y
        dey
        bne -
        ldy #0
-       lda authorinfo+1,x ;right after ':'
        sta SIDtitletx,y
        iny
        cpy #32-1       ;max text field-length in SID-header
        beq +
        inx
        cpx #40
        bne -
+       lda #0          ;put '0' string-termination
        sta SIDtitletx,y
        rts

SetSIDply ;set SID player-pointers and player-type (CIA/video depending on multi-speed)
.if (ECHO_SUPPORT!=1)
        lda #>normalPlayer.player.inisub  ;set SID-addresses & timing-mode to the player based on framespeed
        ldx #<normalPlayer.player.inisub
        ldy normalPlayer.player.FRAME_SPD ;framespeed of loaded tune (in header)
        dey ;cpy #1          ;if framespeed is 1 don't use CIA starter code
        beq +
        lda #>CIAINIT
        ldx #<CIAINIT
+       sta SIDinitadd+0
        stx SIDinitadd+1
        lda #>normalPlayer.player.playsub
        ldx #<normalPlayer.player.playsub
        ldy normalPlayer.player.FRAME_SPD ;framespeed of loaded tune (in header)
        dey ;cpy #1          ;if framespeed is 1 don't use CIA starter code
        beq +
        lda #>CIAPLAY
        ldx #<CIAPLAY
+       sta SIDplayadd+0
        stx SIDplayadd+1
        ldx #3               ;set SID timing type (CIA/PAL) based on frame-speed (1x is PAL)
-       lda #0               ;for VIDEO-frame timing of all subtunes
        ldy normalPlayer.player.FRAME_SPD ;framespeed of loaded tune (in header)
        dey ;cpy #1          ;if framespeed is 1 don't use CIA starter code
        beq +                ;single-speed?
        lda #$FF             ;for CIA-timing of all subtunes
+       sta SIDtimerTyp,x
        dex
        bpl -
.fi
        rts


;=============================================================================================================
relozptr                ;add 16 bit 2's complement 'relative' value (in relocamount) to zeropage-pointer assigned in Y register
        lda relocamount+0
        clc
        adc $00,y
        sta $00,y
        iny             ;to point to hi-byte of zeropage-pointer
        lda relocamount+1
        adc $00,y
        sta $00,y
        rts


relocator               ;sets logical address of player to match relocation address - but doesn't move player physically in memory

        lda relocaddr   ;calculate difference
        sec
        sbc #>PLAYERADDR
        sta relocdiff

;-------------------------------------------------------------------------------
relodata ;relocate pointertable data (seq.pointers,pattern-pointers,instrument-pointers)

;adjust sequence-pointers in subtune-data
        ldx TUNEHEADER+SEQAMOPOS
        dex             ;make range 1...x to 0...x-1
seqrelp txa
        ldy #255        ;will count division result
        sec
-       iny
        sbc #CHN_AMOUNT ;'divide A by 3' loop (or by 6 for 2SID, or by 9 for 3SID)
        bcs -
        adc #CHN_AMOUNT
                        ;result of division in Y, remainder in A
        asl             ;multiply remainder by 2, sequence-pointer address inside subtune-data
        sta compzvar2
        tya             ;result of division
        asl             ;multiply by 8 to get to subtune-data base-address for current subtune
        asl
        asl
        .if (SID_AMOUNT>=2)
        asl ;*16
        .fi
        .if (SID_AMOUNT>=3)
        asl ;*32
        .fi
        sec             ;don't disturb sequence-pointer low-bytes
        adc compzvar2
        tay
        lda (subtuneadd),y
        clc
        adc relocdiff
        sta (subtuneadd),y
        dex
        cpx #$ff
        bne seqrelp
;---------------------
;adjust pointers in pointer-tables: ptnptloadd, ptnpthiadd
        ldy TUNEHEADER+PTAMOUPOS ;amount of instruments
-       lda (ptnpthiadd),y
        clc
        adc relocdiff
        sta (ptnpthiadd),y
        dey
        bne -
;-----------------------
;adjust pointers in pointer-tables: insptloadd, inspthiadd
        ldy TUNEHEADER+INSTAMPOS ;amount of instruments
-       lda (inspthiadd),y
        clc
        adc relocdiff
        sta (inspthiadd),y
        dey
        bpl -


;-------------------------------------------------------------------------------
        lda #$34
        sta banksel     ;the relocation tables might be under IO area

        lda normalPlayer.player.TUNE_HEADER+DRIVERTYPE_POS ;set player-setter table addresses
        asl
        tax
        lda altplayers.PlrRelo+0,x
        sta reltadd+1
        sta reltad2+1
        lda altplayers.PlrRelo+1,x
        sta reltadd+2
        sta reltad2+2

.if (PLAYER_FOR_ROM==0 || SWP_EXPORT==0)  ;don't modify ROM-SWP as it uses zeropage-pointers, not absolute-addressing self-modified code
        lda altplayers.PlrDatP+0,x
        sta reltad3+1
        sta reltad4+1
        lda altplayers.PlrDatP+1,x
        sta reltad3+2
        sta reltad4+2
.fi

;-------------------------------------
;relocate bigFX-table entries
        lda altplayers.PlrFxPt+0,x ;set zeropage-pointers for Big-FX table
        sta inszptr+0
        lda altplayers.PlrFxPt+1,x
        sta inszptr+1
        ldy #(normalPlayer.player.ENDBIGFXTB - normalPlayer.player.BIGFXTABLE) - 2 ;the size of table is the same for all players
-       lda (inszptr),y
        clc
        adc relocdiff
        sta (inszptr),y
        dey
        dey
        bpl -

;-----------------------------------
;relocate absolute-address pointers in SWP-player
.if (SWP_EXPORT!=0)
        lda altplayers.PlrDatP_SWP+0,x
        sta inszptr+0
        lda altplayers.PlrDatP_SWP+1,x
        sta inszptr+1
        ldy #1
-       lda (inszptr),y
        beq +
        clc
        adc relocdiff
        sta (inszptr),y
        iny
        iny
        bne -
+
.fi

;-------------------------------------
;relocate target addresses (operands) of absolute commands in player
        ldx #0
reltadd lda normalPlayer.reloctable,x ;self-written, address of relocation-table
        sta datzptr+0   ;zeropage lo-pointer set
        inx
reltad2 lda normalPlayer.reloctable,x;self-written, address of relocation-table
        sta datzptr+1   ;zeropage hi-pointer set
        beq +           ;end of table found?
        ldy #2          ;we modify only 2nd (hi) address-byte of absolute-addressing 6502 instructions in player code
        lda (datzptr),y
        clc
        adc relocdiff
        sta (datzptr),y

        inx             ;go to next relocation-pointer
        bne reltadd
        inc reltadd+2   ;add Carry
        inc reltad2+2
        gne reltadd     ;=jmp
+       ;end of relocation-table reached


.if (PLAYER_FOR_ROM==0 || SWP_EXPORT==0)  ;don't modify ROM-SWP as it uses zeropage-pointers, not absolute-addressing self-modified code

        ldx #0
reltad3 lda normalPlayer.DataPtr,x ;self-written, address of relocation-table
        sta datzptr+0   ;zeropage lo-pointer set
        inx
reltad4 lda normalPlayer.DataPtr,x;self-written, address of relocation-table
        sta datzptr+1   ;zeropage hi-pointer set
        beq +           ;end of table found?
        ldy #2          ;we modify only 2nd (hi) address-byte of absolute-addressing 6502 instructions in player code
        lda (datzptr),y
        clc
        adc relocdiff
        sta (datzptr),y

        inx             ;go to next relocation-pointer
        bne reltad3
        inc reltadd+2   ;add Carry
        inc reltad2+2
        gne reltad3     ;=jmp
+       ;end of relocation-table reached

.fi ;end of if-section: (PLAYER_FOR_ROM==0 || SWP_EXPORT==0)

        lda #$35
        sta banksel


;-------------------------------
;set SID-header jump-address accordingly
reloSID lda SIDinitadd+0
        clc
        adc relocdiff
        sta SIDinitadd+0
        lda SIDplayadd+0
        clc
        adc relocdiff
        sta SIDplayadd+0

.if (ECHO_SUPPORT!=1)
;--------------------------------
;set CIA-init/play addresses in SID accordingly
        lda CIAADDR+1
        clc
        adc relocdiff
        sta CIAADDR+1
;---------------------------------
;CIA-player addresses
        ldx #(CIAplrA_end-CIAplrA)-2
-       lda CIAplrA+0,x
        sta inszptr+0
        lda CIAplrA+1,x
        sta inszptr+1
        ldy #2          ;point to high-byte of absolute-addressing operand of instruction
        lda (inszptr),y
        clc
        adc relocdiff
        sta (inszptr),y
        dex
        dex
        bpl -
.fi
        rts

.if (ECHO_SUPPORT!=1)
;code to be relocated in multispeed SID's CIA-player
        .section data
CIAplrA .word CIAINIT, p_SIDa1,p_SIDa2,p_SIDa3,p_SIDa4, p_SIDa5,p_SIDa6,p_SIDa7,p_SIDa8, CIAPLAY ;0 signs end of this list
CIAplrA_end
        .send
.fi


;---------------------------------------------------------------------------------
unrelocate              ;set everything back to relative of PLAYERADDR (resume relocation, not to affect further exports)
        lda #0          ;negate relocation-difference
        sec
        sbc relocdiff   ;calculate difference backwards
        sta relocdiff   ;reset reloc.address
        jsr reloSID     ;only SID-file pointers to restare - no need to 'unrelocate' the code and music-data, it will be replaced in next round
        rts

;-------------------------variables used by relocator-------------------
relocaddr .byte $40     ;>PLAYERADDR ;hi-address to relocate
relocdiff .byte $00     ;hi-address relative to base address


;===============================================================================================================
;variables
;---------
namemode .byte 2        ;if value is 2, filename-typing/selection mode
menumode .byte 0        ;if value is nonzero, the menu appears above music editor (display of musicdata stops)
ptrdysi  .byte 0        ;signal that notifies key-repeater to wait for pattern-redraw (not needed in exporter)
bufnln2  .byte 0        ;the length of the appended filename

        .enc "screen"
.if (SID_AMOUNT==1)
importxt .text "   SELECT A SW ",SWMversion," MODULE TO CONVERT...   "
.elsif (SID_AMOUNT==2)
importxt .text "  SELECT A SW",SWversion," 2SID MODULE TO CONVERT..  "
.elsif (SID_AMOUNT==3)
importxt .text "  SELECT A SW",SWversion," 3SID MODULE TO CONVERT..  "
.elsif (SID_AMOUNT==4)
importxt .text "  SELECT A SW",SWversion," 4SID MODULE TO CONVERT..  "
.fi
        .enc "none"

;==============================================================================================================
        .include "include/keyhandler.inc";handle the keypresses - the subroutine-names are found in the datatables.inc file ;callback subroutines for keypresses
        .include "include/menu.inc";for SID-Maker only file-dialog used. - this must be under $e000!

        .dsection data  ;tables/variables being displayed and used ;all data from other sources (between '.section data' and '.send data' comes here


BEFORE_IO_AREA_end = *
IO_AREA_ADDRESS = $D000
.cerror (*>IO_AREA_ADDRESS),"TOO MUCH CODE BEFORE IO-AREA AT $D000! DECREASE 'MUSICDATA_END' TO MOVE THE CODE BACK A BIT!"
LAST_CODE_AREA
;==============================================================================================================
        .include "include/altplayers.inc" ;this file contains the alternative players that can overwrite the default

;editor info copied into the 'exe' export (depending on player-type)
        .enc "screen"
.if (SID_AMOUNT==1)
 .if (ECHO_SUPPORT!=1)
  .if (PLAYER_FOR_ROM==0)
   madewi0 .text "     -- Composed with SID-Wizard v"
  .else
   madewi0 .text "-- Composed with SIDwizard (ROM) v"
  .fi
 .else
 madewi0 .text "-- Composed with SID-Wizard echo v"
 .fi
madewi1 .text "--Composed via SID-Wizard-medium v"
madewi2 .text "-- Composed via SID-Wizard-light v"
madewi3 .text "-- Composed via SID-Wizard-extra v"
madewi4 .text "--- Composed via SID-Wizard-bare v"
madewi5 .text "--- Composed via SID-Wizard-demo v"
.elsif (SID_AMOUNT==2)
madewi0 .text "-- Composed with SID-Wizard 2SID v"
madewi1 .text "Composed via SID-Wizard-med.2SID v"
madewi2 .text "Composed via SIDwizard-light 2SID "
madewi3 .text "Composed via SIDwizard-extra 2SID "
madewi4 .text "Composed via SID-Wizard-bare 2SID "
madewi5 .text "Composed via SID-Wizard-demo 2SID "
.elsif (SID_AMOUNT==3)
madewi0 .text "-- Composed with SID-Wizard 3SID v"
madewi1 .text "Composed via SID-Wizard-med.3SID v"
madewi2 .text "Composed via SIDwizard-light 3SID "
madewi3 .text "Composed via SIDwizard-extra 3SID "
madewi4 .text "Composed via SID-Wizard-bare 3SID "
madewi5 .text "Composed via SID-Wizard-demo 3SID "
.elsif (SID_AMOUNT==4)
madewi0 .text "-- Composed with SID-Wizard 4SID v"
madewi1 .text "Composed via SID-Wizard-med.4SID v"
madewi2 .text "Composed via SIDwizard-light 4SID "
madewi3 .text "Composed via SIDwizard-extra 4SID "
madewi4 .text "Composed via SID-Wizard-bare 4SID "
madewi5 .text "Composed via SID-Wizard-demo 4SID "
.fi
        .enc "none"
madewiX .byte <(madewi0, madewi1, madewi2, madewi3, madewi4, madewi5)-madewi0

LAST_CODE_AREA_end
;-----------------------------------------------------------------------------------
EXPORTER_CODE_END

.cerror (*<MUSICDATA_END),* ;"YOU'RE UNFORTUNATELY OVER 64KB! LESSEN 'MUSICDATA_END' TO MOVE THE CODE BACK A BIT! "
MAX_ALLOWED_ADDRESS = $fff0
.cerror (*>MAX_ALLOWED_ADDRESS),"YOU'RE UNFORTUNATELY OVER 64KB! LESSEN 'MUSICDATA_END' TO MOVE THE CODE BACK A BIT! ", * - MAX_ALLOWED_ADDRESS
;==================================================================================================================
