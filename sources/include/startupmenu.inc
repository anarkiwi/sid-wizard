;ident:8,24
;========================== startup-menu (select playertype/PAL-NTSC/freq.table/help) ========================
;at program-startup it is in pattern-area, then will be overwritten at pattern-initialization at startup)
; $Id: startupmenu.inc 382 2014-06-23 17:53:01Z hermitsoft $
altplayers=STARTUPMENU.altplayers

stmenuXpos=0
stmenuYpos=6
mnptpos=16      ;x-position of menupoints
tickColumn1=0
tickColumn2=39
titlecol=13
decorcol=12
tickcol=6       ;ticks' colour
bordcol=3       ;menu-border colour
featcol=11      ;feature-name colour
fademin=7       ;menupoint-colour position in fade-list
fademax=15      ;selected menupoint-colour in fade-list
sfadespd=2      ;menupoint-fadeout speed
fadespd=2       ;border-fadeout speed

STARTUPMENU .block      ;called from the basic starter




doit    sei

        lda $ba         ;the default device might be where SID-Maker was loaded from
        cmp #13+1       ;if above 13, force 8 as default
        bcs +
        cmp #8
        bcs ++          ;if below 16 and greater than / equal to 8, no forcing, leave as it is
+       lda #ddevice    ;default device number if exporter was loaded from non-disk drive
+       sta menu.devicen+1
        lda #$35
        sta banksel
        ;NMI (RESTORE key) pointer setting - so pressing RESTORE won't freeze depacking/startup
        lda #<NMI
        sta $fffa
        lda #>NMI
        sta $fffb
        lda #<ROMRTI
        sta $0318
        lda #>ROMRTI
        sta $0319
        lda #<ROMIRQ
        sta $0314
        lda #>ROMIRQ
        sta $0315

LoadConfig
        jsr menu.SetCfg2
        ldx #<Config
        ldy #>Config
        lda #$00        ;$00 means: load to memory (not verify)
        jsr KERNAL.LOAD ;call LOAD, outputs last loaded address to X/Y in case of success
        bcc +           ; if carry set, a load error has happened
cfgLerr ldx #"?"
        stx menu.cfgstat
        cmp #KERNAL.IOerror.DEVICE_NOT_PRESENT ;check status in Accumulator after LOAD, Device not present?
        beq +           ;if no drive don't attempt to read its status-channel
        cmp #KERNAL.IOerror.FILE_NOT_FOUND
        bne +           ;if other error than file-not-found, avoid possible incompatibilities of READST, leave LED blinking
        jsr RDERROR     ;suppress flashing drive LED if no config file was found
+       lda #$35
        sta banksel
        jsr CopyFeat    ;copy features to the main editor code

        lda #0
        sta $d011       ;hide remaining graphics after splash-screen-depacking
        sta $d015
        sta $d021
        ;sta $d020       ;will be set by effect
        lda #3
        sta $dd00
        lda #$17
        sta $d018
        ldy #0
-       lda #$20
        sta lowvidram,y
        sta lowvidram+$100,y
        sta lowvidram+$200,y
        sta lowvidram+$300,y
        lda #decorcol
        sta COLRPOS,y
        sta COLRPOS+$100,y
        sta COLRPOS+$200,y
        sta COLRPOS+$300,y
        iny
        bne -

        jsr commonsubs.PALorNTSC ;detect if PAL or NTSC machine, store 0/1/3 result at a memory location 'PALNTSC'
        ;ldx PALNTSC
        cpx #1
        bne +
        ldy #40-1
-       lda tunintx,y
        sta lowvidram+24*40,y
        dey
        bpl -
        jmp +++
+       ldy #4
-       lda NTSCtx,y
        cpx #3 ;Drean?
        bne +
        lda DREANtx,y
+       sta machitx,y
        dey
        bpl -
+       lda Config.TuningType
        beq +
        cmp #3 ;Drean?
        beq +
        jsr disTuni
+
        lda #$1b
        sta $d011

        jsr dismenu             ;display menu (static part)

;-------------------------------
        lda $d020       ;init fader
        and #$0f
        tay
        lda ColBright,y
        tay
        sta colcnt

ScrLoop lda $d011
        bmi ScrLoop
        lda #$20
        cmp $d012
        bne *-3

        dec fadespcnt
        bne KeyBdCh
        lda #fadespd
        sta fadespcnt
        ldy colcnt      ;fade to black
        beq +
        dey
+       sty colcnt
        lda BrightCol,y
        sta $d020

KeyBdCh jsr keyhandler.shcbget ;Shift/C= key status
        lda #$FE
        sta $dc00
        lda $dc01
chCurDn cmp #($ff-$80)
        bne chCurRi
        lda shiftsi
        beq incsele
decsele lda norepeat
        bne endKbCh
        jsr resetsfade
        dec selection
        bpl newmnpt
        lda #NumberOfPlayerTypes-1
        sta selection
newmnpt lda #0
        sta scrollcnt
        jsr CopyFeat    ;copy features to the main editor code
        jmp endKbCh
incsele lda norepeat
        bne endKbCh
        jsr resetsfade
        ldy selection
        iny
        cpy #NumberOfPlayerTypes
        bmi +
        ldy #0
+       sty selection
        jmp newmnpt

chCurRi cmp #($ff-4)
        bne chRetur
        lda norepeat
        bne endKbCh
        ldx Config.TuningType
        inx
        cpx #3+1
        bne +
        ldx #0
+       stx Config.TuningType
        jsr disTuni
        jmp endKbCh

chRetur cmp #($ff-2)
        beq SetPlayer   ;exit loop if return pressed
chSpace	lda #$7F
	sta $dc00
	lda $dc01
	cmp #($FF-$10)
	beq SetPlayer

nokeypr lda #$ff
        sta norepeat    ;causes 0 in 'norepeat'
endKbCh inc norepeat

Display jsr ColMenuPt   ;colourize menu-points based on selection
        jsr playertxt   ;display player-description
        jsr DispFeat    ;display feature-signals

        lda #($d2-1)-8
        jsr doSkewy
        lda #($d2-1)
        jsr doSkewy

        jmp ScrLoop

;-----------------------------
SetPlayer
        lda #0
        sta $d011
        sta $d020

        lda selection
        beq ChkFreqTb   ;normal player? don't modify it...
        jsr altplayers.CopyPlayer ;copy the selected player into place of the default
        lda player.PLAYER_TYPE
	sta DrvType

AdjustEditor            ;adjust some editor-addressing to fully adopt the new player
        lda selection
        beq ChkFreqTb
        cmp #1
        bne + ;medium?
        lda mediumPlayer.player.aEXPTBH+0
        ldx mediumPlayer.player.aEXPTBH+1
        jmp wriPtPt
+       cmp #2
        bne + ;light?
        lda lightPlayer.player.aEXPTBH+0
        ldx lightPlayer.player.aEXPTBH+1
        jmp wriPtPt
+       lda extraPlayer.player.aEXPTBH+0
        ldx extraPlayer.player.aEXPTBH+1
wriPtPt sta playadapter.dispScope.p_EXPT2+1
        stx playadapter.dispScope.p_EXPT2+2
        sta upbordisp.p_EXPT3+1
        stx upbordisp.p_EXPT3+2
        clc
        adc #8 ;+8
        bcc +
        inx
+       sta playadapter.dispScope.p_EXPT1+1
        stx playadapter.dispScope.p_EXPT1+2
        clc
        adc #1 ;+9
        bcc +
        inx
+       sta upbordisp.p_EXPT4+1
        stx upbordisp.p_EXPT4+2

ChkFreqTb
        lda selection
        jsr altplayers.SetFreqTb ;if needed, changes to NTSC table or Verdi-tuning

        ldy selection
        jsr altplayers.SetTypeIcon ;embed the Light / Medium / Bare / Demo / Extra player-type letters into the SID-Wizard logo

.if (SID_AMOUNT>=2)
        jsr Copy2SIDa   ;copy SID2 addressing addresses to the main editor code
.fi
.if (SID_AMOUNT>=3)
        jsr Copy3SIDa   ;copy SID3 addressing addresses to the main editor code
.fi

        rts             ;continue to the editor which is now set


;--------------------------------------------------------------
.include "altplayers.inc" ;this file contains the alternative players that can overwrite the default



;---------------------------------subroutines-----------------------------------
RDERROR .proc
        lda #$00      ; no filename
        tax
        tay
        jsr KERNAL.SETNAM
        lda #15       ; file number
        ldx $BA       ; last used device number
        tay ;ldy #15  ; secondary address (error channel)
        jsr KERNAL.SETLFS 
        jsr KERNAL.OPEN
        bcs error     ; if Carry set (is it?), the file could not be opened  (some people assume reading address $90 is better)
        ldx #15       ; filenumber     
        jsr KERNAL.CHKIN ; call CHKIN (file 15 now used as input)
        ldy #0
loop    jsr KERNAL.READST ; call READST (read status byte)
        cmp #KERNAL.IOerror.NO_ERROR ; test Accumulator for status-info
        bne closdev   ; either EOF or read error
        jsr KERNAL.CHRIN ; call CHRIN (get a byte from file) - CHRIN tends to 'freeze' with exotic/incompatible device
        iny           ; safety count to avoid being stuck in endless loop
        bne loop      ; next byte
closdev lda #15       ; filenumber    
        jsr KERNAL.CLOSE
        ;jsr KERNAL.CLRCHN
        rts
error   ; Akkumulator contains BASIC error code ; most likely error: ; A = $05 (DEVICE NOT PRESENT)
        jmp closdev  ; even if OPEN failed, the file has to be closed
        .pend


dismenu ldy #39
-       lda titletx,y
        sta lowvidram+1*40,y
        lda #titlecol
        sta COLRPOS+1*40,y
        lda #$64
        sta lowvidram,y
        lda #$63
        sta lowvidram+2*40,y
        lda cfgOKtx,y
        ldx menu.cfgstat
        cpx #"?"
        bne +
        lda cfgERtx,y
+       sta lowvidram+3*40,y
        lda selectx,y
        sta lowvidram+5*40,y
        lda machitx,y
        sta lowvidram+23*40,y
        lda #11
        sta COLRPOS+23*40,y
        dey
        bpl -

        lda #<menugraph
        ldx #>menugraph
        sta datzptr+0
        stx datzptr+1
        jsr setmrow1    ;set screen pointers to menu and zpvari2 to size

menudlp ldy #stmenuXsize-1
-       lda (datzptr),y
        ldx #tickcol    ;tick-mark colours
        cpy #39
        beq +++
        cpy #0
        beq +++
        cpy #14         ;menu itself shouldn't be converted to ASCII
        bmi ++
        ldx #bordcol    ;border color of menu
+       cpy #25
        bmi ++
+       ldx #featcol    ;features' text colour at menu-area
+       sta (vidzptr),y ;write characters
        txa
        sta (inszptr),y ;colourize
        dey
        bpl -
        lda datzptr+0
        clc
        adc #stmenuXsize
        sta datzptr+0
        bcc +
        inc datzptr+1
+       jsr nextrow

        dec zpvari2
        bpl menudlp

        ldy #33
-       lda rastitext,y
        sta lowvidram+22*40+3,y
        lda #11
        sta COLRPOS+22*40+3,y
        dey
        bpl -

        rts

setmrow1 ;set screen pointers to menu and zpvari2 to size
        lda #<lowvidram+menumempos
        ldx #>lowvidram+menumempos
        sta vidzptr+0
        stx vidzptr+1
        lda #<COLRPOS+menumempos
        ldx #>COLRPOS+menumempos
        sta inszptr+0
        stx inszptr+1
        ldx #stmenuYsize-1
        stx zpvari2
        rts

nextrow ;increase videoRAM & colourRAM pointers by 40
        lda vidzptr+0
        clc
        adc #40
        sta vidzptr+0
        lda inszptr+0
        clc
        adc #40
        sta inszptr+0
        bcc +
        inc vidzptr+1
        inc inszptr+1
+       rts

;-------------------------------
disTuni ldy #27-1
-       lda tuNormTx,y
        ldx Config.TuningType
        beq ++ ;0?
        lda tuVerdTx,y
        cpx #2
        bmi ++ ;1?
        bne +  ;2/3?
        lda tuJustTx,y
        jmp ++
+       lda tuDreanTx,y
+       sta lowvidram+24*40+13,y
        dey
        bpl -
        rts

;--------------------------------------------------------
ColMenuPt               ;colourize menu-points based on selection
        dec sfadecnt
        bne colsele
        lda #sfadespd
        sta sfadecnt
        ldx #NumberOfPlayerTypes-1
-       lda scolcnt,x
        cmp #fademin
        beq +
        dec scolcnt,x
+       tay
        lda BrightCol,y
        jsr colpont
        dex
        bpl -

colsele ldx selection
        lda BrightCol+fademax

colpont pha             ;input: X=number of menupoint, A=color to it
        lda scolpos1,x 
        sta vidzptr+0
        lda scolpos2,x
        sta vidzptr+1
        pla
        ldy #7
-       sta (vidzptr),y
        dey
        bpl -
colmret rts

resetsfade
        ldx selection
        lda #fademax
        sta scolcnt,x
        rts

;----------------------------------
playertxt
        ldy scrollcnt
        cpy #40
        beq pltxend
        inc scrollcnt
        ldx #0
-       lda lowvidram+19*40+1,x
        sta lowvidram+19*40+0,x
        lda lowvidram+20*40+1,x
        sta lowvidram+20*40+0,x
        inx
        cpx #39
        bne -
        lda selection
        asl
        tax
        lda altplayers.descrAddr+0,x
        sta datzptr+0
        lda altplayers.descrAddr+1,x
        sta datzptr+1
        lda (datzptr),y
putscrl sta lowvidram+19*40+39
        tya
        clc
        adc #40
        tay
        lda (datzptr),y
        sta lowvidram+20*40+39
pltxend rts

scrollcnt .byte 0

;--------------------------------------
CopyFeat                ;copy player-features to the main code
        lda selection
        asl
        tax
        lda altplayers.FeatIadd+0,x
        sta datzptr+0
        lda altplayers.FeatIadd+1,x
        sta datzptr+1
        ldy #(FeatInstances_end-FeatInstance1)-1
-       lda (datzptr),y
wrFeat  sta FeatInstance1,y
        dey
        bpl -
        rts


;-------------------------------------
DispFeat                ;display feature-signals
        jsr setmrow1    ;set screen pointers & zpvari2
        ldx #0          ;counts rows
-       lda #$7a        ;check-mark (tick)
        ldy FeatInstance1,x
        bne +
        lda #" "        ;space (lack of tick)
+       ldy #tickColumn1
        sta (vidzptr),y
        lda #$7a        ;check-mark (tick)
        ldy FeatInstance2,x
        bne +
        lda #" "        ;space (lack of tick)
+       ldy #tickColumn2
        sta (vidzptr),y
        jsr nextrow
        inx
        dec zpvari2
        bpl -

        lda selection
        asl
        asl
        tay
        ldx #0
-       lda altplayers.rastimes,y
        sta lowvidram+22*40+15,x
        lda altplayers.plysizes,y
        sta lowvidram+22*40+33,x
        iny
        inx
        cpx #4
        bne -

        rts




;--------------------------------------
.if (SID_AMOUNT>=2)
Copy2SIDa  ;copy SID2 addressing addresses to the main code
        lda selection
        asl
        tax
        lda altplayers.SID2Aadd+0,x
        sta datzptr+0
        lda altplayers.SID2Aadd+1,x
        sta datzptr+1
        ldy #0
-       lda (datzptr),y
wr2SIDa sta SID2addTbl,y
        sta rd2SIDa+1
        beq + ;exit after writing quiting 0
        iny
        lda (datzptr),y
        sta SID2addTbl,y
        sta rd2SIDa+2
rd2SIDa lda selfmodA
        sec
        sbc #<defaultSID2BASE
        sta SID2regTbl-1,y
        iny
        cpy #size(extraPlayer.SID2ADDtable) ;safety check
        bne - ;avoid endless loop
+       rts
.fi

.if (SID_AMOUNT>=3)
Copy3SIDa  ;copy SID3 addressing addresses to the main code
        lda selection
        asl
        tax
        lda altplayers.SID3Aadd+0,x
        sta datzptr+0
        lda altplayers.SID3Aadd+1,x
        sta datzptr+1
        ldy #0
-       lda (datzptr),y
wr3SIDa sta SID3addTbl,y
        sta rd3SIDa+1
        beq + ;exit after writing quiting 0
        iny
        lda (datzptr),y
        sta SID3addTbl,y
        sta rd3SIDa+2
rd3SIDa lda selfmodA
        sec
        sbc #<defaultSID3BASE
        sta SID3regTbl-1,y
        iny
        cpy #size(extraPlayer.SID3ADDtable) ;safety check
        bne - ;avoid endless loop
+       rts
.fi

;----------------------------------------------------
.align $100
doSkewy                 ;skew letters to italic with $d016
.page
        cmp $d012
        bcs *-3
        ldx #15
        stx $d016
        ldy #9          ;for PAL
        lda PALNTSC
        beq +
        cmp #3 ;Drean?
        bne ++
+       ldy #11         ;for NTSC/Drean
+       dey
        bne *-1
        ldx #6
skewLop lda skewset,x
        sta $d016
        ldy #10
        dey
        bne *-1
        dex
        bpl skewLop
        nop
        rts
.endp

;--------------------------- literals, variables -------------------------------

selection = Config.PlayerType
;selection .byte 0 ;the currently selected menupoint 0..4
norepeat  .byte 0 ;if value is 0, repeat is disabled

        .enc screen
.if (SID_AMOUNT==1)
titletx .text"      SID-WIZARD ",SWversion,"  STARTUP-MENU      ","  "
.elsif (SID_AMOUNT==2)
titletx .text"    SID-WIZARD ",SWversion," 2SID STARTUP-MENU    ","  "
.elsif (SID_AMOUNT==3)
titletx .text"    SID-WIZARD ",SWversion," 3SID STARTUP-MENU    ","  "
.fi
cfgOKtx .text"Configuration file loaded successfully.."
cfgERtx .text"Configuration file was not (yet) found! "
selectx .text"  Please select your preferred player:  "
machitx .text" PAL  machine detected,pitches adjusted."
NTSCtx  .text"NTSC "
DREANtx .text"Drean"
tunintx .text"Select tuning with Cursor-Left/Right key"
tuNormTx  .text           ": Equal-tempered, A5=440Hz "
tuVerdTx  .text           ": Verdi-tuning ,  A5=432Hz "
tuJustTx  .text           ":Just-inton.keyC,A=436.05Hz"
tuDreanTx .text           ":Drean(PALtiming,NTSCpitch)" 
        .enc none

        .enc gfx
        .cdef "az",1, " ?",32, "AZ",65
        .cdef "^^",$70, "__",$40, "[[",$5d, "@@",$7a
        .cdef "~~",$6d, "]]",$7d, "\\",$6e
menugraph .text "@ Orderlist-FX ^________\ ArpTable-NOP @"
menurow1  .text "@ Transpose-FX [ NORMAL [ HR. types    @"
          .text "@ Instr.Octave [        [ Frame1-conf. @"
          .text "@ Chord-Table. [ MEDIUM [ Tempo-progr. @"
          .text "@ Calc.Vibrato [        [ Tempos 0...2 @"
          .text "@ Vibrato-Type [ LIGHT! [ Delayer-FX.  @"
          .text "@ Detune-Table [        [ Subtune-Jump @"
          .text "@ Portam+vibra [ EXTRA! [ Save ZeroPg. @"
          .text "@ WF-ARP speed [        [ GhostRegist. @"
          .text "@ NoteOFF ptr. [ (BARE) [ 11bit-filter @"
          .text "@ PW/Fil.reset [        [ FiltContrlFX @"
          .text "@ Keybrd.track [  DEMO  [ No tableskip @"
          .text "@ FiltShift-FX ~________] SmalFx9,B..F @"
          .enc none
menuend
        .enc screen
rastitext .text "RasterTime: $1C  Player-Size: $A00"
        .enc none

menumempos=40*stmenuYpos+stmenuXpos
stmenuXsize=menurow1-menugraph
stmenuYsize=(menuend-menugraph)/stmenuXsize

scolpos1 .byte <(COLRPOS+menumempos+40*1+mnptpos),<(COLRPOS+menumempos+40*3+mnptpos),<(COLRPOS+menumempos+40*5+mnptpos),<(COLRPOS+menumempos+40*7+mnptpos),<(COLRPOS+menumempos+40*9+mnptpos),<(COLRPOS+menumempos+40*11+mnptpos)
scolpos2 .byte >(COLRPOS+menumempos+40*1+mnptpos),>(COLRPOS+menumempos+40*3+mnptpos),>(COLRPOS+menumempos+40*5+mnptpos),>(COLRPOS+menumempos+40*7+mnptpos),>(COLRPOS+menumempos+40*9+mnptpos),>(COLRPOS+menumempos+40*11+mnptpos)
scolcnt  .fill NumberOfPlayerTypes, fademax ;fader colour-counters for the menupoints
sfadecnt .byte sfadespd

BrightCol .byte $00,$99,$66,$BB,$22,$44,$88,$CC,$EE,$55,$AA,$FF,$33,$DD,$77,$11 ;C64 colours in order of brightness (from darkest to brightest)
ColBright .byte 0,15,4,12,5,9,2,14,6,1,10,3,7,13,8,11 ;pace of C64 colours on brightness-scale
colcnt    .byte 15        ;color-fader's colour-counter
fadespcnt .byte fadespd ;colour-fader speed-counter

skewset .byte 8,9,10,11,12,13,14,15

.bend ;end of Startupmenu-block


;=============================================================================================================
; vim: sw=4 ts=4 syntax=asm:
