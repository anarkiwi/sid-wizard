;===============================================================================
; $Id: irq.inc 376 2014-04-21 20:10:44Z hermitsoft $

;---------------- framespeed-raster table - editor's version -------------------
disprast=lastrast-8 ;($f2) the 1st irq-rasterrow starting with the row26 trick and handling display,etc.
singrast=(row1rast-4)-1 ;preferred location for single-speed playback-rasterbar
upborderpos=$19

;======================== IRQ ENTRY POINT ======================================
IRQ .proc               ;all music routine, rasterfx and key-requests
        pha             ;store sensitive registers and bank status
        lda banksel
        pha
entry2  txa             ;faster entry from MIDI-C64
        pha
        tya
        pha
        lda #$35        ;only IO area is above RAM
        sta banksel
        lda $d019       ;safety check if IRQ was called by raster-IRQ
.if (MIDI_support!=0)
rastsig and #$FF ;selfmod ;and don't allow nested raster-interrupts
.fi
        bmi + ;fspdctr
        jmp retirq
+
.if (MIDI_support!=0)
        inc rastsig+1   ;00: raster IRQ in progress
        ldx MIDIdev
        cpx #2          ;IRQ-MIDI turned on?
        bmi +
-       sta $d019       ;wait till rasterline ends (so no extra IRQ happens)
        lda $d019
        bmi -
        cli
+
.fi

;----------------------- SELECT SINGLE/MULTI RASTERBAR -------------------------
fspdctr lda #selfmod    ;multispeed frame-position timer
        beq rast1st
        
        jsr singmul
 
        jmp dendply

;----------------------- SINGLE-SPEED PLAYBACK-PART HANDLING -------------------
singmul ldy framesp
        cmp singnum,y   ;single-speed playbar always at the best (spriteless,visible) place
        bne multisp     ;check whether multispeed part of raster-irq
singlep lda inirequ+1
        bne singlnd
playchk ldy playadapter.playmod
        ldx plybcol,y   ;rasterbar-colours for different playermodes (in datatables.inc)
        jsr commonsubs.barcolo
        tya ;lda playmod
        beq +           ;if playmode is 0, tune doesn't play
        lda VESSELMODE
        cmp #2
        beq _skipPlayback
        jsr playadapter.playsubb ;runtime:$10..$28 rasterlines; call singlespeed part of the player routine ($1003),measure rastertime
_skipPlayback      
        lda framesp
        cmp #(SID_AMOUNT==1)? 6 : (SID_AMOUNT==2)? 3 : 2 ;need gap before next multispeed-call
        bcs singlnd
+       .if (MIDI_support!=0)
        jsr midisubs.useMIDI
        .fi
        jsr playadapter.jamplay ;keyboard-jamming part in playadapter.inc
singlnd jsr endmulp     ;border colour back
        jmp playadapter.padapter ;runtime:4 rasterlines; player-adapter, connects player to editor by refreshing counters, etc.

;----------------------- MULTISPEED PLAYBACK-PART HANDLING ---------------------
multisp lda inirequ+1 ;ldy playmod ;if playmode is 0, tune doesn't play
        bne endmulp     ;check playmode and call multiplay part of the player routine ($1006)
fastfwd lda #selfmod    ;fast-forward signal
        bne playchk ;singlep
        ldx #multibarcol ;colour of multispeed rasters  ( #multilincol;row 0 colour of multispeed-rasters)
        jsr commonsubs.barcolo
+       jsr player.mulpsub ;multiplay part of the player routine ($1006)
endmulp ldx #bordercolor
        stx $d020
        rts

;========================== IRQ 1ST RASTERBAR ==================================
rast1st                 ;singlespeed playroutine raster + key & cursor handling
        ldx #1
        stx display.syncscr+1 ;vblank-syncronize flag for displayroutine
.if (MIDI_support!=0 && MIDI_Legacy_support!=0)
        ldx #MIDIC64_Watchdog_ini
        stx MIDIC64.watchdog
.fi

;----------------------- GENERATING ROW 26 (ORDERLIST-POSITIONS) ---------------
        ldy #(disprast+1) ;($f4) row26 trick, software-based colour-RAM handling
        cpy $d012
        bcs *-3
        ldx #6
        lda $d001
        cmp #$e2
        bcc +           ;if cursor at row25, timing changes a bit
        ldx #4
        lda menumode
        bne +
        ldx #1
+       dex
        bne *-1
        cmp ($c1,x)     ;wait a bit (a 'nop' was enough for PAL machine, but NTSC needs a bit more delay)
row25ca lda COLRPOS+$3c0 ;set back colors of 25th row (tick colouring) - instead of NOP
        ldx #$1f
        stx $d011       ;doing the 26th row trick
        iny
        sty $d018
        ldx COLRPOS+$3ca
        ldy COLRPOS+$3d4
        sta COLtemp+0
        stx COLtemp+1
        sty COLtemp+2
        lda fxcol
        sta COLRPOS+$3c0 ;set char-colors of 26th row
        sta COLRPOS+$3c1
        sta COLRPOS+$3c2
        sta COLRPOS+$3c3
        sta COLRPOS+$3c4 ;5,6,7,8,9 are left out, because already FX-colour
        sta COLRPOS+$3ca
        sta COLRPOS+$3cb
        sta COLRPOS+$3cc
        sta COLRPOS+$3cd
        sta COLRPOS+$3ce ;0f,10,11,12,13 are already FX-colour
        sta COLRPOS+$3d4
        sta COLRPOS+$3d5
        sta COLRPOS+$3d6
        sta COLRPOS+$3d7
        sta COLRPOS+$3d8 ;19,1a,1b,1c,1d are already FX-colour

        sta COLRPOS+$3de
        sta COLRPOS+$3df
        sta COLRPOS+$3e0

        sta COLRPOS+$3e1
        sta COLRPOS+$3e2
        sta COLRPOS+$3e3
        sta COLRPOS+$3e4
        sta COLRPOS+$3e5
        sta COLRPOS+$3e6
        sta COLRPOS+$3e7

        ldx #15
        ;stx $d020       ;separator line below row25
        stx $d021

        ldy #2
        dey
        bne *-1         ;Y=0 - background color behind orderlist-numbering positions
        lda #11
        ;sta $d020
        sty $d021

        ;lda $ffff
        ;sta commonsubs.FFFFsto+1

;--------------------- PREPARE LOWER BORDER SPRITES (ORDERLIST) ----------------
        ;lda #$ff        ;draw low-border sprites for orderlist (ptr,color in displayer2/'iniscrn')
        ;eor menumode    ;hide cursor if menu is displayed
        ;sta $d015
        lda #1 ;PALNTSC
        ldy #12
-       sta $d003,y
        dey
        dey
        bpl -


;----------------------------- OPEN VERTICAL BORDER ----------------------------
        lda #(lastrast-1) ;($fa) rasterline to open vertical border
        cmp $d012
        bcs *-3
        lda #$17
        sta $d011       ;causes opening of vertical border



;-------------- DISPLAY CURSOR-SPRITE & BLINK ORDERLIST-POSITION ---------------
        jsr cursord     ;time:3 rasterlines; put cursor to coordinate & flash colour
        jsr display2    ;runtime:1 rasterline; set refreshers to refresh window displays outside IRQ
        .if POS_DISP_MOD==2
        jsr blinkpos    ;runtime:1 rasterline; blink current playback-positions (in displayer2.inc)
        .fi



;------------------- SHOW LOWER BORDER SPRITES (ORDERLIST) ---------------------
        ldx #12         ;the lower separator bar colour
        ;lda #0
        ;sta $ffff       ;idle graphics for the lower border replaces now irq hi-byte
;----------------
        lda $d012
        bpl +             ;if MIDI-interrupt caused delay, skip waiting for rasterline
        lda #(lastrast+3) ;($fe) wait for  raster-row below orderlist-numbering
        cmp $d012
        bcs *-3
+       ldy #10
        dey
        bne *-1
        ;stx $d020       ;light-grey line under 26th row of display area
        stx $d021
        .if ( GHOSTPIXEL_ON && PTNBOUND==256 && maxptnlen<250 && maxsubtuneamount<18 )
        sty GHOSTPIXPOS
        .fi
        lda $dd00
        ora #%00000011
        sta $dd00       ;lower border sprites are in other VIC-bank
        jsr commonsubs.xwaitr1 ;wait around 1 rasterline (in commonsubs.inc), returns X=0
        lda #$15        ;video-ram switching before lower border sprites come
        sta $d018
        lda #bordercolor ;backgroucol ;x is 0 after prev. loop...
        nop ;sta $d020
        sta $d021
        .if ( GHOSTPIXEL_ON && PTNBOUND==256 && maxptnlen<250 && maxsubtuneamount<18 )
        lda #%11111110
        sta $d01b
        lda #ghostpixel ;in graph/graphics.inc
        sta GHOSTPIXPOS
        .fi



;--------------------------one multispeed can be here---------------------------
        lda #<(orlcurs / $40);divided by spritesize, 256 sprites in a bank
;        ldx PALNTSC
;        bne +           ;if PAL machine, don't disturb cursor-sprite
        clc
        adc subwpos1+1  ;cursor Y coordinate in Orderlist
        .if (SID_AMOUNT>1)
        sec
        sbc trkposi
        .fi
+       sta lowvidram+$3f8 ;set orderlist-cursor pointer

        ldx #30 ;wait for row after first row of bottom-border sprites
        dex
        bne *-1
        lda #0  ;this switches off sprites in time to prevent timing-glitches in HerMIDI 'GetData'
        sta $d015

mplychk lda fastfwd+1   ;fast-forward
        beq +
.if (SID_AMOUNT==1)
        lda PALNTSC
        beq +++         ;NTSC fast-forward? is handled as one-step bigger speed (5x) and not using this call under orderlist
        bne ++          ;PAL/Drean fast-forward? 'multisp' routine will know to behave as single-speed
.else
        jmp +++         ;never putting playroutine to lower border when doing fast-fwd in 2SID version
.fi
+       lda framesp
        cmp #2
        bmi ++          ;if framespeed is bigger than 2, a multispeed call in displayer-rasterIRQ
+       cmp ($c1,x)     ;visual correction - delay rasterbar not to see its initial flicker
        jsr multisp
+ 

;---------------------------shift screen-top upwards------------------------------------------
        lda #$18
        sta $d011       ;close vertical border (retriggers 25 line mode and back-shifted lines)


;------------------------------ DISPLAY OSCILLOSCOPE ---------------------------
        jsr playadapter.dispScope ;runtime:7..16 rasterlines; display oscilloscope

        ldx PALNTSC
        lda OLendRas,x  ;earliest raster after (below) orderlist on PAL machine
        cmp $d012       ;wait minimum  raster after orderlist, but immediate continuation if we delayed
        bpl *-3



;------------------------ PREPARE UPPER BORDER SPRITES -------------------------
        ldx #0
        stx $d01b       ;sta $d01b ;upper border sprites must be in front of idle-graphics
        lda curwind     ;check for cursor display in orderlist
        cmp #1
        bne +
        lda namemode
        bne +
        ;lda #0         ;X is 0 here after loop
        stx $d027 ;sta $d027
+       lda $dd00
        and %11111100
        sta $dd00
        lda #$84
        sta $d018
        ldy PALNTSC     ;shift on NTSC machine to be better seen...
        lda upborast,y ;#upborderpos ;set upper border sprites
        ldy #12
-       sta $d003,y
        dey
        dey
        bpl -

.if (MIDI_support!=0)
.if (MIDIC64.Vessel_support!=0)
        lda VESSELMODE
        cmp #2
        beq +
        jsr MIDIC64.GetData
        +
.else
.if (HerMIDI_support!=0 && HerMIDI_TX_MODE==MIDIC64.HerMIDI.SynchMode)
        jsr midisubs.HerMIDI_Synchron_GetData
.fi
.fi
.fi


;------------------------ SET ROW25 BACK ---------------------------------------
;----------------------- KEYBOARD-HANDLING -------------------------------------
        jsr commonsubs.row25back_keyer
.if (MIDI_support==0)
        jsr commonsubs.disNote
.fi

;======================= HANDLE RASTER-ROW & FRAMESPEED ========================
        lda fastfwd+1   ;fast-forward
        bne +
        lda framesp ;jsr limitfspd
        cmp #1
        bne +           ;single-speed mode?
        lda #2          ;then use 2 bars (the display/keyhandler AND the separate player-routine)
+       sta fspdctr+1   ;reset framespeed-counter

dendply
        jsr playadapter.dispScope ;it does here only if NTSC mode is on and fspdctr+1 is 1 - runtime:7..16 rasterlines; display oscilloscope
        ldy fastfwd+1
        bne +
-       ldy framesp ;jsr limitfspd
        ;tay
+       lda PALrdiv,y   ;go for upcoming multispeed (add needed raster-delay to current $d012)
        ldx PALNTSC     ;X now used to check framespeed-divisions and maximal rasterline-value
        bne +
        lda NTSCdiv,y   ;NTSC frame-division values
+       adc d012sto+1
        sta d012sto+1
        cpy fspdctr+1
        bne d012sto     ;not 1st multispeed-call after orderlist?
        cmp #(PALfullrast-$100) ;treshold for 1st multispeed - the minimum allowed row for player-rasterbars
        bcs +
        lda minrast,x   ;minimal coordinate set if bit9 polling doesn't work (only when not using rasterlines above $100)
+       sbc framemax    ;Carry stays 1 if bigger
        sta d012sto+1
d012sto lda #selfmod    ;current rasterline when irq entered into it
        dec fspdctr+1
        bne +
        ldx #disprast   ;set the raster-IRQ line for the displayer/keyhandler
        bne ++          ;jump to prevent endless loop if 'disprast' skipped over
+       tax             ;preserve calculated $d012 value
        bit $d011       ;check if we're in lower raster area ($d011 bit 7 is 0)
        bmi +           ;if bit 7 is ON, no need to check
        sbc #2          ;cheat with rastertiming to let 2 raster-rows time for irq to prepare
        cmp $d012       ;check if we possibly ran over the raster with long keyroutines, like copying/pasting long tracks
        bcc -           ;if ran over, leave the intended rasterbar and go to next
+       stx $d012


;---------------------- RETURN FROM IRQ ------------------------------------
retirq  lda #$ff
.if (MIDI_support!=0)
        sta rastsig+1   ;$FF raster interrupt finished
.fi
        sta $d019
        pla
        tay
retirq2 pla
        tax
retirq3 pla             ;written at IRQ entry, stored value of $01 ROM/RAM bank switcher
        sta banksel
        pla
        rti

.pend



;==============================================================================
;--------------------if restore pressed, NMI vector points here
NMI2	pla
	sta banksel
	pla
NMI                     ;NMI (Restore key) ends here, used for page-up in VICE
        bit $dc0d
        inc keyhandler.restsig+1 ;signs for keyhandler that restore was pressed. routine zeroes it
        rti


;-------------------------------------
        .section data
plybcol .byte jambarcol,playbarcol,ptplaybarcol ;player-rasterbar colours
COLtemp .byte 0,0,0     ;temporary color-storage for the (26th row vs. step-highlighting) displayer
PALNTSC .byte 1         ;machine-type set by PALorNTSC procedure - 1 means PAL machine, 0 means NTSC machine 

;framespeeds - lines to be added after each other
.if (SID_AMOUNT==1)
singnum .byte 0,1,1,2,2,3,4,5,5 ;which bar is the single-speed bar in different framespeeds? (0,1..8)
PALrdiv .byte 0, singrast+$27, PALfullrast/2-10, PALfullrast/3-4, PALfullrast/4, PALfullrast/5,    PALfullrast/6-2, PALfullrast/7-3, PALfullrast/8-4
NTSCdiv .byte 0, singrast+$27, NTSCfullrast/2-10, NTSCfullrast/3-5, NTSCfullrast/4+1, NTSCfullrast/5-2, NTSCfullrast/6-3, NTSCfullrast/7-4, NTSCfullrast/8-4
.else
singnum .byte 0,1,1,1,2,2,3,3,4 ;which bar is the single-speed bar in different framespeeds? (0,1..8)
;2SID: singlespeed-part of player at top of screen to ensure time for MIDI-jamming
PALrdiv .byte 0, singrast+7, 120, PALfullrast/3-14, PALfullrast/4, PALfullrast/5,    PALfullrast/6-2, PALfullrast/7-3, PALfullrast/8-4
NTSCdiv .byte 0, singrast+7, NTSCfullrast/2-10, NTSCfullrast/3-5, NTSCfullrast/4+1, NTSCfullrast/5-2, NTSCfullrast/6-3, NTSCfullrast/7-4, NTSCfullrast/8-4
.fi

framemax .byte (PALfullrast-$100-1) ;set by PALorNTSC routine, oldNTSC:$05,NTSC:$06,PAL/Drean:$37
minrast  .byte singrast+(NTSCfullrast-$100) , 3+(PALfullrast-$100) , 3+(PALfullrast-$100) , 3+(PALfullrast-$100) ;1st usable row for play-rasterbars (cheat with timing -especially with NTSC - to let some time for key-handler)
OLendRas .byte $0E, upborderpos, upborderpos, upborderpos ;the minimal rasterrow to set upper border sprite-coordinates
upborast .byte upborderpos+1,upborderpos, upborderpos, upborderpos
        .send


;===============================================================================
