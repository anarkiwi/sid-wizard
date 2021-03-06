;ident:8,24
;=============== data, methods and fields of alternative players ===============
; $Id: altplayers.inc 387 2014-07-09 21:35:08Z hermitsoft $

altplayers .block
.if (COMPILEDAPP==1)
TuningType=Config.TuningType
.fi

CopyPlayer ;copy the selected player into place of the default
        asl
        tax
        lda PlrAddr+0,x
        sta datzptr+0
        lda PlrAddr+1,x
        sta datzptr+1
        lda #<PLAYERADDR
        ldx #>PLAYERADDR
        sta inszptr+0
        stx inszptr+1
        ldy #0
-       lda (datzptr),y
        sta (inszptr),y
        inc datzptr+0
        bne +
        inc datzptr+1
+       inc inszptr+0
        bne +
        inc inszptr+1
+       lda inszptr+0
        cmp #<(PLAYERADDR+MAX_PLAYERSIZE)
        bne -
        lda inszptr+1
        cmp #>(PLAYERADDR+MAX_PLAYERSIZE)
        bne -
        rts

SetFreqTb ;if needed, change to NTSC table
        asl
        tax             ;x points to selected player's freq.table address
        lda frqtHaddr+0,x
        sta datzptr+0
        lda frqtHaddr+1,x
        sta datzptr+1
        lda frqtLaddr+0,x
        sta vidzptr+0
        lda frqtLaddr+1,x
        sta vidzptr+1
        ldy #12*8-1-1
        lda TuningType
        beq chkNTSC
        cmp #3 ;Drean?
        beq setNTSC ;Drean uses the same freqtable as NTSC (only 700Hz difference in their CPU-clock)
        cmp #1
        bne setJust
setVerd lda FreqTableVerdiPalHi+1,y
        sta (datzptr),y
        lda FreqTableVerdiPalLo+1,y
        sta (vidzptr),y
        dey
        bpl setVerd
        rts
setJust lda FreqTableJustPalHi,y
        sta (datzptr),y
        lda FreqTableJustPalLo,y
        sta (vidzptr),y
        dey
        bpl setJust
        rts
chkNTSC lda PALNTSC
        ;beq setNTSC ;Drean uses the same freqtable as NTSC (only 700Hz difference in their CPU-clock)
        ;cmp #3 ;Drean?
        bne retfrtb ;Normal tuning is already in player-code
setNTSC lda FreqTableNtscHi,y
        sta (datzptr),y
        lda FreqTableNtscLo,y
        sta (vidzptr),y
        dey
        bpl setNTSC
retfrtb rts

        .if COMPILEDAPP==1
SetTypeIcon ;embed the Light / Medium / Bare / Demo / Extra player-type texts into the SID-Wizard logo
        beq +           ;no change for normal player
        lda #$34
        sta banksel     ;switch off IO, reveal RAM under it
        dey
        ldx #0
-       lda ptirow1,y
        sta updispr+4*$40+6*3,x
        tya
        clc
        adc #NumberOfPlayerTypes-1
        tay
        inx
        inx
        inx
        cpx #3*7
        bne -
        inc banksel
+       rts
        .fi

PlrList = (normalPlayer, mediumPlayer, lightPlayer, extraPlayer, barePlayer, demoPlayer)

PlrAddr .word (COMPILEDAPP==1)? player.playercode : PlrList[0].player.playercode ;default player isn't overwritten in editor
		.word PlrList[1:].player.playercode

;frequency-table addresses in players (SID-Wizard C-1 note starts from 1, not 0)
frqtHaddr .word (COMPILEDAPP==1)? player.FREQTBH+1 : PlrList[0].player.FREQTBH+1
		.word PlrList[1:].player.FREQTBH+1
frqtLaddr .word (COMPILEDAPP==1)? player.FREQTBL+1 : PlrList[0].player.FREQTBL+1
		.word PlrList[1:].player.FREQTBL+1

         .if (SID_AMOUNT>=2)
SID2Aadd .word PlrList.SID2ADDtable
         .fi
         .if (SID_AMOUNT>=3)
SID3Aadd .word PlrList.SID3ADDtable
         .fi

        .if COMPILEDAPP==1
descrAddr .word PlrList.description
FeatIadd .word PlrList.FeatInstance1
        .enc screen
.if (SID_AMOUNT==1)
rastimes .text"$1C ","$1B ","$17 ","$23 ","$17 ","$19 "
plysizes .text"$A57","$99A","$7FA","$AEF","$71A","$89B" ;.text ^(player.playercode_end-player.playercode), ^(mediumPlayer.playercode_end-mediumPlayer.playercode), text ^(lightPlayer.playercode_end-lightPlayer.playercode), ^(extraPlayer.playercode_end-extraPlayer.playercode)
.elsif (SID_AMOUNT==2)
rastimes .text"$40 ","$3D ","$33 ","$47 ","$32 ","$37 "
plysizes .text"$D33","$C4D","$A2D","$D7F","$904","$B04" ;.text ^(player.playercode_end-player.playercode), ^(mediumPlayer.playercode_end-mediumPlayer.playercode), text ^(lightPlayer.playercode_end-lightPlayer.playercode), ^(extraPlayer.playercode_end-extraPlayer.playercode)
.elsif (SID_AMOUNT==3)
rastimes .text"$58 ","$54 ","$4D ","$62 ","$4C ","$53 "
plysizes .text"$F5A","$E55","$BAC","$FA9","$A95","$D17" ;.text ^(player.playercode_end-player.playercode), ^(mediumPlayer.playercode_end-mediumPlayer.playercode), text ^(lightPlayer.playercode_end-lightPlayer.playercode), ^(extraPlayer.playercode_end-extraPlayer.playercode)
.fi
        .enc none
        .fi

        .if COMPILEDAPP==2
PlrEnds .word PlrList.player.player_top ;these pointers are to logical address equivalents of 'playercode_end'
PlrDatP .word PlrList.DataPtr
PlrValP .word PlrList.PtrValu
PlrRelo .word PlrList.reloctable
PlrFxPt .word PlrList.player.BIGFXTABLE+1
         .if(SWP_EXPORT!=0)
SWPends   .word PLAYERADDR + (PlrList.SWPplayer_top - PlrList.player.playercode)
PlrDatP_SWP .word PLAYERADDR + (PlrList.DataPtr-PlrList.player.playercode)
PlrValP_SWP .word PLAYERADDR + (PlrList.PtrValu-PlrList.player.playercode)
         .fi
        .fi

        .if COMPILEDAPP==1
PlayerTypeIcon
ptirow1 .byte %01011111,%01011111,%00011111,%00011111,%00011111
ptirow2 .byte %00001100,%00101100,%11101100,%11001100,%11001100
ptirow3 .byte %10100101,%10010101,%10001101,%10101101,%10101101
ptirow4 .byte %11101100,%10001100,%11101100,%11001100,%10101100
ptirow5 .byte %10100101,%10000101,%10001101,%10101101,%10101101
ptirow6 .byte %10101101,%11101101,%11101101,%11001101,%11001101
ptirow7 .byte %00000111,%00000111,%00001111,%00001111,%00001111
        .fi

;---------------------------------------------------------------------------------------
;NTSC frequency table for NTSC machines (and Drean)
;--------------------------------------------------
FreqTableNtscHi
                ;      C   C#  D   D#  E   F   F#  G   G#  A   A#  B
                .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01  ; 1
                .byte $02,$02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03  ; 2
                .byte $04,$04,$04,$04,$05,$05,$05,$06,$06,$07,$07,$07  ; 3
                .byte $08,$08,$09,$09,$0a,$0b,$0b,$0c,$0d,$0e,$0e,$0f  ; 4
                .byte $10,$11,$12,$13,$15,$16,$17,$19,$1a,$1c,$1d,$1f  ; 5
                .byte $21,$23,$25,$27,$2a,$2c,$2f,$32,$35,$38,$3b,$3f  ; 6
                .byte $43,$47,$4b,$4f,$54,$59,$5e,$64,$6a,$70,$77,$7e  ; 7
                .byte $86,$8e,$96,$9f,$a9,$b3,$bd,$c9,$d5,$e1,$ef,$fd  ; 8

FreqTableNtscLo
                ;      C   C#  D   D#  E   F   F#  G   G#  A   A#  B
                .byte $0c,$1c,$2d,$3f,$52,$66,$7b,$92,$aa,$c3,$de,$fa  ; 1
                .byte $18,$38,$5a,$7e,$a4,$cc,$f7,$24,$54,$86,$bc,$f5  ; 2
                .byte $31,$71,$b5,$fc,$48,$98,$ee,$48,$a9,$0d,$79,$ea  ; 3
                .byte $62,$e2,$6a,$f8,$90,$30,$dc,$90,$52,$1a,$f2,$d4  ; 4
                .byte $c4,$c4,$d4,$f0,$20,$60,$b8,$20,$a4,$34,$e4,$a8  ; 5
                .byte $88,$88,$a8,$e0,$40,$c0,$70,$40,$48,$68,$c8,$50  ; 6
                .byte $10,$10,$50,$c0,$80,$80,$e0,$80,$90,$d0,$90,$a0  ; 7
                .byte $20,$20,$a0,$80,$00,$00,$c0,$00,$20,$a0,$20,$40  ; 8
                

;Verdi-tuning table for PAL machines
;------------------------------------
VERDI_BASEFREQ=432.00 ;A5 note's frequency (Hz)

-		= (256**3/PAL_CLOCKFREQ) * VERDI_BASEFREQ * 2**(range(-58, 38)/12.00)
FreqTableVerdiPalHi .byte >(-)
FreqTableVerdiPalLo .byte <(-)


;Just-intonation frequency-table (in key of C) for PAL machines (based on integer-based ratios for pure intervals)
;---------------------------------------------------------------
; (reference document: http://www.phy.mtu.edu/~suits/scales.html )
JUST_C1_FREQ= 16.351875 ;C1 note's frequency (Hz) - A4 note = 436.05Hz this way
JUST_RATIOS = [1.00, 25.0/24.0, 9.0/8.0, 6.0/5.0, 5.0/4.0, 4.0/3.0, 45.0/32.0, 3.0/2.0, 8.0/5.0, 5.0/3.0, 9.0/5.0, 15.0/8.0]
;DISP_NOTES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","H"]

FreqTableJustPalHi
.for i=0, i<8, i=i+1 ;i counts octaves
  .byte >( (256**3/PAL_CLOCKFREQ) * JUST_C1_FREQ*JUST_RATIOS * 2**i ) 
  ;.for j=0, j<12, j=j+1 ;j counts notes
  ;.warn DISP_NOTES[j],"-",i+1,": ", JUST_C1_FREQ*JUST_RATIOS[j] * 2**i , " Hz"
  ;.next
.next

FreqTableJustPalLo
.for i=0, i<8, i=i+1 ;i counts octaves
  .byte <( (256**3/PAL_CLOCKFREQ) * JUST_C1_FREQ*JUST_RATIOS * 2**i ) 
.next

                
;-------------------------------------------------------------------------------

.bend



;===================== players of different qualities  =========================
;each player here has a jumptable/header with its code-locations used from outside
;(and each player has an own relocation-table (its location and size is in the player's header)
;the default player routine can be replaced by them on demand in the beginning (startup-menu)
;NUMBEROFPLAYERS=4 ;this value is not used but would be nice to have for later expansions

;player code-locations used by the editor:
;=========================================
;a.fix place at the beginning of the player, no need to give as player-header info:
;---------------------------------------------------------------------------------
;inisub playsub ;used by playadapter.inc
;mulpsub        ;used by irq.inc

;b.fix place and in the editor they might even be zeropage-registers - no need to modify
;---------------------------------------------------------------------------------------
;ACTPTN,x       ;used by displayer1.inc & keyhandler.inc
;SEQPOS,x TMPPOS,x SPDCNT,x PTNPOS,x ;used by playadapter.inc & keyhandler.inc
;ACTINS,x ACTIFX,x ACTFX2,x SLIDEVIB,x ACTNOT,x WFGHOST,x PTNGATE,x PWLOGHO,x  ;-||-

;c.subroutines with player-dependent addresses - a jumptable created for them just for editor-mode
;-------------------------------------------------------------------------------------------------
;SETSTUNE SEQSUB CHKNOTE CNTPLAY COMMONREGS(!pla!) ;
;PTNPLAY - in indirect way (playadapter.inc copies this code into the player)

;d.these memory-addresses should be treated correctly:
;----------------------------------------------------
;StrtPos+1 - solved by pointing from player to a byte with label 'StrtPos'
;ptncode, FSWITCH+1, FLTBAND+1 - solved by making a subruotine in player-jumptable to read/write these locations
;EXPTABH - need to modify editor (4 reads)

;e.addresses that player uses from the editor (no need to workaround, as players are compiled here together):
;-------------------------------------------------------------------------------------------------------------
;div7chn,x olstart,y CTFMETR+1 playmod wasjamm,x PWIHELP,x mutesolo,y


;============= player source files with different feature-sets (compilation settings) =======================

;-------------------------------------------------------------------------------
normalPlayer .block

        .if (COMPILEDAPP==1)
         .enc screen
description .text "The default player which features all   "
          .text "the necessities and almost every extra. "
         .enc none
         #FeatInstances ;macro in settings.cfg - binary representation of the features (FeatInstance1 & FeatInstance2 arrays)
;the default player source is included in editor.asm directly
        .fi

        .if (COMPILEDAPP==2)
         .union
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
player2    #playerm PLAYERADDR+$100, defaultSID2BASE, defaultSID3BASE
          .ends
          .struct
player     #playerm PLAYERADDR, defaultSID2BASE, defaultSID3BASE
          .ends
         .endu
reloctb  #relocm player2, player, PLAYERADDR
        .fi

         .if (SID_AMOUNT>=2)
SID2ADDtable .block
          #SID23ADDm player2SIDa, player, PLAYERADDR  ;SID2-address table
          .bend
         .fi
         .if (SID_AMOUNT>=3)
SID3ADDtable .block
          #SID23ADDm player3SIDa, player, PLAYERADDR  ;SID3-address table
          .bend
         .fi

.bend


;-------------------------------------------------------------------------------
mediumPlayer .block

PLAYERTYPE=1 ;0=normal, 1=medium, 2=light, 3=extra, 4=bare, 5=demo

feature .block
TRANSPOSESUPP_ON = 1 ;IF POSSIBLE TO TRANSPOSE WITH SEQ-FX
OCTAVESHIFT_ON   = 1 ;IF OCTAVE-SHIFTING IS ACCOUNTED FOR INSTRUMENTS OR NOT
CHORDSUPPORT_ON  = 1 ;CHORDS CALLABLE FROM ARP-TABLE OR NOT
CALCVIBRATO_ON   = 1 ;CALCULATED VIBRATO ON/OFF
VIBRATOTYPES_ON  = 0 ;WHETHER DIFFERENT VIBRATO-TYPES ARE SELECTABLE OR JUST THE DEFAULT 
DETUNESUPPORT_ON = 1 ;DETUNE-COLUMN AUDIBLE OR NOT
PORTAVIBRA_ON    = 0 ;DROP BACK TO VIBRATO WHEN PORTAMENTO/LEGATO REACHED TARGET NOTE?
ARPSPEEDSUPP_ON  = 1 ;IF ARPEGGIO-SPEED IS ACCOUNTED OR NOT (FOR MULTISPEED)
GATEOFFPTR_ON    = 0 ;GATE-OFF WF/PW/FILTER-TABLEPOINTER SUPPORT ON/OF
FILTRESETSW_ON   = 1 ;ABILITY TO SWITCH FILTER-RESET OFF WHEN NOTE WITHOUT INSTRUMENT
FILTKBTRACK_ON   = 1 ;FILTER-CUTOFF-FREQUENCY KEYBOARD-TRACKING EFFECT ON/OFF
PWRESETSW_ON     = 1 ;ABILITY TO SWITCH PULSEWIDTH-TABLE RESET OFF WHEN NOTE WITHOUT INSTRUMENT 
PWKEYBTRACK_ON   = 0 ;PULSEWIDTH KEYBOARD-TRACKING EFFECT ON/OFF
HARDRESTYPES_ON  = 0 ;DIFFERENT HARDRESTART-ADSR/TIMER VALUES FOR EACH INSTRUMENT?
FRAME1SWITCH_ON  = 0 ;IF DISABLED, SEXY $09 HARDRESTART APPLIED FOR ALL INSTRUMENTS
TEMPOPRGSUPP_ON  = 1 ;TEMPO-PROGRAM AND FUNKTEMPO-CHANGING SUPPORT ON/OFF (PATTERN-FX)
FASTSPEEDBIND_ON = 0 ;WHETHER TO CONNECT TICK0..2 IF SPEED IS LOWER (TO PRODUCE SOUND)
DELAYSUPPORT_ON  = 0 ;TRACK-DELAY AND NOTE-DELAY SUPPORT ON/OFF
SUBTUNEJUMP_ON   = 0 ;SUBTUNE-JUMPING SUPPORT ON/OFF
RETAINZEROPAGE_ON= 0 ;SAVING/RESTORING ZEROPAGE BYTES BEFORE/AFTER PLAYER ROUTINE 
ALLGHOSTREGS_ON  = 0 ;USE GHOST-REGISTERS FOR ADSR & PULSEWIDTH TOO, WRITE ALL REGISTERS TO SID RAPIDLY AT END OF PLAYER (QUALITY)
FINEFILTSWEEP_ON = 1 ;WHETHER 11BIT OR 8BIT FILTERSWEEP
FILT_CTRL_FX_ON  = 0 ;FILTER-SWITCHES AND RESONANCE ($D417) SETTING FX SUPPORT
FILTERALWAYS_ON  = 0 ;DON'T SKIP FILTER-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - makes PULSEALWAYS_ON happen
PULSEALWAYS_ON   = 0 ;DON'T SKIP PULSE-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME?
VIBSLIDEALWAYS_ON= 0 ;DON'T SKIP VIBRATO/SLIDE/PORTAMENTO (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - overrides previous 2 settings

SUBTUNESUPPORT_ON= (maxsubtuneamount>0)? 1 : 0
MULTISPEEDSUPP_ON= 1
VOLSET_SUPPORT_ON= 1
FILTSHIFT_SUPP_ON= 1
SEQ_FX_SUPPORT_ON= 1
PORTAME_NOTEFX_ON= 1
WFARP_NOP_SUPP_ON= 1
VIBFREQFX_SUPP_ON= 1
FILTER_SMALLFX_ON= 1
DETUNE_SMALLFX_ON= 1
WFCTRL_SMALLFX_ON= 1
.bend

        .if (COMPILEDAPP==1)
         .enc screen
description .text "A moderately reduced player, It has got "
            .text "almost all function but less rastertime."
         .enc none
         #FeatInstances ;macro in settings.cfg - binary representation of the features above (FeatInstance1 & FeatInstance2 arrays)
        .fi

         .union
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
player2    #playerm PLAYERADDR+$100, defaultSID2BASE, defaultSID3BASE
          .ends
          .struct
player     #playerm PLAYERADDR, defaultSID2BASE, defaultSID3BASE
          .ends
         .endu

         .if (COMPILEDAPP==2)
reloctb   #relocm player2, player, PLAYERADDR
         .fi

         .if (SID_AMOUNT>=2)
SID2ADDtable .block
          #SID23ADDm player2SIDa, player, PLAYERADDR  ;SID2-address table
          .bend
         .fi
         .if (SID_AMOUNT>=3)
SID3ADDtable .block
          #SID23ADDm player3SIDa, player, PLAYERADDR  ;SID3-address table
          .bend
         .fi

.bend


;-------------------------------------------------------------------------------
lightPlayer .block

PLAYERTYPE=2 ;0=normal, 1=medium, 2=light, 3=extra, 4=bare, 5=demo

feature .block
TRANSPOSESUPP_ON = 0 ;IF POSSIBLE TO TRANSPOSE WITH SEQ-FX
OCTAVESHIFT_ON   = 0 ;IF OCTAVE-SHIFTING IS ACCOUNTED FOR INSTRUMENTS OR NOT
CHORDSUPPORT_ON  = 0 ;CHORDS CALLABLE FROM ARP-TABLE OR NOT
CALCVIBRATO_ON   = 0 ;CALCULATED VIBRATO ON/OFF
VIBRATOTYPES_ON  = 0 ;WHETHER DIFFERENT VIBRATO-TYPES ARE SELECTABLE OR JUST THE DEFAULT 
DETUNESUPPORT_ON = 0 ;DETUNE-COLUMN AUDIBLE OR NOT
PORTAVIBRA_ON    = 0 ;DROP BACK TO VIBRATO WHEN PORTAMENTO/LEGATO REACHED TARGET NOTE?
ARPSPEEDSUPP_ON  = 0 ;IF ARPEGGIO-SPEED IS ACCOUNTED OR NOT (FOR MULTISPEED)
GATEOFFPTR_ON    = 0 ;GATE-OFF WF/PW/FILTER-TABLEPOINTER SUPPORT ON/OF
FILTRESETSW_ON   = 0 ;ABILITY TO SWITCH FILTER-RESET OFF WHEN NOTE WITHOUT INSTRUMENT
FILTKBTRACK_ON   = 0 ;FILTER-CUTOFF-FREQUENCY KEYBOARD-TRACKING EFFECT ON/OFF
PWRESETSW_ON     = 0 ;ABILITY TO SWITCH PULSEWIDTH-TABLE RESET OFF WHEN NOTE WITHOUT INSTRUMENT 
PWKEYBTRACK_ON   = 0 ;PULSEWIDTH KEYBOARD-TRACKING EFFECT ON/OFF
HARDRESTYPES_ON  = 0 ;DIFFERENT HARDRESTART-ADSR/TIMER VALUES FOR EACH INSTRUMENT?
FRAME1SWITCH_ON  = 0 ;IF DISABLED, SEXY $09 HARDRESTART APPLIED FOR ALL INSTRUMENTS
TEMPOPRGSUPP_ON  = 0 ;TEMPO-PROGRAM AND FUNKTEMPO-CHANGING SUPPORT ON/OFF (PATTERN-FX)
FASTSPEEDBIND_ON = 0 ;WHETHER TO CONNECT TICK0..2 IF SPEED IS LOWER (TO PRODUCE SOUND)
DELAYSUPPORT_ON  = 0 ;TRACK-DELAY AND NOTE-DELAY SUPPORT ON/OFF
SUBTUNEJUMP_ON   = 0 ;SUBTUNE-JUMPING SUPPORT ON/OFF
RETAINZEROPAGE_ON= 0 ;SAVING/RESTORING ZEROPAGE BYTES BEFORE/AFTER PLAYER ROUTINE 
ALLGHOSTREGS_ON  = 0 ;USE GHOST-REGISTERS FOR ADSR & PULSEWIDTH TOO, WRITE ALL REGISTERS TO SID RAPIDLY AT END OF PLAYER (QUALITY)
FINEFILTSWEEP_ON = 0 ;WHETHER 11BIT OR 8BIT FILTERSWEEP
FILT_CTRL_FX_ON  = 0 ;FILTER-SWITCHES AND RESONANCE ($D417) SETTING FX SUPPORT
FILTERALWAYS_ON  = 0 ;DON'T SKIP FILTER-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - makes PULSEALWAYS_ON happen
PULSEALWAYS_ON   = 0 ;DON'T SKIP PULSE-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME?
VIBSLIDEALWAYS_ON= 0 ;DON'T SKIP VIBRATO/SLIDE/PORTAMENTO (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - overrides previous 2 settings

SUBTUNESUPPORT_ON= (maxsubtuneamount>0)? 1 : 0
MULTISPEEDSUPP_ON= 1
VOLSET_SUPPORT_ON= 1
FILTSHIFT_SUPP_ON= 1
SEQ_FX_SUPPORT_ON= 1
PORTAME_NOTEFX_ON= 1
WFARP_NOP_SUPP_ON= 1
VIBFREQFX_SUPP_ON= 1
FILTER_SMALLFX_ON= 1
DETUNE_SMALLFX_ON= 1
WFCTRL_SMALLFX_ON= 1
.bend

        .if (COMPILEDAPP==1)
         .enc screen
description .text "A reduced player with small rastertime, "
            .text "all extra features are switched off...  "
         .enc none
         #FeatInstances ;macro in settings.cfg - binary representation of the features above (FeatInstance1 & FeatInstance2 arrays)
        .fi

         .union
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
player2    #playerm PLAYERADDR+$100, defaultSID2BASE, defaultSID3BASE
          .ends
          .struct
player     #playerm PLAYERADDR, defaultSID2BASE, defaultSID3BASE
          .ends
         .endu

         .if (COMPILEDAPP==2)
reloctb   #relocm player2, player, PLAYERADDR
         .fi

         .if (SID_AMOUNT>=2)
SID2ADDtable .block
          #SID23ADDm player2SIDa, player, PLAYERADDR  ;SID2-address table
          .bend
         .fi
         .if (SID_AMOUNT>=3)
SID3ADDtable .block
          #SID23ADDm player3SIDa, player, PLAYERADDR  ;SID3-address table
          .bend
         .fi

.bend


;-------------------------------------------------------------------------------
extraPlayer .block 

PLAYERTYPE=3 ;0=normal, 1=medium, 2=light, 3=extra, 4=bare, 5=demo

feature .block
TRANSPOSESUPP_ON = 1 ;IF POSSIBLE TO TRANSPOSE WITH SEQ-FX
OCTAVESHIFT_ON   = 1 ;IF OCTAVE-SHIFTING IS ACCOUNTED FOR INSTRUMENTS OR NOT
CHORDSUPPORT_ON  = 1 ;CHORDS CALLABLE FROM ARP-TABLE OR NOT
CALCVIBRATO_ON   = 1 ;CALCULATED VIBRATO ON/OFF
VIBRATOTYPES_ON  = 1 ;WHETHER DIFFERENT VIBRATO-TYPES ARE SELECTABLE OR JUST THE DEFAULT 
DETUNESUPPORT_ON = 1 ;DETUNE-COLUMN AUDIBLE OR NOT
PORTAVIBRA_ON    = 1 ;DROP BACK TO VIBRATO WHEN PORTAMENTO/LEGATO REACHED TARGET NOTE?
ARPSPEEDSUPP_ON  = 1 ;IF ARPEGGIO-SPEED IS ACCOUNTED OR NOT (FOR MULTISPEED)
GATEOFFPTR_ON    = 1 ;GATE-OFF WF/PW/FILTER-TABLEPOINTER SUPPORT ON/OF
FILTRESETSW_ON   = 1 ;ABILITY TO SWITCH FILTER-RESET OFF WHEN NOTE WITHOUT INSTRUMENT
FILTKBTRACK_ON   = 1 ;FILTER-CUTOFF-FREQUENCY KEYBOARD-TRACKING EFFECT ON/OFF
PWRESETSW_ON     = 1 ;ABILITY TO SWITCH PULSEWIDTH-TABLE RESET OFF WHEN NOTE WITHOUT INSTRUMENT 
PWKEYBTRACK_ON   = 1 ;PULSEWIDTH KEYBOARD-TRACKING EFFECT ON/OFF
HARDRESTYPES_ON  = 1 ;DIFFERENT HARDRESTART-ADSR/TIMER VALUES FOR EACH INSTRUMENT?
FRAME1SWITCH_ON  = 1 ;IF DISABLED, SEXY $09 HARDRESTART APPLIED FOR ALL INSTRUMENTS
TEMPOPRGSUPP_ON  = 1 ;TEMPO-PROGRAM AND FUNKTEMPO-CHANGING SUPPORT ON/OFF (PATTERN-FX)
FASTSPEEDBIND_ON = 1 ;WHETHER TO CONNECT TICK0..2 IF SPEED IS LOWER (TO PRODUCE SOUND)
DELAYSUPPORT_ON  = 1 ;TRACK-DELAY AND NOTE-DELAY SUPPORT ON/OFF
SUBTUNEJUMP_ON   = 1 ;SUBTUNE-JUMPING SUPPORT ON/OFF
RETAINZEROPAGE_ON= 1 ;SAVING/RESTORING ZEROPAGE BYTES BEFORE/AFTER PLAYER ROUTINE 
ALLGHOSTREGS_ON  = 1 ;USE GHOST-REGISTERS FOR ADSR & PULSEWIDTH TOO, WRITE ALL REGISTERS TO SID RAPIDLY AT END OF PLAYER (QUALITY)
FINEFILTSWEEP_ON = 1 ;WHETHER 11BIT OR 8BIT FILTERSWEEP
FILT_CTRL_FX_ON  = 1 ;FILTER-SWITCHES AND RESONANCE ($D417) SETTING FX SUPPORT
FILTERALWAYS_ON  = 1 ;DON'T SKIP FILTER-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - makes PULSEALWAYS_ON happen
PULSEALWAYS_ON   = 1 ;DON'T SKIP PULSE-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME?
VIBSLIDEALWAYS_ON= 1 ;DON'T SKIP VIBRATO/SLIDE/PORTAMENTO (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - overrides previous 2 settings

SUBTUNESUPPORT_ON= (maxsubtuneamount>0)? 1 : 0
MULTISPEEDSUPP_ON= 1
VOLSET_SUPPORT_ON= 1
FILTSHIFT_SUPP_ON= 1
SEQ_FX_SUPPORT_ON= 1
PORTAME_NOTEFX_ON= 1
WFARP_NOP_SUPP_ON= 1
VIBFREQFX_SUPP_ON= 1
FILTER_SMALLFX_ON= 1
DETUNE_SMALLFX_ON= 1
WFCTRL_SMALLFX_ON= 1
.bend

        .if (COMPILEDAPP==1)
         .enc screen
description .text "All extra features are switched on, and "
            .text "no matter what rastertime it takes...   "
         .enc none
         #FeatInstances ;macro in settings.cfg - binary representation of the features above (FeatInstance1 & FeatInstance2 arrays)
        .fi

         .union
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
player2    #playerm PLAYERADDR+$100, defaultSID2BASE, defaultSID3BASE
          .ends
          .struct
player     #playerm PLAYERADDR, defaultSID2BASE, defaultSID3BASE
          .ends
         .endu

         .if (COMPILEDAPP==2)
reloctb   #relocm player2, player, PLAYERADDR
         .fi

         .if (SID_AMOUNT>=2)
SID2ADDtable .block
          #SID23ADDm player2SIDa, player, PLAYERADDR  ;SID2-address table
          .bend
         .fi
         .if (SID_AMOUNT>=3)
SID3ADDtable .block
          #SID23ADDm player3SIDa, player, PLAYERADDR  ;SID3-address table
          .bend
         .fi

.bend


;-------------------------------------------------------------------------------
barePlayer .block

PLAYERTYPE=4 ;0=normal, 1=medium, 2=light, 3=extra, 4=bare, 5=demo

feature .block
TRANSPOSESUPP_ON = 0 ;IF POSSIBLE TO TRANSPOSE WITH SEQ-FX
OCTAVESHIFT_ON   = 0 ;IF OCTAVE-SHIFTING IS ACCOUNTED FOR INSTRUMENTS OR NOT
CHORDSUPPORT_ON  = 0 ;CHORDS CALLABLE FROM ARP-TABLE OR NOT
CALCVIBRATO_ON   = 0 ;CALCULATED VIBRATO ON/OFF
VIBRATOTYPES_ON  = 0 ;WHETHER DIFFERENT VIBRATO-TYPES ARE SELECTABLE OR JUST THE DEFAULT 
DETUNESUPPORT_ON = 0 ;DETUNE-COLUMN AUDIBLE OR NOT
PORTAVIBRA_ON    = 0 ;DROP BACK TO VIBRATO WHEN PORTAMENTO/LEGATO REACHED TARGET NOTE?
ARPSPEEDSUPP_ON  = 0 ;IF ARPEGGIO-SPEED IS ACCOUNTED OR NOT (FOR MULTISPEED)
GATEOFFPTR_ON    = 0 ;GATE-OFF WF/PW/FILTER-TABLEPOINTER SUPPORT ON/OF
FILTRESETSW_ON   = 0 ;ABILITY TO SWITCH FILTER-RESET OFF WHEN NOTE WITHOUT INSTRUMENT
FILTKBTRACK_ON   = 0 ;FILTER-CUTOFF-FREQUENCY KEYBOARD-TRACKING EFFECT ON/OFF
PWRESETSW_ON     = 0 ;ABILITY TO SWITCH PULSEWIDTH-TABLE RESET OFF WHEN NOTE WITHOUT INSTRUMENT 
PWKEYBTRACK_ON   = 0 ;PULSEWIDTH KEYBOARD-TRACKING EFFECT ON/OFF
HARDRESTYPES_ON  = 0 ;DIFFERENT HARDRESTART-ADSR/TIMER VALUES FOR EACH INSTRUMENT?
FRAME1SWITCH_ON  = 0 ;IF DISABLED, SEXY $09 HARDRESTART APPLIED FOR ALL INSTRUMENTS
TEMPOPRGSUPP_ON  = 0 ;TEMPO-PROGRAM AND FUNKTEMPO-CHANGING SUPPORT ON/OFF (PATTERN-FX)
FASTSPEEDBIND_ON = 0 ;WHETHER TO CONNECT TICK0..2 IF SPEED IS LOWER (TO PRODUCE SOUND)
DELAYSUPPORT_ON  = 0 ;TRACK-DELAY AND NOTE-DELAY SUPPORT ON/OFF
SUBTUNEJUMP_ON   = 0 ;SUBTUNE-JUMPING SUPPORT ON/OFF
RETAINZEROPAGE_ON= 0 ;SAVING/RESTORING ZEROPAGE BYTES BEFORE/AFTER PLAYER ROUTINE 
ALLGHOSTREGS_ON  = 0 ;USE GHOST-REGISTERS FOR ADSR & PULSEWIDTH TOO, WRITE ALL REGISTERS TO SID RAPIDLY AT END OF PLAYER (QUALITY)
FINEFILTSWEEP_ON = 0 ;WHETHER 11BIT OR 8BIT FILTERSWEEP
FILT_CTRL_FX_ON  = 0 ;FILTER-SWITCHES AND RESONANCE ($D417) SETTING FX SUPPORT
FILTERALWAYS_ON  = 0 ;DON'T SKIP FILTER-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - makes PULSEALWAYS_ON happen
PULSEALWAYS_ON   = 0 ;DON'T SKIP PULSE-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME?
VIBSLIDEALWAYS_ON= 0 ;DON'T SKIP VIBRATO/SLIDE/PORTAMENTO (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - overrides previous 2 settings

SUBTUNESUPPORT_ON= 0
MULTISPEEDSUPP_ON= 0
VOLSET_SUPPORT_ON= 0
FILTSHIFT_SUPP_ON= 0
SEQ_FX_SUPPORT_ON= 0
PORTAME_NOTEFX_ON= 0
WFARP_NOP_SUPP_ON= 0
VIBFREQFX_SUPP_ON= 0
FILTER_SMALLFX_ON= 0
DETUNE_SMALLFX_ON= 0
WFCTRL_SMALLFX_ON= 0
.bend

        .if (COMPILEDAPP==1)
         .enc screen
description .text "Very reduced: No subtunes, orderlist-FX,"
            .text "no multispeed,arptable-nop,some smallfx."
         .enc none
         #FeatInstances ;macro in settings.cfg - binary representation of the features above (FeatInstance1 & FeatInstance2 arrays)
        .fi

         .union
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
player2    #playerm PLAYERADDR+$100, defaultSID2BASE, defaultSID3BASE
          .ends
          .struct
player     #playerm PLAYERADDR, defaultSID2BASE, defaultSID3BASE
          .ends
         .endu

         .if (COMPILEDAPP==2)
reloctb   #relocm player2, player, PLAYERADDR
         .fi

         .if (SID_AMOUNT>=2)
SID2ADDtable .block
          #SID23ADDm player2SIDa, player, PLAYERADDR  ;SID2-address table
          .bend
         .fi
         .if (SID_AMOUNT>=3)
SID3ADDtable .block
          #SID23ADDm player3SIDa, player, PLAYERADDR  ;SID3-address table
          .bend
         .fi

.bend


;-------------------------------------------------------------------------------
demoPlayer .block

PLAYERTYPE=5 ;0=normal, 1=medium, 2=light, 3=extra, 4=bare, 5=demo

feature .block
TRANSPOSESUPP_ON = 1 ;IF POSSIBLE TO TRANSPOSE WITH SEQ-FX
OCTAVESHIFT_ON   = 0 ;IF OCTAVE-SHIFTING IS ACCOUNTED FOR INSTRUMENTS OR NOT
CHORDSUPPORT_ON  = 0 ;CHORDS CALLABLE FROM ARP-TABLE OR NOT
CALCVIBRATO_ON   = 0 ;CALCULATED VIBRATO ON/OFF
VIBRATOTYPES_ON  = 0 ;WHETHER DIFFERENT VIBRATO-TYPES ARE SELECTABLE OR JUST THE DEFAULT 
DETUNESUPPORT_ON = 1 ;DETUNE-COLUMN AUDIBLE OR NOT
PORTAVIBRA_ON    = 0 ;DROP BACK TO VIBRATO WHEN PORTAMENTO/LEGATO REACHED TARGET NOTE?
ARPSPEEDSUPP_ON  = 1 ;IF ARPEGGIO-SPEED IS ACCOUNTED OR NOT (FOR MULTISPEED)
GATEOFFPTR_ON    = 0 ;GATE-OFF WF/PW/FILTER-TABLEPOINTER SUPPORT ON/OF
FILTRESETSW_ON   = 1 ;ABILITY TO SWITCH FILTER-RESET OFF WHEN NOTE WITHOUT INSTRUMENT
FILTKBTRACK_ON   = 0 ;FILTER-CUTOFF-FREQUENCY KEYBOARD-TRACKING EFFECT ON/OFF
PWRESETSW_ON     = 1 ;ABILITY TO SWITCH PULSEWIDTH-TABLE RESET OFF WHEN NOTE WITHOUT INSTRUMENT 
PWKEYBTRACK_ON   = 0 ;PULSEWIDTH KEYBOARD-TRACKING EFFECT ON/OFF
HARDRESTYPES_ON  = 0 ;DIFFERENT HARDRESTART-ADSR/TIMER VALUES FOR EACH INSTRUMENT?
FRAME1SWITCH_ON  = 0 ;IF DISABLED, SEXY $09 HARDRESTART APPLIED FOR ALL INSTRUMENTS
TEMPOPRGSUPP_ON  = 1 ;TEMPO-PROGRAM AND FUNKTEMPO-CHANGING SUPPORT ON/OFF (PATTERN-FX)
FASTSPEEDBIND_ON = 0 ;WHETHER TO CONNECT TICK0..2 IF SPEED IS LOWER (TO PRODUCE SOUND)
DELAYSUPPORT_ON  = 0 ;TRACK-DELAY AND NOTE-DELAY SUPPORT ON/OFF
SUBTUNEJUMP_ON   = 0 ;SUBTUNE-JUMPING SUPPORT ON/OFF
RETAINZEROPAGE_ON= 0 ;SAVING/RESTORING ZEROPAGE BYTES BEFORE/AFTER PLAYER ROUTINE 
ALLGHOSTREGS_ON  = 0 ;USE GHOST-REGISTERS FOR ADSR & PULSEWIDTH TOO, WRITE ALL REGISTERS TO SID RAPIDLY AT END OF PLAYER (QUALITY)
FINEFILTSWEEP_ON = 1 ;WHETHER 11BIT OR 8BIT FILTERSWEEP
FILT_CTRL_FX_ON  = 0 ;FILTER-SWITCHES AND RESONANCE ($D417) SETTING FX SUPPORT
FILTERALWAYS_ON  = 0 ;DON'T SKIP FILTER-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - makes PULSEALWAYS_ON happen
PULSEALWAYS_ON   = 0 ;DON'T SKIP PULSE-PROGRAM (FOR 1-2 FRAMES) TO REDUCE RASTERTIME?
VIBSLIDEALWAYS_ON= 0 ;DON'T SKIP VIBRATO/SLIDE/PORTAMENTO (FOR 1-2 FRAMES) TO REDUCE RASTERTIME? - overrides previous 2 settings

SUBTUNESUPPORT_ON= 0
MULTISPEEDSUPP_ON= 1
VOLSET_SUPPORT_ON= 1
FILTSHIFT_SUPP_ON= 0
SEQ_FX_SUPPORT_ON= 1
PORTAME_NOTEFX_ON= 0
WFARP_NOP_SUPP_ON= 1
VIBFREQFX_SUPP_ON= 0
FILTER_SMALLFX_ON= 0
DETUNE_SMALLFX_ON= 0
WFCTRL_SMALLFX_ON= 0
.bend

        .if (COMPILEDAPP==1)
         .enc screen
description .text "Includes features needed for demo-music,"
            .text "lacks subtune,filtershift,some smallefx."
         .enc none
         #FeatInstances ;macro in settings.cfg - binary representation of the features above (FeatInstance1 & FeatInstance2 arrays)
        .fi

         .union
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
player2    #playerm PLAYERADDR+$100, defaultSID2BASE, defaultSID3BASE
          .ends
          .struct
player     #playerm PLAYERADDR, defaultSID2BASE, defaultSID3BASE
          .ends
         .endu

         .if (COMPILEDAPP==2)
reloctb   #relocm player2, player, PLAYERADDR
         .fi

         .if (SID_AMOUNT>=2)
SID2ADDtable .block
          #SID23ADDm player2SIDa, player, PLAYERADDR  ;SID2-address table
          .bend
         .fi
         .if (SID_AMOUNT>=3)
SID3ADDtable .block
          #SID23ADDm player3SIDa, player, PLAYERADDR  ;SID3-address table
          .bend
         .fi

.bend


;=========================================================================================
; vim: sw=4 ts=4 syntax=asm:
