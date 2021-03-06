;ident:8,24
;******************************************************************************
;                  Hermit SID-WIZARD player routine
;******************************************************************************
; $Id: player.asm 390 2014-07-22 10:33:15Z hermitsoft $

 ;PLAYERZP = $FE        ;routine saves and restores the value of these 2 zeropage locations (so it can overlap)

;APPLICATION-RELATED TWEAKS FOR SHORTER EXECUTION TIME (RASTERTIME) AND SMALLER CODE-SIZE
;---------------------------------------------------------------------
playerm .macro
playercode              ;copiers will know where it starts
        .logical \1     ;set logical compilation address
SID2BASE= \2
SID3BASE= \3

        .if COMPILEDAPP==1 ;IF EDITOR IS BEING COMPILED
PACKEDNOPSUPP_ON = 0    ;PACKED REST/NOP PLAYING IS NOT NEEDED IN TRACKER
ZEROPAGESAVE_ON = 0     ;NOT NEEDED IN EDITOR - SAVING/RESTORING ZEROPAGE BYTES BEFORE/AFTER PLAYER ROUTINE
        .else
PACKEDNOPSUPP_ON = 1    ;IF PACKED REST/NOP PLAYING IS SUPPORTED
ZEROPAGESAVE_ON = feature.RETAINZEROPAGE_ON ;IF ZEROPAGE-SAVING IS DISABLED GLOBALLY
        .fi

SID     .block          ; The SID's registers
FREQ  = SIDBASE+0       ; Frequency (16 bits)
PLSW  = SIDBASE+2       ; Pulse width (12 bits)
WAVE  = SIDBASE+4       ; Waveform
AD    = SIDBASE+5       ; Attack, Decay
SR    = SIDBASE+6       ; Sustain, Release
FCUT  = SIDBASE+21      ; Filter cutoff frequency (11 bits)
RESFC = SIDBASE+23      ; Resonance, Filter control
FMVOL = SIDBASE+24      ; Filter mode, Volume
        .bend

.if (SID_AMOUNT>=2)
SID2    .block          ; The SID's registers
FREQ  = SID2BASE+0      ; Frequency (16 bits)
PLSW  = SID2BASE+2      ; Pulse width (12 bits)
WAVE  = SID2BASE+4      ; Waveform
AD    = SID2BASE+5      ; Attack, Decay
SR    = SID2BASE+6      ; Sustain, Release
FCUT  = SID2BASE+21     ; Filter cutoff frequency (11 bits)
RESFC = SID2BASE+23     ; Resonance, Filter control
FMVOL = SID2BASE+24     ; Filter mode, Volume
        .bend
.fi

.if (SID_AMOUNT>=3)
SID3    .block          ; The SID's registers
FREQ  = SID3BASE+0      ; Frequency (16 bits)
PLSW  = SID3BASE+2      ; Pulse width (12 bits)
WAVE  = SID3BASE+4      ; Waveform
AD    = SID3BASE+5      ; Attack, Decay
SR    = SID3BASE+6      ; Sustain, Release
FCUT  = SID3BASE+21     ; Filter cutoff frequency (11 bits)
RESFC = SID3BASE+23     ; Resonance, Filter control
FMVOL = SID3BASE+24     ; Filter mode, Volume
        .bend
.fi


;================================JUMPTABLE=====================================
PLAYER_JUMPTABLE
inisub  jmp INITER      ;PLAYER & SID INIT ROUTINE
playsub jmp PLAYER      ;PLEYER ROUTINE ITSELF
mulpsub jmp MULPLY      ;MULTISPEED CALL
volusub jmp (SID_AMOUNT==1)? SMALFXA : ExtVol2 ;VOLUME-SETTING CALL
SFXsub  jmp (SFX_SUPPORT!=0)? SFXinit:retsubr     ;INITIALIZE A SOUND-EFFECT ON CHANNEL 3: X=note/pitch, Y=SFX-instrument number, A=length (1..$7F frames)
;(Soci had an idea of pattern-based SFX, but it would be a big overhead if music-channel should be preserved&retrieved after SFX.)
.if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
slowsub jmp setSlowdown ;input in accumulator
.fi

;-------player-header- jumptable&addresses to grab different player-types ------
;memory-positions are given as absolute-addresses for faster processing, players are always at $1000
.if COMPILEDAPP==1
playerheader ;'indirect' jumps (real indirect jumps not good as we're calling by 'jsr' from editor)
PLAYER_TYPE .byte PLAYERTYPE ;current player/driver type (normal=0/medium=1/light=2/extra=3) 
iSETSTU jmp SETSTUNE
iSEQSUB jmp SEQSUB
iCHKNOT jmp CHKNOTE
iCNTPLY jmp CNTPLAY
iCOMMRE jmp COMMONREGS  ;!be careful not to have zeropage-saving turned on for the editor! (would cause 2 pla at the end of COMMONREGS)
iPTNPLY jmp PTNPLAY
.if (MIDI_support!=0)
iBRIGHT jmp (feature.FILTSHIFT_SUPP_ON) ? BIGFX1C : BIGFX0F ;used by MIDI CC 'Brightness'
rDiffLo lda FREQTBL+2,x
        sec
        sbc FREQTBL,x
        rts
rDiffHi lda FREQTBH+2,x
        sbc FREQTBH,x
        rts
iVIBAMP jmp SETVAMP
.fi
StrtPos .byte 0         ;set to nonzero by editor if playing from position with F2
wPtncod sta ptncode,x
        rts
rFSWITC lda FSWITCH+1
        rts
rFLTBND lda FLTBAND+1
        rts
wCHDMOD sta CHDMODE+0 ;LDA=$A9 / LDY=$A0 immediate
        rts
aEXPTBH .word EXPTABH
.else
SUBTUNESUPP .byte feature.SUBTUNESUPPORT_ON * $80 ;used to set SEQSUB pointers directly if no subtune-support
FRAME_SPD   .byte feature.MULTISPEEDSUPP_ON ;saved by exporter from tune-header, used by EXE/SID exports
TrackerID
        .if (SID_AMOUNT==1)   
        .text " sid-wizard ",SWversion," ";containing this string will be easier to recognize the used editor in releases
        .else
         .if (SID_AMOUNT==2)
          .text " sidwiz-2sid ",SWversion
         .elsif (SID_AMOUNT==3)
          .text " sidwiz-3sid ",SWversion
SID3ADD_ID .byte selfmod ;saved by exporter from tune-header, used by EXE export to check saved SID2 address 
         .fi
SID2ADD_ID .byte selfmod ;saved by exporter from tune-header, used by EXE export to check saved SID2 address 
        .fi
.fi

PLAYER_JUMPTABLE_END
;==============================================================================
.if PLAYERZP_VAR  ;BEGINNING OF VARIABLES' SECTION - VARIABLES ARRANGED IN BUNCHES OF 7
 VARIABLES = PLAYERZP_VAR ;using zeropage-variables for player
.else
 VARIABLES = PLAYER_JUMPTABLE_END
.fi

        .section declaration
        .logical VARIABLES
FREQLO   .byte ?        ;(SID.0) FREQUENCY (PITCH) LO-BYTE GHOST-REGISTERS
FREQHI   .byte ?        ;(SID.1) FREQUENCY (PITCH) PITCH HI-BYTE GHOST-REGISTERS
PWLOGHO  .byte ?        ;(SID.2) PULSEWIDTH LOW-BITE GHOST-REGISTERS
PWHIGHO  .byte ?        ;(SID.3) PULSEWIDTH HI-BYTE GHOST-REGISTERS
WFGHOST  .byte ?        ;(SID.4) WAVEFORM/CTRL GHOST-REGISTERS
PTNGATE  .byte ?        ;GATE OFF/ON STATUS CONTROLLED BY PATTERN (AND MUTE/SOLO IN EDITOR?)
PWEEPCNT .byte ?        ;PULSEWIDTH SWEEP-LENGTH TIMER
         .fill (CHN_AMOUNT-1)*7

PACKCNT  .byte ?        ;PACKED NOP/REST ($70..$77) COUNTER
SPDCNT   .byte ?        ;FRAMESPEED COUNTERS (INCREMENTAL)
SEQPOS   .byte ?        ;ORDERLIST SEQUENCE PLAYPOSITION-SHOULDN'T BE ON PAGE-BOUNDARY!
PTNPOS   .byte ?        ;PATTERN PLAY-POSITION (IN MEMORY)
WFTPOS   .byte ?        ;WF-ARP TABLE POSITION-INDEX
PWTPOS   .byte ?        ;PW-TABLE POSITION-INDEX
ARPSCNT  .byte ?        ;ARPEGGIO-SPEED COUNTERS; also used as multispeed 1st frame detector for multispeed
         .fill (CHN_AMOUNT-1)*7

NONZP_3SID .segment ;these are non-zeropage locations in case of more than 2 SID chips
CURPTN   .byte ?        ;CURRENT PATTERN
CURNOT   .byte ?        ;CURRENT NOTES/GATEOFF ON TRACK 0..2 (+FX0, VIB,RING)
DPITCH   .byte ?        ;CURRENT DISCRETE NOTE-PITCH (ISN'T AFFECTED BY EFFECTS)
CURIFX   .byte ?        ;CURRENT INSTR/ONESHOT-FX ON TRACK 0..2
CURINS   .byte ?        ;CURRENT (SELECTED) INSTRUMENT - NOT OVERWRITTEN BY FX/LEGATO
CURFX2   .byte ?        ;CURRENT FX2-NUMBERS ON TRACK 0..2
CURVAL   .byte ?        ;CURRENT FX2-VALUES ON TRACK 0..2
         .fill (CHN_AMOUNT-1)*7
.endm

NONZP_23SID .segment ;these are non-zeropage locations in case of more than 1 SID chips
SLIDEVIB .byte ?        ;SLIDEUP/SLIDEDOWN/PORTAMENTO/VIBRATO-TYPE (EXTRACTED FROM BIT 4..5 OF INSTRUMENT-CONTROL BYTE)
FREQMODL .byte ?        ;FREQUENCY-SLIDE (MODIFICATION) LOW BYTE (SET BY INSTRUMENT/PATTERN-FX)
FREQMODH .byte ?        ;FREQUENCY-SLIDE (MODIFICATION) HIGH-BYTE (SET BY INSTRUMENT/PATTERN-FX)
VIDELCNT .byte ?        ;VIBRATO-DELAY COUNTER (SET BY INSTRUMENT)
VIBFREQU .byte ?        ;VIBRATO-FREQUENCIES (SET BY INSTRUMENT/PATTERN-FX)
VIBRACNT .byte ?        ;VIBRATO COUNTERS (FOR VIBRATO FREQUENCY HANDLING)
TRANSP   .byte ?        ;TRANSPOSE-AMOUNT ON TRACKS (SET BY SEQUENCE-FX)
         .fill (CHN_AMOUNT-1)*7
;upcoming variable-bunches can be stripped out in some player-types if the corresponding features are switched off
        .if (feature.TEMPOPRGSUPP_ON || feature.ARPSPEEDSUPP_ON || feature.PWKEYBTRACK_ON || feature.CHORDSUPPORT_ON || (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0))
TMPPTR   .byte ?        ;STARTPOINTS OF CURRENT TEMPOPROGRAMS ;USED BY OTHER PARTS
TMPPOS   .byte ?        ;CURRENT TEMPO-POSITIONS IN TEMPOTBL (NEEDED BY PLAYADAPTER IN EDITOR TOO)
ARPSPED  .byte ?        ;ARPEGGIO-SPEED (SET BY INSTRUMENT OR PATTERN-FX)
PKBDTRK  .byte ?        ;PULSEWIDTH KEYBOARD-TRACK VALUE
CURCHORD .byte ?        ;CURRENT CHORDS ON TRACKS (INSTRUMENT/PATT.FX SELECTS IT)
CHORDPOS .byte ?        ;PLAYED POSITION (INDEX) OF CURRENT CHORD IN CHORDTABLE
ARPDPITCH .byte ?        ;a help for Slowdown or other pitch-related effects, more frequently updated than DPITCH (at chord/arp pitch-changes)
         .fill (CHN_AMOUNT-1)*7
        .fi
    .if (feature.ALLGHOSTREGS_ON || SID_AMOUNT>1) ;in 2SID version we always use ghost-registers
SIDG .block             ; Ghost registers for buffering SID writes
FREQ .word ?            ;SID'S FREQUENCY GHOST-REGISTER
PLSW .word ?            ;SID'S PULSEWIDTH GHOST-REGISTER
WAVE .byte ?            ;SID'S WAVEFORM/CONTROL GHOST-REGISTER
AD   .byte ?            ;SID'S ATTACK/DECAY GHOST-REGISTER
SR   .byte ?            ;SID'S SUSTAIN/RELEASE GHOST-REGISTER
     .fill (CHN_AMOUNT-1)*7
     .bend
    .fi
.endm ;end NONZP_23SID segment

.if (feature.ALLGHOSTREGS_ON==0 && SID_AMOUNT==1) ; Direct SID write if no ghost registers are used
SIDG  = SID
.fi

.if (SID_AMOUNT==2)
        #NONZP_3SID
.elsif (SID_AMOUNT==1)
        #NONZP_3SID
        #NONZP_23SID
.fi

ENDVARIABLES
        .here
        .send declaration

;==========ALLOCATE MEMORY FOR VARIABLES or PLAYER SIGNATURE & VERSION==========
.if (COMPILEDAPP==1)
 .if (PLAYERZP_VAR==0)
        .fill (ENDVARIABLES-VARIABLES),0
 .fi
.else ;in the editor software-info was not needed
 TRACKERINFO=PLAYER_JUMPTABLE_END ;if variables are there, overwriting zeroes, but initer routine will zero them again
 TUNE_HEADER=VARIABLES ;PLAYER_JUMPTABLE+$20 ;TUNE-HEADER WILL BE COPIED HERE BY EXPORTER. ;align to be better seen in monitor/debugger
        .fill (ENDVARIABLES-VARIABLES)-(*-TRACKERINFO),0 ;padding space if variables take less memory (when zeropage is used for them)
.fi

;------------------------------------------------------------------------------
CONST_VAR               ;CONSTANT VALUES AND MISC.VARIABLES NOT INITED BY INIT-ROUTINE
FLSWTBL  .byte 1        ;.... CONSTANTS-BITMASK FOR SID-FILTERSWITCHES
FLSWTB2  .byte $FE      ;FILTERSWITCH 'AND' TABLE (CONSTANTS)
TRKTMPOS .byte 2        ;TRACK-TEMPO-POSITION TABLE (CONSTANTNS)
TRDELAY  .byte 0        ;TRACK-DELAY AMOUNTS
SEQTEMPO .byte 0        ;SEQ-FX TEMPOCHANGE DELAYER
TRANSP2  .byte 0        ;DELAYED TRANSPOSE-VALUE (BECAUSE SEQ-FX DOES IT EARLY)
DETUNER  .byte 0        ;detuning value on the tracks (affected by wf-arp-detune inst.table or patt.FX)
        .cwarn *>7+CONST_VAR,"MAXIMUM 7 VARIABLES/CONSTANTS ALLOWED PER BUNCH!"
         .byte 2,$FD,4,0,0,0,0, 4,$FB,6,0,0,0,0 ;ENSURE SPACE FOR THE BUNCH OF CONSTANTS/VARIABLES ON ALL THE 3 CHANNELS
.if (SID_AMOUNT>=2)
         .byte 1,$FE,8,0,0,0,0, 2,$FD,10,0,0,0,0, 4,$FB,12,0,0,0,0
 .if (SID_AMOUNT>=3)
         .byte 1,$FE,14,0,0,0,0, 2,$FD,16,0,0,0,0, 4,$FB,18,0,0,0,0
 .fi
.fi
.if (SID_AMOUNT>=2)
 .if (SID_AMOUNT>=3)
NONZP3SID #NONZP_3SID
 .fi
NONZP23SID #NONZP_23SID   ;didn't fit into zeropage
NONZP23SID_END
.fi

.if (COMPILEDAPP==2 && SWP_EXPORT!=0)
SWP_OFFSET .word 0 ;overwritten by INITER routine
.fi
.if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
SLOWDCNT .byte 0 ;counts when to skip frames
.fi
CONST_VAR_END


;******************************************************************************
INITER                  ;INITIALIZE SUBTUNE (INPUT: SUBTUNE-NUMBER IN ACCU.)
.if(COMPILEDAPP==2 && SWP_EXPORT!=0) ;X and Y holds base-address of SWP-format musicdata 
        sta SWPdone+1 ;store subtune
        .if (ZEROPAGESAVE_ON && PLAYERZP_VAR==0) ;no point to save zeropage if it's used for variables
        lda PLAYERZP+0  ;STORE ZEROPAGE  ;SINGLE-SPEED PLAYING
        pha
        lda PLAYERZP+1
        pha
        .fi
        stx SWP_OFFSET+0
        stx SWPptr1+1
        stx SWPptr2+1
        sty SWP_OFFSET+1
        sty SWPptr1+2
        sty SWPptr2+2
        ldx #0
-       lda PLAYER_JUMPTABLE+(DataPtr-player.playercode)+1,x
        beq SWPdone
        sta PLAYERZP+1
        lda PLAYER_JUMPTABLE+(DataPtr-player.playercode)+0,x
        sta PLAYERZP+0
        ldy PLAYER_JUMPTABLE+(PtrValu-player.playercode)+0,x
        ;bmi SWPadva ;illegal offset-values ($FF) skipped
SWPptr1 lda selfmodA,y ;relative-offset pointer low-byte
        clc
        adc SWP_OFFSET+0 ;make it absolute-address
        ldy #1
        sta (PLAYERZP),y
        ldy PLAYER_JUMPTABLE+(PtrValu-player.playercode)+0,x
        iny
SWPptr2 lda selfmodA,y;relative-offset pointer hi-byte
        adc SWP_OFFSET+1 ;make it absolute-address
        ldy #2
        sta (PLAYERZP),y
        dey ;Y=1
        lda PLAYER_JUMPTABLE+(PtrValu-player.playercode)+1,x ;additional offset info
        clc
        adc (PLAYERZP),y
        sta (PLAYERZP),y
        .if (feature.SUBTUNESUPPORT_ON==0)
        sta seq_ptr+1 ;reusing p_seqt1
        .fi
        iny ;Y=2
        lda (PLAYERZP),y
        adc #0 ;add Carry-flag
        ldy PLAYER_JUMPTABLE+(PtrValu-player.playercode)+1,x ;check if negative direction (2's complement)
        bpl +
        sec
        sbc #1 ;convert 2's complement
+       .if (feature.SUBTUNESUPPORT_ON==0)
        cpx #CHN_AMOUNT*2 ;converting sequence pointer-pointers to pointers in place
        bcs +
        sta seq_ptr+2
        ldy #0
        jsr seq_ptr
        clc
        adc SWP_OFFSET+0
        iny ;Y=1
        sta (PLAYERZP),y
        jsr seq_ptr ;Y==1 here
        adc SWP_OFFSET+1
        .fi
+       ldy #2
        sta (PLAYERZP),y
SWPadva inx
        inx
        bne -
SWPdone lda #selfmod ;restore subtune-number
.fi
        jsr SETSTUNE    ;SET SUBTUNE GIVEN IN ACCU (ADDR,SPD), set subtune-tempo
INIPVAR lda #0          ;ZERO ALL COUNTERS & POINTERS & ROWDATA, ETC. 
        ldy #(ENDVARIABLES-VARIABLES)-1
-       sta VARIABLES,y
        dey
        bpl -           ;ACCU MUST BE 0 WHEN THIS LOOP ENDS
.if (SID_AMOUNT>1)
 .if ( (NONZP23SID_END-NONZP23SID) > 0 )
        ldy #(NONZP23SID_END-NONZP23SID)-1
-       sta NONZP23SID,y
        dey
        bpl -
 .fi
.fi
        ldy #$17        ;$d418 is left out from init, so pop/clip might be less noticeable
-       sta SIDBASE,y   ;INIT SID (X CAN BE OVER $20 ON A STOCK C64)
        .if (SID_AMOUNT>=2)
p_SID2a sta SID2BASE,y
        .fi
        .if (SID_AMOUNT>=3)
p_SID3a sta SID3BASE,y
        .fi
        dey
        bpl -
        sta FLTBAND+1   ;RESET FILTER-BAND REGISTER BITS
        .if (SID_AMOUNT>=2)
        sta FLTBAN2+1
        .fi
        .if (SID_AMOUNT>=3)
        sta FLTBAN3+1
        .fi
        .if feature.FILTSHIFT_SUPP_ON
         sta FLSHIFT+1   ;RESET FILTER-SHIFTING EFFECT
         .if (SID_AMOUNT>=2)
         sta FLSHIF2+1
         .fi
         .if (SID_AMOUNT>=3)
         sta FLSHIF3+1
         .fi
        .fi
        .if feature.FILTKBTRACK_ON
        sta CKBDTRK+1   ;INIT FILTER-KEYBOARD TRACK, AS NOT NEEDED IF NO FILTER USED
         .if (SID_AMOUNT>=2)
          sta CKBDTR2+1
         .fi
         .if (SID_AMOUNT>=3)
          sta CKBDTR3+1
         .fi
        .fi
        .if feature.FILT_CTRL_FX_ON
        sta FSWITCH+1   ;initialize filter-switches, especially filt-external switch
         .if (SID_AMOUNT>=2)
          sta FSWITC2+1
         .fi
         .if (SID_AMOUNT>=3)
          sta FSWITC3+1
         .fi
        .fi
INITVOL lda #15         ;MAX. VOLUME VALUE
        sta MAINVOL+1
.if (SID_AMOUNT>=2)
        sta MAINVO2+1
.fi
.if (SID_AMOUNT>=3)
        sta MAINVO3+1
.fi
        .if feature.SEQ_FX_SUPPORT_ON
        sta SEQVOLU+1
        .if (SID_AMOUNT>=2)
        sta SEQVOL2+1
        .fi
        .if (SID_AMOUNT>=3)
        sta SEQVOL3+1
        .fi
        .fi
        sta flprog1.FLTCTRL+1   ;NOT IN 0..2 RANGE - NO FILTER FOR ANY CHANNEL ON INIT
        .if (SID_AMOUNT>=2)
        sta flprog2.FLTCTRL+1
        .fi
        .if (SID_AMOUNT>=3)
        sta flprog3.FLTCTRL+1
        .fi

INITPTN ldx #(CHN_AMOUNT-1)*7 
loop2
        lda #0
        .if feature.TRANSPOSESUPP_ON
        sta TRANSP2,x
        .fi
        .if feature.SEQ_FX_SUPPORT_ON
        sta SEQTEMPO,x
        .fi

SEQFXLO ldy SEQPOS,x    ;IT'S 0 AT THE BEGINNING FOR ALL TRACKS
        jsr SEQSUB      ;GET PATTERNS AT 1ST POSITIONS OF SEQUENCES
        .if COMPILEDAPP==1
        beq TestPos     ;if zero pattern-number somehow, don't set it
        bmi chSeqFx     ;...check above $80
        cmp #maxptnamount+1
        bcc INIPTN      ;if not legal pattern, don't set
        bcs TestPos     ;jump
chSeqFx cmp #$FE        ;IF DELIMITER AT 1ST POSITION, DON'T SET PATTERN
        bcc + ;IF LOWER THAN $FE, PERFORM ORDERLIST-EFFECTS
        ldy playadapter.div7chn,x ;convert (divide) 0..7..14 to 0..1..2
        sta playadapter.skiptrk,y
        jmp endseqset   
        .else
        bpl INIPTN      ;IF 1ST POSITION IS NOT PATTERN, LEAVE IT OR PERFORM SEQ-FX
        cmp #$FE        ;IF DELIMITER AT 1ST POSITION, DON'T SET PATTERN
        bcs endseqset    ;IF LOWER THAN $FE, PERFORM ORDERLIST-EFFECTS
        .fi
+       .if feature.SEQ_FX_SUPPORT_ON
        jsr SEQ_FX      ;HANDLE ORDERLIST/SEQUENCE-FX, INPUT:A, DOESN'T DISTURB X&Y
        .fi
advSeq  inc SEQPOS,x    ;GO TO NEXT SEQUENCE-POSITION
        bne SEQFXLO     ;AVOID ENDLESS LOOP
INIPTN  sta CURPTN,x    ;SET PATTERNS
        .if COMPILEDAPP==1
TestPos lda StrtPos     ;is set to nonzero by editor if playing from position with F2
        beq endseqset   ;if F2-key initiated the start
        ldy playadapter.div7chn,x ;convert (divide) 0..7..14 to 0..1..2
        lda playadapter.olstart,y
        cmp SEQPOS,x
        beq endseqset
        bcs advSeq
        .fi
endseqset
        .if feature.DELAYSUPPORT_ON
        lda #$FF
        sta TRDELAY,x   ;PREPARE POSSIBLE UPCOMING TRACK-DELAY
        .fi
        txa
        sec
        sbc #7          ;CYCLE SID-CHANNELS
        tax
        bpl loop2
.if (COMPILEDAPP==2 && SWP_EXPORT!=0 && ZEROPAGESAVE_ON && PLAYERZP_VAR==0)
        jmp RETPLAY ;restore zeropage
.else
        rts
.fi

.if (COMPILEDAPP==2 && SWP_EXPORT!=0 && feature.SUBTUNESUPPORT_ON==0)
seq_ptr lda selfmodA,y
        rts
.fi

;------------------------------------------------------------------------------
.if (SFX_SUPPORT!=0)
SFXinit ;SFXinit will trigger an instant note+instrument on next PLAYSUB call, bypassing SPDCNT
        sta SFXleng
        stx CURNOT+2*7 ;set SFX-note on channel3
        bit SFXtimer
        bpl + ;if SFX is still being played it's not to update CURINStemp
        ldx CURINS+2*7 ;store current instrument of channel3
        stx CURINStemp
+       sta SFXtimer
        sty CURINS+2*7 ;set SFX-instrument on channel 3
        lda #0
        sta CURIFX+2*7
        sta SLIDEVIB+2*7 ;suppress any performed slide
SFX_HR  sta SID.AD+2*7 ;perform HARD-Restart for stable SFX-start, instrument-number must be in Y
        sta SID.SR+2*7
        sta SID.WAVE+2*7
        rts

SFXleng  .byte $7f ;used to check 1st frame of SFX in player
SFXtimer .byte $ff ;will count up from SFXleng down to 0, 0 signs last frame of SFX for player
CURINStemp .byte 0 ;temporary storage for the music, will be updated if pattern select instrument behind/during the SFX
.fi ;playing an SFX will suppress hard-restart and all subsequent effects (slide/tempo/etc.) on channel3!

.if (SID_AMOUNT>=2)
ExtVol2 .if (SID_AMOUNT>=3)
        ldx #2*3*7
        jsr SMALFXA
        .fi
        ldx #3*7
        jsr SMALFXA
        ldx #0
        jmp SMALFXA
.fi

.if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
setSlowdown 
;        cmp #(slDither_end-slDither)
;        bcs slRetur
        sta SLOWDOWN+1
        .if feature.CALCVIBRATO_ON
        sta SLOWDN3+1
        .fi
        sta SLOWDN4+1
        sta SLOWDN5+1
        sta SLOWDN6+1
;        tay ;setting the counter as well would react immediately (unlike max. 8 frames delay), but not suitable for continuous slowdown due to repetitive zeroes
        eor #$ff
        clc
        adc #1
        sta SLOWDN2+1
;        lda slDither,y
;        sta SLOWDCNT
slRetur rts
.fi


;******************************************************************************
.if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
;0 is normal speed...12 is half-speed...24 is quarter-speed, etc.   (goes together with pitch counted in half-notes)
;limit8 ratios of 0..-24:  1/1,   1/1, 7/8, 5/6, 4/5, 3/4, 5/7, 2/3, 5/8, 3/5, 4/7, 1/2, 1/2,   1/2, 3/7, 3/7, 2/5, 3/8, 3/8, 1/3, 1/3, 2/7, 2/7, 1/4, 1/4
;this table contains the ratios of 'limit8': ratios of 0s and 1s, the leading zeroes determine the length (denominator)
slDither .byte %11111111,  %11111111, %11101111, %00110111, %00011011, %11011101, %01011101, %00101101, %10101101, %00010101, %01010101, %10101010, %10101010
          .byte            %10101010, %01010010, %01010010, %00010100, %10100100, %10100100, %00100100, %00100100, %01001000, %01001000, %10001000, %10001000
slDither_end ;the length of this table determines how much is the slowdown-range
.fi


PLAYER
.if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
        lsr SLOWDCNT
        bne + ;check end: when only leading zeroes remain in the byte (value is 0), reload it...
        ldy SLOWDOWN+1
        lda slDither,y
        sta SLOWDCNT
+       bcc slRetur ;if '1' encountered, play frame, if '0' encountered skip frame
.fi
        .if (ZEROPAGESAVE_ON && PLAYERZP_VAR==0) ;no point to save zeropage if it's used for variables
        lda PLAYERZP+0  ;STORE ZEROPAGE  ;SINGLE-SPEED PLAYING
        pha
        lda PLAYERZP+1
        pha
        .fi
        ldx #(3-1)*7    ;CHANNEL 3
.if (SFX_SUPPORT!=0)
        lda SFXtimer ;value is equal with SFXleng if just called & started SFX
        beq SFXclos   ;last frame of SFX?
        bmi + ;if FX is over
        cmp SFXleng
        bne advaSFX
SFXstrt jsr CHKNOTE ;iCHKNOT - start the note+instrument, 1st waveform...
        jmp advaSFX
SFXclos ldy CURINStemp
        sty CURINS+2*7 ;bring back instrument for channel3
        jsr SFX_HR     ;make SFX silent + a hard-restart for the music's upcoming notes
advaSFX dec SFXtimer
+
.fi
        jsr DOTRACK
.if (SID_AMOUNT>=2)
        ldx #(6-1)*7    ;CHANNEL 6
        jsr DOTRACK
.fi
.if (SID_AMOUNT>=3)
        ldx #(9-1)*7    ;CHANNEL 9
        jsr DOTRACK
.fi
        ldx #(2-1)*7    ;CHANNEL 2
        jsr DOTRACK
.if (SID_AMOUNT>=2)
        ldx #(5-1)*7    ;CHANNEL 5
        jsr DOTRACK
.fi
.if (SID_AMOUNT>=3)
        ldx #(8-1)*7    ;CHANNEL 8
        jsr DOTRACK
.fi
        ldx #(1-1)*7    ;CHANNEL 1
        jsr DOTRACK
.if (SID_AMOUNT>=2)
        ldx #(4-1)*7    ;CHANNEL 4
        jsr DOTRACK
.fi
.if (SID_AMOUNT>=3)
        ldx #(7-1)*7    ;CHANNEL 7
        jsr DOTRACK
.fi

COMMONREGS              ;CALCULATING AND WRITING COMMON SID REGISTERS - ENTRY POINT FOR EDITOR

        .if (feature.ALLGHOSTREGS_ON || SID_AMOUNT>1) ;IF GHOST-REGISTERS ARE USED FOR EVERY SID-REGISTERS (EXTRA) - takes 3-4 more rasterlines
        ldx #2*7
        sec
loop3   lda SIDG.SR,x   ;important to keep some distance between SR and Waveform-setting for more reliable notestart!!!!!
        sta SID.SR,x
        lda SIDG.AD,x   ;important to keep some distance between AD and Waveform-setting for more reliable notestart!!!!!
        sta SID.AD,x
        lda SIDG.FREQ,x
        sta SID.FREQ,x
        lda SIDG.FREQ+1,x
        sta SID.FREQ+1,x
        lda SIDG.PLSW,x
        sta SID.PLSW,x
        lda SIDG.PLSW+1,x
        sta SID.PLSW+1,x
        lda SIDG.WAVE,x ;keep distance from ADSR setting for more reliable notestart!!!
        sta SID.WAVE,x
.if (SID_AMOUNT>=2)
        lda SIDG.SR+3*7,x   ;important to keep some distance between SR and Waveform-setting for more reliable notestart!!!!!
p_SID2b sta SID2.SR,x
        lda SIDG.AD+3*7,x   ;important to keep some distance between AD and Waveform-setting for more reliable notestart!!!!!
p_SID2c sta SID2.AD,x
        lda SIDG.FREQ+3*7,x
p_SID2d sta SID2.FREQ,x
        lda SIDG.FREQ+1+3*7,x
p_SID2e sta SID2.FREQ+1,x
        lda SIDG.PLSW+3*7,x
p_SID2f sta SID2.PLSW,x
        lda SIDG.PLSW+1+3*7,x
p_SID2g sta SID2.PLSW+1,x
        lda SIDG.WAVE+3*7,x ;keep distance from ADSR setting for more reliable notestart!!!
p_SID2h sta SID2.WAVE,x
.fi
.if (SID_AMOUNT>=3)
        lda SIDG.SR+2*3*7,x   ;important to keep some distance between SR and Waveform-setting for more reliable notestart!!!!!
p_SID3b sta SID3.SR,x
        lda SIDG.AD+2*3*7,x   ;important to keep some distance between AD and Waveform-setting for more reliable notestart!!!!!
p_SID3c sta SID3.AD,x
        lda SIDG.FREQ+2*3*7,x
p_SID3d sta SID3.FREQ,x
        lda SIDG.FREQ+1+2*3*7,x
p_SID3e sta SID3.FREQ+1,x
        lda SIDG.PLSW+2*3*7,x
p_SID3f sta SID3.PLSW,x
        lda SIDG.PLSW+1+2*3*7,x
p_SID3g sta SID3.PLSW+1,x
        lda SIDG.WAVE+2*3*7,x ;keep distance from ADSR setting for more reliable notestart!!!
p_SID3h sta SID3.WAVE,x
.fi
        txa
        sbc #7
        tax
         .if (*+2-$80<=loop3)
         bcs loop3
         .else
         bcc +
         jmp loop3
+        .fi
        .fi

FSWITCH lda #$00 ;$08   ;SELF-WRITTEN - TRACKS' FITER-SWITCHES' NIBBLE (EXTERNAL FILTER ON/OFF,BigFx-$1F - REDUCE SID-noise )
RESONIB ora #selfmod    ;RESONANCE-NIBBLE OF FILTER (UPPER 4 BITS)
        sta SID.RESFC
MAINVOL lda #$0F        ;SELF-WRITTEN CODE - MAIN VOLUME NIBBLE OF $D418
FLTBAND ora #selfmod    ;FILTERBAND-GHOSTREGS MUST BE HERE (ON HI-NIBBLE)
        sta SID.FMVOL
        clc             ;PRE-SET CARRY FOR ADDITIONS
CKBDTRK .if (feature.FILTKBTRACK_ON)
        lda #selfmod    ;KEYBOARD-TRACK EFFECT AMOUNT (MULTIPLY/DIVIDE) IF 0, NO EFFECT
        beq CTFHGHO     ;IF 0, NO KEYBOARD-TRACKING
        ldx flprog1.FLTCTRL+1   ;X = CURRENT FILTER-CONTROLLER TRACK
        adc DPITCH,x    ;CURRENT FILTER-CONTROLLER TRACK'S NOTE-NUMBER ;THE ADDITION (OR SUBSTRACTION IF A>$80)
        tay
        lda EXPTABH,y   ;USE EXPONENT TABLE AS QUASI MULTIPLY/DIVIDE FUNCTION BASED ON CKBDTRK
CTFHGHO adc #selfmod    ;HIGH BYTE OF FILTER CUTOFF-FREQUENCY
        .else
CTFHGHO lda #selfmod    ;HIGH BYTE OF FILTER CUTOFF-FREQUENCY
        .fi
        .if feature.FILTSHIFT_SUPP_ON
FLSHIFT adc #0          ;PATTERN-FX INITIATED FILTER-SHIFTING
        .fi
        .if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
SLOWDN2 ldy #selfmod
        beq wrCtfHi
        cmp #$10 ;treshold value in FREQTBH (starting value of EXPTAB2)
        bcs slUpper ;upper half?
slLower tax ;value is 0..$F
        tya
        adc LOGTBL,x
        bpl + ;avoid wrapping around
        lda #0 ;lowest audible value
+       jmp getSlFi
slUpper lsr ;value is $10..$ff
        lsr
        lsr
        lsr
        tax
        tya
        adc LOGTBL,x
        adc #(EXPTAB2-FREQTBH) ;offset ; not necessary to check negative in 'slUpper' as it's quite above position 0
getSlFi tay
        lda FREQTBH,y
        .fi
wrCtfHi sta SID.FCUT+1
        .if COMPILEDAPP==1
        sta CTFMETR+1   ;EASE CUTOFF-METER DISPLAY FOR EDITOR
        .fi
CTFLGHO .if feature.FINEFILTSWEEP_ON
        lda #selfmod    ;LOW NIBBLE OF FILTER CUTOFF-FREQUENCY
        sta SID.FCUT+0  ;FOR RASTERTIME REASONS LOW-BITS OF CUTOFF IS NOT KEYBOARD-TRACKED
        .fi

.if (SID_AMOUNT>=2)
FSWITC2 lda #$00 ;$08   ;SELF-WRITTEN - TRACKS' FITER-SWITCHES' NIBBLE (EXTERNAL FILTER ON/OFF,BigFx-$1F - REDUCE SID-noise )
RESONI2 ora #selfmod    ;RESONANCE-NIBBLE OF FILTER (UPPER 4 BITS)
p_SID2i sta SID2.RESFC
MAINVO2 lda #$0F        ;SELF-WRITTEN CODE - MAIN VOLUME NIBBLE OF $D418
FLTBAN2 ora #selfmod    ;FILTERBAND-GHOSTREGS MUST BE HERE (ON HI-NIBBLE)
p_SID2j sta SID2.FMVOL
        clc             ;PRE-SET CARRY FOR ADDITIONS
CKBDTR2 .if feature.FILTKBTRACK_ON
        lda #selfmod    ;KEYBOARD-TRACK EFFECT AMOUNT (MULTIPLY/DIVIDE) IF 0, NO EFFECT
        beq CTFHGH2     ;IF 0, NO KEYBOARD-TRACKING
        ldx flprog2.FLTCTRL+1   ;X = CURRENT FILTER-CONTROLLER TRACK
        adc DPITCH,x    ;CURRENT FILTER-CONTROLLER TRACK'S NOTE-NUMBER ;THE ADDITION (OR SUBSTRACTION IF A>$80)
        tay
        lda EXPTABH,y   ;USE EXPONENT TABLE AS QUASI MULTIPLY/DIVIDE FUNCTION BASED ON CKBDTRK
CTFHGH2 adc #selfmod    ;HIGH BYTE OF FILTER CUTOFF-FREQUENCY
        .else
CTFHGH2 lda #selfmod    ;HIGH BYTE OF FILTER CUTOFF-FREQUENCY
        .fi
        .if feature.FILTSHIFT_SUPP_ON
FLSHIF2 adc #0          ;PATTERN-FX INITIATED FILTER-SHIFTING
        .fi
p_SID2k sta SID2.FCUT+1
CTFLGH2 .if feature.FINEFILTSWEEP_ON
        lda #selfmod    ;LOW NIBBLE OF FILTER CUTOFF-FREQUENCY
p_SID2l sta SID2.FCUT    ;FOR RASTERTIME REASONS LOW-BITS OF CUTOFF IS NOT KEYBOARD-TRACKED
        .fi
.fi
.if (SID_AMOUNT>=3)
FSWITC3 lda #$00 ;$08   ;SELF-WRITTEN - TRACKS' FITER-SWITCHES' NIBBLE (EXTERNAL FILTER ON/OFF,BigFx-$1F - REDUCE SID-noise )
RESONI3 ora #selfmod    ;RESONANCE-NIBBLE OF FILTER (UPPER 4 BITS)
p_SID3i sta SID3.RESFC
MAINVO3 lda #$0F        ;SELF-WRITTEN CODE - MAIN VOLUME NIBBLE OF $D418
FLTBAN3 ora #selfmod    ;FILTERBAND-GHOSTREGS MUST BE HERE (ON HI-NIBBLE)
p_SID3j sta SID3.FMVOL
        clc             ;PRE-SET CARRY FOR ADDITIONS
CKBDTR3 .if feature.FILTKBTRACK_ON
        lda #selfmod    ;KEYBOARD-TRACK EFFECT AMOUNT (MULTIPLY/DIVIDE) IF 0, NO EFFECT
        beq CTFHGH3     ;IF 0, NO KEYBOARD-TRACKING
        ldx flprog3.FLTCTRL+1   ;X = CURRENT FILTER-CONTROLLER TRACK
        adc DPITCH,x    ;CURRENT FILTER-CONTROLLER TRACK'S NOTE-NUMBER ;THE ADDITION (OR SUBSTRACTION IF A>$80)
        tay
        lda EXPTABH,y   ;USE EXPONENT TABLE AS QUASI MULTIPLY/DIVIDE FUNCTION BASED ON CKBDTRK
CTFHGH3 adc #selfmod    ;HIGH BYTE OF FILTER CUTOFF-FREQUENCY
        .else
CTFHGH3 lda #selfmod    ;HIGH BYTE OF FILTER CUTOFF-FREQUENCY
        .fi
        .if feature.FILTSHIFT_SUPP_ON
FLSHIF3 adc #0          ;PATTERN-FX INITIATED FILTER-SHIFTING
        .fi
p_SID3k sta SID3.FCUT+1
CTFLGH3 .if feature.FINEFILTSWEEP_ON
        lda #selfmod    ;LOW NIBBLE OF FILTER CUTOFF-FREQUENCY
p_SID3l sta SID3.FCUT    ;FOR RASTERTIME REASONS LOW-BITS OF CUTOFF IS NOT KEYBOARD-TRACKED
        .fi
.fi
        
RETPLAY                 ;RETURN FROM PLAYER
        .if (ZEROPAGESAVE_ON && PLAYERZP_VAR==0) ;no point to save zeropage if it's used for variables
         pla             ;RESTORE ZEROPAGE
         sta PLAYERZP+1
         pla
         sta PLAYERZP+0
        .fi
retsubr rts


;******************************************************************************
MULPLY                 ;MULTISPEED-PLAYING ROUTINE
.if feature.MULTISPEEDSUPP_ON
        .if (ZEROPAGESAVE_ON && PLAYERZP_VAR==0) ;no point to save zeropage if it's used for variables
        lda PLAYERZP+0  ;STORE ZEROPAGE  ;MULTI-SPEED PLAYING
        pha
        lda PLAYERZP+1
        pha
        .fi
        ldx #(3-1)*7    ;CHANNEL 3
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.if (SID_AMOUNT>=2)
        ldx #(6-1)*7    ;CHANNEL 6
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.fi
.if (SID_AMOUNT>=3)
        ldx #(9-1)*7    ;CHANNEL 9
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.fi
        ldx #(2-1)*7    ;CHANNEL 2
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.if (SID_AMOUNT>=2)
        ldx #(5-1)*7    ;CHANNEL 5
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.fi
.if (SID_AMOUNT>=3)
        ldx #(8-1)*7    ;CHANNEL 8
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.fi
        ldx #(1-1)*7    ;CHANNEL 1
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.if (SID_AMOUNT>=2)
        ldx #(4-1)*7    ;CHANNEL 4
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.fi
.if (SID_AMOUNT>=3)
        ldx #(7-1)*7    ;CHANNEL 7
        jsr MULCNTP     ;PLAY INSTRUMENT-TABLE(S)
.fi
        jmp COMMONREGS  ;PLAY FILTER TOO ;JMP RETPLAY ;RESTURN FROM MULTISPEED-PLAYER PART
;--------------------
MULCNTP .if COMPILEDAPP==1
        lda playadapter.playmod
        ora playadapter.wasjamm,x ;if no jammed note yet, don't play multispeed either
        beq retsubr
        .fi
        ;don't allow multispeed to destruct 1st frame waveform-setting
        lda ARPSCNT,x   ;ARPSCNT also used as multispeed 1st frame detector
        bmi retsubr     ;if still $FF  (reset on frame1 to $FF and only set to arp-speed in next frame)
        ldy CURINS,x    ;PUT CURRENT INSTRUMENT'S BASE-ADDRESS TO ZEROPAGE-POINTERS
p_insl1 lda INSPTLO,y   ;Y IS NUMBER OF CURRENT INSTRUMENT
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        clc
        adc SWP_OFFSET+0
.fi
        sta PLAYERZP+0
p_insh1 lda INSPTHI,y
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        adc SWP_OFFSET+1
.fi
        sta PLAYERZP+1
        ldy #7          ;CHECK ARPSPEED DATA OF CURRENT INSTRUMENT (BIT 6 & 7)
        lda (PLAYERZP),y
        bmi MULTIFI     ;IF INSTRUMENT-ARPEGGIO SPEED IS BIGGER THAN $80 (BIT7=1), MULTISPEED FILTER TOO
        and #$40        ;IF BIT6=1, PULSEWIDTH IS MULTISPEED TOO
        bne MULTIPW     ;IF SPEED BIT6=1
        jmp WFARPTB     ;MULTISPEED SUPPORT ONLY FOR WAVEFORM-TABLE
MULTIPW jmp SETPWID     ;MULTISPEED SUPPORT FOR PULSEWIDTH-TABLE TOO
MULTIFI jmp FILTPRG     ;MULTISPEED SUPPORT FOR PULSEWIDTH-TABLE AND FILTERTABLE TOO
.else
        rts
.fi

;******************************************************************************
DOTRACK .if (COMPILEDAPP==1)
        ldy playadapter.div7chn,x
        lda playadapter.skiptrk,y
        bpl + ;if track is disabled by INITER don't advance in that track
        rts
        .fi
+       .if feature.TEMPOPRGSUPP_ON
        ldy TMPPOS,x    ;HANDLE TEMPO (SUBTUNE-TEMPO/FUNKTEMPO/TEMPO-PROGRAM)
        .fi
        lda SPDCNT,x
        .if feature.TEMPOPRGSUPP_ON
        iny             ;PREPARE Y FOR ADVANCE IN TEMPOTABLE
        .fi
        sec
        ;tempoprogram-table base-address to adjust in SID-maker
        .if feature.TEMPOPRGSUPP_ON
p_tmpt1 sbc TEMPOTBL-1,y ;COMPARE SPEED-COUNTER TO CURRENT TEMPO, AND CHECK BIT 7
        .else
p_tmpt1 sbc TEMPOTBL+0  ;SINGLETEMPO
        .fi
        beq advTprg     ;IF EQUAL, GO TO ADVANCE IN TEMPO-PROGRAM
        bvc chkTick     ;CHECK FOR $80 RESULT TOO. IF NOT MATCH, GO TO REST OF TICKS
        .if feature.TEMPOPRGSUPP_ON
        ldy TMPPTR,x    ;SPDCNT=TEMPO-80, GET CURRENT TEMPO-PROGRAM'S POSITION
        .fi
        lda #0          ;HERE WE SET ACCU (TEMPOCOUNTER) TO 0 TO GET NEXT ROW
advTprg sta SPDCNT,x    ;ZERO SPEEDCOUNTER,INCREMENTED SOON TO 1 (IN THIS FRAME)
        .if feature.TEMPOPRGSUPP_ON
        tya             ;Y HERE IS EQUAL TO THE TEMPOTABLE POINTER WE WOULD SET
        sta TMPPOS,x    ;THEN RETURN BACK TO START OF TEMPOPROGRAM (SET TEMPOCNT)
        .fi
chkTick lda SPDCNT,x    ;CHECK SPECIAL TICKS (SPDCNT IS IN 0..TEMPO RANGE)
        inc SPDCNT,x    ;INC SPEEDCOUNTERS TO STEP FORWARD 1 TICK
TICKSEL cmp #0
        beq TICK_0      ;CHECK FOR TICK 0 (BRANCH IF SPDCNT = 0 )
        jmp CHKTCK1     ;CHECK IF TICK1


;------------------------------------------------------------
TICK_0  ldy CURPTN,x    ;PUT CURRENT PATTERN'S ADDRESS TO ZEROPAGE-POINTERS
p_ptnl1 lda PPTRLO,y    ;Y IS NUMBER OF CURRENT PATTERN
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        clc
        adc SWP_OFFSET+0
.fi
        sta PLAYERZP+0
p_ptnh1 lda PPTRHI,y
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        adc SWP_OFFSET+1
.fi
        sta PLAYERZP+1
READROW lda #0
        sta CURIFX,x    ;ENSURE ONE-SHOT INSTRUMENT-SELECTION/SMALL-FX
        sta CURFX2,x    ;ENSURE ONE-SHOT SMALL-/BIG-FX
        .if feature.SEQ_FX_SUPPORT_ON
SEQVOLU lda #$0F        ;SELF-WRITTEN - USED TO DELAY SETTING OF MAIN-VOLUME SEQ.FX
        sta MAINVOL+1
        .if (SID_AMOUNT>=2)
SEQVOL2 lda #$0F        ;SELF-WRITTEN - USED TO DELAY SETTING OF MAIN-VOLUME SEQ.FX
        sta MAINVO2+1
        .fi
        .if (SID_AMOUNT>=3)
SEQVOL3 lda #$0F        ;SELF-WRITTEN - USED TO DELAY SETTING OF MAIN-VOLUME SEQ.FX
        sta MAINVO3+1
        .fi
        .fi
        .if feature.TEMPOPRGSUPP_ON
        lda SEQTEMPO,x  ;PERFORM TRACK-TEMPO SEQ.FX (DELAYED)
        beq endseqtmp
        jsr TRAKTMP     ;SET TRACK-TEMPO
        lda #0
        sta SEQTEMPO,x
endseqtmp
        .fi

        ldy PTNPOS,x    ;READ CONTENT OF CURRENT PATTERN-ROW-----------
        .if PACKEDNOPSUPP_ON ;NOT RECOMMENDED TO SWITCH OFF SUPPORT, UNLESS YOU DON'T EVER WANT IT
        lda PACKCNT,x   ;HANDLE PACKED NOP (DECREMENT PACK-COUNTER AND SKIP READING)
        bne PACKNOP     ;IF PACKED NOP COUNTER IS NONZERO, DECREASE IT.
        .fi
CHNOTEC lda (PLAYERZP),y ;READ CURRENT NOTE FROM NOTE-COLUMN
        .if PACKEDNOPSUPP_ON ;NOT RECOMMENDED TO SWITCH OFF SUPPORT, UNLESS YOU DON'T EVER WANT IT
        cmp #PACKEDMIN  ;CHECK PACKED NOP, STORE 0 IN CURNOT,X FOR IT AND SET PACK-COUNTER!
        bcc NOPACKR     ;IF A < PACKEDMIN
        cmp #PACKEDMAX+1
        bcs NOPACKR     ;IF A > PACKEDMAX
        ;sec             ;A IS IN PACKED NOP RANGE
        sbc #PACKEDMIN-2-1 ;CHANGE RANGE $70..$77 TO $1..$8, additional -1 because of C=0
        sta PACKCNT,x   ;store repeater amount in X
PACKNOP dec PACKCNT,x
        lda #$00        ;PUT 'NOP' INTO ACCU IF PACKED REST IS EXECUTED
NOPACKR cmp #0          ;RE-TEST ACCU BIT 7
        .fi             ;END OF CHECKING PACKED NOP SUPPORT
        bmi gocolu2     ;IF NOTE ABOVE $80 GO TO CHECK NEXT COLUMN
        sta CURNOT,x
        bpl SETPPOS     ;IF NOTE BELOW $80, END OF ROW
gocolu2 and #$7F        ;ELIMINATE BIT7
        sta CURNOT,x
        iny             ;CHECK FOR INST-SELECTION/SMALL-FX
CHINSTC lda (PLAYERZP),y ;READ CURRENT INST/SMALL-FX
        bmi gocolu3     ;IF INST/FX BELOW $80, END OF ROW
        sta CURIFX,x    ;LATER SEPARATE INSTRUMENT ($1..$3F) FROM SMALL-FX ($3F..$7F)
        bpl SETPPOS

gocolu3 and #$7F        ;ELIMINATE BIT7
        sta CURIFX,x
        iny             ;CHECK FOR BIG-FX
CHFXCOL lda (PLAYERZP),y ;READ CURRENT SMALL-/BIGFX
        sta CURFX2,x
        and #$E0
        bne SETPPOS     ;IF ABOVE $1F (SMALLFX), END OF ROW
        iny             ;BIG-FX BRANCH
CHFXVAL lda (PLAYERZP),y ;READ CURRENT EFX-VALUE (BIG-FX)
        sta CURVAL,x
SETPPOS tya
        sta PTNPOS,x

.if (SFX_SUPPORT!=0)
        cpx #2*7 ;is it channel 3?
        bne +
        lda SFXtimer
        bmi +
supress lda #0          ;suppress subsequent pattern-effects on channel3 while SFX is played
        sta CURFX2+2*7
        sta CURNOT+2*7  ;suppress note-triggering & hard-restart during SFX-playback
InsUpdt ldy CURIFX+2*7
        beq +
        cpy #$3f ;valid instrument-selection (not ins.FX?)
        bcs setAIFX
        sty CURINStemp
setAIFX sta CURIFX+2*7
        jmp HRENDER
+
.fi
        .if feature.FASTSPEEDBIND_ON
CHKTMP1 ldy TMPPOS,x    ;CHECK IF TEMPO IS NOT BIGGER THAN 1
p_tmpt2 lda TEMPOTBL,y
        and #$7F
        cmp #2
        bpl +
        jmp PTN_SEQ     ;IF TEMPO IS NOT BIGGER THAN 1, DON'T DO HR2 BUT HANDLE PATTERN & SEQUENCE
+
        .fi
        .if feature.HARDRESTYPES_ON
        lda #2          ;SIGN TICK2 TO HARDRESTART-ROUTINE
        .fi
HARDRST ldy CURNOT,x    ;CHECK FOR 2 FRAME LONG HARD-RESTART ;CHECK IF NOTE COMES AT ALL
        beq NONEWNO     ;IF NO NEW NOTE STARTING, NO HARD-RESTART PERFORMED
        cpy #$60        ;CHECK IF NOTE-FX (BIGGER THAN $60)
        bcc NEWNOTE     ;IF NOTE-FX, DON-T PERFORM HARDRESTART
NONEWNO jmp CNTPLAY     ;NO NEED TO PREPARE NEXT NOTE HERE, SIMPLY PLAY CURRENT NOTE

NEWNOTE ldy CURFX2,x    ;CURRENT BIG EFFECT
        cpy #$03        ;CHECK IF PORTAMENTO BIG-FX IS BEING EXECUTED
        beq NONEWNO
        .if feature.PORTAME_NOTEFX_ON
        ldy SLIDEVIB,x  ;was there portamento note-FX? ($FF)
        iny 
        beq NONEWNO
        .fi
        ldy CURIFX,x    ;THERE IS A MUSICAL NOTE, EXAMINE INSTRUMENT-COLUMN
        .if feature.HARDRESTYPES_ON
        beq prevIns     ;IF NOP ($00), GO TO USE PREVIOUSLY SELECTED INSTRUMENT FOR HARD RESTART
        .fi
        cpy #$3F        ;HARDWIRED VALUE OF LEGATO, CARRY BIT AFFECTED
        beq NONEWNO     ;IF LEGATO, NO HARD-RESTART PERFORMED
        .if feature.HARDRESTYPES_ON
        bmi wasinst     ;IF INSTRUMENT SELECTION (SMALLER THAN $3F)
prevIns ldy CURINS,x    ;IF NOP OR INST.FX, USE PREVIOUSLY SELECTED INSTRUMENT
wasinst pha             ;STORE HR-TICK-TIMER-NUMBER
p_insl2 lda INSPTLO,y   ;Y IS NUMBER OF CURRENT INSTRUMENT
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        clc
        adc SWP_OFFSET+0
.fi
        sta PLAYERZP+0
p_insh2 lda INSPTHI,y
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        adc SWP_OFFSET+1
.fi
        sta PLAYERZP+1
        pla             ;RESTORE HR-TICK-TIMER-NUMBER
ISHARDR ldy #0          ;GET CONTROL BYTE OF UPCOMING INSTRUMENT
        and (PLAYERZP),y ;EXTRACT 2 FRAME HARD-RESTART SWITCH-BIT
        beq HRENDER     ;BRANCH IF 2 FRAME LONG HARDRESTART, ELSE DON'T PERFORM HR YET, BUT SKIP PW/FILT
        .fi
HRGTOFF lda #$FE
        sta PTNGATE,x   ;WF-ARP TABLE MUTING
        and WFGHOST,x
        sta WFGHOST,x
        .if feature.HARDRESTYPES_ON
        ldy #2
        lda (PLAYERZP),y ;GET HR-SR
        sta SIDG.SR,x
        dey             ;Y=1
        lda (PLAYERZP),y ;GET HR-AD
        sta SIDG.AD,x
        dey             ;Y=0
        lda (PLAYERZP),y ;Y=0 HERE
        and #4          ;EXTRACT GATE-OFF HARD-RESTART SWITCH
        beq HRENDER     ;Attack=0,Decay=0 for staccato playing
        lda #$18        ;mute the sound strongly with TEST-bit, but keep oscillator
        sta WFGHOST,x
        sta SIDG.WAVE,x
        rts
        .else           ;IF ONLY DEFAULT HARDRESTARTVALUE SELECTED
defauHR lda #>DEFAULTHRADSR ;$0F?
        sta SIDG.AD,x
        lda #<DEFAULTHRADSR ;$F0?
        sta SIDG.SR,x
        .fi             ;END OF CHECKING IF HARDRESTART-TYPE AND HR-ADSR IS THE SAME FOR ALL INSTRUMENTS
HRENDER ;end of hard-restart
        ldy CURINS,x    ;PUT CURRENT INSTRUMENT'S BASE-ADDRESS TO ZEROPAGE-POINTERS
p_insl3 lda INSPTLO,y   ;Y IS NUMBER OF CURRENT INSTRUMENT
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        clc
        adc SWP_OFFSET+0
.fi
        sta PLAYERZP+0
p_insh3 lda INSPTHI,y
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        adc SWP_OFFSET+1
.fi
        sta PLAYERZP+1  ;this value shouldn't be 0 anytime
        .if feature.VIBSLIDEALWAYS_ON ;if still want slide/vibrato to go, but no point for that
        jmp CNTPLY2
        .elsif feature.FILTERALWAYS_ON
        jmp FILTPRG
        .elsif feature.PULSEALWAYS_ON
        jmp SETPWID
        .else
        jmp WFARPTB     ;CNTPLAY2 ;SKIP PULSEWIDTH-FILTER-TABLES AND VIBRATO IF NEW NOTE COMES BUT NO HR
        .fi


;-----------------------------------------------------------
CHKTCK1 cmp #2
        bpl CHKTCK2     ;CHECK FOR TICK 1 (BRANCH IF SPDCNT>1)
TICK_1  ldy CURPTN,x    ;PUT CURRENT PATTERN'S BASE-ADDRESS TO ZEROPAGE-POINTERS
p_ptnl2 lda PPTRLO,y    ;Y IS NUMBER OF CURRENT PATTERN
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        clc
        adc SWP_OFFSET+0
.fi
        sta PLAYERZP+0
p_ptnh2 lda PPTRHI,y
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        adc SWP_OFFSET+1
.fi
        sta PLAYERZP+1
PTN_SEQ ldy PTNPOS,x    ;FETCH PATTERN-POSITION TO Y
        .if feature.TRANSPOSESUPP_ON
        lda TRANSP2,x   ;PERFORM TRANSPOSE-SEQ.FX
        sta TRANSP,x    ;DELAYER TO COMPENSATE EARLY SEQ.FX
        .fi

        .if PACKEDNOPSUPP_ON ;NOT RECOMMENDED TO SWITCH OFF SUPPORT, UNLESS YOU DON'T EVER WANT IT
        lda PACKCNT,x   ;HANDLE PACKED NOP (IF COUNTING, DON'T ICREMENT PTNPOS,X)
        bne SETPPO2     ;IF PACKED NOP COUNTER IS NONZERO, STAY IN PLACE
        .fi
        iny
        lda (PLAYERZP),y ;READ NEXT ROW 1ST BYTE, TO CHECK PATTERN-END
        cmp #$FF        ;HARDWIRED PTTERN-DELIMITER (PATTERN-END)
        bne SETPPO2     ;IF NOT END OF PATTERN, JUST STORE PTNCNT
ptncode .byte $BC, <SEQPOS, >SEQPOS ;ldy SEQPOS,x ;editor manipulates this opcode & operand to accomplish pattern-playing (JMP PTNPLAY)
SEQFXLP iny             ;RETURN POINT FOR CONTINUOUS SEQUENCE-EFFECT CHECKING
SEQFXL2 jsr SEQSUB      ;READ NEXT DATA FROM SEQ (ORDERLIST)
        bpl SETPNUM     ;IF BELOW $80, JUST GO SETTING SEQPOS & PATTERN-NUMBER NORMALLY
        cmp #$FE        ;HARDWIRED SEQUENCE-DELIMITER (END)
        bne chkloop
        dec SPDCNT,x    ;KEEP IT AWAY FROM COUNTING-UP
        jmp CNTPLAY     ;CYCLECH ;IF $FE THEN END OF PLAYING (COUNTER STOPS AT '1' FOR TRACK)

chkloop cmp #$FF        ;HARDWIRED SEQUENCE-DELIMITER (JUMP/LOOP)
        .if feature.SEQ_FX_SUPPORT_ON
        beq LOOPSEQ     ;IF NO FF AND ABOVE &80, DO SEQUENCE-FX (TRANSPOSE/REPEAT/ETC.)
        jsr SEQ_FX      ;ACCU ABOVE $80 & BELOW $FE: HANDLE ORDERLIST/SEQUENCE-FX
        iny
        jsr SEQSUB      ;CHECK NEXT VALUE IN SEQUENCE
        cmp #$FF        ;IF $FF-COMMAND DIRECTLY AFTER SEQ-FX, DON'T ALLOW LOOPING (SAFETY CHECK TO PREVENT FREEZE)
        beq PTNPLAY     ;SETSPOS ;IF ILLEGAL SEQFX-$FF SEQUENCE FOUND, SKIP LOOPING
        bne SEQFXL2     ;GO BACK TO CHECK IF DELIMITER/SEQ-FX AT NEXT POSITION
        .else
        bne PTNPLAY
        .fi
LOOPSEQ 
        .if COMPILEDAPP==1
        cpy #0          ;if $FF-loopsignal at position 0 we should prevent looping that causes freeze
        beq PTNPLAY
        .fi
        iny             ;LOOP SEQUENCE TO GIVEN POSITION (AFTER $FF DELIMITER)
        jsr SEQSUB      ;READ SEQ JUMP-ADDRESS (NEXT DATA IN ORDERLIST)
        bpl lpindex     ;IF SIMPLE JUMP-ADDRESS (BELOW $80)
        .if (feature.SUBTUNESUPPORT_ON && feature.SUBTUNEJUMP_ON)
        jsr SETSEQA     ;set Sequence-pointer for the track ;sta SUBTJMP+1 ;CAUSE SUBTUNE-JUMPING IN PLAYER'S MAIN PART
        .fi
        lda #0
lpindex tay
        jmp SEQFXL2     ;AND ALSO READ THIS POSITION'S PTN-NUMBER/SEQ.FX

SETPNUM sta CURPTN,x    ;SET PATTERN-NUMBER TO ACCU
SETSPOS tya
        sta SEQPOS,x    ;AND SET SEQ-POS TO
PTNPLAY ldy #0
SETPPO2 tya
        sta PTNPOS,x
        .if feature.FASTSPEEDBIND_ON
CHKTMP2 ldy TMPPOS,x    ;CHECK IF TEMPO IS NOT BIGGER THAN 2
p_tmpt3 lda TEMPOTBL,y
        and #$7F
        cmp #3
        bmi TICK_2      ;IF TEMPO IS NOT BIGGER THAN 2, DON'T DO HR1 BUT HANDLE NOTE/INS/FX
        .fi
        .if feature.HARDRESTYPES_ON
HRDR1FR lda #1          ;STORE HR-TICK-TIMER-NUMBER
        .fi
        jmp HARDRST     ;JUMP TO HARD-RESTART CHECKER & PERFORMER ROUTINE


;--------------------------------------------------------------------------------------
CHKTCK2 beq TICK_2      ;COMPARE SPDCNT,X TO '2'
        jmp CNTPLAY     ;HERE SPDCNT<>2, REST OF THE TICKS (ONLY CNTPLAY)
TICK_2
        .if (feature.PWRESETSW_ON + feature.FILTRESETSW_ON)
        ldy #%11111111  ;SIGN LATER IF SELECTION OF INSTRUMENT HAPPENED OR NOT
        .fi
        lda CURIFX,x    ;CHECK INST/FX COLUMN
        beq CHKNOTE     ;IF NOP, NOTHING TO DO
        cmp #maxinstamount ;CHECK IF NORMAL INSTRUMENT (BELOW $3F)
        bpl CHKNOTE     ;IF Y > MAX.INST.AMOUNT, SKIP INST.SELECTION
SEL_INS sta CURINS,x    ;SELECT INSTRUMENT IF Y IS IN RANGE 1..MAX.INST
        .if (feature.PWRESETSW_ON + feature.FILTRESETSW_ON)
        ldy #%00111111  ;CAUSE FILTER&PW-TABLE TO RESET AT INSTRUMENT-SELECTION
CHKNOTE sty TABLRST+1   ;SIGN FOR PW/CTF RESETTER IF INSTRUMENT WAS JUST SELECTED
        .else
CHKNOTE                 ;
        .fi

        ldy CURINS,x    ;PUT CURRENT INSTRUMENT'S BASE-ADDRESS TO ZEROPAGE-POINTERS
        bne STRTINS     ;ONLY PERFORM REST OF TICK2 IF ANY INSTRUMENT IS SELECTED, SAVE RASTERTIME
        jmp LEGATOO
STRTINS                 ;ENTRY POINT USED BY PLAYADAPTER TO SET INSTRUMENT
p_insl4 lda INSPTLO,y   ;Y IS NUMBER OF CURRENT INSTRUMENT
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        clc
        adc SWP_OFFSET+0
.fi
        sta PLAYERZP+0
p_insh4 lda INSPTHI,y
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        adc SWP_OFFSET+1
.fi
        sta PLAYERZP+1

COLUMN1 lda CURNOT,x    ;CHECK NOTE COLUMN
        bne chkinst     ;IF NOP, NO NOTE-FX, CHECK FOR INSTRUMENT-/PATTERN-FX
        jsr INSPTFX     ;INST/PATTERNFX. (nothing (NOP) was in note-column)
        jmp CNTPLY2     ;SKIP POINTER-SETTING (WAS SET IN THIS TICK)

chkinst cmp #$60        ;CHECK IF NORMAL NOTE (BELOW $60)
        bmi CLEGATO     ;IF A < $60 CHECK FOR LEGATO, IF A >= $60 DO NOTE-FX THEN INS/PTN-FX
        jsr NOTE_FX     ;HANDLE NOTE/INS./PATTERN-EFFECTS, INPUT: ACCU
        jmp CNTPLY2

CLEGATO clc             ;SETTING NOTE-PITCH FIRST BASED ON CURNOT,X LOADED TO ACCU PREVIOUSLY
        .if feature.OCTAVESHIFT_ON
        ldy #9          ;INDEX OF OCTAVE-SHIFT OF INSTRUMENT
        adc (PLAYERZP),y
        .fi
        .if feature.TRANSPOSESUPP_ON
        clc
        adc TRANSP,x    ;ADD 2'S COMPLEMENT TRANSPOSE-VALUE (SET BY SEQUENCE-FX)
        .fi
        sta DPITCH,x    ;DISCRETE PITCH (0..$5F)
.if (COMPILEDAPP==1 && MIDI_support!=0)
        sta playadapter.discretePitch,x ;help editor to calculate correct slide-pitches
.fi
.if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
        sta ARPDPITCH,x ;help slowdown-support
.fi

        lda #$03        ;PORTAMENTO-BIGFX-VALUE?
        cmp CURFX2,x    ;CHECK IF PORTAMENTO-EFFECT IN THE ROW
        beq LEGATOO     ;IF PORTAMENTO, NO INITIALIZATION OF SOUND NEEDED
        .if feature.PORTAME_NOTEFX_ON
        adc SLIDEVIB,x  ;CHECK IF NOTE-FX PORTAMENTO ($FF) WAS INITIATED BEFOREHANDS
        bcs SETPRTA     ;IF YES, DO TONE-PORTAMENTO (SPEED ALREADY CALCULATED BY PORTAMENTO NOTE-FX)
        .fi
CHILEGA lda #$3F        ;CHECK IF CURIFX WAS $3F "LEGATO" ;IF $3F, TIED NOTE (LEGATO/HAMMER-ON), SKIP SOUND-INIT
        cmp CURIFX,x    ;CHECK CURIFX (WITHOUT BIT 7), GET LEGATO INFO
        bne STRTSND     ;IF NO LEGATO AND NO PORTAMENTO, NORMAL SOUND-START
SETLEGA lda #$7F        ;BIG SPEED FOR PORTAMENTO IS EQUAL TO LEGATO
        sta FREQMODH,x  ;MAXIMAL PORTAMENTO-SLIDE
SETPRTA lda #$83        ;HANDLE NOTE-PITCH SETTING FOR LEGATO!!!
        sta SLIDEVIB,x  ;SET PORTAMENTO
LEGATOO jsr INSPTFX     ;INST/PATTERNFX. (WASN'T FX in note-column)
        jmp WRWFGHO     ;WRITE 'WAVEFORM-CONTROL' SID-REGISTER ONLY (just in case if editor stopped at SPDCNT=2 phase, muting)

STRTSND                 ;STARTING THE SOUND - INIT ADSR, WAVEFORM-ARP, PULSEPROGRAM, FILTERPROGRAM, ETC.

        ldy #0          ;INIT INSTRUMENT COUNTERS/POINTERS
        ;READ CONTROL-BYTE OF INSTRUMENT FROM INDEX 0
        lda (PLAYERZP),y ;(bit0-1:HRtimer,bit2:HRgateoff,bit3:TestbitHR,bit4-5:vib.type,bit6:pulseresetOFF,bit7-filtresetOff)
        .if (feature.PWRESETSW_ON + feature.FILTRESETSW_ON)
TABLRST and #$3F        ;%00111111 ;SELF-WRITTEN - IF INSTRUMENT SELECTED, CAUSES TABLES TO RESET
        .fi
        sta INSCTRL+1   ;STORE IT FOR LATER USES
        and #$30
        sta SLIDEVIB,x  ;VIBRATO-CONTROL PART OF INSTRUMENT-CONTROL REGISTER

;--------upcoming parts can be arranged to provide good timing for the ADSR/WAVEFORM note-starter
;after so many trials it seems ADSR setting should be either very close or very far from WAVEFORM-setting 
;to produce reliable sound (otherwise, near treshold, some ADSR settings sound very unreliable on 1st frame)

INSCTRL lda #selfmod    ;INSTRUMENT-CONTROL BYTE STORED
        .if feature.FRAME1SWITCH_ON ;CHECK IF WAVEFORM1 SWITCH DISABLED
        and #8          ;1ST WAVEFORM-SETTING (SEXY TESTBIT-START IF ON)
        beq SETWFTP     ;IF ZERO, DON'T INIT WAVEFORM-REGISTER
        ldy DPITCH,x
        lda FREQTBH,y
.if (COMPILEDAPP==1 && MIDI_support!=0)
        clc
        adc playadapter.pitchShiftHi,x ;calculated by editor
.fi
        sta SIDG.FREQ+1,x
SETWFR1 ldy #$0F        ;POSITION OF 1ST FRAME WAVEFORM IN INSTRUMENT ($09 BY DEFAULT)
        lda (PLAYERZP),y ;SET TEST-BIT (ORA #8) FOR 1ST FRAME ;CADAVER SAYS THAT $01 IS BETTER THAN $09/$89 AS 1ST FRAME WAVEFORM FOR NOISE
        .else           ;IF SEXY-HARDRESTART SWITCH DISABLED
        lda #$09        ;SIMPLER, ALWAYS 9 AT FRAME 1
        .fi
        sta WFGHOST,x   ;1ST FRAME WAVEFORM
SETWFTP lda #WFTABLEPOS ;RESET WFTABLE COUNTER - can be overridden from Big-FX 4
        sta WFTPOS,x

        lda #$FF
        sta PTNGATE,x
        .if feature.MULTISPEEDSUPP_ON
        sta ARPSCNT,x   ;used for multispeed to determine note 1st frame
        .fi
        .if feature.ARPSPEEDSUPP_ON
        ldy #7          ;ARPSPEED OF INSTRUMENT
        lda (PLAYERZP),y
        sta ARPSPED,x
        .fi

INSTVIB jsr SETVIB0     ;SET VIBRATO-AMPLITUDE AND FREQUENCY AND FREQ.COUNTER

        .if feature.CHORDSUPPORT_ON
SETCHDP ldy #8          ;DEFAULT CHORD FOR INSTRUMENT
        lda (PLAYERZP),y
        sta CURCHORD,x
        tay
p_chdp1 lda CHDPTRLO,y  ;GET CHORD-BASEPOINTER
        sta CHORDPOS,x  ;SET BASE-INDEX OF CHORD IN CHORDTABLE
        .fi

        .if feature.PWRESETSW_ON
        bit INSCTRL+1   ;CHECK BIT6 - IF PW-RESET IS OFF FOR INSTRUMENT AND NO INST.SELECTION / MODIFICATION
        bvs ENDPWRESET  ;BIT6 - PW RESET SWITCH
        .fi
        ldy #$0A        ;RESET PWTABLE COUNTER
        lda (PLAYERZP),y
        sta PWTPOS,x
ENDPWRESET

        .if feature.FILTRESETSW_ON
        bit INSCTRL+1   ;INST.CONTROL BYTE
         .if (*+2+$7F>=ENDFILRESET)
         bmi ENDFILRESET ;CHECK BIT7 - IF FILTER-RESET IS OFF FOR INSTRUMENT AND NO INST.SELECTION /MODIFICATION
         .else
         bpl +
         jmp ENDFILRESET
+        .fi
        .fi
        .if (COMPILEDAPP==1 && FILTER_MUTESOLO==1) ;was advised at CSDB (by Leticia)
        ldy playadapter.playmod
        beq SETFLTP     ;in stopped (jamming) mode no need to use filter-mute/solo feature
        ldy playadapter.div7chn,x ;mute filter-program as well to avoid clicks on 6581 SID's muted channels
        lda mutesolo,y  ;$FE or $FF
        .if (SID_AMOUNT==1)
         cmp #$FE
         beq SWOFFLT
        .elsif (SID_AMOUNT>=2)
         cmp #$FE
         bne SETFLTP
         .if (SID_AMOUNT>=3)
          cpx #2*3*7
          bcc +
          bcs SWOFFL3
         .fi
+        cpx #3*7
         bcs SWOFFL2
         bcc SWOFFLT ;jump
        .fi
        .fi
SETFLTP ldy #$0B        ;RESET FILTER-TABLE COUNTER
        lda (PLAYERZP),y
        tay
        .if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
         cpx #2*3*7
         bcc +
         lda (PLAYERZP),y ;READ FROM 1ST ROW OF FILTERTABLE
         beq SWONFL3     ;IF $00 IN 1ST ROW, INSTRUMENT IS FILTERED BUT DOESN'T CONTROL FILTER
         cmp #$FF        ;IF $FF, INSTRUMENT NOT FILTERED, SWITCHING FILTERSWITCH ON/OFF
         beq STOPFL3
         stx flprog3.FLTCTRL+1
         sty flprog3.FLTPOSI+1
SWONFL3  lda FLSWTBL,x
         ora FSWITC3+1
         bne wriFsw3     ;JUMP
STOPFL3  cpx flprog3.FLTCTRL+1
         bne SWOFFL3     ;if this instrument controlled filter beforehand, stop its filter execution by pointing to $FF
         sty flprog3.FLTPOSI+1
SWOFFL3  lda FLSWTB2,x
         and FSWITC3+1
wriFsw3  sta FSWITC3+1
         jmp ENDFILRESET  
         .fi
+       cpx #3*7
        bcc +
        lda (PLAYERZP),y ;READ FROM 1ST ROW OF FILTERTABLE
        beq SWONFL2     ;IF $00 IN 1ST ROW, INSTRUMENT IS FILTERED BUT DOESN'T CONTROL FILTER
        cmp #$FF        ;IF $FF, INSTRUMENT NOT FILTERED, SWITCHING FILTERSWITCH ON/OFF
        beq STOPFL2
        stx flprog2.FLTCTRL+1
        sty flprog2.FLTPOSI+1
SWONFL2 lda FLSWTBL,x
        ora FSWITC2+1
        bne wriFsw2     ;JUMP
STOPFL2 cpx flprog2.FLTCTRL+1
        bne SWOFFL2     ;if this instrument controlled filter beforehand, stop its filter execution by pointing to $FF
        sty flprog2.FLTPOSI+1
SWOFFL2 lda FLSWTB2,x
        and FSWITC2+1
wriFsw2 sta FSWITC2+1
        jmp ENDFILRESET  
        .fi
+       lda (PLAYERZP),y ;READ FROM 1ST ROW OF FILTERTABLE
        beq SWONFLT     ;IF $00 IN 1ST ROW, INSTRUMENT IS FILTERED BUT DOESN'T CONTROL FILTER
        cmp #$FF        ;IF $FF, INSTRUMENT NOT FILTERED, SWITCHING FILTERSWITCH ON/OFF
        beq STOPFLT
        stx flprog1.FLTCTRL+1   ;MAKE CURRENT INSTRUMENT THE FILTER-CONTROLLER FOR SID
        sty flprog1.FLTPOSI+1
SWONFLT lda FLSWTBL,x
        ora FSWITCH+1   ;SWITCH ON SID-FILTER FOR INSTRUMENT (CHANNEL) - 'OR' BIT IN SWITCH-BIT TABLE
        bne wriFswi     ;JUMP
STOPFLT cpx flprog1.FLTCTRL+1
        bne SWOFFLT     ;if this instrument controlled filter beforehand, stop its filter execution by pointing to $FF
        sty flprog1.FLTPOSI+1
SWOFFLT lda FLSWTB2,x
        and FSWITCH+1   ;SWITCH OFF SID-FILTER FOR INSTRUMENT (CHANNEL) - 'XOR' BIT IN SWITCH-BIT TABLE
wriFswi sta FSWITCH+1
ENDFILRESET

SETADSR ldy #4          ;SR
        lda (PLAYERZP),y ;READ 'SUSTAIN & RELEASE'
        .if (COMPILEDAPP!=2 || SLOWDOWN_SUPP==0)
        sta SIDG.SR,x
        dey             ;3 ;AD
        lda (PLAYERZP),y ;READ 'ATTACK & DECAY'
        .else
        pha
        and #$0f        ;Release
        tay
        lda ADSR_OFFS,y
SLOWDN4 adc #selfmod
        tay
        pla
        adc ADSR_EXPTB,y
        sta SIDG.SR,x
        ldy #3          ;AD
        lda (PLAYERZP),y
        sta slAstor+1
        lsr
        lsr
        lsr
        lsr
        tay             ;Attack
        lda ADSR_OFFS,y
        clc
SLOWDN5 adc #selfmod
        tay
        lda ADSR_EXPT2,y
        and #$f0
slAstor adc #selfmod
        pha
        and #$0f        ;Decay
        tay
        lda ADSR_OFFS,y
SLOWDN6 adc #selfmod
        tay
        pla
        adc ADSR_EXPTB,y
        .fi
        sta SIDG.AD,x

        jsr INSPTFX     ;INST/PATTERNFX. (WASN'T FX in note-column)
        jmp WRWFGHO     ;WRITE SID-REGISTER (WAVEFORM-CONTROL)


;------------------------------------------------------------------------
CNTPLAY ldy CURINS,x    ;PUT CURRENT INSTRUMENT'S BASE-ADDRESS TO ZEROPAGE-POINTERS
        bne CNTPLY1     ;ONLY PERFORM CNTPLAY IF THERE'S ANY INSTRUMENT SELECTED
        rts             ;JMP RETRACK
CNTPLY1                 ;
p_insl5 lda INSPTLO,y   ;Y IS NUMBER OF CURRENT INSTRUMENT
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        clc
        adc SWP_OFFSET+0
.fi
        sta PLAYERZP+0
p_insh5 lda INSPTHI,y
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        adc SWP_OFFSET+1
.fi
        sta PLAYERZP+1
CNTPLY2                 ;CONTINUOUS PLAY - SLIDE,VIBRATO,SWEEP,TABLES,ETC.


VIBSLIDE                ;VIBRATO AND SLIDE/PORTAMENTO PATTERN-FX AS WELL (A BIT TRICKY)
        ldy SLIDEVIB,x  ;READ SLIDE/PORTAMENTO/VIBRATO TYPE FROM INST.CONTROL BYTE
        .if feature.VIBRATOTYPES_ON
        beq INCVIBR     ;INCREASING TYPE OF VIBRATO?
        .fi
        bpl NORMVIB     ;IF BELOW $80, IT'S NORMAL VIBRATO
SLIDES  cpy #$82        ;DECIDE WHICH KIND OF SLIDE - SETTING CARRY-FLAG AND N-FLAG
        beq SUBFREQ     ;IF $82
        bpl +
        jmp ADDFREQ     ;IF $81
+
        .if feature.PORTAVIBRA_ON==1
        cpy #$FF        ;portamento Note-FX?
        beq TARGETN     ;then just continue vibrato yet (for this row)
        .fi
PORTAME .if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
         ldy ARPDPITCH,x
        .else 
         ldy DPITCH,x    ;HANDLE PORTAMENTO
        .fi
        lda FREQTBL,y   ;COMPARE TO DESIRED NOTE AFTER PORTAMENTO
        sbc FREQLO,x    ;SET CARRY
        sta STORFRL+1   ;STORE DIFFERENCE LO-BYTE
        lda FREQTBH,y   ;HI-BYTE OF DESIRED NOTE-PITCH
        sbc FREQHI,x    ;COMPARE WITH CARRY
        tay             ;STORE DIFFERENCE HI-BYTE
STORFRL lda #selfmod    ;STORED LO-BYTE OF DIFFERENCE
        bcs PORTAUP     ;IF DESIRED PITCH IS BIGGER THAN CURRENT PITCH, DO PORTAMENTO-UP
PORTADN adc FREQMODL,x  ;PORTAMENTO-DOWN
        tya             ;RESTORE HI-BYTE OF DIFFERENCE
        adc FREQMODH,x  ;TEST HI-BYTE
        bcc SUBFREQ     ;IF DESIRED NOTE IS NOT REACHED YET
PORTEND ;player jumped here when the target-note frequency was reached for portamento
        .if feature.PORTAVIBRA_ON==1
        jsr FORCVIB     ;reset SLIDEVIB for instrument
        jsr SETVIB1     ;reset vibrato of instrument after slide
        .fi
TARGETN .if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
         ldy ARPDPITCH,x
        .else
         ldy DPITCH,x    ;HANDLE PORTAMENTO
        .fi
        lda FREQTBL,y   ;LO-BYTE OF DESIRED PITCH
        sta FREQLO,x    ;DESIRED NOTE WAS REACHED, END OF PORTAMENTO, SET EXACT FREQUENCY
        lda FREQTBH,y   ;HI-BYTE OF DESIRED PITCH
        jmp STORVHI

NORMVIB ldy VIDELCNT,x  ;HANDLE VIBRATO-DELAY IN CASE OF NORMAL VIBRATOS
        bmi DOVIBRA     ;IF DELAY-COUNTER FINISHED
        dec VIDELCNT,x
        jmp ENDVIBSLIDE

        .if feature.VIBRATOTYPES_ON
INCVIBR lda FREQMODL,x
        clc             ;LDA FREQMODL,X ;WAS LOADED PREVIOUSLY INTO ACCU
        adc VIDELCNT,x  ;AMPLITUDE-INCREASE RATIO IS TAKEN FROM VIBRATO-DELAY DATA
        sta FREQMODL,x
        bcc DOVIBRA
        inc FREQMODH,x
        .fi
DOVIBRA lda VIBRACNT,x
        bne decVcnt     ;IF NOT ZERO (COUNTED DOWN) SIMPLY SUBSTRACT 1 (OR NOTHING, IF SLIDE)
        lda VIBFREQU,x
decVcnt sec
        sbc #1
        sta VIBRACNT,x
        asl             ;IF HALF OF COUNTER REACHED, CHANGE VIBRATO-SLIDE-DIRECTION
        cmp VIBFREQU,x  ;SET CARRY BIT BASED ON WHICH HALF THE FREQUENCY COUNTER IS IN
        bcc ADDFREQ     ;CARRY IS SET HERE CORRECTLY, NO NEED TO INITIALIZE IN NEXT STEP
SUBFREQ lda FREQLO,x
        sbc FREQMODL,x  ;SUBSTRACT LOW-BYTE PART, SET CARRY-BIT
        sta FREQLO,x
        lda FREQHI,x
        sbc FREQMODH,x  ;SUBSTRACT HIGH-BYTE PART AND CARRY-BIT
        jmp STORVHI

PORTAUP sbc FREQMODL,x  ;PORTAMENTO-UP
        tya             ;RESTORE HI-BYTE OF DIFFERENCE
        sbc FREQMODH,x  ;TEST HI-BYTE
        bcc PORTEND     ;IF DESIRED NOTE IS REACHED, END OF PORTAMENTO, SET EXACT FREQUENCY
ADDFREQ lda FREQLO,x    ;PRE-FETCH DATA
        adc FREQMODL,x  ;ADD LOW-BYTE PART, SET CARRY-BIT
        sta FREQLO,x
        lda FREQHI,x
        adc FREQMODH,x  ;ADD HIGH-BYTE PART AND CARRY-BIT
STORVHI sta FREQHI,x
ENDVIBSLIDE

;FILTER-RESO-CUTOFF-BAND HANDLING --- DETECTS FILTER-CONTROLLER CHANNEL!!!!
FilterProgram .macro
SID_ID=\1
.if (SID_ID==1)
CTFL_GHO=CTFLGHO
CTFH_GHO=CTFHGHO
FLT_BAND=FLTBAND
RESO_NIB=RESONIB
F_SWITCH=FSWITCH
C_KBDTRK=CKBDTRK
.elsif (SID_ID==2)
CTFL_GHO=CTFLGH2
CTFH_GHO=CTFHGH2
FLT_BAND=FLTBAN2
RESO_NIB=RESONI2
F_SWITCH=FSWITC2
C_KBDTRK=CKBDTR2
.elsif (SID_ID==3)
CTFL_GHO=CTFLGH3
CTFH_GHO=CTFHGH3
FLT_BAND=FLTBAN3
RESO_NIB=RESONI3
F_SWITCH=FSWITC3
C_KBDTRK=CKBDTR3
.fi      
FLTCTRL cpx #selfmod    ;CHECK WHICH CHANNEL CONTROLS THE FILTER
        bne SwUpEnd     ;IF NOT CURRENT TRACK, DONT'T RUN ITS FILTER-TABLE
FLTPOSI ldy #selfmod    ;FILTERTABLE-POSITION OF CURRENT FILTER-CONTROLLER TRACK
        lda (PLAYERZP),y ;FILTERTABLE 1ST COLUMN BAND-RESO. / FILTERSWEEP-CYCLECOUNT
        bmi NOCWEEP     ;IF FILTER-SETTING OR JUMP IN CURRENT POSITION
FISWEEP iny             ;POINT TO 2ND COLUMN
CWEPCNT cmp #selfmod    ;FILTER CUTOFF-FREQUENCY SWEEP COUNTER
        beq FLADVAN     ;IF SWEEP-PROGRAM REACHED ITS END, ADVANCE TO NEXT TABLE-ROW
        inc CWEPCNT+1
        clc             ;CARRY=0 - TREAT $FF AS -1 IN UPCOMING FINE CUTOFF-SWEEP
        lda (PLAYERZP),y ;CUTOFF SIGNED AMOUNT TO ADD/SUBSTRACT IN ONE FRAME
        .if feature.FINEFILTSWEEP_ON
        bpl CWEEPUP     ;IF POSITIVE (BIT7=0) ADD VALUE, OTHERWSE SUBSTRACT
CWEEPDN ora #$F8        ;SART SUBSTRACTING 7-BIT VALUE FROM 8+3 BIT VALUE
        adc CTFL_GHO+1
        php             ;STORE CARRY-BIT
        and #7
        sta CTFL_GHO+1
        lda (PLAYERZP),y ;CUTOFF SIGNED AMOUNT TO ADD/SUBSTRACT IN ONE FRAME
        eor #$FF
        lsr
        lsr
        lsr
        eor #$FF
        jmp strFswp

CWEEPUP and #7          ;SART ADDING 7-BIT VALUE TO 8+3 BIT VALUE
        adc CTFL_GHO+1  ;CLC NOT NEEDED ;CARRY-BIT WAS SET BEFORE
        cmp #8          ;SETS CARRY IF LOW 3 BITS OVERLOADED
        php             ;STORE CARRY
        and #7
        sta CTFL_GHO+1
        lda (PLAYERZP),y ;CUTOFF SIGNED AMOUNT TO ADD/SUBSTRACT IN ONE FRAME
        lsr
        lsr
        lsr
strFswp plp             ;GET CARRY BACK
        .fi             ;END OF FINE (11BIT) FILTERSWEEP-CHECKING
        adc CTFH_GHO+1
        sta CTFH_GHO+1
SwUpEnd jmp ENDFLTB

NOCWEEP cmp #$FE        ;....LOOP/JUMP COMMAND ?
        beq FLTJUMP     ;CHECK IF $FE JUMP-COMMAND
        bcc SETFILT     ;IF $80 >= BUT < $FE, SET FILTER
        bcs ENDFLTB     ;IF $FF FILTERTABLE-ENDSIGNAL, DO NOTHING

FLTJUMP iny             ;POINT TO JUMP-ADDRESS IN 2ND COLUMN
        lda (PLAYERZP),y ;JUMP-ADDRESS
        cmp FLTPOSI+1
        beq ENDFLTB     ;IF JUMPING TO ITSELF, NO NEED TO EXECUTE ANY JUMPING, BUT RESTING INSTEAD
        tay
        lda (PLAYERZP),y ;READ 1ST COLUMN FROM JUMP-TARGET ADDRESS
        bpl SETFPOS     ;IF BELOW $80, JUST INIT FOR SWEEP COMING IN NEXT TURN
SETFILT pha             ;SET FILTER, IF 1ST NIBBLE IS MORE THAN 8, AND NOT $FE/$FF
        and #$70        ;MASK FOR FILTERBAND-SWITCHES
        sta FLT_BAND+1        ;SET FILTER-BAND
        pla
        asl             ;GET AND SHIFT RESONANCE NIBBLE - x8 FIRST
        asl
        asl
        asl             ;x16
        sta RESO_NIB+1        ;RESONANCE-NIBBLE
        iny             ;POINT TO 2ND COLUMN
        lda (PLAYERZP),y ;CUTOFF-HI VALUE
        sta CTFH_GHO+1  ;SET CUTOFF-HI..
        .if feature.FINEFILTSWEEP_ON
        lda #0
        sta CTFL_GHO+1  ;SET CUTOFF-LO..
        .fi
FLADVAN iny             ;POINT TO 3RD COLUMN
        .if feature.FILTKBTRACK_ON
        lda (PLAYERZP),y
        bpl wrKbTrk     ;A < $80?
        cmp #$90        ;A > $8F?
        bcs wrKbTrk
        and #$0f
        sta F_SWITCH+1
        lda #0
wrKbTrk sta C_KBDTRK+1        ;READ AND ACK. KEYBOARD-TRACK FOR FILTER
        .fi
        iny             ;POINT TO NEXT ROW 1ST COLUMN
SETFPOS sty FLTPOSI+1
        lda #0
        sta CWEPCNT+1   ;INITIALIZE SWEEP-COUNTER AFTER SETTING FILTER...
ENDFLTB
.endm

FILTPRG .if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
         cpx #2*3*7
         bcs flprog3
         jmp chFlPr2
flprog3 #FilterProgram 3
        jmp FILTPRG_END
         .fi
chFlPr2 cpx #3*7
        bcs flprog2
        jmp flprog1 
flprog2 #FilterProgram 2
        jmp FILTPRG_END
        .fi
flprog1 #FilterProgram 1
FILTPRG_END

;PULSEWIDTH-TABLE HANDLING
SETPWID ldy PWTPOS,x
        lda (PLAYERZP),y ;PWTABLE 1ST COLUMN PW-HI-NIBBLE / PW-SWEEP-CYCLECOUNT
        bmi NOPWEEP     ;IF PW-SETTING OR JUMP IN CURRENT POSITION
PWSWEEP iny             ;POINT TO 2ND COLUMN
        cmp PWEEPCNT,x  ;IF SWEEP-PROGRAM REACHED ITS END
        beq PWADVAN     ;THEN ADVANCE TO NEXT PWTABLE-ROW
        inc PWEEPCNT,x
        lda (PLAYERZP),y ;PW SIGNED AMOUNT TO ADD/SUBSTRACT IN ONE FRAME
        bpl positPW     ;DECIDE IF ADD/SUBSTRACT, PRE-DECREMENT HI-BYTE IS KINDA SUBSTRACTION
        dec PWHIGHO,x
positPW clc             ;NOT NECESSARY IF PRECISION NOT NEEDED, AS CARRY WAS SET TO 1 PREVIOUSLY BY 'CMP PWEEPCNT,X'
        adc PWLOGHO,x
        sta PWLOGHO,x
        bcc ENDPWTB
        inc PWHIGHO,x
        bcs ENDPWTB

NOPWEEP cmp #$FE        ;....LOOP/JUMP COMMAND ?
        beq PWTJUMP     ;CHECK IF $FE JUMP-COMMAND
        bcc SETPULW     ;IF $80 >= BUT < $FE, SET PULSEWIDTH
        bcs ENDPWTB     ;IF $FF PWTABLE-ENDSIGNAL, DO NOTHING

PWTJUMP iny             ;POINT TO JUMP-ADDRESS IN 2ND COLUMN
        lda (PLAYERZP),y ;JUMP-ADDRESS
        cmp PWTPOS,x
        beq ENDPWTB     ;IF JUMPING TO ITSELF, NO NEED TO EXECUTE ANY JUMPING, BUT RESTING INSTEAD
        tay
        lda (PLAYERZP),y ;READ 1ST COLUMN FROM JUMP-TARGET ADDRESS
        bpl SEPWPOS     ;IF BELOW $80, JUST INIT FOR SWEEP COMING IN NEXT TURN
SETPULW and #$7F        ;SET PULSEWIDTH, IF 1ST NIBBLE IS MORE THAN 8, AND NOT $FE/$FF
        sta PWHIGHO,x
        iny             ;POINT TO 2ND COLUMN
        lda (PLAYERZP),y ;PW-LOW VALUE
        sta PWLOGHO,x
PWADVAN iny             ;POINT TO 3RD COLUMN
        .if feature.PWKEYBTRACK_ON
        lda (PLAYERZP),y ;READ AND ACK. KEYBOARD TRACK FOR PW
        sta PKBDTRK,x
        .fi
        iny             ;POINT TO NEXT ROW 1ST COLUMN
SEPWPOS tya
        sta PWTPOS,x
        lda #0
        sta PWEEPCNT,x  ;INITIALIZE SWEEP-COUNTER AFTER SETTING PW...
ENDPWTB
        .if feature.PWKEYBTRACK_ON
        clc             ;PRE-SET CARRY
        lda PKBDTRK,x   ;ADJUST STEPPING-AMOUNT PER NOTE (KINDA DIVISION USING EXP.TABLE)
        beq combiKT     ;IF NO KEYBOARD-TRACKING (NO DIVISION WITH ZERO)
        adc DPITCH,x    ;A=CURRENT NOTE ON CURRENT TRACK
        tay
        lda EXPTABH,y   ;TRANSFORM ADDITION/SUBSTRACTION TO QUASI MULTIPLICATION/DIVISION
        sbc EXPTABH-1,y ;FURTHER DIVIDE THE VALUE TO FIT IN 4 UPPER BITS OF PULSEWIDTH
combiKT adc PWHIGHO,x   ;UPPER 4 BITS OF PULSEWIDTH, ONLY THIS KB.TRACKED FOR SPEED REASONS
        .else
        lda PWHIGHO,x   ;UPPER 4 BITS OF PULSEWIDTH
        .fi
        sta SIDG.PLSW+1,x
        .if COMPILEDAPP==1 ;HELP EDITOR TO DISPLAY PULSEWIDTH WITH KEYBOARD-TRACK
        sta playadapter.PWHIHELP,x
        .fi
        lda PWLOGHO,x
        sta SIDG.PLSW,x ;LOWER 8 BITS OF PULSEWIDTH

WFARPTB
        .if feature.ARPSPEEDSUPP_ON
        dec ARPSCNT,x   ;DECREASE ARPEGGIO-SPEED COUNTER
        bpl SKIPWFT
        lda ARPSPED,x
        .fi
        and #$3F        ;whatever Accu is, this 'AND #$3F' will make it 'positive' for multispeed-detector
        sta ARPSCNT,x   ;used by multispeed to determine new note 1st waveform (set to $FF by frame1)
RDWFROW ldy WFTPOS,x
        lda (PLAYERZP),y
        cmp #$FE        ;....LOOP/JUMP COMMAND ?
        beq WFAJUMP     ;IF $FE JUMP-COMMAND
        bcc WFTdone     ;IF $FF WFTABLE-ENDSIGNAL, DO NOTHING
SKIPWFT jmp ENDWFTB     ;WHEN COUNTER NOT COUNTED DOWN
WFTdone
        .if feature.ARPSPEEDSUPP_ON
        cmp #$10        ;TRESHOLD BETWEEN ARPSPEED/WAVEFORM-SETTING
        bcs SEWFARP     ;IF $00..$0F ARP-SPEED CHANGE (REPEAT $XX TICKS) - IN CASE OF 0, NOP - NO REPEAT
        sta ARPSCNT,x
        bcc SETJARP
        .else
        bcc SEWFARP
        .fi
WFAJUMP iny             ;SET REAL ADDRESS (MUL3)
        lda (PLAYERZP),y ;READ JUMP-ADDRESS NEXT TO $FE
        .if (*+2+$7F>=ENDWFTB)
        bmi ENDWFTB     ;IF BIGGER THAN JUMP ROW IS BIGGER THAN $2B (ADDRESS IS BIGGER THAN 3*$28+8)
        .else
        bpl +
        jmp ENDWFTB
+       .fi
        sta WFTPOS,x
        tay             ;INDEX TO JUMP-ADDRESS
        lda (PLAYERZP),y;READ WAVEFORM-COLUMN FROM JUMP-TARGET ADDRESS
SEWFARP and PTNGATE,x
        sta WFGHOST,x
SETJARP iny             ;2 CYCLES
        lda (PLAYERZP),y ;ARP.ABS/REL PITCH ;6 CYCLES
        iny             ;2 CYCLES
        .if feature.CHORDSUPPORT_ON
        cmp #$7F        ;CHECK IF JUMPING TO CHORD
        beq PLYCHRD     ;BNE NORMARP ;IF $7F, HANDLE CHORD INSTEAD OF WF-TABLE ARPEGGIO-COLUMN & DON'T ADVANCE IN TABLE
        .fi
        sta ASTOREZ+1   ;4 CYCLES
        .if feature.DETUNESUPPORT_ON
        lda (PLAYERZP),y ;6 CYCLES
        cmp #$FF        ;detune-NOP?
        beq +
        sta DETUNER,x
+
        .fi
        iny             ;2 CYCLES
        tya             ;2 CYCLES
        sta WFTPOS,x    ;5 CYCLES
ASTOREZ lda #selfmod    ;STORED PITCH-VALUE (ARP.COLUMN)

NORMARP bpl RELPTCH     ;IF REL.PITCH-UP (CARRY WAS SET BY 'CMP #$7F') - GOOD ENTRY POINT FOR 1ST WAVEFORM
        .if feature.WFARP_NOP_SUPP_ON
        cmp #$80        ;NOP?
        beq ENDWFTB     ;DON'T CHANGE PITCH IF NOP
        .fi
        cmp #$E0
        bcc ABSPTCH     ;IF ABSOLUTE PICTH
        .if feature.CHORDSUPPORT_ON
        bcs RELPTCH     ;IF REL.PITCH-DOWN

PLYCHRD lda (PLAYERZP),y ;6 CYCLES ;READ DETUNE FOR CURRENT ROW
        sta DETUNER,x
.if (COMPILEDAPP==1)
        lda DPITCH,x    ;telling player which note called the chord
        sta playadapter.padapter.plynote+1
        lda CURCHORD,x  ;telling which chord was called
        sta playadapter.followplay.plychrd+1 ;tells current chord to editor in follow-play mode
.fi
        ldy CHORDPOS,x  ;READ CHORDNOTE AT PLAYED CHORD-INDEX
p_chdt1 lda CHORDS,y    ;LOAD RELATIVE CHORD-PITCH
        cmp #$7E        ;$7E? - RETURN FROM CHORD TO WFARP-TABLE?
        bne chLoopC
        ldy CURCHORD,x  ;LOOP TO BEGINNING OF CURRENT CHORD
p_chdp2 lda CHDPTRLO,y  ;GET CHORD-BASEPOINTER
        sta CHORDPOS,x  ;RESET PLAYED CHORD-INDEX TO PREPARE FOR NEXT POSSIBLE CHORD-LAUNCH
        lda WFTPOS,x    ;INDEX OF CURRENT ROW
        adc #3-1        ;CARRY IS 1 AFTER 'CMP' DIDN'T UNDERFLOW, SO MUST BE SUBSTRACTED
        sta WFTPOS,x
        jmp RDWFROW     ;RDWFORM ;GO TO BEGINNING OF NEXT ROW
chLoopC cmp #$7F        ;$7F? - LOOP CHORD?
        bne DOCHORD
LOOPCHD ldy CURCHORD,x  ;LOOP TO BEGINNING OF CURRENT CHORD
p_chdp3 lda CHDPTRLO,y  ;GET CHORD-BASEPOINTER
        sta CHORDPOS,x
        tay
p_chdt2 lda CHORDS,y    ;LOAD RELATIVE CHORD-PITCH
DOCHORD inc CHORDPOS,x  ;ADVANCE IN PLAYED CHORD-INDEX
        .fi             ;END OF CHECKING CHORD-SUPPORT
.if (COMPILEDAPP==1)
CHDMODE lda #0 ;self-written command by editor (LDA=$A9 / LDY=$A0 based on NoteMode & curwind)
.fi

RELPTCH clc             ;ADD VALUE TO DISCRETE NOTE-PITCH
        adc DPITCH,x
ABSPTCH and #$7F        ;FOR ABSOLUTE PITCH
.if (COMPILEDAPP==1 && MIDI_support!=0)
        sta playadapter.discretePitch,x ;help editor to calculate correct slide-pitches
.fi
.if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
        sta ARPDPITCH,x ;help slowdown-support
.fi
        tay
        lda FREQTBL,y
        sta FREQLO,x    ;HANDLE PITCH-DETUNING (DELAYED VIBRATO STILL APPLICABLE)
        lda FREQTBH,y
        sta FREQHI,x
ENDWFTB
;-----------------------
WRPITCH lda FREQLO,x
        .if feature.DETUNESUPPORT_ON
        adc DETUNER,x   ;RESTORE DETUNE-VALUE - 2 CYCLES
        .fi
.if (COMPILEDAPP==1 && MIDI_support!=0)
        .if feature.DETUNESUPPORT_ON
        php ;store carry-flag of 1st addition
        .fi
        clc
        adc playadapter.pitchShiftLo,x ;calculated by editor
.fi
        sta (COMPILEDAPP!=2 || SLOWDOWN_SUPP==0)?  SIDG.FREQ+0,x : SlowPiL+1
        lda FREQHI,x
.if (COMPILEDAPP==1 && MIDI_support!=0)
        adc playadapter.pitchShiftHi,x ;calculated by editor
        .if feature.DETUNESUPPORT_ON
        plp ;restore Carry-flag
        adc #0 ;add detuner's carry-flag
        .fi
.else
        .if feature.DETUNESUPPORT_ON
        adc #0          ;ADD CARRY (IF DETUNER HAS UPPER PART)
        .fi
.fi
        sta (COMPILEDAPP!=2 || SLOWDOWN_SUPP==0)? SIDG.FREQ+1,x : SlowPiH+1

.if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
        lda ARPDPITCH,x
        tay
        sec
SLOWDOWN sbc #selfmod
        bcs + ;protect from wrapping around
        lda #1 ;expectable negative values are turned to 0
        sta SIDG.FREQ+0,x
        jmp wrSIDfH
+       stx slXstor+1
        tax
        lda FREQTBL,y
        ;sec ;carry set before, and if in right range, haven't changed to 0
        sbc FREQTBL,x 
        sta slDiffL+1
        lda FREQTBH,y
        sbc FREQTBH,x
        sta slDiffH+1
SlowPiL lda #selfmod
        sec
slDiffL sbc #selfmod
slXstor ldx #selfmod
        sta SIDG.FREQ+0,x
SlowPiH lda #selfmod
slDiffH sbc #selfmod
wrSIDfH sta SIDG.FREQ+1,x 
.fi

;-------------------------------------------
WRWFGHO lda WFGHOST,x
        .if COMPILEDAPP==1 ;muting in editor
        ldy playadapter.playmod
        beq skpMute     ;in stopped (jamming) mode no need to use mute/solo feature
        ldy playadapter.div7chn,x
        and mutesolo,y  ;$FE or $FF
        jmp endMute
skpMute and playadapter.wasjamm,x ;mutes sound after directly pausing / switching instrument until a key is pressed for jamming
endMute 
        .fi
WRSIDR4 sta SIDG.WAVE,x ;WAVEFORM-CONTROL SID-REGISTER

;-----------------------------------------------------------
RETRACK rts             ;RETURN FROM CURRENT SID-TRACK


;****************************COMMON SUBROUTINES********************************
SETSTUNE ;SET SUBTUNE, INPUT: SUBTUNE-NUMBER MUST BE GIVEN IN ACCU (0..F)
.if feature.SUBTUNESUPPORT_ON
        ldx #7*0
        jsr SETSEQA     ;SET SEQUENCE ON ONE TRACK (USED BY SUBTUNE-JUMP FX); INPUT: SUBTUNE IN ACCU, CHANNEL*7 IN X (0/7/14)
        ldx #7*1
        jsr SETSEQB     ;SET SELECTED SUBTUNE'S SEQUENCE ON ONE TRACK; INPUT: CHANNEL*7 IN X (0/7/14)
        ldx #7*2
        jsr SETSEQB     ;SET SELECTED SUBTUNE'S SEQUENCE ON ONE TRACK; INPUT: CHANNEL*7 IN X (0/7/14)
        .if (SID_AMOUNT>=2)
        ldx #7*3
        jsr SETSEQB     ;SET SEQUENCE ON ONE TRACK (USED BY SUBTUNE-JUMP FX); INPUT: SUBTUNE IN ACCU, CHANNEL*7 IN X
        ldx #7*4
        jsr SETSEQB     ;SET SELECTED SUBTUNE'S SEQUENCE ON ONE TRACK; INPUT: CHANNEL*7 IN X
        ldx #7*5
        jsr SETSEQB     ;SET SELECTED SUBTUNE'S SEQUENCE ON ONE TRACK; INPUT: CHANNEL*7 IN X
         .if (SID_AMOUNT>=3)
         ldx #7*6
         jsr SETSEQB     ;SET SEQUENCE ON ONE TRACK (USED BY SUBTUNE-JUMP FX); INPUT: SUBTUNE IN ACCU, CHANNEL*7 IN X
         ldx #7*7
         jsr SETSEQB     ;SET SELECTED SUBTUNE'S SEQUENCE ON ONE TRACK; INPUT: CHANNEL*7 IN X
         ldx #7*8
         jsr SETSEQB     ;SET SELECTED SUBTUNE'S SEQUENCE ON ONE TRACK; INPUT: CHANNEL*7 IN X
         tya
         clc
         adc #(31-2*CHN_AMOUNT)
         tay
         .else
         iny
         iny
         .fi
        .fi
        .if (SID_AMOUNT<3)
        iny             ; Y = subtune funktempo position  - using return value in Y after last channel's SETSEQ
        .fi
p_subt3 lda SUBTUNES,y  ;COPY SUBTUNE-FUNKTEMPO1 TO TEMPOTABLE PLACE 0
p_tmpt4 sta TEMPOTBL+0
p_subt4 lda SUBTUNES+1,y ;COPY SUBTUNE-FUNKTEMPO2 TO TEMPOTABLE PLACE 1
p_tmpt5 sta TEMPOTBL+1
        rts

SETSEQA ;SET SEQUENCE ON ONE TRACK (USED BY SUBTUNE-JUMP FX); INPUT: SUBTUNE IN ACCU, CHANNEL IN X (0/7/14) - X PRESERVED
        .if (feature.SUBTUNESUPPORT_ON && feature.SUBTUNEJUMP_ON)
        stx XSTORE+1    ;retain X
        .fi
        ;and #$1F       ;unneeded because upcoming 'asl' throws higher bits ;clear bit7 (and other bits to restrict number of subtunes)
        asl             ;GET SUBTUNE ADDRESS FROM ACCU MULTIPLIED 8 -ASL;ASL;ASL
        asl
        asl ;*8
        .if (SID_AMOUNT>=2)
        asl ;*16
        .fi
        .if (SID_AMOUNT>=3)
        asl ;*32
        .fi
        sta SUBTPOS+1   ;temporary starage for calculated subtune-position
SETSEQB lda TRKTMPOS,x  ;A = 2|4|6|8|10|12 depending on track given in X (0/7/14/etc.)
        asl             ;A=4|8|12|16|20|24 ;carry=0
        sbc #3-1        ;A = A-(3-CARRY) = 1|5|9|13|17|21
        tax             ;X=1|5|9|13|17|21 **************
        lsr             ;A=0|2|4|6|8|10 ;carry=1 unfortunately
        clc
SUBTPOS adc #selfmod    ;stored calculated base-position of subtune-data (seq.pointers and funktempo)
        tay             ;Y=SUBTUNEBASE + 0|2|4 *************
p_subt1 lda SUBTUNES,y  ;READ ORDERLIST LO-ADDRESSES FOR SUBTUNE
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        clc
        adc SWP_OFFSET+0
.fi
        sta p_seqt1,x   ;SET ORDERLIST LO-ADDRESSES FOR SUBTUNE
        iny
p_subt2 lda SUBTUNES,y  ;READ ORDERLIST HI-ADDRESSES FOR SUBTUNE
.if(COMPILEDAPP==2 && SWP_EXPORT!=0)
        adc SWP_OFFSET+1
.fi
        sta p_seqt1+1,x ;SET ORDERLIST HI-ADDRESSES FOR SUBTUNE
        .if (feature.SUBTUNESUPPORT_ON && feature.SUBTUNEJUMP_ON)
XSTORE  ldx #0          ;restore X
        .fi
;SOURCE:  SUBT[A] + 0-1, 2-3, 4-5     SUBT[A]  + 6,7
;DESTIN:  p_seqt1 + 1-2, 5-6, 9-10    TEMPOTBL + 0,1
.else ;if no subtune-support ( demo- / bare-player )
p_subt3 lda (SID_AMOUNT!=3)? SUBTUNES+8*SID_AMOUNT-2 : SUBTUNES+30
p_tmpt4 sta TEMPOTBL+0
 .if (feature.TEMPOPRGSUPP_ON!=0)
p_subt4 lda (SID_AMOUNT!=3)? SUBTUNES+8*SID_AMOUNT-1 : SUBTUNES+31
p_tmpt5 sta TEMPOTBL+1
 .fi
.fi
        rts


;-----------------------
SEQSUB  
.if (SID_AMOUNT>=2)
        .if (SID_AMOUNT>=3)
        cpx #2*7*3
        bcc +
SID3SEQ cpx #7*7
        beq p_seqt8
        bpl p_seqt9
        bmi p_seqt7 ;jump
        .fi
+       cpx #7*3
        bcc SID1SEQ
SID2SEQ cpx #7*4
        beq p_seqt5
        bpl p_seqt6
        bmi p_seqt4 ;jump
.fi
SID1SEQ cpx #7*1        ;SEQUENCE (ORDERLIST) READER SUBROUTINE,Y=POS.
        beq p_seqt2     ;CALLED AS SUBROUTINE,READS TRACK-SEQ OF REG.X
        bpl p_seqt3
p_seqt1 lda SEQUENCES,y ;LOAD PTNUM/FX FROM TRACK 1 ORDERLIST
        rts
p_seqt2 lda SEQUENCES+seqbound,y ;LOAD PTNUM/FX FROM TRACK 2 ORDERLIST
        rts
p_seqt3 lda SEQUENCES+2*seqbound,y ;LOAD PTNUM/FX FROM TRACK 3 ORDERLIST
        rts
.if (SID_AMOUNT>=2)
p_seqt4 lda SEQUENCES+3*seqbound,y ;LOAD PTNUM/FX FROM TRACK 4 ORDERLIST
        rts
p_seqt5 lda SEQUENCES+4*seqbound,y ;LOAD PTNUM/FX FROM TRACK 5 ORDERLIST
        rts
p_seqt6 lda SEQUENCES+5*seqbound,y ;LOAD PTNUM/FX FROM TRACK 6 ORDERLIST
        rts
.fi
.if (SID_AMOUNT>=3)
p_seqt7 lda SEQUENCES+6*seqbound,y ;LOAD PTNUM/FX FROM TRACK 7 ORDERLIST
        rts
p_seqt8 lda SEQUENCES+7*seqbound,y ;LOAD PTNUM/FX FROM TRACK 8 ORDERLIST
        rts
p_seqt9 lda SEQUENCES+8*seqbound,y ;LOAD PTNUM/FX FROM TRACK 9 ORDERLIST
        rts
.fi

;-------------------------------------------------------------------------------
        .if feature.CALCVIBRATO_ON ;DIFFERENT EXPONENT-TABLE BASES AND TRESHOLD FOR CALCULATED VIBRATO
EXPTBASE = EXPTABH
        .else
EXPTBASE = FREQTBH-1
        .fi
EXPTRESHOLD = ENDFREQTBH-EXPTBASE

SETVIB0 ldy #6          ;READ VIBRATO-DELAY/INCREMENT AMOUNT FROM INSTRUMENT-DATA
        lda (PLAYERZP),y
        sta VIDELCNT,x  ;MAX.$7F, COUNTS BACK TILL $FF
SETVIB1 ldy #5          ;5 INDEX TO VIBRATO FREQUENCY AND AMPLITUDE INSTRUMENT-DATA
        lda (PLAYERZP),y
SETVIBR pha             ;SET AMPLITUDE AND RATE OF VIBRATO. INPUT: ACCU (HI-NIBBLE IS AMPLITUDE)
        and #$0F
        asl             ;ASL, BIT 1 IS ALWAYS 0, SO IT CAN BE CLIPPED INTO 2 EQUAL TIMEFRAMES FOR SLIDE UP/DOWN
        sta VIBFREQU,x
        lsr             ;HALF-TIMERSTART FOR DOWN-ORIENTED VIBRATO (LIKE GUITAR TREMOLO-ARM)
        .if feature.VIBRATOTYPES_ON
        ldy SLIDEVIB,x  ;SELF-WRITTEN CODE - VIBRATO-CONTROL-EXACT FROM INSTRUMENT-CONTROL REGISTER
        cpy #$20
        bpl chUpVib     ;NOT INCREASING-TYPE VIBRATO OR DEFAULT VIBRATO?
        .fi
        lsr             ;FOR QUARTER TIMERSTART - NORMAL VIBRATO
        .if feature.VIBRATOTYPES_ON
chUpVib cpy #$30
        bne setVcnt
        lda #0          ;0 TIMERSTART FOR UP-ORIENTED VIBRATO (LIKE GUITAR STRING STRETCHING)
        .fi
setVcnt sta VIBRACNT,x
        pla
SETVAMP and #$F0        ;SET AMPLITUDE (NO NEED TO MASK OUT FREQ.NIBBLE AS IT'S NOT SIGNIFICANT
        lsr             ;0..127
SETFMOD                 ;SET FREQUENCY-MODIFIER VALUES FOR SLIDE/PORTAMENTO, OR RESET THEM IF NO SLIDE/VIBRATO
        .if feature.CALCVIBRATO_ON
        beq wrFmodL     ;IF 0, NO CALCULATION OF PITCH
        lsr             ;vibrato-0..64, slide-0..127
        adc DPITCH,x    ;vibrato-0..160, slide-0..222 , CALCULATE PITCH-DEPENDENT AMPLITUDE-COMPENSATION HERE!!!!
         .if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
SLOWDN3    sbc #selfmod   ;weaken vibrato if slowdown initiated
        bcs +
        lda #0
+
         .fi
        .fi
LOOKUPA tay             ;0..LOOK UP AMPLITUDE VALUE FROM FREQUENCY/EXPONENT-TABLES
CHKTEND cpy #EXPTRESHOLD+(ENDFREQTBH-FREQTBH)
        bcs MAXSLID     ;IF POINTS OVER TABLE
        cpy #EXPTRESHOLD ;DECIDE ON FINE/ROUGH HALF OF THE EXPONENT-TABLE
        bcs calcVib     ;if bigger than $60 limit of table, switch to rough table
        lda EXPTBASE,y
wrFmodL sta FREQMODL,x
        lda #0          ;IN FINE-RANGE FREQ-ADD-HI SHOULD BE 0
        beq wrFmodH     ;jump
MAXSLID ldy #EXPTRESHOLD+(ENDFREQTBH-FREQTBH)
calcVib lda FREQTBL-EXPTRESHOLD,y
        sta FREQMODL,x
        lda FREQTBH-EXPTRESHOLD,y ;EXPTABH,Y ;EXPONENTIAL TABLE SIMULATES MULTIPLICATION - CALCULATED VIBRATO
wrFmodH sta FREQMODH,x
        rts


;******************************* NUMERIC DATA *********************************
;SID EQUAL-TEMPERED frequency table:
;----------------------------------------------------
;EXPONENT-TABLE HI-BYTES (USED FOR MULTIPLICATION)
        .if (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0)
LOGTBL .byte 0, 7, 15, 21, 26, 29, 32, 35, 37, 39, 40, 42, 43, 44, 46, 47  ;based on EXPTAB2 $x8 middle values (AND ROUGHLY FREQTBH)
;ADSR register-value:        0   1   2   3   4   5   6   7  8   9    A    B    C    D     E     F
;Attack timing        (ms):  2,  8, 16, 24, 38, 56, 68, 80,100,250, 500, 800,1000,3000, 5000, 8000
;Decay/Release timing (ms):  6, 24, 48, 72,114,168,204,240,300,750,1500,2400,3000,9000,15000,24000 ;3 * Attacktiming
ADSR_OFFS .byte 9,20,21,22,24,23,21,20,15,20,20,20,13,19,12,0 ;1,17,22,24,24,24,21,17,9,20,16,19,5,16,12,0 ;offset0 index of Attack/Decay/Release=0..F in ADSR_EXPTB, +24 results around correct difference
ADSR_EXPTB .byte 0,0,0,0,0,0,0,0,0,0,0,0, 0,0 ;differences between normal and offset=-24 ADSR=0..F values: 1,3,4,5,5,4,4,3,2,3,3,3,2,3,2,1
ADSR_EXPT2=EXPTAB2-25
        .fi ;this must be right before EXPTABH
        .if ( (feature.CALCVIBRATO_ON+feature.PWKEYBTRACK_ON+feature.FILTKBTRACK_ON+(COMPILEDAPP==1)) || (COMPILEDAPP==2 && SLOWDOWN_SUPP!=0) )
EXPTABH .byte 0,0,0,0,0,0,0,0, 0,0;LET EXPONENT TABLE BE A BIT MORE COMPLETE (FOR SUBSTRACTING KB.TRACK)
        .fi             ;USED AS CALCULATED VIBRATO-AMPLITUDE TABLE TOO
        .byte 0         ;FOR UNCALCULATED ZERO VIBRATO
FREQTBH .byte $01, $01,$01, $01,$01,$01 ;0  ;13  ;16TH ELEMENT IS $01 IN EXP.TABLE  ;be aware: C-1 note is the second value!
        .byte $01,$01,$01,$01,$01,$01   ;6  ;19
        .byte $02,$02,$02,$02,$02,$02   ;12 ;25
        .byte $02, $03,$03,$03,$03,$03  ;18 ;31  ;32ND ELEMENT IS $03 IN EXP.TABLE
        .byte $04,$04,$04,$04,$05,$05   ;24 ;37  ;!!! $04 IS 37TH ELEMENT IN EXP.TABLE
        .byte $05,$06,$06,$06,$07, $07  ;30 ;43  ;48TH ELEMENT IS $07 IN EXP.TABLE
        .byte $08,$08,$09,$09, $0a,$0a  ;36 ;49  ;!!!! $0A IS 53TH ELEMENT IN EX.TABLE
        .byte $0b,$0c,$0d,$0d,$0e, $0f  ;42 ;55  ;$0F IS 60TH ELEMENT IN EXP.TABLE
        ;-------------------------------------
EXPTAB2 .byte $10,$11,$12, $13,$14,$15  ;48 ;61  ;!!!64TH (72-8) ELEMENT IS $13 ($1F-$0C) IN EXP.TABLE
        .byte $17, $18,$1a,$1b,$1d, $1f ;54 ;67  ;!!!! $1F ($2E-$0F) IS 72ND (64+8) ELEMENT IN EXP.TABLE,!!!$18 ($0A+$0E) IS 68TH
        .byte $20,$22, $24,$27,$29,$2b  ;60 ;73  ;$24 IS 75TH ELEMENT IN EXP.TABLE
        .byte $2e,$31, $34,$37,$3a, $3e ;66 ;79  ;!!!$2E ($1F+$0F) IS 79TH (72+7), $3E IS 84TH, 80TH ELEMENT IS $31 IN EXP.TABLE
        .byte $41,$45,$49,$4e,$52,$57   ;72 ;85
        .byte $5c,$62,$68,$6e,$75,$7c   ;78 ;91
        .byte $83,$8b,$93,$9c,$a5,$af   ;84 ;97
        .byte $b9,$c4,$d0,$dd,$ea,$f8   ;90 ;103
ENDFREQTBH
        .if (feature.CALCVIBRATO_ON+feature.PWKEYBTRACK_ON+feature.FILTKBTRACK_ON+(COMPILEDAPP==1))
        .byte $F9,$FA,$FB,$FC,$FD,$FE,$FF,$FF ;EXPAND EXPONENT-TABLE WITH SLOPE FOR KB.TRACKING
        .fi
ENDEXPTABH 
;------------------------------------------------------------------------------
FREQTBL
        .byte $07, $16,$27,$38,$4b,$5e ;be aware: C-1 note is the second value!
        .byte $73,$89,$a1,$ba,$d4,$f0
        .byte $0d,$2c,$4e,$71,$96,$bd
        .byte $e7,$13,$42,$74,$a8,$e0
        .byte $1b,$59,$9c,$e2,$2c,$7b
        .byte $ce,$27,$84,$e8,$51,$c0
        .byte $36,$b3,$38,$c4,$59,$f6
        .byte $9d,$4e,$09,$d0,$a2,$81
        ;---------------------------------------------
        .byte $6d,$67,$70,$88,$b2,$ed
        .byte $3a,$9c,$13,$a0,$44,$02
        .byte $da,$ce,$e0,$11,$64,$da
        .byte $75,$38,$26,$40,$89,$04
        .byte $b4,$9c,$c0,$22,$c8,$b4
        .byte $eb,$71,$4c,$80,$12,$08
        .byte $68,$38,$80,$45,$90,$68
        .byte $d6,$e3,$98,$00,$24,$10


;*************************** EFFECTS' SECTION *******************************************
.if feature.SEQ_FX_SUPPORT_ON
;---------------- HANDLE ORDERLIST/SEQUENCE-EFFECTS, INPUT:A, DON'T DISTURB X&Y!
SEQ_FX  cmp #$A0        ;IF BELOW $A0: TRANSPOSE-FX ($80..$8F:DOWN, $90..$9F:UP)
        bcs chVolFx     ;FX ABOVE $A0?
        .if feature.TRANSPOSESUPP_ON
        ;SEC - CARRY WAS SET BY 'CMP' JUST BEFORE, AND NEXT -1 WILL COMPENSATE
        sbc #$90-1      ;TRANSFORM RANGE $80..$FD TO $F0..$6D
        sta TRANSP2,x   ;SET TRANSPOSE-VALUE (2'S COMPLEMENT)
        .fi
        rts

chVolFx cmp #$B0        ;IF VOLUME-FX ($A0..$AF: SET VOLUME TO 0..15 )
        bcs chTmpFx     ;FX ABOVE $B0?
        and #$0F
        .if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
         cpx #2*3*7
         bcc +
         sta SEQVOL3+1
         rts
         .fi 
+       cpx #3*7
        bcc +
        sta SEQVOL2+1
        rts
        .fi
+       sta SEQVOLU+1   ;WILL BE SET TO MAINVOL+1 IN NEXT ROUND (COZ DELAY NEEDED)
        rts
chTmpFx cmp #$F0        ;TEMPO-SETTING ....;SEC - CARRY WAS SET TO 0 BY 'CMP'
        bcs retTmFx
        .if feature.TEMPOPRGSUPP_ON
        sbc #$B0-1      ;SHIFT RANGE $B0..$EF TO $00..$5D
        sta SEQTEMPO,x  ;TEMPORARY DELAYED VALUE FOR TRACK-TEMPO SETTING
        .fi
retTmFx rts
.fi

;------------------------------------------------------------------------------
NOTEFXTBL 
        .if feature.PORTAME_NOTEFX_ON
        .byte NPORTAM-INDEXJ1-2
        .else
        .byte ENDNOTEFX-INDEXJ1-2 
        .fi
        .byte NSYNCON-INDEXJ1-2,NSYNCOF-INDEXJ1-2,NRINGON-INDEXJ1-2
        .byte NRINGOF-INDEXJ1-2,NGATEON-INDEXJ1-2,NGATEOF-INDEXJ1-2,0

NOTE_FX                 ;NOTE-FX/INSTRUMENT-FX/PATTERN-FX - ALTERATIONS BEFORE CNTPLAY AND SID-WRITING
CHVIBFX cmp #PACKEDMAX+1 ;$78 ;CHECK VIBRATO-FX IN NOTE-COLUMN - IF SOMEHOW $70-$7F, TREAT AS VIBRATO
        bpl SELNTFX     ;IF MORE THAN $70 IT IS NOTE-FX, OTHERWISE TREATED AS VIBRATO
        sta VALSTOR+1
        jsr VIBAMFX
        jmp ENDNOTEFX

SELNTFX tay             ;JUMP TO ADDRESS ACCORDINGLY TO PERFORM NOTE-FX
        lda NOTEFXTBL-$78,y ;INDEX FROM 1ST NOTE-FX (AFTER PACKED-NOP VALUE)
        sta INDEXJ1+1
        lda WFGHOST,x   ;PRELOAD WAVEFORM-REGISTER
        clc             ;ENSURE JUMP INSTEAD OF CONDITIONAL BRANCHING
INDEXJ1 bcc *+2         ;SELF-WRITTEN, THIS SMALLFX-BRANCH SECTION MUST FIT IN $7F BYTES

        .if feature.PORTAME_NOTEFX_ON
NPORTAM ldy #$FF        ;PRE-SIGN NOTE-PORTAMENTO FOR 'TICK2'
        lda #DEFAULTPORTA ;AN AVERAGE SPEED-VALUE FOR THE TONE-PORTAMENTO
        jsr SETSLID     ;SET SLIDE-TYPE AND SLIDE-SPEED
        jmp ENDNOTEFX
        .fi

NSYNCON ora #%00000010  ;SET SYNC BIT ;SYNCONFX
        bne WRWFORM
NSYNCOF and #%11111101  ;RESET SYNC BIT ;SYNCOFFX
        jmp WRWFORM
NRINGON ora #%00000100  ;SET RING BIT ;RINGONFX
        bne WRWFORM
NRINGOF and #%11111011  ;RESET RING BIT ;RINGOFFX
WRWFORM sta WFGHOST,x
        jmp ENDNOTEFX

NGATEON lda #$FF        ;GATEONFX ;$7D ;GATE-ON?  ;NOTE-FX - INPUT:ACCU
        sta PTNGATE,x
        lda WFGHOST,x
        ora #1
        bne WRWFORM

NGATEOF                 ;GATEOFFX ;$7E ;GATE-OFF?
        .if feature.GATEOFFPTR_ON
        ldy #$0C        ;SET TABLEPOSITIONS TO GATE-OFF POINTERS
        lda (PLAYERZP),y ;GATE-OFF POINTER FOR WF-ARP. TABLE
        bne SETGOFF     ;IF WFARP-GATEOFF NONZERO, SET IT
        .fi
        lda #$FE        ;NORMAL GATE-OFF GATE-BIT OFF 'AND'-ER
        sta PTNGATE,x
        and WFGHOST,x
        sta WFGHOST,x
        .if feature.GATEOFFPTR_ON
        jmp chPWgof

SETGOFF sta WFTPOS,x    ;RESET ARP-SPEED COUNTER?
chPWgof ldy #$0D
        lda (PLAYERZP),y ;GATE-OFF POINTER FOR PULSEWIDTH-TABLE
        beq chFlTrk
        sta PWTPOS,x
chFlTrk  .if (SID_AMOUNT>=2)
          .if (SID_AMOUNT>=3)
          cpx #2*3*7
          bcc +
          cpx flprog3.FLTCTRL+1   ;CHECK IF THIS IS FILTER-CONTROLLER TRACK
          bne endGoff     ;IF NOT FILTER-CONTROLLER TRACK, DON'T MODIFY ANYTHING
          iny             ;$0E
          lda (PLAYERZP),y ;GATE-OFF POINTER FOR FILTER-TABLE
          beq endGoff
          sta flprog3.FLTPOSI+1
          jmp endGoff
          .fi
+        cpx #3*7
         bcc +
         cpx flprog2.FLTCTRL+1   ;CHECK IF THIS IS FILTER-CONTROLLER TRACK
         bne endGoff     ;IF NOT FILTER-CONTROLLER TRACK, DON'T MODIFY ANYTHING
         iny             ;$0E
         lda (PLAYERZP),y ;GATE-OFF POINTER FOR FILTER-TABLE
         beq endGoff
         sta flprog2.FLTPOSI+1
         jmp endGoff
         .fi
+       cpx flprog1.FLTCTRL+1   ;CHECK IF THIS IS FILTER-CONTROLLER TRACK
        bne endGoff     ;IF NOT FILTER-CONTROLLER TRACK, DON'T MODIFY ANYTHING
        iny             ;$0E
        lda (PLAYERZP),y ;GATE-OFF POINTER FOR FILTER-TABLE
        beq endGoff
        sta flprog1.FLTPOSI+1
endGoff
        .fi
ENDNOTEFX

;---------------------------------------------------------------------
INSPTFX                 ;ENTRY POINT FOR INST.&PATTERN-FX TOGETHER (SKIPPED NOTE-FX)
INST_FX lda CURIFX,x    ;CHECK INST/FX COLUMN ;PRIO3
        cmp #$40
        bmi PATT_FX     ;IF INSTRUMENT or NOP
        jsr SMALPFX     ;IF INSTRUMENT-FX

;---------------------------------------------------------------------
PATT_FX lda CURFX2,x    ;PRIO2
        beq RETURFX     ;IF NOP - MAYBE NOT NEEDED TO DISTINGUISH, TAKEN FROM FX-TABLE
        cmp #$20        ;LOWER LIMIT OF SMALL-FX
        bcs SMALPFX

;------------------------------------------
BIGPTFX asl             ;WE'RE READING WORDS
        tay
        lda BIGFXTABLE-2,y ;NO FX 0, SO CHECKING FX1 AT POSITION 0
        sta INDEXJP+1
        lda BIGFXTABLE-1,y ;NO FX 0, SO CHECKING FX1 AT POSITION 0
        sta INDEXJP+2
        lda CURVAL,x    ;PRELOAD FX VALUE
INDEXJP jmp RETURFX     ;SELF-WRITTEN JUMP-ADDRESS FOR BIGFX

RETURFX rts             ;FOR SAFETY AND NOP

;-----------------------SOME FX-VALUE CONVERSION ROUTINES------
SETINBL lda (PLAYERZP),y ;INSTRUMENT-BYTE
SETNIBL and #$F0        ;SET LOW NIBBLE AT ACCU
        ora VALSTOR+1
        rts

SETINBH lda (PLAYERZP),y ;INSTRUMENT-BYTE
SETNIBH and #$0F        ;SET HIGH NIBBLE AT ACCU
        sta MERGEST+1
VALSTOR lda #selfmod
        asl
        asl
        asl
        asl             ;x16
MERGEST ora #selfmod
        rts

MULTI3C                 ;MULTIPLY ACCU WITH 3, RESULT IS STORED INTO ACCU
        sta MUL3TMP+1
        asl
MUL3TMP adc #selfmod
        rts

;-------------------------------SMALL-FX---------------------------------------
SMALLFXTBL .byte SMALFX2-INDEXJ2-2,SMALFX3-INDEXJ2-2 ;THIS TABLE IS FOR INSTRUMENT-FX AND SMALL PATTERN-FX
           .byte SMALFX4-INDEXJ2-2,SMALFX5-INDEXJ2-2,SMALFX6-INDEXJ2-2,SMALFX7-INDEXJ2-2
           .byte SMALFX8-INDEXJ2-2,SMALFX9-INDEXJ2-2,SMALFXA-INDEXJ2-2,SMALFXB-INDEXJ2-2
           .byte SMALFXC-INDEXJ2-2,SMALFXD-INDEXJ2-2,SMALFXE-INDEXJ2-2,SMALFXF-INDEXJ2-2

SMALPFX pha
        lsr             ;GET SMALLFX-TYPE -
        lsr
        lsr
        lsr             ;...DIVIDE BY 16
        tay
        lda SMALLFXTBL-2,y ;NOTHING BELOW 2 FOR SMALLFX
        sta INDEXJ2+1
        pla
        and #$0F        ;ACCU=EFFECT-VALUE (ARGUMENT)
        sta VALSTOR+1   ;STORE FX-VALUE NIBBLE FOR LATER USE
        clc             ;ENSURE JUMP INSTEAD OF CONDITIONAL BRANCHING
INDEXJ2 bcc *+2         ;THIS SMALLFX-BRANCH SECTION MUST FIT IN $7F BYTES
ENDSMFX rts             ;FOR SAFETY AND RETURN FROM NOP

SMALFX2                 ;SET 'ATTACK' NIBBLE OF ADSR  - (NO SMALLFX-1, AS $00..$1F VALUES ARE BIGFX)
        .if (feature.ALLGHOSTREGS_ON==0 && SID_AMOUNT==1)
        ldy #3          ;AD - INDEX
        jsr SETINBH     ;GET&SET INSTRUMENT-DATA HIGH-NIBBLE WITH ACCU
        .else
        lda SIDG.AD,x
        jsr SETNIBH
        .fi
        jmp WRITEAD

SMALFX3                 ;SET 'DECAY' NIBBLE OF ADSR
        .if (feature.ALLGHOSTREGS_ON==0 && SID_AMOUNT==1)
        ldy #3          ;AD - INDEX
        jsr SETINBL     ;GET&SET INSTRUMENT-DATA LOW-NIBBLE WITH ACCU
        .else
        lda SIDG.AD,x
        jsr SETNIBL
        .fi
WRITEAD sta SIDG.AD,x
        rts

SMALFX4                 ;'4' = SID-REGISTER4 - ADJUST WAVEFORM
        lda WFGHOST,x
        jsr SETNIBH
WRITEWF sta WFGHOST,x
        rts

SMALFX5                 ;SET 'SUSTAIN' NIBBLE OF ADSR (NOTE-VOLUME)
        .if (feature.ALLGHOSTREGS_ON==0 && SID_AMOUNT==1)
        ldy #4          ;SR - INDEX
        jsr SETINBH
        .else
        lda SIDG.SR,x
        jsr SETNIBH
        .fi
        jmp WRITESR

SMALFX6                 ;'6' = SID-REGISTER6/RELEASE - SET 'RELEASE' NIBBLE OF ADSR
        .if (feature.ALLGHOSTREGS_ON==0 && SID_AMOUNT==1)
        ldy #4          ;SR - INDEX
        jsr SETINBL     ;GET&SET INSTRUMENT-DATA LOW-NIBBLE WITH ACCU
        .else
        lda SIDG.SR,x
        jsr SETNIBL
        .fi
WRITESR sta SIDG.SR,x
        rts

SMALFX7                 ;'7' = SET 'MAJ7 OR OTHER CHORD' FOR INSTRUMENT (OTHER THAN THE DEFAULT ASSIGNED CHORD)
        .if feature.CHORDSUPPORT_ON
        sta CURCHORD,x
        tay
p_chdp4 lda CHDPTRLO,y  ;GET CHORD-BASEPOINTER
        sta CHORDPOS,x
        .fi
        rts

SMALFX8                 ;SET VIBRATO AMPLITUDE
VIBAMFX ldy #5
        jsr SETINBH
        jmp BIGFX08

FORCVIB ldy #0          ;RESET SLIDE/VIBRATO - SET IT TO VIBRATO BY DEFAULT
        lda (PLAYERZP),y ;READ VIBRATO-TYPE FROM INSTRUMENT
FORCVI2 and #$30
        sta SLIDEVIB,x
        rts

SMALFX9                 ;SET VIBRATO FREQUENCY
        .if feature.VIBFREQFX_SUPP_ON
        asl             ;ASL, BIT0 IS ALWAYS 0, SO IT CAN BE CLIPPED INTO 2 EQUAL TIMEFRAMES FOR SLIDE UP/DOWN
        sta VIBFREQU,x
        .fi
        rts

SMALFXA                 ;'A' = ADJUST MAIN VOLUME
        .if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
          cpx #2*3*7
          bcs SID3VOL
         .fi
         cpx #3*7
         bcs SID2VOL
        .fi
        sta MAINVOL+1
        .if feature.SEQ_FX_SUPPORT_ON
         sta SEQVOLU+1
        .fi
        rts

SMALFXB                 ;'B' = BAND-NIBBLE OF FILTER (LOW/MID/HI/3OFF)
       .if (SID_AMOUNT==1)
        .if feature.FILTER_SMALLFX_ON
        asl
        asl
        asl
        asl             ;x16
        sta FLTBAND+1
        .fi
        rts
       .else
        jmp SIDBND2
       .fi
;
SMALFXC                 ;'C' = CHORD-SPEED AND ARPEGGIO-SPEED SETTING
        .if feature.ARPSPEEDSUPP_ON
        sta ARPSPED,x
        lda #$FF        ;CAUSE AN INSTANT START WITH NEW COUNTER 
        sta ARPSCNT,x   ;(ARPSCNT,x is also used to determine new note 1st frame for multispeed)
        .fi
        rts

SMALFXD 
        .if feature.DETUNE_SMALLFX_ON
        asl             ;x8 ;'D' = DETUNE CURRENT NOTE
        asl
        asl
        jmp SETDETU
        .fi

SMALFXE                 ;'E' = ENABLE/DISABLE TEST-BIT, RING-BIT, SYNC-BIT, GATE-BIT
        .if feature.WFCTRL_SMALLFX_ON
        lda WFGHOST,x
        jsr SETNIBL     ;SET LOW NIBBLE
        .fi
        rts ;jmp WRITEWF

        .cerror *>INDEXJ2+$7F,"SOME OF LAST FX ARE OUT OF MAX BRANCH RANGE!!!"
SMALFXF                 ;'F' = FILTER RESONANCE (STRENGTH) NIBBLE SETTING
        .if feature.FILTER_SMALLFX_ON
        asl
        asl
        asl
        asl             ;x16
        sta RESONIB+1
        .fi
        rts

.if (SID_AMOUNT>=2)
SID2VOL sta MAINVO2+1
        .if feature.SEQ_FX_SUPPORT_ON
         sta SEQVOL2+1
        .fi
        rts
 .if (SID_AMOUNT>=3)
SID3VOL sta MAINVO3+1
        .if feature.SEQ_FX_SUPPORT_ON
         sta SEQVOL3+1
        .fi
        rts
 .fi
SIDBND2 .if feature.FILTER_SMALLFX_ON
        asl
        asl
        asl
        asl             ;x16
         .if (SID_AMOUNT>=3)
         cpx #2*3*7
         bcc +
         sta FLTBAN3+1
         rts
         .fi
+       cpx #3*7
        bcc +
        sta FLTBAN2+1
        rts
+       sta FLTBAND+1
        .fi        
        rts
.fi


;==========================================================================================
;IN COMING FX SECTION THE EFFECT-SUBROUTINES CAN BE SET IN ORDER OF STATISTICAL USE-RATE
;IF THE LEAST IMPORTANT EFFECTS ARE NOT USED IN TUNE, THE CODE CAN BE WIPED OUT BY
;SID-MAKER APP. PLAYER-CODE SIZE-REDUCTION COULD BE GAINED THAT WAY....
;------------------------------------------------------------------------------------------
;-----------------------------------------------------BIG-FX JUMP ADDRESSES---------------
BIGFXTABLE .block
        .word BIGFX01,BIGFX02,BIGFX03,BIGFX04,BIGFX05,BIGFX06,BIGFX07
        .word BIGFX08,BIGFX09,BIGFX0A,BIGFX0B,BIGFX0C,BIGFX0D,BIGFX0E,BIGFX0F
        .word BIGFX10,BIGFX11,BIGFX12,BIGFX13,BIGFX14,BIGFX15,BIGFX16,BIGFX17
        .word BIGFX18,BIGFX19,BIGFX1A,BIGFX1B,BIGFX1C,BIGFX1D,BIGFX1E,BIGFX1F
        .bend
ENDBIGFXTB ;Big-FX table should be of the same size with the same positions in every players

;-------------------------------BIGFX------------------------------------------

BIGFX01                 ;PITCH SLIDE UP - FX-NUMBER IS THE SAME AS GOATTRACKER'S
        ldy #$81        ;THIS VALUE CAUSES UP-SLIDE
        bne SETSLID

BIGFX02                 ;PITCH SLIDE DOWN - FX-NUMBER IS THE SAME AS GOATTRACKER'S
        ldy #$82        ;THIS VALUE CAUSES DOWN-SLIDE
SETSLID pha
        tya
        sta SLIDEVIB,x  ;CAUSE SLIDE INSTEAD OF VIBRATO
        pla             ;THEN CALCULATE SLIDE-AMOUNT BASED ON NOTE-PITCH
        jmp SETFMOD

BIGFX03                 ;TONE PORTAMENTO - FX-NUMBER IS THE SAME AS GOATTRACKER'S
        ldy #$83
        bne SETSLID

BIGFX04 = WRITEWF       ;'4' = 4TH REGISTER WAVEFORM SIMPLE SETTING
;        jmp WRITEWF

BIGFX05 = WRITEAD       ;'5' = 5TH SID-REGISTER SETTING - ATTACK AND DECAY

BIGFX06 = WRITESR       ;'6' = 6TH SID-REGISTER SETTING - SUSTAIN AND RELEASE

BIGFX07 = SMALFX7       ;'7' = SELECT (MAJOR-7 OR ANY OTHER CHORD) FOR CURRENT INSTRUMENT

BIGFX08                 ;'8' = SET VIBRATO FREQUENCY AND AMPLITUDE (NUMBER '8' IS A SIN/COS LISSAJOUS)
        pha
        jsr FORCVIB     ;FORCE VIBRATO INSTEAD OF SLIDE, READ TYPE FROM INSTRUMENT
        pla
        jmp SETVIBR     ;SET VIBRATO-AMPLITUDE AND FREQUENCY AND FREQ.COUNTER

BIGFX09 ;GO TO WAVEFORM-ARPEGGIO TABLE POSITION
        jsr MULTI3C     ;MULTIPLY ACCU BY 3, STORE RESULT IN ACCU
        adc #WFTABLEPOS
        sta WFTPOS,x
        rts

BIGFX0A                 ;'A' = ADJUST INST.PARAMETER-A - PULSEWIDTH-PROGRAM TABLEPOINTER
        jsr MULTI3C
        ldy #$0A
        adc (PLAYERZP),y ;PULSEWIDTH-PROGRAM POSITION INDEX
        sta PWTPOS,x
        lda #0
        sta PWEEPCNT,x
        rts

BIGFX0B                 ;'B' = INSTRUMENT-PARAMETER 'B' - SET FILTER-PROGRAM TABLEPOINTER
        jsr MULTI3C
        ldy #$0B
        adc (PLAYERZP),y ;FILTER-PROGRAM POSITION INDEX
        sta flprog1.FLTPOSI+1   ;,X
        lda #0
        sta flprog1.CWEPCNT+1  ;other SIDs? (2SID/3SID)
        rts

BIGFX0C = SMALFXC       ;'C' = CHORDSPEED/ARPSPEED (MAX $3F)

BIGFX0D                 ;'D' = FINE-DETUNE CURRENT TRACK WITH GIVEN AMOUNT
SETDETU sta DETUNER,x ;adc FREQLO,x
;        sta FREQLO,x   ;HANDLE PITCH-DETUNING (DELAYED VIBRATO STILL APPLICABLE)
;        bcc retDetu
;        inc FREQHI,x
retDetu rts

BIGFX0E                 ;SIMPLE PULSEWIDTH-SETTING - INSTRUMENT TABLE CAN OVERRIDE IT
        and #$0F
        sta PWHIGHO,x
        rts

BIGFX0F                 ;'F' = FILTER-CUTOFF-HIGH BYTE SETTING
        .if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
         cpx #2*3*7
         bcc +
         sta CTFHGH3+1
         rts
         .fi
+       cpx #3*7
        bcc +
        sta CTFHGH2+1
        rts
        .fi
+       sta CTFHGHO+1
        rts

;------------------------------------------------------------------
BIGFX10                 ;SET MAIN SINGLETEMPO
        ora #$80        ;SINGLE TEMPO, NOT CONTINUED TO FUNKTEMPO OR TEMPOPROGRAM
MAINTMP                 ;ENTRY POINT USED BY OTHER SPEED-SETTERS
        
p_tmpt6 sta TEMPOTBL+0  ;1ST SPEED VALUE
        lda #0          ;RESET TRACK-TEMPOPOINTERS TO 1ST TEMPOTABLE-POSITION
MAINTM2
        .if feature.TEMPOPRGSUPP_ON
        sta TMPPOS+0
        sta TMPPOS+7
        sta TMPPOS+2*7
        sta TMPPTR+0
        sta TMPPTR+7
        sta TMPPTR+2*7
        .if (SID_AMOUNT>=2)
        sta TMPPOS+3*7+0
        sta TMPPOS+3*7+7
        sta TMPPOS+3*7+2*7
        sta TMPPTR+3*7+0
        sta TMPPTR+3*7+7
        sta TMPPTR+3*7+2*7         
        .fi
        .if (SID_AMOUNT>=3)
        sta TMPPOS+2*3*7+0
        sta TMPPOS+2*3*7+7
        sta TMPPOS+2*3*7+2*7
        sta TMPPTR+2*3*7+0
        sta TMPPTR+2*3*7+7
        sta TMPPTR+2*3*7+2*7         
        .fi
        .fi
        rts

BIGFX11                 ;SET MAIN FUNKTEMPO SPEED 1 AND SPEED 2
        .if feature.TEMPOPRGSUPP_ON
        pha
        and #$0F        ;SET 2ND FUNKTEMPO
        ora #$80        ;THE TEMPOPROGRA STOPS HERE
p_tmpt7 sta TEMPOTBL+1
        pla
        lsr
        lsr
        lsr
        lsr             ;/16
        bpl MAINTMP     ;SET 1ST FUNKTEMPO, RESET TABLEPOINTERS
        .fi

BIGFX12                 ;SET MAIN TEMPOPROGRAM
        .if feature.TEMPOPRGSUPP_ON
         .if (*+2+$7F>=RTBIGFX)
         beq RTBIGFX     ;NO TEMPOPROGRAM 0
         .else
         bne +
         jmp RTBIGFX
+        .fi
        tay
p_tmpp1 lda TEMPTRLO,y
        jmp MAINTM2
        .fi

BIGFX13                 ;SET TRACK SINGLETEMPO
        .if feature.TEMPOPRGSUPP_ON
TRAKTMP ora #$80        ;MAKE THE TEMPOPROGRAM STOP HERE
TRKTMP2 ldy TRKTMPOS,x  ;GET POSITION OF TRACKTEMPO IN TEMPOTABLE (2,4,6)
p_tmpt8 sta TEMPOTBL,y
        tya
TRKTMP3 sta TMPPTR,x
        sta TMPPOS,x
        rts
        .fi

BIGFX14                 ;SET TRACK FUNKTEMPO SPEED 1 AND SPEED 2
        .if feature.TEMPOPRGSUPP_ON
        pha
        lsr
        lsr
        lsr
        lsr             ;/16
        jsr TRKTMP2     ;Y is calculated inside this routine
        pla
        and #$0F        ;SET 2ND FUNKTEMPO
        ora #$80
p_tmpt9 sta TEMPOTBL+1,y ;SET 2ND FUNKTEMPO (Y CALCULATED IN SUBROUTINE PREVIOUSLY)
        rts
        .fi
;
BIGFX15                 ;SET TRACK TEMPOPROGRAM
        .if feature.TEMPOPRGSUPP_ON
        beq RTBIGFX
        tay
p_tmpp2 lda TEMPTRLO,y
        jmp TRKTMP3
        .else
        rts
        .fi

BIGFX16 = FORCVI2       ;SELECT VIBRATO-TYPE (USEFUL WHEN NEED INCREASING VIBRATO OCCASIONALLY)

BIGFX17                 ;?...LEFT FOR EXPANSION
BIGFX18                 ;?...LEFT FOR EXPANSION
BIGFX19                 ;?...LEFT FOR EXPANSION
BIGFX1A                 ;?...LEFT FOR EXPANSION
BIGFX1B                 ;?...LEFT FOR EXPANSION

BIGFX1C
        .if feature.FILTSHIFT_SUPP_ON
         .if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
         cpx #2*3*7
         bcc +
         sta FLSHIF3+1
         .fi
+        cpx #3*7
         bcc +
         sta FLSHIF2+1
         rts
         .fi 
+        sta FLSHIFT+1   ;SHIFTS OVERALL FILTER WITH THIS AMOUNT (80..$FF:NEGATIVE, 00..$70:POSITIVE)
        .fi
        rts

BIGFX1D                 ;'D' = DELAY TRACK BY $00..$FE FRAMES
        .if feature.DELAYSUPPORT_ON
DELAYER ldy TRDELAY,x
        beq cntDely     ;$00 SIGNS ENDING OF DELAYER-LOOP
                        ;PREPARINGING THE NEXT POSSIBLE DELAY
                        ;FINISH DELAY BY NOT DISTURBING SPDCNT,X

        iny             ;$FF SIGNS BEGINNING OF DELAYER LOOP
        bne keepDly
        sta TRDELAY,x   ;IF STARTING DELAY, WRITE ACCU TO DELAY-COUNTER
keepDly lda #2          ;THIS STOPS THE TRACK FOR A WHILE
        sta SPDCNT,x
cntDely dec TRDELAY,x   ;COUNTING DELAY
        rts
        .fi

BIGFX1E                 ;DELAY CURRENT NOTE BY $00..$FF FRAMES
        .if feature.DELAYSUPPORT_ON
        ldy TRDELAY,x
        bne goDelay     ;$00 SIGNS ENDING OF DELAYER-LOOP
        clc
        adc SPDCNT,x
        sta SPDCNT,x    ;COMPENSATE CAUSED DELAY BY INCREASING SPEED-COUNTER
goDelay jmp DELAYER
        .else
        rts
        .fi

BIGFX1F                 ;Set FiltSw/Reso register ($d417) ;filt-external on/off & resonance-setting (if ON, reduces SID-noise)
       .if feature.FILT_CTRL_FX_ON
        pha
        and #$0F
        .if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
         cpx #2*3*7
         bcc +
         sta FSWITC3+1
         rts
         .fi
+       cpx #3*7
        bcc +
        sta FSWITC2+1 
        jmp ++
        .fi
+       sta FSWITCH+1   ;now it's direct setting, resonance nybble can be overwritten by program, filt.ex. stays intact after set
+       pla
        and #$F0
        .if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
         cpx #2*3*7
         bcc +
         sta RESONI3+1
         rts
         .fi
         cpx #3*7
         bcc +
         sta RESONI2+1
         rts
        .fi
+       sta RESONIB+1
       .fi

RTBIGFX rts

;.text "---" ;only to check real exported player-size visually, to set in 'altplayers.inc' for startup-menu before release
player_top
        .here           ;restore physical address
playercode_end          ;copiers will know where it ends
.endm ;end of 'player' macro



;========================================== reloc-table, etc. =================================================
;Relocation table generator by Soci/Singular
;Basically the player code is compiled twice to different locations. This small macro
;compares the resulting binaries and creates relocation entries for the changed addresses.
;
;The BIGFXTABLE is handled specially and is therefore excluded from this relocation table.

relocm .segment

;==============================================================================
;this table tells which zeropage-pointer to load to operands of instructions given in 'DataPtr' above
;the number after means what value to add to (or substract from) the pointer
PtrValu 
        .if feature.SUBTUNESUPPORT_ON
         .byte subtuneadd,0, subtuneadd,0, subtuneadd,1, tempotbadd,1 ;p_subt1, p_subt2, p_subt4, p_tmpt5
         .byte subtuneadd,0 ; p_subt3
        .else
         .byte subtuneadd,0, subtuneadd,2, subtuneadd,4 ;not a final solution, these are pointers to pointers
         .if (SID_AMOUNT>=2)
          .byte subtuneadd,6, subtuneadd,8, subtuneadd,10
         .fi
         .if (SID_AMOUNT>=3)
          .byte subtuneadd,12, subtuneadd,14, subtuneadd,16
         .fi
         .byte subtuneadd,(SID_AMOUNT!=3)? SID_AMOUNT*8-2 : 30 ; p_subt3
         .if (feature.TEMPOPRGSUPP_ON!=0)
         .byte subtuneadd,(SID_AMOUNT!=3)? SID_AMOUNT*8-1 : 31 ; p_subt4
         .byte tempotbadd,1 ; p_tmpt5
         .fi
        .fi
        .byte ptnptloadd,0, ptnptloadd,0, ptnpthiadd,0, ptnpthiadd,0 ;p_ptnl1, p_ptnl2,  p_ptnh1, p_ptnh2
        .if feature.MULTISPEEDSUPP_ON
         .byte insptloadd,0, inspthiadd,0 ;p_insl1, p_insh1
        .fi
        .byte insptloadd,0, insptloadd,0, insptloadd,0 ;p_insl3, p_insl4, p_insl5
        .byte inspthiadd,0, inspthiadd,0, inspthiadd,0 ;p_insh3, p_insh4, p_insh5
        .if feature.HARDRESTYPES_ON
         .byte insptloadd,0, inspthiadd,0 ;p_insl2, p_insh2
        .fi
        
        .if feature.TEMPOPRGSUPP_ON
         .byte tempotbadd,$FF ;p_tmpt1; $FF means 'substract 1'
        .else
         .byte tempotbadd,0 ;p_tmpt1
        .fi
        
        .byte tempotbadd,0, tempotbadd,0 ;p_tmpt4, p_tmpt6
        
        .if feature.FASTSPEEDBIND_ON
         .byte tempotbadd,0, tempotbadd,0 ;p_tmpt2, p_tmpt3
        .fi
        
        .if feature.TEMPOPRGSUPP_ON
         .byte tempotbadd,1, tempotbadd,0, tempotbadd,1 ;p_tmpt7, p_tmpt8, p_tmpt9
         .byte tempoptadd,0, tempoptadd,0 ;p_tmpp1, p_tmpp2
        .fi
        
        .if feature.CHORDSUPPORT_ON
         .byte chordtbadd,0, chordtbadd,0 ;p_chdt1, p_chdt2
         .byte chordptadd,0, chordptadd,0, chordptadd,0, chordptadd,0 ;p_chdp1, p_chdp2, p_chdp3, p_chdp4
        .fi
        .byte 0         ;signs end of player-setter data 
PtrValu_end


;-----------------------------------------------------------------------------------
;addresses of absolute-instructions which point to music data (set to music-data in 'setplayer' even before relocation)
DataPtr .block
        .if feature.SUBTUNESUPPORT_ON
         .word player.p_subt1, player.p_subt2, player.p_subt4, player.p_tmpt5
         .word player.p_subt3
        .else
         .word player.p_seqt1,player.p_seqt2,player.p_seqt3 ;!!!!!!!!!! these should be the first entries in this table for good exporter.asm operation
         .if (SID_AMOUNT>=2)
          .word player.p_seqt4,player.p_seqt5,player.p_seqt6
         .fi
         .if (SID_AMOUNT>=3)
          .word player.p_seqt7,player.p_seqt8,player.p_seqt9
         .fi
         .word player.p_subt3
         .if (feature.TEMPOPRGSUPP_ON!=0)
         .word player.p_subt4, player.p_tmpt5
         .fi
        .fi
        .word player.p_ptnl1, player.p_ptnl2, player.p_ptnh1, player.p_ptnh2
        .if feature.MULTISPEEDSUPP_ON
         .word player.p_insl1, player.p_insh1
        .fi 
        .word player.p_insl3, player.p_insl4, player.p_insl5
        .word player.p_insh3, player.p_insh4, player.p_insh5
        .if feature.HARDRESTYPES_ON 
         .word player.p_insl2, player.p_insh2
        .fi
        
        .word player.p_tmpt1
        
        .word player.p_tmpt4, player.p_tmpt6
        
        .if feature.FASTSPEEDBIND_ON
         .word player.p_tmpt2, player.p_tmpt3
        .fi
        
        .if feature.TEMPOPRGSUPP_ON
         .word player.p_tmpt7, player.p_tmpt8, player.p_tmpt9
         .word player.p_tmpp1, player.p_tmpp2
        .fi
        
        .if feature.CHORDSUPPORT_ON
         .word player.p_chdt1, player.p_chdt2
         .word player.p_chdp1, player.p_chdp2, player.p_chdp3, player.p_chdp4
        .fi
        .word 0         ;signs end of data
        .bend
DataPtr_end

.if (COMPILEDAPP==2 && SWP_EXPORT!=0)
;.text "---" ;only to check real exported player-size visually, to set in 'altplayers.inc' for startup-menu before release
SWPplayer_top
.fi ;end of SWP-mode checking

;other relocation targets:
reloctable .block
        .proff
playr2 = \1
playr = \2
playraddr = \3
list .var ()
        .for i = 0, i < len(playr) , i = i + 1
         .if playr2[i] == playr[i]
         .elsif ((playr2[i]-playr[i])) & $ff == 1
          .if ((playraddr + i) < player.BIGFXTABLE) || ((playraddr + i) >= player.ENDBIGFXTB)
list       .var list..(playraddr+i-2,)
          .fi
         .else
          .error "Not possible to relocate."
         .fi
        .next
        .pron
         .word list
        .word 0         ;signs end of relocation data
        .bend

reloctable_end

.endm ;end of 'relocm' segment


;============================== SID2 address table ============================
.if (SID_AMOUNT>=2)
SID23ADDm .segment
        .proff
playrNonSID = \1
playrNormal = \2
playrNormalAddr = \3
list .var ()
        .for i = 0, i < len(playrNormal), i = i + 1
         .if playrNonSID[i] != playrNormal[i]
list      .var list..(playrNormalAddr+i-1,) ;points to low-byte of SID2 address
         .fi
        .next
        .pron
        .word list
        .word 0         ;signs end of data
 .endm ;end of 'SID23ADDm' segment
.fi


;******************************************************************************
; vim: sw=4 ts=4 syntax=asm:
