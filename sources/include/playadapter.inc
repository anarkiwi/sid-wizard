;ident:8,24
;=======================================================================================
;Player-adapter - ensures sophisticated player and editor communication (flexibility)
; $Id: playadapter.inc 387 2014-07-09 21:35:08Z hermitsoft $
;---------------------------------------------------------------------------------------

playadapter .block ;private stuff

padapter .proc          ;ensures refreshment of editor-counters
        ldx #CHN_AMOUNT-1 ;check all the 3 channels: 2..0
-       ldy mul7chn,x
        lda player.SEQPOS,y ;played pattern on channel 2..0 in player
        cmp prevspo,x   ;check, if played pattern changed
        beq +           ;skip if not changed
        inc seqrefr     ;if changed, tell editor
+       sta prevspo,x   ;store current data to temporary storage for further comparison
        lda player.TMPPOS,y ;check if speed-counter reaches tempo
        tay
        bit TEMPOPRGSUPP
        bmi +
        ldy #0          ;pre-select - if no tempo-program support, the 1st place of tempo-table is used
+       lda TEMPOTBL,y
        and #$7f
        beq +           ;if tempo is 0
        ldy mul7chn,x
        cmp player.SPDCNT,y
        bne +           ;if SPDCNT is max., increment in editor's pattern-row
        lda playmod
        beq +           ;if playback stopped, don't dare to count, my little wizard!
        inc ptrowpos,x  ;ONLY FOR EDITOR to display position easily
+       ldy mul7chn,x
        lda player.PTNPOS,y
        bne +           ;if pattern-play position is zero, reset editor's pattern-row
        lda #0
        sta ptrowpos,x  ;ONLY FOR EDITOR to display position easily
+       lda playmod
        beq advplad     ;if jamming mode don't mute any tracks
        lda mutesolo,x
        and #1
        bne advplad
.if (SID_AMOUNT>=2)
         .if (SID_AMOUNT>=3)
         cpy #2*3*7
         bcc +
         tya
         sec
         sbc #2*3*7-5
         tay
         lda menu.SID3add+0
         sta ptnzptr+0
         lda menu.SID3add+1
         jmp wADSR23
+        .fi
        cpy #3*7
        bcc +
        tya
        sec
        sbc #3*7-5      ;'5' is AD-register position for SID2
        tay
        lda menu.SID2add+0
        sta ptnzptr+0
        lda menu.SID2add+1
wADSR23 sta ptnzptr+1
        lda #0
        sta (ptnzptr),y
        iny             ;point to SR-register of SID2
        sta (ptnzptr),y
        jmp advplad
.fi
+       sta SIDBASE+6,y ;A=0 here -> immediate muting of long-released instruments
        sta SIDBASE+5,y
advplad dex
        .if ( *+2-$80 <= - )
        bpl -
        .else
        bmi +
        jmp -
+       .fi
        lda playmod
        beq +
plynote lda #selfmod
        beq +
        sta display.BasNote+1
+       rts
        .pend

dispScope .proc         ;display oscilloscope
        .if OSCILLOSCOPE==1 ;buffer signal-data for oscilloscope
        lda PALNTSC
        bne +
        ldx framesp
        cpx #6
        bmi +
        jmp endscop     ;don't display scope and take rastertime in higher NTSC speeds 
+       eor #1
        sec
        sbc IRQ.fspdctr+1
        ora IRQ.fastfwd+1 ;if fast-forward is on no scope-display at all anywhere
        bne endscop     ;if PAL machine, scope can be displayed on 1st raster, 
        jsr player.rFSWITC ;lda player.FSWITCH+1 ;check if channel 3 is filtered
        and #4
        sta osc3fil+1
        beq +           ;if no filter, don't calculate anything
        jsr player.rFLTBND ;lda player.FLTBAND+1 ;check if filter is lowpass/highpass
        and #$10        ;check lowpass-bit
        sta filpass+1
        lda CTFMETR+1   ;value of CUTOFF-METER DISPLAY FOR EDITOR
        lsr             ;0..127
        lsr             ;0..63 - fits into Exponential table of player (used for multiplication)
        adc #34         ;correct curvature
        cmp #64         ;maximum allowed value to prevent signal overload
        bmi +
        lda #64         ;to fine-tune the 8580 SID-filter emulation
+       sta filfreq+1
        tay
p_EXPT1 lda player.EXPTABH+8,y ;leave 8 spaces below in exp.table for negative signal-values
        sta flbasep+1 ;and at the same time, don't use unprecise values at the beginning
+
        lda #8          ;initialize software-filter (emulates filter for scope)
        sta capstore
        ldx #0          ;help displayer2 to display oscilloscope
oscoplp lda SIDBASE+$1b ;OSC3 (oscillator of 3rd channel)
        eor #$ff        ;mirror Y axis if needed for correct display (value=255-value)
        lsr             ;0..127
        lsr             ;0..63
        lsr             ;0..31
        lsr             ;0..15 ;local region of exp-table less distorted - used for multiplication
        sta signale+1
osc3fil ldy #selfmod    ;zero if channel 3 is not filtered
        beq nofilt      ;if channel 3 not filtered, no need to emulate
        sec
        sbc capstore    ;calculate difference of current signal and the previous (1 frame before)
        clc             ;range from $f0..$0f to $00..$1f
        adc #16         ;0..1f
        lsr             ;$00..$0f  - INPUT 1 (MULTIPLICAND)
        clc
filfreq adc #selfmod    ;Multiplier/divisor (a matter of perspective which one)
        tay
p_EXPT2 lda player.EXPTABH,y ;FREQHI,y ;;;
        sec
flbasep sbc #selfmod
modisig clc             ;modify signal by filtered difference
        adc capstore
        sta capstore    ;store output value into buffer - used by software-filter emulation
filpass ldy #selfmod    ;$10 if lowpass, otherwise 0
        bne +           ;if hi-pass, substract low-passed filtervalue from original signal
signale lda #selfmod    ;OSC3-scaled signal
        sbc capstore
        adc #9          ;shift up if there's negative in the difference
+
nofilt  sta OSC3Buffer,x
        inx
        cpx #12
        bne oscoplp
        lda SIDBASE+$1c ;ENV3 (envelope of 3rd channel)
        beq +           ;if 0, no need to calculate
        lsr             ;0..127
        lsr             ;0..63 - fits into Exponential table of player (used for multiplication)
        adc #10         ;correct curvature
        cmp #64         ;maximum allowed value to prevent signal overload
        bmi +
        lda #64         ;to fine-tune the 8580 SID-filter emulation
+       sta ENV3sto+1   ;in 'displayer2'
        .fi
endscop rts
        .pend

;-----------------------------------------------
        .section data

capstore .byte 0        ;previous frame value used by capacitor

playmod  .byte $0       ;if 0, no play (edit/jam), if 1 - play tune, if 2 - patternplay
mul7chn  .byte 7*range(CHN_AMOUNT)  ;bytes for easy multiplication with 7 (convert player's X indexing to editor's)
prevspo  .fill CHN_AMOUNT ;temporary storage to store previous sequence-positions
ptrowpos .fill CHN_AMOUNT ;the row played by the player - ONLY NEEDED FOR EDITOR to display
seqrefr  .byte 1        ;signs, if orderlist-display needs refreshment
skiptrk  .fill CHN_AMOUNT ;>$80 value is used to skip tracks if they're out of range (when F1/F2 pressed and tune is initialized)

maxrasti .byte 0        ;calculated max. reached rastertime since playback started (stopping the playback should reset it)

;opcode-data needed to alter player-code
ptnpcod jmp player.iPTNPLY            ;code to player for pattern-playing mode
norpcod .byte $BC, <player.SEQPOS, >player.SEQPOS ;ldy SEQPOS,x ;code to player for normal playing mode

;constants/variables based on 0/7/14 X indexing - for player-editor cooperation
PWHIHELP  .byte 0       ;HELP FOR EDITOR WITH GIVING CORRECT VALUES OF PULSEWIDTH HI-NIBBLE
playerins .byte 1       ;when stop pressed, current instruments (CURINS,X) is saved here, then restored when continued
div7chn   .byte 0       ;convert 0/7/14 indexing to 0/1/2 indexing (and 21/28/35 for 2SID into 3/4/5)
wasjamm   .byte 0       ;tells player after stop that there was a jammed note already on the channel
discretePitch .byte 0   ;set by player for calculations
pitchShiftLo  .byte 0   ;help for player, calculated pitch-difference low-byte for pitch-wheel shifting
pitchShiftHi  .byte 0   ;help for player, calculated pitch-difference hi-byte for pitch-wheel shifting
bunch23   .byte 0,1,1,0,0,0,0, 0,1,2,0,0,0,0
.if (SID_AMOUNT>=2)
bunch456  .byte 0,1,3,0,0,0,0, 0,1,4,0,0,0,0, 0,1,5,0,0,0,0
.fi
.if (SID_AMOUNT>=3)
bunch789  .byte 0,1,6,0,0,0,0, 0,1,7,0,0,0,0, 0,1,8,0,0,0,0
.fi

olstart .fill CHN_AMOUNT,0     ;orderlist F2key-playing startpositions

        .send

;=======================================================================================
inisubb .proc           ;player-adapter version of initer ($1000) of player routine
        ldx #CHN_AMOUNT-1
-       lda #0
        sta ptrowpos,x  ;reset the row played by the player
        sta skiptrk,x   ;allow all tracks by default, INITER will probably disable zero-length or overridden tracks
.if (MIDI_support!=0)
        sta midisubs.jamvibr,x
        sta midisubs.previbr,x
.fi
        dex
        bpl -
        sta padapter.plynote+1
        .if (SID_AMOUNT>=2)
        jsr menu.WrSID2add
        .fi
        .if (SID_AMOUNT>=3)
        jsr menu.WrSID3add
        .fi
        lda selsubt+1
        jsr player.inisub
        jsr commonsubs.resetimer
        jmp resetJamIns ;good for instruments which has PW/Filter-reset switched off
        .pend

playsubb .proc          ;player-adapter version of player ($1003) parto of driver/player routine
        lda #$A0 ;LDY
        jsr player.wCHDMOD
        lda $d012
        sta storast+1

        jsr player.playsub

        lda $d012
        ldy #0          ;show real rastertime-bar of player
        sty $d020
        sec             ;calculate current frame's singlespeed-rastertime (cursor-sprite can increase it)
        sbc #(SID_AMOUNT==1)? 5: (SID_AMOUNT==2)? 10 : 15 ;substract the time taken by editor & CPU-cycle eaters: badlines and possible sprite-cursor during playback-rasters
storast sbc #selfmod    ;raster-row before, if calculations are correct, should match measures of 'exe' export
        cmp maxrasti    ;maxrasti=max(Accu,maxrasti)
        bcc +
        sta maxrasti
+       jsr commonsubs.timecount ;runtime:1 rasterline; still counts the timer to follow the music timeline
        rts
        .pend

;-------------------
resetune .proc          ;request 'reset/init tune' after subtune-selection, or after loaded (or maybe full stop)
        jsr reseply
resptsq ldx #CHN_AMOUNT-1
-       ldy #0        
        sty seqdpos+1
        sty subwpos2+1  ;beginning of orderlist
        sta ptrowpos,x
seqread jsr SEQSUBb
        beq invalip     ;if pattern-value is 0 somehow
        cmp #maxptnamount+1
        bcc respatn     ;if pattern value is valid
        cmp #$fe        ;is it hardwired sequence-delimiter?
        bcs invalip     ;not to switch to invalid pattern for empty subtunes
        iny             ;if there's SEQ-Fx at the position check for next position
        bne seqread     ;safety jump - if no valid pattern found yet
respatn sta curptn1,x
        lda #0
        sta prowpos,x
        stx subwpos3+0  ;beginning of pattern
skipres dex
        bpl -
        rts
invalip lda #1          ;assign pattern 1 if no better selection
        ldy curptn1,x   ;problem with read value, check if current selected pattern is invalid, if invalid, assign pattern 1
        beq respatn     ;if zero, reset to 1
        bmi respatn     ;if above $80, reset to 1
        jmp skipres     ;skip resetting to 1 if legal patterns are selected
        
reseply .if (SID_AMOUNT>=2)
        jsr menu.WrSID2add
        .fi
        .if (SID_AMOUNT>=3)
        jsr menu.WrSID3add
        .fi
        ;.if (PWRESETSW_ON+FILTRESETSW_ON)
        jsr resetJamIns ;good for instruments which has PW/Filter-reset switched off
        ;.fi
        lda selsubt+1
        jsr player.iSETSTU ;SET SUBTUNE (PLAYER-ROUTINE)
        inc inirequ+1   ;request init from main routine - direct init will screw up IRQ
        inc seqrefr     ;refresh sequence-display
        ldx #CHN_AMOUNT-1
-       lda #0
        sta playmod     ;stop playback after switching subtune
        sta maxrasti    ;reset rastertime-measuring
        sta followp+1
        sta IRQ.fspdctr+1
        sta prevspo,x
        dex
        bpl -
        rts
        
;------------------
SEQSUBb lda mul7chn,x   ;editor's version of SEQSUB - X is transformed for player then restored, Y intact
        stx mul7st1+1   ;store X
        tax
        jsr player.iSEQSUB ;get data at 1st positions of orderlist sequences into Accumulator
mul7st1 ldx #selfmod    ;restore X
        cmp #0          ;reget status flags for accu
        rts
        .pend


;-------------------------------
ptplyon .proc
        ldx #2             ;set patternplay mode in player by overwriting an instruction and operand
-       lda ptnpcod,x
        jsr player.wPtncod ;sta player.ptncode,x ;overwtrite instruction in player-routine
        dex
        bpl -
        rts
        .pend

ptploff .proc
        ldx #2             ;switch-off pattern-playing mode
-       lda norpcod,x
        jsr player.wPtncod ;sta player.ptncode,x ;overwtrite instruction in player-routine
        dex
        bpl -
        rts
        .pend
;=======================================================================================
followp = followplay.followp

followplay .proc        ;if follow-mode is on, this subroutine overrides key navigation and controls patterns
followp lda #selfmod    ;followplay signal, if 0-no follow, if 1-follow orderlist by patterns (actptn)
        beq retpnt0
        bit Config.confbits
        bvc +
plychrd lda #1          ;self-modified by player
        beq +           ;safety-check, don't allow 0
        sta display.selchor+1   ;adjust chord
        jsr keyhandler.chdzero  ;display new chord
+       ldx #CHN_AMOUNT-1
-       ldy mul7chn,x
        lda player.CURPTN,y ;help from player
        beq SkiPPos
        cmp #maxptnamount+1
        bcs SkiPPos     ;not to switch to invalid pattern for empty subtunes
        ldy skiptrk,x
        bmi skiptst
        sta curptn1,x   ;go to currently played patterns
skiptst lda ptrowpos,x  ;help from player
        sec
        sbc #12         ;row at half of the screen, stay in focus
        bcs +
        lda #0
+       sta folsto1+1

        ldy curptn1,x
        lda ptnlength,y
        sec
        sbc #12         ;row at half of the screen, stay in focus
        cmp ptrowpos,x
        bcc SkiPPos
folsto1 lda #0          ;self-written
        cmp #maxptnlen
        bcs SkiPPos     ;don't allow invalid positions
        sta prowpos,x
SkiPPos dex
        bpl -
        ldx subwpos1+0  ;current track
        lda ptrowpos,x
        sec
        sbc prowpos,x
        bcc retpnt0     ;don't allow cursor go above row 0 in display area
        cmp #25
        bcs retpnt0     ;but in case, don't allow cursor to go through screen
        sta subwpos3+0
retpnt0 rts
        .pend

;===========================================================================
pause .proc             ;pause playback ;in player the patternplay-code (overwritten by playadapter) was altered, no need to resume playermode
        ldx #(CHN_AMOUNT-1)*7 ;release notes
-       lda player.CURINS,x ;store currently played instruments
        sta playerins,x
        txa
        sec
        sbc #7
        tax
        bpl -
        jsr resetJamIns
        rts
        .pend
;-----------------
resetJamIns .proc
        ldx #(CHN_AMOUNT-1)*7 
-       lda display.selinst+1 ;and replace them with currently selected instrument
        sta player.CURINS,x
        jsr adapins     ;reset X-channel current instrument's pointers and ADSR
        lda #0
        sta wasjamm,x
        txa
        sec
        sbc #7
        tax
        bpl -
        rts
        .pend
;----------------------
continue .proc          ;continue playback if F4 was pressed after pause
        ldx #(CHN_AMOUNT-1)*7 ;if continuing playback is started
-       lda playerins,x ;restore player's instruments which were stored when tune was stopped
        sta player.CURINS,x
        jsr adapins     ;reset X-channel current instrument's pointers and ADSR
        txa
        sec
        sbc #7
        tax
        bpl -
        rts
        .pend
;----------------
adapins ;reset X-channel current instrument's pointers and ADSR
        lda #0
        sta player.CURIFX,x   ;stop any running instrument-effects
        sta player.CURFX2,x   ;stop any running pattern-effects
        sta player.SLIDEVIB,x ;stop any running portamento effects
;        .if (PWRESETSW_ON+FILTRESETSW_ON)
        lda #1
        sta player.CURNOT,x   ;cause new note to be reset
        ldy #%00111111  ;CAUSE FILTER&PW-TABLE TO RESET AT INSTRUMENT-SELECTION
;        .fi
        jsr player.iCHKNOT
        lda #0
        sta SIDBASE+5,x ;stop sounds when stopping tune
        sta SIDBASE+6,x
        lda #$08
        sta SIDBASE+4,x ;1st jammed sound should start well
        sta player.WFGHOST,x
        lda #$FE
        sta player.PTNGATE,x
        lda #GATEOFFX
        sta player.CURNOT,x ;cause new note to be reset
        rts

;=======================================================================================
jamnote = jamplay.jamnote
jambuf = jamplay.jambuf

jamplay .proc           ;player caller for keyboard-jamming (called if 'playmod'=0)
        lda #$A0 ;LDY
        bit Config.confbits
        bvc + ;note-mode in chord-table?
        ldx curwind
        cpx #3 ;chord-window?
        bne +
        lda #$A9 ;LDA - disables chord-playing in chord-window if note-mode is used
+       jsr player.wCHDMOD 
jamnote lda #selfmod    ;note hit by user
        bne jambuf
        jmp afternote   ;if no note was pressed
jambuf  ldx #selfmod    ;note stored in previous frame - to determine instrument-start
        beq insinit     ;start instrument (tick2) if transition from 'note-released' to 'note presse'
        cpx jamnote+1   ;if note-value changed it's legato
        beq inscont     ;if note-value not changed, CNTPLAY
        ;legato (pressing notes on keyboard without rest inbetween)
inslega jsr commonsubs.calcoct ;add octave*12
        bmi insleg2
        rts
insleg2 ldy curwind
        cpy #2
        beq +           ;if we're in instrument window, test note on 3rd channel to display it on scope
        ldy subwpos1+0  ;current track
+       ldx mul7chn,y
        sta player.CURNOT,x
.if (MIDI_support!=0)
        lda #0
        sta midisubs.jamvibr,y
.fi
        lda #$3f        ;legato effect code for instrument-column
        sta player.CURIFX,x
        ;lda #$7F        ;BIG SPEED FOR PORTAMENTO IS EQUAL TO LEGATO
        ;sta player.FREQMODH,x  ;MAXIMAL PORTAMENTO-SLIDE
        jsr dotick2     ;perform notestart
        jmp dojam

insinit jsr commonsubs.calcoct ;add octave*12
        bpl jamretu
        ldy #0
        sty InstrFx+1
        jsr insini2
        jmp dojam
insini2 ldy curwind
        cpy #3 ;chord-table?
        beq +  ;if we're in chord-table, don't modify base-note
        sta display.BasNote+1 ;set base-note for chord-editing
        cpy #2
        beq chaNote     ;if we're in instrument window, test note on 3rd channel to display it on scope
+       ldy subwpos1+0  ;current track
chaNote ldx mul7chn,y
        sta player.CURNOT,x
        lda display.selinst+1 ;selected instrument
        sta player.CURINS,x
InstrFx lda #selfmod ;place for 'SUSTAIN' change fx $5x aka MIDI-note 'velocity'
        sta player.CURIFX,x
dotick2                 ;perform notestart (tick2)
;        .if (PWRESETSW_ON+FILTRESETSW_ON)
        ldy #%11111111  ;Not reselecting instrument, not restarting filter/pw if their reset is switched off
;        .fi
        lda #$FF        ;ensure unmuting
        sta wasjamm,x
        jmp player.iCHKNOT
        
inscont lda playmod
        bne jamretu
        ldx #(CHN_AMOUNT-1)*7 ;continuation of pressed note
-       lda #0
        sta player.CURIFX,x
        sta player.CURFX2,x
        jsr player.iCNTPLY
        txa
        sec
        sbc #7
        tax
        bpl -
dojam   jsr player.iCOMMRE ;set common SID registers
        lda jamnote+1
        sta jambuf+1
jamretu rts

afternote               ;if no key pressed
        ldx jambuf+1
.if (MIDI_support!=0)
        bne +           ;check with previous note-value if transition from 'note pressed' to 'note released' happened
        ldx midisubs.useMIDI.refresh+1
        beq inscont
        lda #0
        sta midisubs.useMIDI.refresh+1
        rts
.else
        beq inscont
.fi
+       jsr aftern2
        jmp dojam
aftern2 ldy #2          ;pre-selection of Y for last track (preferred because it can be displayed on Oscilloscope)
        cpy curwind
        beq chanOff     ;if we're in instrument window, jamming happens on 3rd track
        ldy subwpos1+0  ;current track
chanOff ldx mul7chn,y
        lda #GATEOFFX   ;$7e ;do a 'gate-off' effect
        sta player.CURNOT,x
;        .if (PWRESETSW_ON+FILTRESETSW_ON)
        ldy #%11111111  ;Not reselecting instrument, not restarting filter/pw if their reset is switched off
;        .fi
        jsr player.iCHKNOT
        lda Config.confbits    ;if dovetailing mode is ON cycle tracks
        and #$10
        beq +
        lda curwind
        bne +                  ;do it only in pattern-editor
        jsr keyhandler.tabptfw ;go to next track upon pressing note
+       rts

        .pend
        .bend
;=======================================================================================
; vim: sw=4 ts=4 syntax=asm:
