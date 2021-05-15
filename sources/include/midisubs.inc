;---------------------------------------------------------------------
;subroutines related to MIDI-controller hardware and 'HerMIDI-C64.asm' (only included if MIDI_support!=0)
; $Id: midisubs.inc 378 2014-04-22 19:42:01Z hermitsoft $
;---------------------------------------------------------------------

midisubs .block

.if (HerMIDI_support!=0)
HerMIDI=MIDIC64.HerMIDI
.fi

;-------------------------------------------------------------------------------
initMIDI .proc
CtrlCmd jmp MIDIC64.OpenDev  ;MIDIC64.Status holds MIDI-status on exit
        .pend

closeMIDI .proc
        lda #0
        sta jamnote
        ldx #CHN_AMOUNT-1
-       sta jamvibr,x
        ldy playadapter.mul7chn,x
        sta playadapter.pitchShiftLo,y
        sta playadapter.pitchShiftLo,y
        dex
        bpl -        
        jmp MIDIC64.CloseDev
        .pend


;-------------------------------------------------------------------------------
.if (HerMIDI_support!=0 && HerMIDI_TX_MODE==MIDIC64.HerMIDI.SynchMode)
HerMIDI_Synchron_GetData ;called from irq.inc
        lda MIDIdev
        beq retSync
        cmp #1     ;HerMIDI?
        bne retSync
        lda Config.confbits
        and #2 ;hiding rasterbar-display ON?
        bne +
        lda #11
        sta $d020
+       jsr MIDIC64.GetData ;get MIDI-events into C64's buffer (only HerMIDI uses this yet)
        lda #0
        sta $d020
retSync rts
.fi

chkMIDI ldx Config.MIDIchn
        cpx #Config.ChnOff
        beq retcMID
        ldx MIDIdev
retcMID rts

sendSt  lda #MIDI.StartSqPlay ;playback-start command to other MIDI-controlled sequencer
sendMID jsr chkMIDI
        beq retcMID
putMIDI ;ldx MIDIdev ;chkMIDI should always precede (sets X)  
        cpx #1     ;HerMIDI?
        beq retcMID
        jmp MIDIC64.PutData ;send out MIDI beat-clock (the beat of 1st SID-channel, others don't matter)


;-------------------------------------------------------------------------------
useMIDI .proc
        ;inc $d020
        jsr chkMIDI
        beq endMIDI
        lda playadapter.playmod
        beq +
        lda player.SPDCNT+0
        cmp #2     ;note-start? (after hard-restart)
        bne +
        ; lda #MIDI.TimingClock  ;send out MIDI beat-clock (the beat of 1st SID-channel, others don't matter)
        ; jsr sendMID
+       lda Config.confbits
        and #8 ;poly-mode?
        asl ;0/16
        asl ;0/32
        asl ;0/64 ;testable with 'bit' command
        sta polyswi
        lda curwind
        cmp #2
        beq +     ;if we're in instrument window, test note on 3rd channel to display it on scope
        lda subwpos1+0  ;current track
+       sta jamchan+1
        .if (HerMIDI_support!=0 && Vessel_support == 0 && HerMIDI_TX_MODE==MIDIC64.HerMIDI.AsyncMode)
        jsr MIDIC64.GetData ;get MIDI-events into C64's buffer (only HerMIDI uses this yet)
        .fi
;process the events
EventLp jsr MIDIC64.GetEvent  ;reads an event from the buffer to A,X,Y; Carry-flag=1 if read out
        bcs rtEvent ;if Carry=1 on exit buffer has been read out
        jsr useCmd
NextEvn jmp EventLp
rtEvent jsr calcPsh
        jsr setVibr
refresh lda #selfmod ;if fresh note was pressed this refreshes the 1st frame on SID
        beq endMIDI
        jsr player.iCOMMRE ;set common SID registers
endMIDI jsr commonsubs.disNote
        ;dec $d020
returns rts

;-------------------------------------------------------------------------------
useCmd  cmp #MIDI.OMNI_min ;check OMNI/system messages first
        bcc notOMNI
chStart cmp #MIDI.StartSqPlay
        bne chStop
        inc keyhandler.keyer.midstrt+1  ;jmp keyhandler.f1er.PlayBeg
	rts
chStop  cmp #MIDI.StopSeqPlay
        beq pauseco
chConti cmp #MIDI.ContinueSeq
        bne returns
pauseco jmp keyhandler.f3er.pauseco

notOMNI sta CmdStor+1
        and #$0f ;extract channel-nybble
        sta ChnStor+1
        lda Config.MIDIchn
        beq CmdStor ;if ALL/OMNI, don't check status-byte
        sec
        sbc #1 ;channel is 0..$F, not 1..$10 in MIDI-standard
ChnStor cmp #selfmod ;check if Channel matches
        bne returns ;retMIDI ;if no match, return from useCmd
CmdStor lda #selfmod
        and #$F0 ;eliminate channel-nybble

;--------------------------------------------------------------------       
chNotOn cmp #MIDI.NoteOn
        beq NoteOn
        jmp cNoteOf

NoteOn  inx ;first note's value is 1 in SID-Wizard
        cpx #SWM_NOTE_MAX+1
        bcs returns ;retMIDI
        cpy #0 ;on some keyboards NoteOn vith 0 velocity means Note-Off
        bne +
        jmp NoteOf2
+       bit polyswi
        bvs polyply ;in polyphonic mode there's no legato
        lda jamnote ;previous note
        bne legato ;if previous note nonzero, it's legato
freshnot inc refresh+1 
        stx jamnote 
        jsr setvelo ;input: Y
        lda jamnote
        jsr playadapter.jamplay.insini2
        jmp chEditM
legato  stx jamnote
        txa
        jsr playadapter.jamplay.insleg2
chEditM lda menumode
        bne returns
        lda curwind
        cmp #3 ;chord-table?
        beq + ;if we're in chord-table, don't modify base-note
        ldx jamnote
        stx display.BasNote+1 ;set chord base-note
+       lda jammode+1
        ora playadapter.followp+1 ;if follow-playing, don't allow typing notes into pattern
        ora ptrdysi     ;should wait for pattern-redraw if continuous keys pressed too fast
        bne retMID0
chWindw lda curwind
        cmp #3 ;chord-window
        bne +
        bit Config.confbits
        bvc retMID0     ;note-mode in chord-window?
wriChrd jsr commonsubs.getctpos ;get chordtable-position under cursor (position in X, value in A)
        cmp #$7e        ;don't overwrite end signal 1st digit
        beq appendc
        cmp #$7f        ;don't overwrite return signal 1st digit
        bne wriChd2
appendc jsr keyhandler.inskeye.inschd2
wriChd2 lda jamnote
        jmp datawriter.chwrchw.ChrdNot
+       ora subwpos2+0
        bne retMID0
wriPatt jsr commonsubs.setptnptr ;set pattern pointers in advance based upon current track/pattern position
        jsr commonsubs.compcur ;don't write value if end of pattern, overwrites Y!
        beq retMID0
        jsr keyhandler.SaveUndo.SaveUn2
        lda jamnote
        jmp datawriter.notewri.justwrn
polyply inc refresh+1
        stx jamnote
        jsr setvelo ;input: Y
        ldy #CHN_AMOUNT-1
chkfull lda polynotes,y
        beq + ;found empty place, not yet full
        dey
        bpl chkfull
        ldy #$80 ;will tell that no empty place remaining
+       sty emptpos+1
putnote ldy polycnt  ;set upcoming place
        dey
        bpl +
        ldy #CHN_AMOUNT-1
+       sty polycnt
.if (SID_AMOUNT>=2)
        lda spreadchn,y
        tay
.fi
        lda polynotes,y
        beq +
emptpos lda #selfmod
        bmi + ;if totally full, no chance, use upcoming place
        tay ;filling the empty gap
        sty polycnt
+       lda jamnote
        sta polynotes,y
        jsr playadapter.jamplay.chaNote
        jmp chEditM
retMID0 rts

setvelo sty velocity+1
        jsr commonsubs.setinsptr
        ldx #SWI_SR_POS
        jsr commonsubs.instpt1 ;check SR-value
        and #$F0
        cmp #$80 ;SUSTAIN-value where MIDI-jamming has velocity-sensing
        beq velocity
noInsFx lda #0
        sta jamvelo
        beq wrInsFx ;jump
velocity lda #selfmod ;0..$7F
        lsr ;0..$3F
        lsr ;0..$1F
        lsr ;0..$F
        bne +
        lda #1   ;don't allow 0 velocity
+       sta jamvelo
        ora #$50 ;$5x note-volume instrument-effect
wrInsFx sta playadapter.jamplay.InstrFx+1
        rts

;-------------------------------------------------------------------
cNoteOf cmp #MIDI.NoteOff
        bne chPrChg
NoteOff inx ;first note's value is 1 in SID-Wizard
NoteOf2 cpx #SWM_NOTE_MAX+1
        bcs retMID1
        inc refresh+1
        stx offnote+1
        jsr noInsFx ;switch off if maybe remaining CURIFX
        bit polyswi
        bvs polyOff ;in polyphonic mode there's no legato
        cpx jamnote
        bne leavprv
        ldx #0
        stx jamnote
        jmp playadapter.jamplay.aftern2
leavprv rts
polyOff ldy #CHN_AMOUNT-1
offnote lda #selfmod
        cmp polynotes,y
        bne +
        lda #0
        sta jamnote
        sta polynotes,y
        sty ystore+1
        jsr playadapter.jamplay.chanOff
ystore  ldy #selfmod
+       dey
        bpl offnote
retMID1 rts

;--------------------------------------------------------------------
chPrChg cmp #MIDI.PrgChange
        bne chPWhel
SetInst cpy #1
        bcc DispIns
        cpy #maxinstamount
        bcs DispIns
        sty display.selinst+1
DispIns inc display.insrefr+1 ;to display it
        rts

;---------------------------------------------------------------------
chPWhel cmp #MIDI.PitchWheel
        bne chAftrT
        tya ;00..$40..$7F
        asl ;00..$80..$FE
        sec
        sbc #$80 ;-$80..$00..+$7E
        sta jamslid+1
        rts

;---------------------------------------------------------------------
chAftrT cmp #MIDI.AfterTouch
        beq MIDIvib

;---------------------------------------------------------------------
chChnPr cmp #MIDI.ChPressure
        beq MIDIvib

;---------------------------------------------------------------------
chCCtrl cmp #MIDI.CommonCtrl
        bne retMIDI
chModWh cpx #MIDI.CC.ModWheel
        bne chBrigh
MIDIvib ldx #CHN_AMOUNT-1
-       tya
        asl
        bit polyswi
        bvs + 
        cpx jamchan+1
        beq +      
        lda #0
        sta previbr,x
+       sta jamvibr,x
        dex
        bpl -
        rts
chBrigh cpx #MIDI.CC.Brightness ;filter cutoff-frequency
        bne chVolum
        tya
        asl
.if (SID_AMOUNT>=3)
        ldx #2*3*7
        jsr player.iBRIGHT        
.fi
.if (SID_AMOUNT>=2)
        ldx #1*3*7
        jsr player.iBRIGHT        
.fi
        ldx #0*3*7
        jmp player.iBRIGHT
chVolum cpx #MIDI.CC.ChanVolume ;channel-volume setting
        bne cSndOff
        tya ;0..$7F
        lsr ;0..$3f
        lsr ;0..$1f
        lsr ;0..$F
        jmp player.volusub

cSndOff cpx #MIDI.CC.AllSoundOff
        beq +
cNotOff cpx #MIDI.CC.AllNoteOff
        bne chReset
+       jmp playadapter.resetJamIns

chReset cpx #MIDI.CC.ResetCtrler
        bne retMIDI
        jsr keyhandler.f1er.PlayBeg
        lda #0
        sta playadapter.playmod
        sta playadapter.followp+1 
        rts

;---------------------------------------------------------------------
retMIDI rts
        .pend


;----------------------------------------------------------------------
calcPsh ldy #(CHN_AMOUNT-1) ;calculate pitch-shifts for channels
PshiLop ldx playadapter.mul7chn,y
jamslid lda #selfmod    ;pitchweel slide-value
        bne +
noslide sta playadapter.pitchShiftLo,x
        jmp wriPsHi
+       bpl pluslid
minusld eor #$ff
pluslid asl ;$00..$fc (positive half)
        and #%11111000  ;shifted '$00..$1F'
        sta zpvari0
        lda playadapter.discretePitch,x
        beq noslide
        bit polyswi    ;if mono-mode, pitch is shifted only for current channel
        bvs +
jamchan cpy #selfmod   ;check if processing current channel
        bne advachn ;noslide
+       tax
        bit jamslid+1
        bpl +
        dex
        dex
+       jsr player.rDiffLo
        sta ptnzptr
        jsr player.rDiffHi ;ptnzptr+A*256 = $20..$1b10
        lsr ;/2
        ror ptnzptr ;$10..$D88
        lsr ;/2
        ror ptnzptr ;$0008..$06C4  ;max result= $06C4 * $20 = $D880, will fit into 16bit
        sta addAcc2+1
mulAwhl ldx ptnzptr+0   ;'peasant' multiplication of 11bit frequency-difference & 5bit pitchwheel-shift
        dex
        stx addAccu+1
        lda #0          ;we'll accumulate result low-byte in Accumulator
        ldx #5          ;loop to test all 5 digits of the pitch-shift value 
-       asl             ;shifting (partial) result low-byte
        rol zpvari0     ;shifting (partial) result high-byte and setting Carry-flag to the next bit of pitchwheel-shift 
        bcc +           ;Carry is the currently tested digit of pitchwheel, if 1 we accumulate freq.diff, else discard it  
addAccu adc #selfmod
        pha
        lda zpvari0     ;used as multiplier (MSBs) and result high-byte accumulator at the same time
addAcc2 adc #selfmod
        sta zpvari0
        pla
+       dex
        bne -    ;0..$d880
        lsr zpvari0     ;after multiplication we reduce the value to the desired range (whole-note difference)
        ror ;/2  ; 0..$6c40
        lsr zpvari0
        ror ;/4  ; 0..$3620
        lsr zpvari0
        ror ;/8  ; 0..$1b10
        ldx playadapter.mul7chn,y
        bit jamslid+1
        bpl +
        eor #$ff
+       sta playadapter.pitchShiftLo,x
        lda zpvari0
        bit jamslid+1
        bpl wriPsHi
        eor #$ff
wriPsHi sta playadapter.pitchShiftHi,x
advachn dey
        bpl PshiLop
        rts

;--------------------------
setVibr ldx #(CHN_AMOUNT-1)*7 ;set vibrato-amplitudes for all channels
VibAlop ldy playadapter.div7chn,x
        lda midisubs.jamvibr,y
        beq +
        lda player.SLIDEVIB,x
        bpl + ;if legato mode, force vibrato after legato if needed
        lda #$10 ;code of normal vibrato
        sta player.SLIDEVIB,x
        jmp ++
+       lda player.VIBFREQU,x ;determine whether a whole sinus has been spent already
        lsr             
        lsr
        cmp player.VIBRACNT,x  
        bne ++
        lda midisubs.jamvibr,y
        bne histovi
        cmp previbr,y         ;check if finishing existing vibrato
        beq ++
histovi sta previbr,y
+       jsr player.iVIBAMP
        ;ldy div7chn,x
        ;lda #0 ;forget vibrato-setting after it was performed
        ;sta midisubs.jamvibr,y
+       txa
        sec
        sbc #7
        tax
        bpl VibAlop
retVibA rts


;-------------------------------------------------------------------------------
jamnote .byte 0
jamvelo .byte 0
jamvibr .fill CHN_AMOUNT,0
previbr .fill CHN_AMOUNT,0
polyswi .byte 0 ;0/$80

polycnt .byte 0 ;counts&cycles polyphonic tracks
polynotes .fill CHN_AMOUNT,0
.if (SID_AMOUNT==2)
spreadchn .byte 2,5,1,4,0,3
.elsif (SID_AMOUNT==3)
spreadchn .byte 2,8,5,1,7,4,0,6,3
.fi

;-------------------------------------------------------------------------------

.bend
