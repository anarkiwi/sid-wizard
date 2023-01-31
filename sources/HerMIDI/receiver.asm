;ident:8,24
;=======================================================================
;C64-side receiver-code example for HerMIDI (through IEC Serial port)
;-----------------------------------------------------------------------
;2015 Hermit Software Hungary (Mihaly Horvath) hermitsoft@users.sourceforge.net
;-----------------------------------------------------------------------

VRAM=$0400
CRAM=$d800
Logging_base = $2000
Logging_top  = $A000

zpv1=4
chancnt=5
zpt1=6
logptr=8

selfmod=$00 ;default value for self-modified code's immediate operands
PacketSize_Sync = HerMIDI.MaxBuffSize-1 ;can be less if that's enough
PacketSize_Async= 6*3 ;slower mode, must need rastertime for display

SID_base=$d400
;SID_AMOUNT=1 ;number of available SID chips for playback of notes
SID_CHN_MAX=3 ;*SID_AMOUNT ;number of available SID-channels
ARP_LEN_MAX=4 ;maximum arpeggio length to use on a SID-channel

;Override HerMIDI library defaults
HerMIDI.INC_SYNCHR = 1  ;if nonzero: synchron-mode READ gets included
HerMIDI.INC_DIRECT = 1  ;if nonzero: DirectMode READ (DirecRx) gets included
HerMIDI.INC_ASYNCH = 1  ;if nonzero: asynchron-mode READ gets included
HerMIDI.INC_EVENTS = 1  ;if nonzero: 'GetEvent' subroutine gets included


;-----------------------------------------------------------------------
;BASIC starter code:
        *= $0801
        .word ss,0
        .null $9e,^start ;"SYS" BASIC INTSRUCTION
ss      .word 0
        
start   sei
        lda #$35
        sta 1
        lda #<dummyirq
        ldx #>dummyirq
        sta $fffa
        stx $fffb
        lda #<(Logging_top-$100)
        ldx #>(Logging_top-$100)
        sta logptr+0
        stx logptr+1
initlog ldy #0
-       lda #HerMIDI.MIDI.Undefined ;used as gap between frames
-       sta (logptr),y
        iny
        bne -
        dec logptr+1
        lda logptr+1
        cmp #>(Logging_base-$100)
        bne --
        lda #<Logging_base
        ldx #>Logging_base
        sta logptr+0
        stx logptr+1
init    jsr scrinit
        jsr InitSID
        
;HerMIDI initializtion:-------------------------
CtrlCmd lda #HerMIDI.SynchMode+PacketSize_Sync ;ControlCmd 
        jsr HerMIDI.PowerON ;Carry-flag & Status hold MIDI-status on exit
;End of HerMIDI initialization------------------
        
        jsr dispStatus ;uses Carry-flag and 'Status' to display
        
framelp ldx modenum+1
        lda moderast,x
        sta rastsub+1
        cmp $d012
        bne *-3

        lda #$ff
        sta $d020
        
;HerMIDI data-request----------------------------
        jsr HerMIDI.GetData    ;-----------------
;End of HerMIDI data-request---------------------
        ;jsr FakeEvents ;used only for offline-testing of GetEvent routine
        
        lda $d012
        sec
rastsub sbc #0
        sta rastime

chDirec ldy HerMIDI.ControlCmd
        cpy #HerMIDI.DirectMode
        bne +
        ldx #120 ;just a test how the direct signals are coming
-       jsr HerMIDI.GetData
        rol HerMIDI.EventBuffer+0 ;storing Carry-flag 
        rol $d020
        dex
        bne -
        
+       lda #0
        sta $d020
        
;        inc $d020
        lda logptr+1
        cmp #>Logging_top
        bcs +
        lda HerMIDI.BuffIndex
        beq +
        ldy #0 ;logging the messages byte-by-byte into memory
-       lda HerMIDI.MIDIbuffer,y
        sta (logptr),y
        iny
        cpy HerMIDI.BuffIndex
        bne -
        iny ;mark the places of frames between buffer-readouts
        tya
        clc
        adc logptr+0
        sta logptr+0
        lda logptr+1
        adc #0 ;add Carry
        sta logptr+1
+       ;dec $d020
        
        jsr processEvents ;check for notes/effects, store them for player

        ;inc $d020
        jsr PlaySID ;player routine

        ;inc $d020
        jsr displayMIDI ;displays MIDI buffer
       
        ;lda #9
        ;sta $d020

        lda #$00
        sta $dc00
        lda #$ff
        cmp $dc01 ;check keypress
        beq framelp
        
;HerMIDI shut-down requet-------------------------
        jsr HerMIDI.PowrOFF  ;--------------------
;End of HerMIDIshut-down request------------------
        
modenum ldx #selfmod
        inx
        cpx #(modelist_end-modelist)
        bmi +
        ldx #0
+       stx modenum+1
        lda modelist,x
        sta CtrlCmd+1
        
        lda #$ff
        cmp $d012 ;avoid button-bouncing
        bne *-3
        ;lda #$ff
        cmp $dc01 ;wait for key-release
        bne *-3
        jmp init
rastime .byte 0 ;measured rastertime of transfer for a MIDIevent package
modelist .byte HerMIDI.SynchMode+PacketSize_Sync
         .byte HerMIDI.AsyncMode+PacketSize_Async
         .byte HerMIDI.DirectMode
modelist_end
moderast .byte $ff,$3e,$4e ;starting rasterlines of different Tx-modes
;(line $ff for Synchron-mode coz it has no badlines & stolen CPU-cycles)

dummyirq rti


;=======================================================================
.align $100 ;avoid page-crossings if possible
HerMIDI .binclude "HerMIDI-C64.asm";all functions needed to control/use HerMIDI
MIDI    = HerMIDI.MIDI  ;export MIDI constants

;===============================MISC.===================================
;fake events to test GetEvents routine
fakeventON .byte $FE, $92,$20,$5A,          $30,$5B, $40,$5C ;'running status' mode simulation
fakeventON_end
fakeventOFF .byte     $92,$20,$00, $FE, $82,$30,$25, $40,$10 ;'running status' mode simulation
fakeventOFF_end
fakecnt .byte 0 ;for timing of fake events
fakerate=50

FakeEvents ;mimic GetData with the fake events, only used for offline-simulation 
        ldx #0
        stx HerMIDI.GetEvent.rdIndex+1
        dec fakecnt
        bpl +
        lda #fakerate
        sta fakecnt
+       ldx fakecnt
        beq FakeOff
        cpx #fakerate/2
        beq FakeOn
noFake  ldy #0
        jmp between ;if not 0 and middle, simply return
FakeOn  ldy #0
-       lda fakeventON,y
        sta HerMIDI.MIDIbuffer,y
        iny
        cpy #(fakeventON_end-fakeventON)
        bne -
        jmp between
FakeOff ldy #0
-       lda fakeventOFF,y
        sta HerMIDI.MIDIbuffer,y
        iny
        cpy #(fakeventOFF_end-fakeventOFF)
        bne -
between sty HerMIDI.BuffIndex
fakeret rts

processEvents ;convert MIDI-messages to C64 notes and effects
        ;lda HerMIDI.ControlCmd
        ;and #HerMIDI.ShortEvent
        ;bne rtEvent ;if ShortEvent is selected data is preprocessed by HerMIDI
EventLp jsr HerMIDI.GetEvent  ;reads an event from the buffer to A,X,Y; Carry-flag=1 if read out
        bcs rtEvent ;if Carry=1 on exit buffer has been read out
        jsr useCmd
NextEvn jmp EventLp
rtEvent rts

useCmd  and #$F0 ;eliminate channel-nybble
        cmp #MIDI.NoteOn
        bne +
NoteOn  cpy #0 ;on some keyboards NoteOn vith 0 velocity means Note-Off
        beq NoteOff
        sty velosto+1
        ldy #0
-       lda NoteBuffer,y
        beq PutNote ;found empty slot?
        iny
        cpy #(NoteBuffer_end-NoteBuffer)
        bne -
        rts
PutNote inx
        txa
        sta NoteBuffer,y
        sta LatestNote
velosto lda #selfmod
        sta VeloBuffer,y
        rts
+       cmp #MIDI.NoteOff
        bne +
NoteOff inx
        ldy #0
-       txa
        cmp NoteBuffer,y 
        bne nomatch
DelNote lda #0 ;silence :)
        sta NoteBuffer,y
nomatch iny
        cpy #(NoteBuffer_end-NoteBuffer)
        bne -
        rts
+       cmp #MIDI.PrgChange
        bne +
ProgChg sty SelInst  ;Y=patch/instrument-number
        rts
+       cmp #MIDI.PitchWheel
        bne +
Pitcher sty PitchWh ;Y=MSB for pitch-wheel
        rts
+       cmp #MIDI.CommonCtrl
        bne +
chkModu cpx #MIDI.CC.ModWheel
        bne chkVolu
        sty Vibrato ;MSB
        lda #0
        sta VibVal
        lda #vibfreq/4
        sta VibCnt
        rts
chkVolu cpx #MIDI.CC.ChanVolume
        bne chCutof
        sty MainVol
chCutof cpx #MIDI.CC.Brightness
        bne chReson
        sty Cutoff
        rts
chReson cpx #MIDI.CC.Resonance
        bne chExpre
        sty Resonan
        rts
chExpre cpx #MIDI.CC.Expression
        bne CC_end
        sty KbTrack
CC_end  rts
+       ;What else?
        rts

NoteBuffer .fill SID_CHN_MAX*ARP_LEN_MAX,0 ;max. 4x arpeggios
NoteBuffer_end
LatestNote .byte 0
VeloBuffer .fill (NoteBuffer_end-NoteBuffer),0
MainVol .byte $7F 
Vibrato .byte $00
PitchWh .byte $00 ;$80..$ff:negative shift, $00..$7f:positive shift
Cutoff  .byte $7f
Resonan .byte $40
KbTrack .byte $01 ;if nonzero, keyboard-tracking of cutoff is performed
SelInst .byte $02 ;selected instrument (waveform)


;-------------------------------------------------------
InitSID ldy #$18
-       lda SIDval,y
        sta SID_base,y
        dey
        bpl -
        lda #0 
        ldy #(NoteBuffer_end-NoteBuffer)-1
-       sta NoteBuffer,y
        sta VeloBuffer,y
        dey
        bpl -
        sta LatestNote
        sta Vibrato ;A==0 here
        sta VibVal
        sta KbTrack
        lda #vibfreq/4
        sta VibCnt
        lda #$40
        sta PitchWh
        sta Resonan
        sta Cutoff
        lda #$7F
        sta MainVol
        lda #2
        sta SelInst
        rts

PlaySID lda MainVol
        lsr
        lsr
        lsr
        ora #$10 ;low-pass filter
        sta SID.VOL_BAND
        sta VOL_GHOST
SetReso lda Resonan
        asl
        and #$F0
        ora #7 ;filt all channels
        sta SID.filtSw_Reso
SetWfrm ldy SelInst
        lda Instruments.Waveforms,y
        sta WF_GHOST
        ldx #SID_CHN_MAX-1
-       ldy ChnPos,x
        sta SID.WF,y
        dex
        bpl -
SetVibr lda Vibrato
        lsr ;/2 ;0..$3f
        lsr ;/4 ;0..$1f
        lsr ;/8 ;0..$0f
        sta VibAmpS+1
        ldx VibCnt
        bne +
        ldx #vibfreq
+       dex
        stx VibCnt
        txa
        asl
        cmp #vibfreq
        bcc vibraup
vibdown lda VibVal
        sec
VibAmpS sbc #selfmod ;Vibrato
        jmp +
vibraup lda VibVal
        clc
        adc VibAmpS+1 ;Vibrato
+       sta VibVal
SetDetu lda PitchWh
        cmp #$79 ;maximum pitch-shifting upwards is a whole-note, we limit range to it
        bcc +
        lda #$79 ;this upper limit has been experimented for this 'receiver.asm' code
+       cmp #9  ;minimum pitch-shifting downwards is a whole-note, we limit range to it
        bcs +
        lda #9 ;this lower limit has been experimented for this 'receiver.asm' code
+       sec
        sbc #$40 ;$80..$ff:negative shift, $00..$7f:positive shift
        clc
        adc VibVal
        sta Detune 
doNotes ldx #SID_CHN_MAX-1
        stx chancnt
chnloop ldx chancnt
        lda ArpIndex,x
        clc
        adc chancnt
        tay
        lda VeloBuffer,y
        pha
        lda NoteBuffer,y
        pha
        jsr CalcDetune
        pla
        tay
        lda FREQTBH,y ;note pitch high-byte
        pha
        lda FREQTBL,y ;note pitch low-byte
        beq + ;don't slide/detune/vibrate empty note
        bit Detune
        bpl addDtuL
subDtuL sec
        sbc DetuneL
        jmp +
addDtuL clc   ;(fortunately no 00 in FREQTBL for notes)
        adc DetuneL
+       ldy ChnPos,x
        sta SID.pitchL,y
        pla ;discrete note-pitch high-byte
        beq + ;don't slide/detune/vibrate empty note
        bit Detune
        bpl addDtuH
subDtuH sbc DetuneH ;minus Carry-flag set before
        jmp +
addDtuH adc DetuneH ;plus Carry-flag set before
+       sta SID.pitchH,y
        pla ;velocity
        lsr
        lsr
        lsr
        lsr ;range: 0..8
        sta SID.PWH,y
AdvaArp dec ArpSpCnt,x
        bpl SetCtf
        lda #arpspeed
        sta ArpSpCnt,x
+       ldy #ARP_LEN_MAX
-       ldx chancnt
        lda ArpIndex,x ;find next note in the arpeggio (if exists)
        clc
        adc #SID_CHN_MAX
        cmp #(NoteBuffer_end-NoteBuffer)
        bcc +
        lda #0
+       sta ArpIndex,x
        clc
        adc chancnt
        tax
        lda NoteBuffer,x
        bne SetCtf
        dey
        bne - ;if no note at all, this counts down...
        lda #0
        ldx chancnt
        sta ArpSpCnt,x
SetCtf  lda KbTrack
        beq CTFHGHO     ;IF 0, NO KEYBOARD-TRACKING
        sec
        sbc #$10 ;allow $f0..$FF range too, because it has finer keyboard-tracking than $01..$6f
        adc LatestNote  ;CURRENT NOTE-NUMBER ;THE ADDITION (OR SUBSTRACTION IF A>$80)
        tay
        lda EXPTABH,y   ;USE EXPONENT TABLE AS QUASY MULTIPLY/DIVIDE FUNCTION BASED ON CKBDTRK
CTFHGHO clc
        adc Cutoff      ;HIGH BYTE OF FILTER CUTOFF-FREQUENCY
        sta SID.filtH
nextChn dec chancnt
        bmi retPlay
        jmp chnloop
retPlay rts

CalcDetune ;input: note-index in Y, uses 'Detune' as multiplier, X is not touched!
        lda Detune
        beq wrFmodL ;IF 0, NO CALCULATION OF PITCH, just writing 0 to DetuneL and DetuneH
        bpl posDetu ;positive direction?
negDetu lda #0 ;negative, turn it into positive by substracting from 256
        sec
        sbc Detune
posDetu sta mulstor+1
        lsr ;A=0.5*Detune
        lsr ;A=0.25*Detune
        clc
mulstor adc #selfmod ;A = Detune+0.25*Detune = 1.25*Detune
        cmp #$4b ;maximum pitch-shifting upwards is a whole-note, we limit range to it
        bcc +
        lda #$4b
+       clc
        adc NoteBuffer,y ;CALCULATE PITCH-DEPENDENT AMPLITUDE-COMPENSATION HERE!!!!
LOOKUPA tay             ;0..LOOK UP AMPLITUDE VALUE FROM FREQUENCY/EXPONENT-TABLES
CHKTEND cpy #EXPTRESHOLD+(ENDFREQTBH-FREQTBH)
        bcs MAXSLID     ;IF POINTS OVER TABLE
        cpy #EXPTRESHOLD ;DECIDE ON FINE/ROUGH HALF OF THE EXPONENT-TABLE
        bcs calcVib     ;if bigger than $60 limit of table, switch to rough table
        lda EXPTBASE,y
wrFmodL sta DetuneL
        lda #0          ;IN FINE-RANGE FREQ-ADD-HI SHOULD BE 0
        beq wrFmodH     ;jump
MAXSLID ldy #EXPTRESHOLD+(ENDFREQTBH-FREQTBH)
calcVib lda FREQTBL-EXPTRESHOLD,y
        sta DetuneL
        lda FREQTBH-EXPTRESHOLD,y ;EXPTABH,Y ;EXPONENTIAL TABLE SIMULATES MULTIPLICATION - CALCULATED VIBRATO
wrFmodH sta DetuneH
retCalc rts

VOL_GHOST .byte 0
WF_GHOST  .byte 0
Detune  .byte 0
DetuneL .byte 0
DetuneH .byte 0

arpspeed=2
ArpIndex .fill SID_CHN_MAX,0
ArpSpCnt .fill SID_CHN_MAX,0

vibfreq=8
VibCnt .byte vibfreq/4
VibVal .byte 0

SIDval .byte $00,$00,$88,$08,$41,$22,$F8, $00,$00,$08,$88,$41,$22,$F8, $00,$00,$88,$08,$41,$22,$F8
       .byte $ff,$ff,$87,$1F

ChnPos .byte 0,7,14, $20,$27,$34, $40,$47,$54, $60,$67,$74 ;SID-channel positions in SID-register areas

EXPTBASE = EXPTABH
EXPTRESHOLD = ENDFREQTBH-EXPTBASE

EXPTABH .byte 0,0,0,0,0,0,0,0, 0,0;LET EXPONENT TABLE BE A BIT MORE COMPLETE (FOR SUBSTRACTING KB.TRACK)
        .byte 0         ;FOR UNCALCULATED ZERO VIBRATO
FREQTBH .byte $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;C-0 note is the second value!
        .byte $02,$02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03
        .byte $04,$04,$04,$04,$05,$05,$05,$06,$06,$06,$07,$07
        .byte $08,$08,$09,$09,$0a,$0a,$0b,$0c,$0d,$0d,$0e,$0f
        .byte $10,$11,$12,$13,$14,$15,$17,$18,$1a,$1b,$1d,$1f
        .byte $20,$22,$24,$27,$29,$2b,$2e,$31,$34,$37,$3a,$3e
        .byte $41,$45,$49,$4e,$52,$57,$5c,$62,$68,$6e,$75,$7c
        .byte $83,$8b,$93,$9c,$a5,$af,$b9,$c4,$d0,$dd,$ea,$f8
ENDFREQTBH
        .byte $F9,$FA,$FB,$FC,$FD,$FE,$FF,$FF ;EXPAND EXPONENT-TABLE WITH SLOPE FOR KB.TRACKING
ENDEXPTABH

FREQTBL .byte $00,$16,$27,$38,$4b,$5e,$73,$89,$a1,$ba,$d4,$f0 ;C-0 note is the second value!
        .byte $0d,$2c,$4e,$71,$96,$bd,$e7,$13,$42,$74,$a8,$e0
        .byte $1b,$59,$9c,$e2,$2c,$7b,$ce,$27,$84,$e8,$51,$c0
        .byte $36,$b3,$38,$c4,$59,$f6,$9d,$4e,$09,$d0,$a2,$81
        .byte $6d,$67,$70,$88,$b2,$ed,$3a,$9c,$13,$a0,$44,$02
        .byte $da,$ce,$e0,$11,$64,$da,$75,$38,$26,$40,$89,$04
        .byte $b4,$9c,$c0,$22,$c8,$b4,$eb,$71,$4c,$80,$12,$08
        .byte $68,$38,$80,$45,$90,$68,$d6,$e3,$98,$00,$24,$10

SID .block
 pitchL=SID_base+0
 pitchH=SID_base+1
 PWL=SID_base+2
 PWH=SID_base+3
 WF=SID_base+4
 AD=SID_base+5
 SR=SID_base+6
 filtL=SID_base+21
 filtH=SID_base+22
 filtSw_Reso=SID_base+23
 VOL_BAND=SID_base+24
.bend

Instruments .block
Waveforms
.for i=0,i<127,i=i+8
ring .var 4*(i/16) & 4
sync .var 2*(i/8) & 2
 .byte $11+ring+sync,$21+ring+sync,$41+ring+sync,$81+ring+sync,$31+ring+sync,$51+ring+sync,$61+ring+sync,$71+ring+sync
.next
.bend
Instruments_end


;-----------------------------------------------------------------
dispStatus
        ldy #39
-       ldx allrigtxt,y
        lda HerMIDI.StatusByte
        and #HerMIDI.NotFound
        beq putstch
NoFound ldx error1txt,y
        lda HerMIDI.StatusByte
        and #HerMIDI.DATAisUp
        bne DATA_up
DATA_dn lda HerMIDI.ControlCmd
        bne putstch            ;if DATA=0V, good for Sync/Async
        beq + ;jump            ;and wrong for DirectMode
DATA_up lda HerMIDI.ControlCmd
        beq putstch            ;if DATA=5V, good for DirectMode
+       ldx error2txt,y        ;and wrong for Sync/Async
putstch txa
        sta VRAM+24*40,y
        dey
        bpl -
        ldy #33-1
-       lda mode0txt,y
        ldx HerMIDI.ControlCmd
        bpl +
        lda mode2txt,y
+       cpx #HerMIDI.DirectMode
        bne +
        lda mode1txt,y
+       sta VRAM+23*40+(40-33)/2,y
        dey
        bpl -
        rts

displayMIDI
        ldx #HerMIDI.MaxBuffSize-1
-       lda hexchar,x
        sta VRAM+4*40,x
        lda HerMIDI.EventBuffer,x
        sta VRAM+5*40,x
        lda #" "
        cpx HerMIDI.BuffIndex
        bcs +
        lda #"*"
+       cpx HerMIDI.PacketSize
        bne +
        lda #"<"
+       sta VRAM+6*40,x
        stx xstore1+1
        txa
        asl
        clc
        adc xstore1+1
        tay ;X*3
        lda HerMIDI.EventBuffer,x
        jsr hexdisp
        sta VRAM+8*40+1,y
        txa
        sta VRAM+8*40+2,y
xstore1 ldx #selfmod
        lda #1 ;colour of highlighted events
        cpx HerMIDI.BuffIndex
        bcc +
        lda #11 ;colour of old (leftover) events
+       sta CRAM+5*40,x
        sta CRAM+8*40+1,y
        sta CRAM+8*40+2,y
        dex
        bpl -
        ldy HerMIDI.ControlCmd
        cpy #HerMIDI.DirectMode
        beq +
        lda rastime
        jsr hexdisp
        sta VRAM+23*40+34
        stx VRAM+23*40+35
+       lda HerMIDI.EventBuffer+0 ;display first value in buffer in bin.
        sta zpv1
        ldy #7
-       lda #"0"
        lsr zpv1
        bcc +
        lda #"1"
+       sta VRAM+5*40+30,y
        dey
        bpl -
        lda MainVol
        jsr hexdisp
        sta VRAM+11*40+7
        stx VRAM+11*40+8
        lda SelInst
        jsr hexdisp
        sta VRAM+11*40+16
        stx VRAM+11*40+17
        lda PitchWh
        jsr hexdisp
        sta VRAM+11*40+27
        stx VRAM+11*40+28
        lda Vibrato
        jsr hexdisp
        sta VRAM+11*40+38
        stx VRAM+11*40+39
        lda VOL_GHOST
        jsr hexdisp
        sta VRAM+13*40+7
        stx VRAM+13*40+8
        lda WF_GHOST
        jsr hexdisp
        sta VRAM+13*40+16
        stx VRAM+13*40+17
        lda WF_GHOST
        and #$f0
        lsr ;/2
        lsr ;/4
        tax
        ldy #0
-       lda WFnames,x
        sta VRAM+13*40+22,y
        inx
        iny
        cpy #4
        bne -
        ldx #"0"
        lda WF_GHOST
        and #4 ;ringmod?
        beq +
        ldx #"1"
+       stx VRAM+13*40+32
        ldx #"0"
        lda WF_GHOST
        and #2 ;sync?
        beq +
        ldx #"1"
+       stx VRAM+13*40+39
        ldx #"0"
        lda KbTrack
        jsr hexdisp
        sta VRAM+12*40+38
        stx VRAM+12*40+39
        lda Cutoff
        jsr hexdisp
        sta VRAM+12*40+14
        stx VRAM+12*40+15
        lda Resonan
        jsr hexdisp
        sta VRAM+12*40+27
        stx VRAM+12*40+28
DisNote ldx #(NoteBuffer_end-NoteBuffer)-1
-       ldy NoteBuffer,x
        lda noteCh3,y
        pha
        lda noteCh2,y
        pha
        lda noteCh1,y
        pha
        txa
        asl ;x2
        asl ;x4
        tay
        pla 
        sta VRAM+16*40,y
        pla
        sta VRAM+16*40+1,y
        pla
        sta VRAM+16*40+2,y
        dex
        bpl -  
        rts
        
        .enc 'screen'
WFnames .text "----","TRIA","SAW ","TR+S","PULS","P+TR","P+S ","P+TS","NOISE"
        .enc 'none'

hexdisp pha ;input: Accu, output: digit1 in Accu, digit2 in X
        lsr
        lsr
        lsr
        lsr
        tax
        lda hexchar,x
        sta astore1+1
        pla
        and #$0f
        tax
        lda hexchar,x
        tax
astore1 lda #selfmod
        rts

scrinit
        lda #$1b
        sta $d011
        lda #$16
        sta $d018
        lda #0
        sta $d015 ;sprites won't steal CPU cycles (in synchron-mode)
        sta $d020
        sta $d021
        tay
-       lda #" "
        sta VRAM+$000,y
        sta VRAM+$100,y
        sta VRAM+$200,y
        sta VRAM+$300,y
        lda #5
        sta CRAM+$000,y
        sta CRAM+$100,y
        sta CRAM+$200,y
        sta CRAM+$300,y
        iny
        bne -
        ldy #39
-       lda titletxt,y
        sta VRAM+0*40,y
        lda #13
        sta CRAM+0*40,y
        lda #"-"
        sta VRAM+1*40,y
        sta VRAM+3*40,y
        sta VRAM+10*40,y
        sta VRAM+22*40,y
        lda helptxt,y
        sta VRAM+2*40,y
        lda #3
        sta CRAM+24*40,y
        lda paramtxt,y
        sta VRAM+11*40,y
        lda valuetxt,y
        sta VRAM+13*40,y
        lda valu2txt,y
        sta VRAM+12*40,y
        lda help2txt,y
        sta VRAM+20*40,y
        lda help3txt,y
        sta VRAM+21*40,y
        dey
        bpl -
        rts


;------------------------------------------------------------------------------
.enc 'screen'
hexchar .text"0123456789ABCDEF","GHIJKLMNOPQRSTUV"
noteCh1 .text ".CCDDEFFGGAAHCCDDEFFGGAAHCCDDEFFGGAAHCCDDEFFGGAAHCCDDEFFGGAAHCCDDEFFGGAAHCCDDEFFGGAAHCCDDEFFGGAAH...."
noteCh2 .text ".-#-#--#-#-#--#-#--#-#-#--#-#--#-#-#--#-#--#-#-#--#-#--#-#-#--#-#--#-#-#--#-#--#-#-#--#-#--#-#-#-...."
noteCh3	.text ".111111111111222222222222333333333333444444444444555555555555666666666666777777777777888888888888...."

titletxt .text "HERMIDI-INTERFACE 1.0 TESTER APPLICATION"
helptxt  .text "PRESS SPACE TO CYCLE DATA-TRANSFER MODES"
paramtxt .text "VOLUME:00 PATCH:00 PITCHWH:00 VIBRATO:00"
valuetxt .text " $D418:00 $D404:00 WF:PULS RING:1 SYNC:1"
valu2txt .text "FILTER-CUTOFF:00 RESONANCE:00  KB.TRK:00"
help2txt .text "YOU CAN USE MIDI-CONTROLLER'S POTMETERS:"
help3txt .text "E.G.:CC71-RESONANCE,CC74-CUTOFF,CC11-EXP"
mode0txt .text "SYNCHRON TX-MODE,  RASTERTIME:$00"
mode1txt .text "DIRECT-MODE (BYPASS TO DATA-LINE)"
mode2txt .text "ASYNCHRON TX-MODE, RASTERTIME:$00"
allrigtxt .text "HERMIDI-INTERFACE IS DETECTED AND USED. "
error1txt .text "  ERROR: HERMIDI DEVICE WAS NOT FOUND!  " 
error2txt .text "BUSERROR:NOT FOUND,DATALINE CAN'T BE SET" 
.enc 'none'
;=======================================================================
; vim: sw=4 ts=4 syntax=asm:
