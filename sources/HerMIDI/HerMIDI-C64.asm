;=======================================================================
; C64 source-file (firmware) with algorithms to use HerMIDI in programs
;2015 Hermit Software Hungary (Mihaly Horvath)
;=======================================================================
;usage: lda #cmd, jsr HerMIDI.OPEN   to init, badlines allowed here
;       jsr HerMIDI.CLOSE  to switch off HerMIDI and free up IEC bus
;       jsr HerMIDI.READ   regularly (in every frame) to get buffer
;(syntax is 64tass but maybe easy to adopt routines to other assemblers)


;==================== Preprocessor-configuration =======================
        .weak           ;These defaults and can be redefined from outside
INC_SYNCHR = 1          ;if nonzero: synchron-mode READ gets included
INC_ASYNCH = 1          ;if nonzero: asynchron-mode READ gets included
INC_DIRECT = 1          ;if nonzero:DirectMode READ (DirecRx) gets included
INC_EVENTS = 1          ;if nonzero: 'GetEvent' subroutine gets included
Status  = StatusByte    ;by default use internal status byte, can be external
MIDIbuffer = EventBuffer;by default use internal MIDIbuffer, can be external
        .endweak

;========================= HerMIDI Constants ===========================
MaxBuffSize=25 ;maximal MIDIevent-buffer size supported by HerMIDI
MIDI_EOD = MIDI.Undefined ;value received when MIDI-buffer is read out

;'ControlCmd' values (control commands sent at init after init.string):
 DirectMode=$00 ;(Bypass) just redirects pure MIDI-signal to DATA-line
 ;Immediate=$80 ;as soon a MIDI-byte arrives it's sent to C64 (planned) 
 ;Bit7 ($80): 0=Synchron byte transfer-mode, 1=Asynchron byte-transfer
  SynchMode = %00000000 ;Bit7=0, +$00
  AsyncMode = %10000000 ;Bit7=1, +$80, Don't modify this value!
 ;bit6 ($40): in Short-mode HerMIDI transforms MIDI-events to short form
  ;FullEvents= %00000000 ;Bit6=0, +$00
  ;ShortEvent= %01000000 ;Bit6=1, +$40 ;to be realized
 ;Bit5 ($20): yet undefined, maybe use to set MIDI-plug as input/output
 ;Bit4..0 ($00..$1f) values: $00 is DirectMode (Don't modify this!)
  ;$01..MaxBuffSize: buffersize, maybe less needed if playing monophonic

;Status values (stored in 'HerMIDI.Status' byte)
  Found_OK=$00
  NotFound=CLKin
  TurnedOff=NotFound+DATAin ;artificial Status not equal to SerIO inputs
  DATAisUp=DATAin


;*************** Jumptable (subroutine entry points) *******************
.if (*==$0000)
        *=$CD00  ;.align $100; for precompiled version (PIC-firmware)
OPEN    jmp PowerON ;don't forget to set Accumulator before (ControlCmd)
READ    jmp GetData ;'OPEN' should preceed,automatically selects Tx-mode
CLOSE   jmp PowrOFF
EVENT   jmp GetEvent ;'jsr READ' should preceed this call to fill buffer
;FWaddress+: 0:OPEN, 3:READ, 6:CLOSE, 9: EVENT
;$0C onwards: variables..
.fi


;========================= Variables, Arrays  ==========================
;order of these not to be changed, so their place is known from outside
;FWaddress+: $0C:Status, $0D:BuffIndex, $0E:PacketSize, $0F:ControlCmd
StatusByte .byte 0 ;zero:all OK, bit6=1 if unsuccessful init
BuffIndex  .byte 0 ;Holds amount of read bytes after MIDIbuffer-reading
PacketSize .byte MaxBuffSize ;can be changed at every init.

IniString:
ControlCmd .byte SynchMode+MaxBuffSize  ;Command, sent last
.enc 'inistring'
.cdef "az",$61, "AZ",$41, "09",$30, "  ",$20, "!!",$21, "..",$2e
DeviceID   .text "C64 needs HerMIDI interface 1.0!" ;string to switch on
.enc 'none'
IniString_end ;(String bytes may depend on used assembler & directives!)

;variables used only inside, can be anywhere:
ByteTemp    .byte 0 ;Temp.bufferdata, used by lsr/asl Carry-flag setters

;======================= DATA-TRANSFER ROUTINES ========================
GetData lda Status ;not checking INCLUDE_XX here,coz would modify timing
        and #NotFound
        beq +
        rts
+       lda ControlCmd ;$00:Direct, bit7=0:Synchron, bit7=1: Asynchron
        bne +
        jmp DirectRx   ;Direct-moe: HerMIDI simply redirects MIDI signal
+       bpl SynchRx
        jmp AsyncRx

;-----------------------------------------------------------------------
;Synchron MIDI-databyte receiving protocol (only in top/bottom borders):
;0.Default state: DATA=0V, CLK=0V (opposite of async), ATN=5V
;1.Pull up DATA=5V, tells HerMIDI to send a MIDIevent-buffer databyte
;2.HerMIDI sends bits in two rounds (4-bit packs).Rounds look like this:
;a.HerMIDI DATA=0V acts as clock for C64 to read first databit on CLK
;  then, after 8 cycles HerMIDI sets next databits in both DATA&CLK
;  then after again 8 cycles HerMIDI sends 4th bit in CLK,sets DATA=5V
;b.Second round is the same but with the other nybble of the databyte
;3.If we haven't got EOD($FD) or reached maximum of buffer,back to step2
.if (*/256 != (*+SynchRx_end-SynchRx)/256) ;check if page crossed
 .align $100 ;if page was crossed align to boundary
.fi
SynchRx .proc
.if (INC_SYNCHR!=0)
.page ;error if page-boundary crossed and timing ruined by extra cycles
        lda $d015
        sta d015sto+1
        ldx #0            ;2 cycles ;will count received bytes
        stx $d015 ;avoid CPU-cycle stealing sprites during execution
        .if (INC_EVENTS!=0)
        stx GetEvent.rdIndex+1 ;4 cycles
        .fi
        lda SerIO
        and #$03+CLKin    ;A=%00000VVV by default
        cmp #CLKin     ;check if HerMIDI was accidentally turned off(CLK=5V)
        bcs retSync    ;(using timeouts below would slow bitrate down.) 
        sta SerIO      ;set DATA=5V frees bus,tells HerMIDI to send data
        bit SerIO         ;4 cycles ;check if MIDI-keyboard is turned off,
        bpl EndOfDt       ;2/3 cycles ;and pulls DATA to 0V -> avoid startup-freeze
        ora #bit6         ;2 cycles
        sta decoder+1     ;4 cycles ;will be used when decoding value of byte
BufLoop lda SerIO         ;4 cycles
        bmi *-3           ;3/2 cycles ;A=%0D000XXX when exiting loop,D=bit0
        lsr               ;2 cycles   ;A=%00D000XX
        ora SerIO         ;4 cycles   ;A=%DDD00XXX append with bit2 & bit1
        lsr               ;2 cycles   ;A=%0DDD0XXX
        lsr               ;2 cycles   ;A=%00DDD0XX
        ora SerIO         ;4 cycles   ;A=%1DDDDXXX append with bit3
Nybble2 ldy SerIO         ;4 cycles   ;get bit6
        bmi *-3           ;3/2 cycles
        lsr               ;2 cycles   ;A=%01DDDDXX
        eor SerIO         ;4 cycles   ;A=%DdDDDdXX append with bit5 & bit4
decoder eor #selfmod      ;2 cycles   ;A=%DDDDDDXX ;decode EOR-ed bit2
        cpy #CLKin        ;2 cycles   ;Carry-flag = bit6
        ldy SerIO         ;4 cycles   ;DATA=5V
        ror               ;2 cycles   ;A=%DDDDDDDX
        cpy #DATAin+CLKin ;2/3 cycles ;Carry-flag=bit7
        ror               ;2 cycles   ;A=%DDDDDDDD, we got it! :)
storeBy cmp #MIDI_EOD     ;2 cycles
        beq EndOfDt       ;2/3 cycles
        sta MIDIbuffer,x  ;5 cycles
        cpx PacketSize    ;4 cycles
        beq EndOfDt       ;2/3 cycles
        inx               ;2 cycles
        bne BufLoop       ;3/2 cycles ;37 cycles spent since last 'SerIO' check
EndOfDt lda SerIO    ;4 cycles
        ora #DATAout ;2 cycles ;set DATA=0V, closes current transfer
        sta SerIO    ;4 cycles
        stx BuffIndex
retSync ;restore value of $d015
d015sto lda #selfmod
        sta $d015
        rts
.endp
.else
        rts
.fi
.pend
SynchRx_end


;-------------------------------------------------------------------
;Asynchron MIDI-databyte receiving protocol:
;0.Default state: DATA=0V by C64, CLK=5V, ATN=5V
;1.Pull up DATA=5V, tells HerMIDI to send MIDIevent-buffer databytes
;2.Bits with Makela's async mehthod: HerMIDI sets 0:CLK=0V / 1:DATA=0V
;3.C64 acknowledges the receipt by pulling the 5V-remained line to 0V
;4.Looping to step 2 until 'EOD' byte comes (all bits received)
;5.End of Midi-event receiver, set DATA=0V (default), free up: CLK=5V
AsyncRx .proc
.if (INC_ASYNCH!=0)
bitcount=ByteTemp
        lda SerIO
BytLoop and #$03  ;set DATA=5V frees bus,tells HerMIDI to send databytes
        sta BitLoop+1 ;caching $DD00 values for faster bitloop
        pha
        ora #DATAout
        sta BitIs0+1
        pla
        ora #CLKout
        tay ;CLK=0V setting
        ldx #0
        .if (INC_EVENTS!=0)
        stx GetEvent.rdIndex+1
        .fi
        clc ;init Carry-flag for the 1st transfer
BufLoop stx bytecnt+1
        ldx #8 ;will count bits in a byte
        stx bitcount
        lda #0 ;init Accu so it will always init Carry-flag to 0 on ROR
BitLoop ldx #selfmod
        stx SerIO
ChkData inx ;time-out counter to prevent freezing at this state
        bmi ReInit ;RetAsyn ;timed out? :(
        bit SerIO     ;wait for databit from HerMIDI
        bvc BitIs0    ;if CLK=0V it's a 0
        bmi ChkData   ;if CLK=DATA=5V, loop
BitIs1  sty SerIO     ;acknowledge bit-receipt to HerMIDI
        bit SerIO     ;wait for HerMIDI to leave bus by setting DATA=5V
        bpl *-3
        sec
        bcs + ;jump
BitIs0  ldx #selfmod
        stx SerIO     ;acknowledge bit-receipt to HerMIDI
        bit SerIO     ;wait for HerMIDI to leave bus by setting CLK=5V
        bvc *-3
        ;clc ;not needed because ror and cpx below init Carry=0 at loop
+       ror           ;processing Carry-flag
        dec bitcount
        bne BitLoop
bytecnt ldx #selfmod
        cmp #MIDI_EOD
        beq EndOfDt
        sta MIDIbuffer,x
        cpx PacketSize  ;check if maybe we reached buffer's top,Carry=0!
        bcs EndOfDt ;beq EndOfDt
        inx
        bne BufLoop
EndOfDt stx BuffIndex
RetAsyn lda SerIO
        and #$ff-CLKout ;CLK default state is 5V for asynchron-mode
        ora #DATAout    ;DATA=0V, tell there's no need for more data
        sta SerIO
.fi
        rts        ;output: BuffIndex (and X)
.pend


;-------------------------------------------------------------------
;!22 cycles spent since 'jsr HerMIDI.READ',call this directly instead!
;if called directly with 'jsr HerMIDI.DirecRx', takes exactly 25 cycles,
;and so can be used as a part of a MIDI-bit reader with 32-cycle steps
DirectRx .proc ;C64 gets MIDI directly,slow way,timing will need $d011=0
.if (INC_DIRECT!=0)
        lda SerIO    ;4   ;read DATA-line 6+4 cycles after 'jsr DirecRx'
        and #DATAin  ;2   ;unmask DATA-line bit
        bne +        ;2/3 ;set Carry accordingly
        bit 0        ;3 cycles
        clc          ;2/0
        rts          ;6/0 ;sum:25 cycles (including jsr+rts)
+       nop          ;2
        sec          ;0/2
.fi
        rts          ;0/6 ;sum:25 cycles (including jsr+rts)
.pend



;===================== Initialization routines =========================
;String-sending protocol:(not checking INCLUDE_XX here,sensitive timing)
;0. Default state: ATN=5V (CLK=5V,DATA=5V, all false)
;1. Pull ATN line low, pull it back after min. 4 cycles
;2. Set character-databit on DATA line (logical 0 = low 0V, 1 = high 5V)
;3. Pull CLK line low (0V), pull it back high (5V) after min. 3 cycles
;4. Iterate to step2: go on with each bit (bit7 is last) in the same way
;5. Repeat the character-loop until end of string (32 characters)
;6. Tell sync./async. mode by setting DATA-out to 5V:async / 0V:sync 
;7. Tx default: Pull DATA-out down to 0V, HerMIDI sets CLK=0V if success
.if (INC_ASYNCH!=0)
ReInit  lda ControlCmd
.fi
PowerON .proc
        ;php ;store I-flag
        ;sei ;disable interrupt during setting
        sta ControlCmd   ;Input:Accu, saved to HerMIDI.ControlCmd
        and #$1f
        sta PacketSize   ;can be set at every init...
        lda $d030
        and #$ff-1       ;switch off any C128 or Turbo Chameleon64 turbo
        sta $d030
        sta $d07f        ;if SuperCPU is present, slow it down to 1MHz
;        ldy #50          ;wait more than HerMIDI watchdog-timer (18ms)
;-       jsr wait1ms      ;so HerMIDI can detect previous ATN wasn't call
;        dey              ;to switch it ON, just the end of a LOAD or so.
;        bne -
        ;lda #$FE
        ;cmp $D012    ;waiting for clean area without badlines
        ;bne *-3      ;(so timing will be stabler)
        ldx #1           ;ATN-pulse width for switching HerMIDI on
        jsr ATNpuls      ;tell HerMIDI to switch on, carry tells TurnedOff
        lda #TurnedOff
        bcc retPwOn      ;avoid startup-freeze at TurnedOff state
        ldx #(IniString_end-IniString)-1 ;2  ;Backwards, ControlCmd last
StrLoop lda IniString,x  ;5 ;cca 8960cycles (140lines) to send IniString
        sta ByteTemp     ;4
        ldy #8           ;2 ;around 35*8=280 cycles to send 1 byte
ChrLoop lsr ByteTemp     ;6 ;around 35 cycles to send 1 bit
        lda SerIO        ;4
        and #$ff-DATAout ;2 ;let logical1 be 5V(pullup) on IEC DATA-line
        bcs +            ;3/2
        ora #DATAout     ;2 ;let logical0 be 0V (sink) on IEC DATA-line
+       sta SerIO        ;4 ;around 30 cycles from ATN-high at 1st time
        ora #CLKout      ;2 ;tell HerMIDI that data is ready to be sent
        sta SerIO        ;4
        and #$ff-CLKout  ;2 ;min.3 microseconds long pulse needed on CLK
        sta SerIO        ;4 ;badlines & sprites might elongate this time
        dey              ;2
        bne ChrLoop      ;3/2
        dex              ;2 ;X-loop: extra 15 cycles at every new byte
        bpl StrLoop      ;3/2
        ora #DATAout       ;Sync/Async Transmit default state is DATA=0V
        .if (INC_DIRECT!=0)
        ldx ControlCmd   
        bne +              ;skips if Synchron/Asynchron modes
        and #$ff-DATAout   ;in Direct-mode set DATA=5V instead
        .fi
+       sta SerIO          ;init DATAline to Tx-default (CLK=5V already)
        lda #0             ;init variables and..
        sta BuffIndex
        ldx #MaxBuffSize-1 ;init buffer.(takes around 25*10=250 cycles)
-       sta MIDIbuffer,x
        dex
        bpl -
        .if (INC_EVENTS!=0)
        sta GetEvent.ValuCnt+1 ;maybe we're in the middle of databytes
        sta GetEvent.CmdLeng+1
        lda #MIDI.Undefined    ;so prevent processing them after init
        sta GetEvent.Command+1
        .fi
        lda SerIO          ;(30 cycles must be spent since last CLK=5V)
        and #DATAin+CLKin  ;HerMIDI tells if successfully initialized
retPwOn sta Status         ;store the response from HerMIDI
        ldx #100           ;causes 100*5=500 cycles in 'delayer'
        jsr delayer        ;wait until HerMIDI stops keeping CLK at 0V
        ;plp ;restore I-flag
        asl
        asl ;Carry-flag holds CLK-input on exit (Carry=0 if success)
        rts
.pend

;-------------------------------------------------------------------
PowrOFF ldx #160 ;ATN pulsewidth around 800ms for HerMIDI to check it 
        stx Status       ;sign that the device is switched off
ATNpuls lda SerIO   ;Do ATN=0V pulse (X=width) and wait for IEC bus free
        and #$03         ;Init all IEC-lines to FALSE (5V) state 
        ora #ATNout ;+DATAout ;Call Attention of HerMIDI who is listening
        sta SerIO           ;(DATA=0V to aid fast 'TurnedOff' detection)
        jsr delayer      ;input:X, wait for HerMIDI to detect ATN-pulse
        and #$03         ;Init all IEC-lines to FALSE (5V) state
        sta SerIO ;(don't wait too much from here to check DATA=0V)
        lda SerIO ;check immediately if MIDI-keyboard is turned off,
        asl       ;and pulls DATA to 0V -> Carry will tell, avoid freeze
wait1ms ldx #200         ;wait min. 1 millisecond for other devices,
delayer dex              ;like 1541 drive to acknowledge 'EOI' (CLK=5V)
        bne *-1
        rts


;=========================== MISC. ROUTINES ============================
GetEvent .proc ;reads an event from the buffer to A,X,Y; C=1 if read out
.if (INC_EVENTS!=0)
rdIndex ldx #selfmod ;keeps track where we are in the buffer
ValuCnt ldy #selfmod ;stored remaining amount of value-bytes
chIndex cpx BuffIndex
        bcc rdEbyte  ;sets Carry-flag=1 if buffer is read out and exits
BuffEnd sty ValuCnt+1 ;we'll pick up where we left off
        rts
rdEbyte lda MIDIbuffer,x
        bpl StorVal ;status (command above $7F) or value (below $80) ?
        cmp #MIDI.RealTime_min ;transparent (command not stored)
        bcs AdvBuf3 ;if above (RealTime Messages), set Accumulator only
StorCmd sta Command+1 ;if below (System Messages), cancel prev. statusbyte
chkOMNI cmp #MIDI.OMNI_min ;all system messages will return immediately
        bcc VoicMsg
        ldy #0        ;System messages cancel 'running status' mode
        sty CmdLeng+1
        jmp AdvBuf3 
VoicMsg ldy #2 ;Y=2 ;pre-set for MIDI-commands that have 2-byte value
        and #%11100000 ;check bit5 and bit6 ($20 + $40), plus bit 7
        cmp #%11000000 ;if '110' it's $Cx or $Dx with only 1-byte value
        bne +
        dey    ;Y=1
+       sty CmdLeng+1 ;store length for 'running status' that may come
        jmp AdvBuf2
StorVal cpy #1      ;check if Y is 0 or 1 or 2 (0:'running status' case)
        bpl +       ;if Y != 0  (Y>=1), check which databyte to send
CmdLeng ldy #selfmod ;Y was 0,'running status', reusing previous command
        bne StorVal ;retain previous non-transparent status-byte (cmd)
+       beq StoVal2 ;if Y is 1, only the 2nd databyte is left
StoVal1 sta Value1+1
        dey ;advance to Value2;(Y=1 for MIDI-commands with 1-byte value)
AdvBuf2 inx ;advance in buffer
        bne chIndex ;jump
StoVal2 ldy #0
        sty ValuCnt+1 ;initialize databyte-counter for next message
        tay          ;Y=Value2
Command lda #selfmod ;A=Command (MIDI-message's status-byte)
AdvBuf3 inx
        stx rdIndex+1  
Value1  ldx #selfmod ;X=Value1  (and Value2 is Y)
        clc ;tell caller that reading the buffer is not yet over
.fi
        rts
.pend


;***********************************************************************
code_end ;end of firmware-code to be stored (on PIC for example)

EventBuffer .fill MaxBuffSize,0

.cwarn(code_end+MaxBuffSize<$E000 && code_end+MaxBuffSize>MaxAddr),"HerMIDI content shouldn't be in $D000...$E000 I/O area!"
;===================== MISC. INTERNAL CONSTANTS ========================
MaxAddr=$CFFF   ;code shouldn't be above this address (uses IO & KERNAL)

;binary bit-values
bit0=1
bit1=2
bit2=4
bit3=8
bit4=16
bit5=32
bit6=64
bit7=128

SerIO=$dd00    ;register used for Serial-bus I/O, I/O bit-values:
  ATNout = bit3 ;$08
  CLKout = bit4 ;$10
  DATAout= bit5 ;$20
  CLKin  = bit6 ;$40
  DATAin = bit7 ;$80

selfmod=$00    ;default value for self-modified code immediate-operands

.weak
KERNAL .block
;used KERNAL ROM entries:
SETNAM = $ffbd ;Set file name parameters. Input: A = File name length; 
               ;X/Y = Pointer to file name. (Real address: $FDF9)
SETLFS = $ffba ;Set file parameters. Input: A = Logical number; 
               ;X=DeviceNumber;Y=Secondary address.(Real address: $FE00)
LOAD   = $ffd5 ;LOAD. Load or verify file. (Must call SETLFS and SETNAM 
               ;beforehands.) Input: A: 0 = Load, 1-255 = Verify; 
               ;X/Y = Load address (if secondary address = 0).
               ;Output: Carry: 0 = No errors, 1 = Error; 
               ;A = KERNAL error code (if Carry = 1); 
               ;X/Y = Address of last byte loaded/verified (if Carry=0).
               ;Used registers: A, X, Y. (Real address: $F49E.)  
;low-level KERNAL ROM entries, if ever needed:
;---------------------------------------------
;TALK    = $ED09 ;$FFB4
;LISTEN  = $ED0C ;$FFB1
;UNTALK  = $EDEF ;$FFAB
;UNLISTEN= $EDFE ;$FFAE
;CLK5V =$EE85 ;FALSE
;CLK0V =$EE8E ;TRUE
;DATA5V=$EE97 ;FALSE
;DATA0V=$EEA0 ;TRUE
;ATN5V =$EDBE ;FALSE
;DELAY1MS=$EEB3 ;output: A = X (input), X preserved
.bend ;end of 'KERNAL' block
.endweak


;=======================================================================




;===================== General Constants, Specs ========================
MIDI .block      ;their adjacent bytes can be of values $00..$7F (bit7 off)
;MIDI-event numbers (called 'status bytes'...2nd Nybble is channel No.)
NoteOff    = $80 ;followed by 2 bytes: note-number,velocity
NoteOn     = $90 ;followed by 2 bytes: notenumber,velocity(if 0:NoteOff)
AfterTouch = $A0 ;followed by 2 bytes: note-number, strength
CommonCtrl = $B0 ;followed by 2 bytes: CC-number(see below), 7-bit value
PrgChange  = $C0 ;followed by 1 byte: patch/program number
ChPressure = $D0 ;followed by 1 byte value of channelpressure/aftertouch
PitchWheel = $E0 ;followed by 2 bytes: value high & low 7 bits
;Some OMNI (all channels) effects:
OMNI_min = $F0 ;all commands above this value are OMNI (no channel)
SysExMessg = $F0 ;many bytes (firm ID, data) can follow, ends with $F7
SongSelect = $F3 ;followed by song-number
SysExOver  = $F7 ;end of System-Exclusive message
RealTime_min=$F8 ;1-byte transparent commands above (no 'running status')
TimingClock= $F8 ;Timing Clock. Sent 24/QN when synchronization required
StartSqPlay= $FA ;Play current sequence. (Followed by Timing Clocks.) 
ContinueSeq= $FB ;Continue at the point the sequence was Stopped.
StopSeqPlay= $FC ;Stop the current sequence.
Undefined  = $FD ;can be used for signaling 'End of Data' if needed
ActiveSens = $FE ;no byte after
CC .block ;preceeded by $Bx, followed by 7-bit value (MSB bit7=0):
  BankSelect = $00
  ModWheel   = $01
  BreathCtrl = $02
  FootControl= $04
  PortamTime = $05
  ChanVolume = $07
  Balance    = $08
  Panning    = $0a
  Expression = $0b
  Effect1    = $0c
  Effect2    = $0d
  GeneralFx1 = $10
  GeneralFx2 = $11   ;Expression
  ;$20..$3f: the LSB for the CC-effects $00..$1f
  DamperPedal= $40   ;followed by switch-value: 0..$3f / $40..$7f
  Portamento = $41   ;followed by switch-value: 0..$3f / $40..$7f
  Sustenuto  = $42   ;followed by switch-value: 0..$3f / $40..$7f
  SoftPedal  = $43   ;followed by switch-value: 0..$3f / $40..$7f
  Legato     = $44   ;followed by switch-value: 0..$3f / $40..$7f
  Hold2      = $45   ;followed by switch-value: 0..$3f / $40..$7f
  Resonance  = $47   ;resonance/timbre/harmonic intensity  
  ReleaseTime= $48    ;Sound Controllers: $46..$4f
  AttackTime = $49
  Brightness = $4a   ;cutoff-frequency
  DecayTime  = $4b
  VibratoRate= $4c
  VibraDepth = $4d
  VibraDelay = $4e    ;General Purpose Controllers: $50..$$53
  PortamCtrl = $54
  ReverbFx   = $5b    ;Effects: 5b..$5f
  TremoloFx  = $5c
  ChorusFx   = $5d
  DetuneFx   = $5e
  PhaserDepth= $5f
  AllSoundOff= $78   ;Channel Mode Message followed by value $00
  ResetCtrler= $79   ;Channel Mode Message followed by value $00
  AllNoteOff = $7b   ;Channel Mode Message followed by value $00
 .bend
.bend



;=======================================================================
