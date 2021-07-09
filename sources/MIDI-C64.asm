;ident:8,24
;=======================================================================
;Multi-device MIDI-input library for the Commodore 64
;supports: HerMIDI, Sequential (SCI), Passport/Syntech,
;          Datel/JMS/Siel/C-LAB, Namesoft, Maplin, Moog Song Producer
;Year:2014, code: Mihaly Horvath (Hermit), NameSoft-info: Frank Buss
; $Id: MIDI-C64.asm 360 2014-02-15 14:50:28Z soci $
;=======================================================================


;====================== MIDIC64 External Settings ======================
.weak
MIDIC64_EXT_SETTING = 0
.endweak
.if !(MIDIC64_EXT_SETTING) ;if caller program sets it,inherit settings
MIDIC64_EXT_SETTING=0 ;upcoming settins for memory footprint reduction
MIDIC64_TX_ENABLE=0 ;whether to support Transmitting MIDI-data or not
MIDI_Legacy_support=1 ;standard/legacy extension-port devices using ACIA
MIDIC64_INC_EVENTS=1 ;whether 'EVENT' & 'GetEvent' routine is needed
MIDIC64_INC_NAMES=0 ;include names of devices?
MIDIbuffer_size=32 ;should be bigger than HerMIDI.MaxBuffSize (it uses)
MIDIC64_Watchdog_ini=200 ;0:no watchdog, 1..$ff: init value of watchdog
HerMIDI_support=0 ;enables support for HerMIDI Serial/IEC-port device
.if (HerMIDI_support!=0)
HerMIDI_TX_MODE = MIDIC64.HerMIDI.AsyncMode
HerMIDI_PacketSize=MIDIC64.HerMIDI.MaxBuffSize
.fi
.fi
.weak
HerMIDI_INC_SYNCHR=1 ;if nonzero: synchron-mode READ gets included
HerMIDI_INC_ASYNCH=1 ;if nonzero: asynchron-mode READ gets included
HerMIDI_INC_DIRECT=0 ;if nonzero:DirectMode READ (DirecRx) gets included
HerMIDI_INC_EVENTS=0 ;if HerMIDI's 'GetEvent' routine included
MIDIdev=MIDIC64.DeviceID
NMI=MIDIC64.RTIcode
IRQ=MIDIC64.RTIcode
MIDIbuffer=MIDIC64.EventBuffer
.endweak
;=======================================================================


MIDIC64 .block


;================ MIDIC64 Internal Constants & Settings ================
.if (MIDI_Legacy_support!=0)
;Control-Register's bits in Expansion port devices using ACIA chip:
RxIntEnable= bit7 ;1:Enable Receive-Interrupt, 0:Disable Rx-Interrupt
TxControl  = bit6 ;1:Transmits a Break level on the Transmit Data Output
TxIntEnable= bit5 ;1:Enable Transmit-Interrupt, 0: Disable Tx-Interrupt
WordSelect= 1*bit4+0*bit3+1*bit2 ;'101': 8 bits, no parity, 1 stop-bit
MIDIreset = 1*bit1+1*bit0 ;#03 to 'MIDIcontrol' register resets the ACIA
;Status-Register's bits in Expansion port devices using ACIA chip:
IntRequest  = bit7 ;Set 1 when: RxDataRfull/TxDataRempty/RxOverrun is on
ParityError = bit6 ;Set 1 on parity-error if 'WordSelect' has parity set
RxOverrun   = bit5 ;Is set to 1 when byte(s) were lost in data-stream
FramingError= bit4 ;Set 1 if latest received character improperly framed
ClearToSend = bit3 ;0:MIDI-device ready to receive, 1:MIDI-dev not ready
DataCdetect = bit2 ;0:no byte, 1:databyte interrupt,read Status to clear
TxDataRempty= bit1 ;Set to 1 when TX-register is ready for writing.
RxDataRfull = bit0 ;Set to 1 when RX-register is ready for reading.
MIDI_Legacy_list = [Vessel]
.else
MIDI_Legacy_list = []
.fi
Vessel_support=1*MIDI_Legacy_support
Passport_support = 0
DatelJMS_support = 0
NameSoft_support = 0
NameSoftIRQ_support = 0
Maplin_support = 0
MoogSP_support = 0
;MSSIAH_support=0 ;no tech. info, probably in 'Legacy' group as well
;MIDI-device identifiers:
HerMIDI_ID=1 ;number/ID of HerMIDI device
.if (HerMIDI_support!=0)
Legacy_min=2 ;number/ID of 1st device in Legacy-category
.else
Legacy_min=1
.fi
Legacy_max=Legacy_min+len(MIDI_Legacy_list)-1;Legacy-cat.max.ID
IRQmode=$40
NMImode=$80


;============================= JUMPTABLE ===============================
.if (*==$0000)
*=$C000 ;for standalone-build (pre-compiled form)
OPEN    jmp OpenDev ;don't forget to set Accumulator before (ControlCmd)
READ    jmp GetData ;'OPEN' should preceed,automatically selects Tx-mode
CLOSE   jmp CloseDev
EVENT   jmp GetEvent ;'jsr READ' should preceed this call to fill buffer
;Base Address+: 0:OPEN, 3:READ, 6:CLOSE, 9: EVENT
.fi

;======================== Variables, Arrays ============================
;Base Address+: $0c: Status, $0d: device-type/ID (if jumptable before)
Status     .byte $ff ;if zero, all OK, if nonzero it's error-code
DeviceID   .byte 0 ;MIDI-hardware type ID
;names corresponding to device-IDs (usable by caller programs)
EventBuffer .proc
        .fill MIDIbuffer_size,0
.pend

.if (MIDIC64_INC_NAMES!=0)
DeviceAmount=size(DevName)/8-1
.enc 'screen'
.if (HerMIDI_support!=0)
DevName .text "  NONE  ","HERMIDI ",MIDI_Legacy_list.DevName
.else
DevName .text "  NONE  ",MIDI_Legacy_list.DevName
.fi
.enc 'none'
.else
DeviceAmount=Legacy_max
.fi

.if (MIDI_Legacy_support!=0)
MoogSP_ID=8 ;Used to distinguish Moog Song Producer device at init/close

ControlAdd .word MIDI_Legacy_list.MIDIcontrol
StatusAddr .word MIDI_Legacy_list.MIDIstatus
RxAddress .word MIDI_Legacy_list.MIDI_Rx
TxAddress .word MIDI_Legacy_list.MIDI_Tx
InitDevVal .byte MIDI_Legacy_list.MIDIenable
IntrptType .byte MIDI_Legacy_list.IntType
IenableVal .byte MIDI_Legacy_list.IRQenable
IenableVal_end
rdCtrl  lda selfmodA
        rts
wrCtrl  sta selfmodA
        rts
rdStat  lda selfmodA
        rts
wrStat  sta selfmodA
        rts
.if (MIDIC64_Watchdog_ini!=0)
watchdog .byte MIDIC64_Watchdog_ini;to prevent loop,main-prg sets in every 20ms
.fi
.fi


;*********************** MAIN CROSS-HW ROUTINES ************************


;-------------------------------- OPEN ---------------------------------
OpenDev .proc ;reads control-info in A, opens/turns on MIDI-device
        .if (MIDIC64_INC_EVENTS!=0)
        lda #0
        sta GetEvent.ValuCnt+1 ;maybe we're in the middle of databytes
        sta GetEvent.CmdLeng+1
        lda #MIDI.Undefined    ;so prevent processing them after init
        sta GetEvent.Command+1
        .fi
        ldx MIDIdev
        beq retOpen     ;0 means no/dummy MIDI-device
        cpx #Legacy_min
        bcs chLegac
        .if (HerMIDI_support!=0)
OpenHM  lda #HerMIDI_TX_MODE+HerMIDI_PacketSize
        jmp HerMIDI.PowerON ;OPEN
        .fi
retOpen rts

chLegac cpx #Legacy_max+1
        bcs chOther ;could go to other upper categories later
.if (MIDI_Legacy_support!=0) 
OpenLgc ;open standard/legacy MIDI-devices   
SetAddr txa ;MIDI-device-number in X
        asl ;*2
        tay
        .if (Vessel_support!=0)
        ; Set PA2 to 1 to signal OUTPUT
        lda $dd00
        ora #%00000100
        sta $dd00
        ; Set Port B to output
        lda #$ff
        sta $dd03

        ; Reset
        lda #$fd
        sta $dd01
        lda #$00
        sta $dd01

        ;Send command 0x5, enable all channels
        lda #$fd
        sta $dd01
        lda #$05
        sta $dd01
        lda #$ff
        sta $dd01
        lda #$ff
        sta $dd01

        ;Send command 0x7, enable all commands on all channels
        lda #$70
vEnCh   ldx #$fd
        stx $dd01
        ldx #$07
        stx $dd01
        sta $dd01
        adc #$01
        cmp #$80
        bne vEnCh

        ;Send command 0x6, enable all real time messages.
        lda #$fd
        sta $dd01
        lda #$06
        sta $dd01
        lda #$ff
        sta $dd01
        lda #$ff
        sta $dd01

        lda #0
        sta Status
        .else
        lda (ControlAdd-Legacy_min*2)+0,y 
        sta rdCtrl+1
        sta wrCtrl+1
        lda (ControlAdd-Legacy_min*2)+1,y 
        sta rdCtrl+2
        sta wrCtrl+2
        lda (StatusAddr-Legacy_min*2)+0,y 
        sta rdStat+1
        sta wrStat+1
        sta GetData.StatusB+1
        lda (StatusAddr-Legacy_min*2)+1,y 
        sta rdStat+2
        sta wrStat+2
        sta GetData.StatusB+2
        lda (RxAddress-Legacy_min*2)+0,y 
        sta GetData.RxByte+1
        lda (RxAddress-Legacy_min*2)+1,y 
        sta GetData.RxByte+2
        lda (TxAddress-Legacy_min*2)+0,y 
        sta PutData.TxByte+1
        lda (TxAddress-Legacy_min*2)+1,y 
        sta PutData.TxByte+2
        .fi
clrBuff lda #0 ;clear MIDIbuffer
        .if (MIDIC64_INC_EVENTS!=0)
        sta GetEvent.rdIndex+1
        .fi
        sta GetData.WrIndex+1
        .if (Vessel_support==0)
Reset   lda #MIDIreset
        jsr wrCtrl
        lda InitDevVal-Legacy_min,x ;set MIDI-device, disable NMI/IRQ
        jsr wrCtrl
        jsr SetMIDIintA
chkStat jsr rdStat
        and #ClearToSend ;check if init successful
        sta Status
        bne retLega ;if unsuccessful, don't enable NMI/IRQ
startRx lda IenableVal-Legacy_min,x ; enable NMI/IRQ from MIDI interface
        jsr wrCtrl
        .if (MoogSP_support!=0)
        cpx #MoogSP_ID
        bne retLega
        lda #MoogSP.Control_Enable
        sta MoogSP.Control_Latch1
        .fi
        .fi
retLega rts
.fi
chOther ;no other devices yet...
        rts
        .pend


;-------------------------------- READ ---------------------------------
GetData .proc ;reads databytes and puts them into MIDI-databuffer
        ldx MIDIdev
        beq retData ;0 means no/dummy MIDI-device
        cpx #Legacy_min
        bcs chLegac
        .if (HerMIDI_support!=0)
         .if (MIDIC64_INC_EVENTS!=0 && HerMIDI_INC_EVENTS==0)
          lda #0 ;HerMIDI's buffer is not a ringbuffer
          sta GetEvent.rdIndex+1
         .fi
         jmp HerMIDI.GetData ;READ
        .else
         rts
        .fi

chLegac cpx #Legacy_max+1
        bcs chOther ;could go to other upper categories later
        .if (MIDI_Legacy_support!=0)
         .if (Vessel_support!=0)
         ;Reset PA2 to signal INPUT mode
         lda $dd00
         and #%11111011 ;Set bit2 to 0
         sta $dd00
         ;Set Port B to input
         lda #$00
         sta $dd03

         ;Read the available number of bytes. Max number of bytes in one go is 255 (not 256)
         ldy $dd01 ;Read bytecount from Port B
         beq VesselEmpty
         sty $d020
RxByte   lda $dd01 ;Read MIDI byte from Port B
WrIndex  ldx #selfmod
         sta MIDIbuffer,x
         inx
         cpx #MIDIbuffer_size
         bcc +
         ldx #0        ; this makes it behave as a ring-buffer
+        stx WrIndex+1
         dey
         bne RxByte
VesselEmpty
         ; Set PA2 to 1 to signal OUTPUT
         lda $dd00
         ora #%00000100
         sta $dd00
         ; Set Port B to output
         lda #$ff
         sta $dd03
         .fi
        .fi
chOther ;no other devices yet...
retData rts

.if (MIDI_Legacy_support!=0)
InterruptGetByte
        jmp IntEnd3
        pha
        lda banksel
        pha
        lda #$35        ; enable IO
        sta banksel
StatusB lda selfmodA  ; test if it was a NMI/IRQ from the MIDI interface
        bmi chInput   ;if not, give control to host program's interrupt
        ;pla           ; restore previous bank selection and end IRQ
        ;sta banksel
        ;pla
HostInt jmp selfmodA  ;jmp NMI/IRQ-continue with host-program's interrupt
chInput and #RxDataRfull ;test if note at input. If not, probably pulled off
        beq retData   ;if no input cable maybe pulled off, prevent freeze?
stInput txa
        pha
;RxByte  lda selfmodA  ; get MIDI byte and store in MIDIbuffer0
;WrIndex ldx #selfmod
;        sta MIDIbuffer,x
;        inx
;        cpx #MIDIbuffer_size
;        bcc +
;        ldx #0        ; this makes it behave as a ring-buffer
;+       stx WrIndex+1
 .if (MIDIC64_Watchdog_ini!=0)
        dec watchdog
        bne IntEnd
        tya
        pha
        jsr CloseDev
        pla
        tay
.fi
IntEnd  pla         ; restore previous bank selection and end IRQ
        tax
IntEnd2 pla
        sta banksel
        pla
IntEnd3
.fi
        .pend
RTIcode rti ;check if not defined yet externally


;------------------------------- WRITE ---------------------------------
PutData .proc
TxByte  sta selfmodA
        rts
        .pend


;------------------------------- CLOSE ---------------------------------
CloseDev .proc ;disables / turns off the MIDI-device
        lda #$ff ;sign that device is switched off
        sta Status
        ldx MIDIdev
        beq rtClose ;0 means no/dummy MIDI-device
        cpx #Legacy_min
        bcs chLegac
        .if (HerMIDI_support!=0)
        jmp HerMIDI.PowrOFF ;CLOSE
        .else
        rts
        .fi

chLegac cpx #Legacy_max+1
        bcs chOther ;could go to other upper categories later
.if (MIDI_Legacy_support!=0)
CloseLg lda #MIDIreset ;InitDevVal-Legacy_min,x ;disable NMI/IRQ
        jsr wrCtrl
        .if (MoogSP_support!=0)
        cpx #MoogSP_ID
        bne SetIntA
        lda #MoogSP.Control_Disable
        sta MoogSP.Control_Latch1
        .fi
SetIntA lda IntrptType-Legacy_min,x
        bmi +
        lda #<IRQ
        ldy #>IRQ
        jmp setIRQ
+       lda #<NMI
        ldy #>NMI
        jmp setNMI
;        lda #<ROMRTI ;dummy, if host-program's NMI is under ROM
;        ldy #>ROMRTI
;        jmp setRNMI
;retLega rts 
.fi
chOther ;no other devices yet...
rtClose rts
        .pend


;------------------------------- EVENT ---------------------------------
GetEvent .proc ;reads an event from the buffer to A,X,Y; C=1 if read out
.if (MIDIC64_INC_EVENTS!=0)
        ldx MIDIdev
        beq rtGetBy ;0 means no/dummy MIDI-device
        cpx #Legacy_min
        bcs chLegac
       .if (HerMIDI_support!=0)
        .if (HerMIDI_INC_EVENTS!=0)
         jmp HerMIDI.GetEvent
        .else ;in MIDIC64 HerMIDI uses this instead of its own
         lda HerMIDI.BuffIndex
         jmp GetLevn
        .fi
       .else
        sec ;tell host program there's no event for HerMIDI
        rts
       .fi

chLegac cpx #Legacy_max+1
        bcs chOther ;could go to other upper categories later
.if (MIDI_Legacy_support!=0 || (HerMIDI_support!=0 && HerMIDI_INC_EVENTS==0))
        lda GetData.WrIndex+1
GetLevn sta chIndex+1
rdIndex ldx #selfmod ;keeps track where we are reading in the buffer
ValuCnt ldy #selfmod ;stored remaining amount of value-bytes
chIndex cpx #selfmod ;stored value of buffer write-index
        bne rdEbyte  ;sets Carry-flag=1 if buffer is read out and exits
BuffEnd sty ValuCnt+1 ;we'll pick up in next round where we left off
        rts
rdEbyte lda MIDIbuffer,x
        bpl StorVal ;status (command above $7F) or value (below $80) ?
        cmp #MIDI.RealTime_min ;transparent (command not stored)
        bcs AdvBuf3 ;if above (RealTime Messages), set Accumulator only
StorCmd sta Command+1
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
        cpx #MIDIbuffer_size
        bcc +
        ldx #0 ;this makes a ring-buffer behaviour for legacy devices
+       jmp chIndex
StoVal2 ldy #0
        sty ValuCnt+1 ;initialize databyte-counter for next message
        tay ;Y=Value2
Command lda #selfmod
AdvBuf3 inx
        cpx #MIDIbuffer_size
        bcc +
        ldx #0 ;this makes a ring-buffer behaviour for legacy devices
+       stx rdIndex+1  
Value1  ldx #selfmod ;and Value2 is Y
        clc ;tell caller that reading the buffer is not yet over
        rts
.fi ;end of checking MIDI_Legacy_support
chOther ;no other types of devices yet...
.fi ;end of checking MIDIC64_INC_EVENTS
rtGetBy sec
        rts
        .pend
        
;-------------------------------common---------------------------------
.if (MIDI_Legacy_support!=0) 
SetMIDIintA lda #<GetData.InterruptGetByte
        ldy #>GetData.InterruptGetByte
SetIntA pha ;inputs: X=DeviceID, A=low-addr, Y=hi-addr
        lda IntrptType-Legacy_min,x
        bmi setNMIg
setIRQg pla
        jsr setIRQ
        lda #<IRQ.entry2
        ldy #>IRQ.entry2
        jmp +
setNMIg pla
        jsr setNMI
        lda #<NMI2
        ldy #>NMI2
+       sta GetData.HostInt+1
        sty GetData.HostInt+2
        rts
        
setIRQ  php ;store I-flag
        sei ;disable interrupt during setting
        sta $fffe
        sty $ffff
        plp ;restore I-flag
        rts
setNMI  sta $fffa
        sty $fffb
setRNMI sta $0318 ;because NMI can even happen while ROM is turned on
        sty $0319
        rts
.fi

;******************* Device-specific values & codes: *******************


;========================== HerMIDI object =============================
.if (HerMIDI_support!=0)
HerMIDI.INC_SYNCHR = HerMIDI_INC_SYNCHR
HerMIDI.INC_ASYNCH = HerMIDI_INC_ASYNCH
HerMIDI.INC_DIRECT = HerMIDI_INC_DIRECT
HerMIDI.INC_EVENTS = HerMIDI_INC_EVENTS
HerMIDI.Status = Status
HerMIDI.MIDIbuffer = MIDIbuffer
HerMIDI .binclude "HerMIDI/HerMIDI-C64.asm" ;contains HerMIDI-device's routines
.fi ;end of HerMIDI_support checking


;========================= Vessel object ===========================
.if (Vessel_support!=0)
Vessel .block
DevName = " VESSEL "
MIDIcontrol = $de00 ; not used
MIDIstatus  = $de02 ; not used
MIDI_Tx   = $dd01   ;write-only
MIDI_Rx   = $dd01   ;read-only
MIDIspeed= 1 
CounterDiv = (MIDIspeed==1) ? 0*bit1+1*bit0 : 1*bit1+0*bit0;div by 16/64
MIDIenable = WordSelect+CounterDiv ;NMI is disabled with this value
IRQenable  = RxIntEnable+WordSelect+CounterDiv
IntType = NMImode
.bend
.fi


;========================== Passport object ============================
.if (Passport_support!=0) ;PASSPORT/ (SYNTECH is the same)
Passport .block
DevName = "PASSPORT"
MIDIcontrol = $de08 ;write
MIDIstatus  = $de08 ;read
MIDI_Tx   = $de09   ;write
MIDI_Rx   = $de09   ;read
MIDIspeed= 1 
CounterDiv = (MIDIspeed==1) ? 0*bit1+1*bit0 : 1*bit1+0*bit0;div by 16/64
MIDIenable = WordSelect+CounterDiv ;IRQ is disabled with this value
IRQenable  = RxIntEnable+WordSelect+CounterDiv
IntType = IRQmode
.bend
.fi


;=========================== Datel object ==============================
.if (DatelJMS_support!=0) ;DATEL/SIEL/JMS/C-LAB (double-speed devices)
DatelJMS .block
DevName = "DATELJMS"
MIDIcontrol = $de04 ;write-only
MIDIstatus  = $de06 ;read-only
MIDI_Tx   = $de05   ;write-only
MIDI_Rx   = $de07   ;read-only
MIDIspeed= 2 
CounterDiv = (MIDIspeed==1) ? 0*bit1+1*bit0 : 1*bit1+0*bit0;div by 16/64
MIDIenable = WordSelect+CounterDiv ;IRQ is disabled with this value
IRQenable  = RxIntEnable+WordSelect+CounterDiv
IntType = IRQmode
.bend
.fi


;========================== NameSoft object ============================
.if (NameSoft_support!=0);codebase supported by Gartenzwerg(Frank Buss)
NameSoft .block
DevName = "NAMESOFT"
MIDIcontrol = $de00  ;write-only
MIDIstatus  = $de02  ;read-only
MIDI_Tx   = $de01    ;write-only
MIDI_Rx   = $de03    ;read-only
MIDIspeed= 1 
CounterDiv = (MIDIspeed==1) ? 0*bit1+1*bit0 : 1*bit1+0*bit0;div by 16/64
MIDIenable = WordSelect+CounterDiv ;NMI is disabled with this value
IRQenable  = RxIntEnable+WordSelect+CounterDiv
IntType = NMImode
.bend
.fi


;========================NameSoft-FB IRQ object =======================
.if (NameSoftIRQ_support!=0) ;NameSoft with FB uning IRQ instead of NMI
NameSoftIRQ .block        ;It's essentially the same as Sequential MIDI
DevName = "NAMESIRQ"
MIDIcontrol = $de00  ;write-only
MIDIstatus  = $de02  ;read-only
MIDI_Tx   = $de01    ;write-only
MIDI_Rx   = $de03    ;read-only
MIDIspeed= 1 
CounterDiv = (MIDIspeed==1) ? 0*bit1+1*bit0 : 1*bit1+0*bit0;div by 16/64
MIDIenable = WordSelect+CounterDiv ;NMI is disabled with this value
IRQenable  = RxIntEnable+WordSelect+CounterDiv
IntType = IRQmode
.bend
.fi


;=========================== Maplin object =============================
.if (Maplin_support!=0) ;Electronics - Maplin magazine (Vice/C64-midi.c)
Maplin .block
DevName = " MAPLIN "
MIDIcontrol = $df00 ;write
MIDIstatus  = $df00 ;read
MIDI_Tx   = $df01   ;write
MIDI_Rx   = $df01   ;read
MIDIspeed= 2 
CounterDiv = (MIDIspeed==1) ? 0*bit1+1*bit0 : 1*bit1+0*bit0;div by 16/64
MIDIenable = WordSelect+CounterDiv ;IRQ is disabled with this value
IRQenable  = RxIntEnable+WordSelect+CounterDiv
IntType = IRQmode
.bend
.fi


;====================== Moog Song Producer object ======================
.if (MoogSP_support!=0) ;Moog Song Producer
MoogSP .block
DevName = "MOOG SP."
MIDIcontrol = $de00 ;write (U17 "W")
MIDIstatus  = $de00 ;read  (U17 "W")
MIDI_Tx   = $de01   ;write (U17 "W")
MIDI_Rx   = $de01   ;read  (U17 "W")
;$de02: U18 "X" CR/SDR, $de03: U18 "X" TDR/RDR, 
;$de04: U19 "Y" CR/SDR, $de05: U19 "Y" TDR/RDR, 
;$de06: U20 "Z" CR/SDR, $de07: U20 "Z" TDR/RDR,
;$de08: Drum Trigger Latch 1 , $de09: Drum Trigger Latch 2
Control_Latch1 = $de0a ;set to $14 after init,set to $1C after disabling
Control_Enable  = $1C
Control_Disable = $14
;Control_Latch2 = $de0b
;$de0c, $de0d: FootSwitch inputs
MIDIspeed= 1 
CounterDiv = (MIDIspeed==1) ? 0*bit1+1*bit0 : 1*bit1+0*bit0;div by 16/64
MIDIenable = WordSelect+CounterDiv ;IRQ is disabled with this value
IRQenable  = RxIntEnable+WordSelect+CounterDiv
IntType = IRQmode
.bend
.fi


;=========================== MSSIAH object =============================
;Unfortunately no tech. info available for MSSIAH's MIDI-input
;DevName= " MSSIAH "


;***********************************************************************
MIDIC64_code_end
.cwarn(*>MaxAddr),"HerMIDI code & data shouldn't be over $d000!"


;===================== MISC. INTERNAL CONSTANTS ========================
MaxAddr=$cfff   ;code shouldn't be above this address (uses IO & KERNAL)
banksel=$01
;binary bit-values
bit0=1
bit1=2
bit2=4
bit3=8
bit4=16
bit5=32
bit6=64
bit7=128
selfmod=$00    ;default value of self-modified immediate operands
selfmodA=$1111 ;default value of self-modified absolute address operands
ROMRTI=$ea86 ;the RTI command at the end of $EA31 IRQ routine
;=======================================================================


.bend ;end of 'MIDIC64' block


;===================== General Constants, Specs ========================
MIDI .block ;their adjacent bytes can be of values $00..$7F (bit7 off)
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


;-----------------------------------------------------------------------
; Information about the Motorola MC6850 ACIA chip:
;ACIA (Asynchronous Communications Interface Adapter) Registers
;Bit: Control Register:          Status Register:  
;7     Receive Interrupt Enable   Interrupt Request (IRQ)
;6     Transmit Control 2         Parity Error (PE)
;5     Transmit Control 1         Receiver Overrun (OVRN)
;4     Word Select 3              Framing Error (FE)
;3     Word Select 2              Clear to Send (CTS) 
;2     Word Select 1              Data Carrier Detect (DCD)
;1     Counter Divide Select 2    Transmit Data Register Empty (TDRE)
;0     Counter Divide Select 1    Receive Data Register Full (RDRF)


;=======================================================================
; vim: sw=4 ts=4 syntax=asm:
