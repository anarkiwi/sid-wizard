;ident:8,24
;=========================SOME USEFUL COMMON SUPPORTING ROUTINES===============
; $Id: commonsubs.inc 385 2014-07-01 12:59:56Z hermitsoft $
;--------------------------------------------------------------------------------------
commonsubs .block


;moved here to have some space below $1000, etc.:
;-------------------------------------------------------------------------------

setInterrupt 
        lda #$7f
        sta $dc0d
        sta $dd0d
        lda $dc0d
        lda $dd0d
        lda #<IRQ
        sta $fffe       ;maskable IRQ pointer setting
        lda #>IRQ
        sta $ffff
        lda #<NMI
        sta $fffa
        lda #>NMI
        sta $fffb
        ldx #1
        stx $d01a       ;enable raster-IRQ
        stx $d019       ;suppress possibly running raster-IRQ
        stx $d012       ;(IRQ will wait till row $F4 anyway)
        ;stx inirequ+1   ;ensure that player initialization happens at startup
setKERNALdiskIO
.if (KERNAL_LOADSAVE !=0 || FINAL_CARTRIDGE_3_FIX!=0)
        lda #<KERNAL.LOAD2 ;disable turbo for Final Cartridge III as it seems to clash with programs above $F000
        ldx #>KERNAL.LOAD2
        sta KERNAL.ILOAD+0
        stx KERNAL.ILOAD+1
        lda #<KERNAL.SAVE2
        ldx #>KERNAL.SAVE2
        sta KERNAL.ISAVE+0
        stx KERNAL.ISAVE+1
.fi
        rts

initpos lda #0
        ldy #zerovar1_end-zerovar1
-       sta zerovar1,y
        dey
        bpl -
        rts

NoteArp ldy #$38 ;SEC   ;activate Note-display mode in chord-table
        sty display.chNoteM
        ldy #$0d        ;columnlength for chordtable
        jsr display.incptr2
        ldy #$18 ;CLC   ;back to normal (number-displaying mode)
        sty display.chNoteM   
        rts

;--------------------------------------------------------------------------
barcolo ;input: color in X - displays/hides rasterbar or not, depending on config-bits
        lda Config.confbits
        and #2          ;disabled rasterbars?
        bne +
        nop
        nop
        stx $d020
+       rts

;------------------------------------------------------
disNote 
.if (MIDI_support!=0)
.enc screen
disPoly ldx #"D" ;'De-
        ldy #"T" ;Tune'
        lda Config.confbits
        and #8 ;Polyphonic MIDI-mode?
        beq +
        lda Config.MIDIchn ;only if MIDI is used
        cmp #Config.ChnOff
        beq +
        ldx #$b9 ;PO
        ldy #$bf ;LY
+       stx vidram+5*40+35
        sty vidram+5*40+36
.enc none
.fi
        lda #$7a ;"Chords" table-top
        ldy #$7b
.if (MIDI_support!=0)
        ldx midisubs.jamnote
        beq +
        lda midisubs.jamvelo
        beq ++
        sta vidram+5*40+36
        lda #$8a ;'Vel' (velocity)
        sta vidram+5*40+35
        jmp ++
.fi
+       bit Config.confbits
        bvc ++          ;Note-mode in chord-table?
        ldx display.BasNote+1
+       dec banksel ;$34
        lda notechar,x
        ldy octachar,x
        inc banksel ;$35
+       sta vidram+5*40+37
        sty vidram+5*40+38
        rts

;------------------------------------------------
afterptn                ;clear space after PTN in GT-like display mode (in JCH mode next ptn comes)
        lda vidzptr
        clc
        adc #$28
        sta vidzptr
        bcc nohinc6
        inc vidzptr+1
nohinc6 dec zpvari4
        bmi endcold
        ldy #7
        lda #$bd        ;space (empty char)
afpdlp  sta (vidzptr),y
        dey
        bpl afpdlp
        gmi afterptn   ;jmp afterptn ;optimize 1 byte
endcold rts            ;optimize 1 byte

;-------------------------------------------------------------------------------
row25back_keyer

row25back               ;set back row25 colours (only the different colors) after row26 trick
        lda #$ff
        eor menumode    ;hide cursor if menu is displayed
        sta $d015
;FFFFsto lda #selfmod    ;it was idle pattern zeroed, now we restore it as IRQ hi-pointer
;        sta $ffff
        ldx ipacol
        stx COLRPOS+$3de
        stx COLRPOS+$3df
        stx COLRPOS+$3e0
        lda itahdrcol
        sta COLRPOS+$3c0+$21
        stx COLRPOS+$3c0+$22
        stx COLRPOS+$3c0+$23
        stx COLRPOS+$3c0+$24
        sta COLRPOS+$3c0+$25
        sta COLRPOS+$3c0+$26
        stx COLRPOS+$3c0+$27

        ldx #3
-       
.if (SID_AMOUNT==1)
        lda mutesolo-1,x ;-1, because x counts from 3 to 1, not from 2 to 0
.elsif (SID_AMOUNT>1)
        txa
        clc
        adc trkposi
        tay
        lda mutesolo-1,y
.fi
        and #1
        tay
        lda msolocol,y  ;mute/solo colour index in datatables.inc
        pha
        bit Config.confbits ;instrument auto-append on/off bit
        bmi +           ;if ON, instruments are the same colour as notes
        lda msolocol+2,y ;instrument-column mute-solo colours (in datatables.inc)
+       ldy stplptr-1,x ;step-highlight column indexes (screen-positions)
        sta COLRPOS+$3c2+1,y
        sta COLRPOS+$3c3+1,y
        pla
        sta COLRPOS+$3c0+1,y ;note-mutesolo color-position indexes
        sta COLRPOS+$3c1+1,y
        lda COLtemp-1,x ;swapping the step-highlighting colours back
        sta COLRPOS+$3c0,y
        dex
        bne -
;        rts

keyer   lda #0
        sta IRQ.d012sto+1   ;1st multispeed-bar position will be count from this rasterline ($100)
        sta IRQ.fastfwd+1   ;reset fast-forward in every frames
        jsr commonsubs.reftpos     ;runtime:1 rasterline; refresh table-base positions in jmtbase to add them in displayer/datawriter
        jsr keyhandler.keyer       ;runtime:8..14 rasterlines; better here if raster-runthrough is detected for multispeed (batch functions take much time)
        jmp playadapter.followplay ;runtime:1 rasterline; if follow-mode is on, this subroutine overrides key navigation and controls patterns



instinit .proc ;initialize (zero) cursor/scroll positions at instrument-switching
        lda #0
        ldy #2
-       sta inswXbuf+1,y ;stx subwpos3+2 ;zero cursor Y-position in window 2 (instr.window)
        sta inswYbuf+1,y ;stx subwpos2+2 ;zero cursor X-position in window 2
        sta wfarpos,y
        dey
        bpl -
;        bit PWRESETSW ;.if (PWRESETSW_ON+FILTRESETSW_ON)
;        bmi +
;        bit FILTRESETSW
;        bmi +
        lda playadapter.playmod
        bne +           ;only if stopped
        jsr playadapter.resetJamIns ;good for instruments which has PW/Filter-reset switched off
        +
goirefr inc display.insrefr+1 ;to display it
        rts
        .pend


seqInst .proc
        jsr commonsubs.setseqptr ;get sequence pointers
        ldy #seqlength-1 ;one before last byte
        jsr commonsubs.seqaddb
        ;cmp #$ff       ;if sequence is full
        ;beq retpnto
        cmp #$fe        ;if sequence is full
        bcs retpnto ;beq retpnto
        jsr commonsubs.getcurptn ;get orderlist memory index under cursor to A and Y
        jsr commonsubs.seqaddb
        beq retpnto     ;don't let pattern $00 to be overwritten (it's special)
        dey
        jsr commonsubs.seqaddb ;check if we are not on loop-jumpposition number or on 00 value
        cmp #$ff
        beq retpnto     ;if it's jumpposition operand, don't proceed with insertion
        ;iny
        sty olinsrc+1
        lda commonsubs.seqaddb+1
        sta olinsrt+1
        sta olinsrb+1
        lda commonsubs.seqaddb+2
        sta olinsrt+2
        sta olinsrb+2
        ldx #seqlength-1
        ldy #seqlength
olinsrt lda selfmodA,x  ;insertion loop, move the rest forward
olinsrb sta selfmodA,y
        dex
        dey
olinsrc cpx #selfmod
        bne olinsrt
        lda #$01        ;value to write into empty place, and Y holds current position
        jsr commonsubs.seqaddc
retpnto rts 
        .pend

moreSeqInst .proc       ;insert into the sequence(s) 
seqstpr1=keyhandler.norcrri.seqstpr1
        lda subwpos1+1
        pha
        lda #CHN_AMOUNT-1
        sta subwpos1+1
-       jsr commonsubs.seqInst
        dec subwpos1+1
        bpl -
        pla
        sta subwpos1+1
adjust  iny
        jsr commonsubs.seqaddb
        cmp #$fe
        bcc retpnto ;beq +
        ;cmp #$ff
        ;bne retpnto    ;deleret
+       lda subwpos2+1  ;X cursor-position in orderlist-editor
        cmp #39
        beq +
        jsr seqstpr1    ;step to still see $ff and inserted amount
+       jsr seqstpr1
retpnto rts
        .pend


;---------------------------------------------------
PALorNTSC .proc ;detect if PAL or NTSC machine, store 0/1/3 result at a memory location 'PALNTSC' in 'irq.inc'
l1      lda $d012
l2      cmp $d012
        beq l2          ;waiting 1 rasterline's time (no matter CPU speed)
        bmi l1        
;the Accumulator here contains the last rasterline that was before $100   ($37:PAL/Drean, $06:NTSC, $05:oldNTSC)
        sta framemax    ;set maximal raster-row number                     %00110111      %00000110 %00000101
        ldx #0          ;0 for NTSC
        cmp #<(PALfullrast-1) ;$37 ;PAL/Drean machine?
        bne +
        ldy #$A1 ;have enough loops (63/(65-63)=32 rows) to differentiate PAL vs Drean (they arrive on different lines)
.page ;avoid added cycles by page-boundary, we measure the 2 extra cycles per line of Drean
-       stx $d015    ;4 cycles delay (plus avoid cycles stolen by sprites (badlines are not an issue in border area))
        lda $d012    ;4 cycles ;this is safest if we're at the middle of the rasterrow here
        dey          ;2 cycles 
        bne -        ;2 cycles (3 at exit)
.endp
        inx             ;1 for PAL
        cmp #$20    ;rasterline $20 if Drean, $31 if PAL
        bne +
        ldx #3 ;$81     ;3 for Drean
        stx Config.TuningType
+       stx PALNTSC     ;set to 0 if NTSC, 1 if PAL, 3 if Drean (PAL-N)
        rts
        .pend

xwaitr1 .proc ;wait around 1 raster-row (used in irq.inc)
        ldx #5
xwaiter dex
        bne xwaiter
        nop
        rts
        .pend

timecount .proc         ;counts the timer if playmode is 1 (songplay mode) - called from IRQ
        lda seconcnt
        bpl nominchg
        ldx PALNTSC
        lda framesec,x
        sta seconcnt
;        lda playmod     ;checked in irq already
;        beq retpnt1
        sed             ;set decimal mode for timer BCD digits
        lda songtime+1  ;seconds
        clc
        adc #1
        sta songtime+1
        cld             ;disable decimal mode for timer BCD digits
        cmp #$60
        bne nominchg
        lda #0
        sta songtime+1
        sed             ;set decimal mode for timer BCD digits
        lda songtime    ;minutes
        clc
        adc #1
        cld             ;disable decimal mode for timer BCD digits
        cmp #$60
        bne countfull
        lda #0
countfull sta songtime
nominchg dec seconcnt
retpnt1 rts
        .pend

seconcnt .byte PALframes1second
framesec .byte NTSCframes1second, PALframes1second, 50, DREANframes1second

;------------------------------------------------------------------------------
resetimer .proc         ;resets the timer
        lda #0
        sta songtime    ;reset minutes
        sta songtime+1  ;reset seconds
        ldy PALNTSC
        lda framesec,y
        sta seconcnt
        rts
        .pend

;================================================================================
setptnptr .proc         ;set pattern-pointers (memory index of display-position calculated by displayer)
        sty ystorew+1   ;for safety
        ldx subwpos1+0  ;current track
        ldy curptn1,x   ;current pattern
        lda ptnMpos,x   ;set selected pattern's lo-pointer
        clc
        adc PPTRLO,y    ;set selected pattern's lo-pointer with display-shift, this real pointer is calculated by displayer
        sta instpt1+1
        sta instpt2+1
        sta ptnzptr+0   ;zeropage lo-pointer dedicated for pattern-routines inside IRQ
        lda ptnMpos,x
        lda PPTRHI,y
        adc #0          ;add Carry-flag as well
        sta instpt1+2
        sta instpt2+2
        sta ptnzptr+1   ;zeropage hi-pointer dedicated for pattern-routines inside IRQ
ystorew ldy #selfmod    ;restore Y
        rts
        .pend

setinsptr .proc         ;can be run concurrently in main part and IRQ, because only 1 instrument is selected at a time
        ldx display.selinst+1 ;load selected instrumet's basepointer to readers/writers
setinp2 lda INSPTLO,x
        sta instpt1+1
        sta instpt2+1
        sta inszptr
        lda INSPTHI,x
        sta instpt1+2
        sta instpt2+2
        sta inszptr+1
        rts
        .pend

instpt1 .proc
        lda selfmodA,x  ;instrument/pattern basepointer for X indexing - read
        rts
        .pend

instpt2 .proc
        sta selfmodA,x  ;instrument/pattern basepointer for X indexing - write
        rts
        .pend

setseqptr .proc         ;set pointer of current sequence data
        lda subwpos1+1  ;current track in orderlist
        .cerror setsep2 & 0 ;dependency hack
        .pend

setsep2 .proc
        asl
        sta olastor+1
        jsr getsubaddr  ;subtune orderlist-pointer to A and Y, clear C flag
olastor adc #selfmod
        tay
        lda SUBTUNES,y  ;READ ORDERLIST LO-ADDRESSES FOR SUBTUNE
        sta seqaddb+1   ;SET ORDERLIST LO-ADDRESSES FOR SUBTUNE
        sta seqaddc+1   ;SET ORDERLIST LO-ADDRESSES FOR SUBTUNE
        sta seqadde+1   ;SET ORDERLIST LO-ADDRESSES FOR SUBTUNE
        iny
        lda SUBTUNES,y  ;READ ORDERLIST HI-ADDRESSES FOR SUBTUNE
        sta seqaddb+2   ;SET ORDERLIST HI-ADDRESSES FOR SUBTUNE
        sta seqaddc+2   ;SET ORDERLIST LO-ADDRESSES FOR SUBTUNE
        sta seqadde+2   ;SET ORDERLIST HI-ADDRESSES FOR SUBTUNE
        rts
        .pend

seqaddb .proc
        lda selfmodA,y  ;orderlist pointer for Y indexing - read
        rts
        .pend

seqaddc .proc
        sta selfmodA,y  ;orderlist pointer for Y indexing - write
        rts
        .pend

;------------------------------------------------------------------------------
getcurnote .proc        ;get note-data in current track/pattern at cursor position to A
        ldx subwpos1+0  ;current track
        lda ptncurs,x
        tax             ;X is now the note's position/index in pattern memory
        jmp instpt1
        .pend

getcurptn .proc         ;get orderlist memory index under cursor to A and Y
        lda subwpos2+1  ;orderlist X cursor position
        lsr
        clc
        adc seqdpos+1   ;add orderlist-displayposition
        tay
        rts
        .pend

cSeqLop .proc           ;check if loop-position under cursor, Carry=1 if it is
        jsr setseqptr   ;get & set sequence pointers
        jsr getcurptn   ;get orderlist memory index under cursor to A and Y
        dey
        jsr seqaddb
        iny
        cmp #$ff        ;at loop-position?
+       rts             ;Carry-flag is set accordingly
        .pend

;---------------------------
chkcury .proc           ;check, if the cursor is inside the new track's pattern area
        jsr compcur
        bcc insidep
        lda ptnlength,y ;place the cursor to last row of pattern ending upperwards
        sec
        sbc prowpos,x   ;substract displayed row position from patternlength, that will be new cursor-position
        bcs +
        lda ptnlength,y
        sta prowpos,x
        lda #0
+       sta subwpos3+0
insidep rts
        .pend

;------------------------------------------------------------------------------
compcur .proc           ;compare cursor+patterndisplay position to patternlength
        ldx subwpos1+0  ;current tracknumber to X
        ldy curptn1,x   ;current pattern to Y
        lda prowpos,x   ;current pattern displayposition to A
        clc             ;important to compare in 16 bits
        adc subwpos3+0  ;current track display's current row (cursor row)
        bcs +           ;if sum of patternlength and cursorpos is bigger than $ff - important to compare in 16 bits
        cmp ptnlength,y ;compare to length of current pattern
+       rts
        .pend

;------------------------------------------------------------------------------
wrhxad1 = writehex1.wrhxad1;self-written
writehex1 .proc         ;merge typed 1st hexa digit with existing data
        and #$0f
        sta wrhxad2+1
wrhxad1 lda #selfmod
        asl
        asl
        asl
        asl
wrhxad2 ora #selfmod
        rts
        .pend

wrhxad3 = writehex2.wrhxad3 ;self-written
writehex2 .proc         ;merge typed 2nd hexa digit with existing data
        and #$f0
wrhxad3 ora #selfmod
        rts
        .pend

;------------------------------------------------------------------------------
getsubaddr .proc        ;get subtune orderlist-pointer to A and Y, clear C flag
        lda selsubt+1
.if (SID_AMOUNT>=2)
        asl
.fi
.if (SID_AMOUNT>=3)
        asl
.fi
mul8toY asl
        asl
        asl             ;GET SUBTUNE ADDRESS FROM ACCU MULTIPLIED 8
        tay
        clc
retoldi rts
        .pend

;-----------------------------------------------
calcsubsp1 .proc        ;subtune given in A, outputs speed1 in A, pos. in Y
        lda selsubt+1
        ;and #$3f
        asl
        asl
        asl
.if (SID_AMOUNT>=2)
        asl
.fi
.if (SID_AMOUNT>=3)
        asl
.fi
        clc
        adc #(SID_AMOUNT==1)? 6 : (SID_AMOUNT==2)? 6*2+2 : 6*3+12
        tay
        lda SUBTUNES,y  ;why not subtunes+6 instead? Maybe many things are built on this already, maybe not..
        rts
        .pend

;------------------------------------------------------------------------------
calcmwpos .proc         ;calculate main inst.window instrument data position
        asl
        asl
        ldx subwpos1+2  ;instrument-subwindow
        adc inswYbuf,x  ;adc subwpos3+2
        tax
        lda mainstrw,x
        tax
        rts

mainstrw .section data
        .byte 3,1,5,7,4,2,6,8 ;place of data in instrument (for datawriter)
        .send data
        .pend

;------------------------------------------------------------------------------
ihexdis .proc
        lda (inszptr),y
        iny
        .cerror hexdis2 & 0 ;dependency hack
        .pend

hexdis2 .proc
        pha             ;input:accu, output: nybbles in Accu and X-register
        and #$0f
        tax
        pla
        .cerror div16 & 0 ;dependency hack
        .pend

div16 .proc
        lsr
        lsr
        lsr
        lsr
        rts
        .pend

        .enc screen
hexchar .text "0123456789ABCDEF"
        .enc none

div12 .proc             ;divide Accumulator by 12, result is in Y, remainder in Accumulator 
        ldy #7          ;loop to divide by 12
-       cmp OCTMULS,y
        bpl +
        dey
        bpl -
+       sec
        sbc OCTMULS,y   ;remainder to Accu
        rts
        .pend
        .section data
OCTMULS .byte 0,12,24,36,48,60,72,84;TRANSPOSITION VALUE OF OCTAVES TO ADD/SUBSTRACT FROM NOTE
        .send data 

incOctave ;called from keyhandler (C= + +)
        lda jamoctave+1
        cmp #7
        bpl +
        inc jamoctave+1
+       rts

decOctave
        lda jamoctave+1
        beq keyrett
        dec jamoctave+1
keyrett rts


;------------------------------------------------------------------------------
chptful .proc           ;check if pattern is full, A and X preserved, carry bit is 0 if pattern is full
        pha
        txa
        pha
        ldx subwpos1+0  ;current tracknumber to X
        ldy curptn1,x   ;current pattern to Y
        lda ptnsize,y   ;precheck if pattern is not full, later checks in code are obsolete
        cmp #maxptnlen  ;checking not #$ff because 4 byte insertion is possible with new bigfx
        pla
        tax
        pla
        rts
        .pend
;-------------------------------
getlast .proc           ;get memory position of last row data to X - relative to displayposition
        ldy subwpos1+0  ;current track
        ldx curptn1,y   ;current pattern
        lda ptnlength,x
        .cerror detmemp & 0 ;dependency hack
        .pend

detmemp .proc
        sec             ;get memory position given (accu) row data to X - relative to displayposition
        sbc prowpos,y   ;accu here is row-pos. of row relative to displayposition
        tay
        dey             ;? patternlength change done afterwards in caller
        ldx #0          ;starts as 0, beginning of display area, accumulates the mem-amount in X
xdetlop cpy #0          ;test Y
        beq retget2
        jsr instpt1
        bpl decyer
        inx
        jsr instpt1
        bpl decyer
        inx
        jsr instpt1
        and #$e0
        bne decyer
        inx
decyer  inx
        dey
        bne xdetlop
retget2 rts
        .pend

;-------------------------------------------------------------------
multip3 .proc           ;multiply value in Accu by 3, the result into Accu, original value to X
        stx vidzptr
        sty vidzptr+1
        tax
        ;and #$3f        ;max. amount of table-rows
        .cerror multi3b & 0 ;dependency hack
        .pend

multi3b .proc
        sta mul3sto+1
        asl
        clc
mul3sto adc #selfmod
        clc
        rts
        .pend

div3acu .proc
        ldy #0          ;divide accu by 3, result in Y and Accu
-       sec
        cmp #3
        bcc +
        sbc #3
        iny
        bne -
+       tya
        rts
        .pend

;------------------------------------------------------------------------------
getitbi .proc           ;get current instrument table row pointer to X
        lda #WFTABLEPOS ;WF/ARP table position relative to instrument-base (globalconstants.inc)
        ldy subwpos1+2  ;current instrument subwindow
        dey             ;transform from 1..3 to 0..2
        beq +           ;if WAVEFORM-ARP table
        tya
        clc
        adc #$0a-1      ;PW/Cutoff-table pointer (relative to instrument basepointer)
        tax
        jsr instpt1     ;get relative PW/Cutoff-table pointer
+       sta ipostor+1

        lda wfarpos,y
        clc
        ldx subwpos1+2  ;instrument-subwindow
        adc inswYbuf,x  ;adc subwpos3+2 ;Y cursor-coordinate in instrument-subtable
        jsr multi3b     ;multiply A by 3, clear Carry-flag
ipostor adc #selfmod
        tax
getiret rts
        .pend

reftpos .proc           ;refresh table-base positions in jmtbase to add them in displayer/datawriter
        ;jsr setinsptr   ;load selected instrumet's basepointer to zeropage pointers
        ldx #SWI_PULSETBPT_POS ;$0a ;index to PW-table pointer
        jsr instpt1
        sta jmtbase+2
        inx             ;index to filtertable-pointer
        jsr instpt1
        sta jmtbase+3
        rts
        .pend

jmtbase .byte 0,WFTABLEPOS,$11,$12 ;table-basepointers to add to $FE jumpaddress calculation
        ;(helps player by pre-calculating real table-positions at typing)

;------------------------------------------------------------------------------
getctpos .proc
        ldx display.selchor+1 ;gets current chorddata position to X, value to A
        lda CHDPTRLO,x
        clc
        adc chorpos
        adc subwpos1+3
        tax
        lda CHORDS,x
        rts
        .pend

gettpos .proc
        ldx display.seltemp+1 ;check if tempoprogram-length has been reached
        lda TEMPTRLO,x
        clc
        adc temppos
        adc subwpos1+4
        tax
        lda TEMPOTBL,x
        rts
        .pend

;------------------------------------------------------------------------------
sseqend .proc           ;set position to end of orderlist, if there is $00 under cursor (when moving cursor to shorter sequence)
        ;test if $ff is at previous place or $fe is in current place in orderlist
        jsr cSeqLop     ;check if loop-position under cursor, Carry=1 if it is
        bcs retsseq
        jsr seqaddb     ;get data under orderlist cursor into A, index in orderlist is Y
        bne retsseq     ;if not zero, do nothing
        ldy #0          ;seek for $fe/$ff endsignals from beginning of sequence
seqslop jsr seqaddb     ;maybe slow this way, maybe not that much
        cmp #$fe
        bcs + ;beq +
        ;cmp #$ff
        ;beq +
        iny
        cpy #seqlength
        bne seqslop
        ldy #0          ;if not found, position 0 seems to be safe
+       tya             ;Y and A holds the endsignal position here
        pha
        sec
        sbc seqdpos+1   ;the distance of seq endsignal-position from display-position
        bcc +           ;if end not inside screen (no need to check bigger, just smaller)
        asl
        sta subwpos2+1  ;set cursor to end
        pla
        rts
+       pla
        sta seqdpos+1
        lda #0
        sta subwpos2+1
retsseq rts
        .pend

;------------------------------------------------------------------------------
seqadde = jmptopt.seqadde ;self-written
seleptn = jmptopt.seleptn
postop2 = jmptopt.postop2
jmptopt .proc           ;jump to pattern under orderlist cursor, y holds track number
        sty jptysto+1
        tya
        jsr setsep2     ;set pointer of sequence data selected by Y

        jsr getcurptn   ;get orderlist memory index under cursor to A and Y
        tax
        dey
        jsr seqaddb     ;get data before cursorposition, check, if $ff
        iny
        cmp #$ff        ;if it's loop-position value, don't change selected pattern
        beq jptret      ;+

jptysto ldy #selfmod
seqadde lda selfmodA,x
        beq jptret      ;+  ;if zero pattern, no go.
seleptn ldx playadapter.followp+1 ;if followplay is on, don't disturb pointers
        bne jptret
        cmp #maxptnamount+1
        bcs jptret      ;+  ;if bigger than the allowed number of patterns
        sta curptn1,y
        jsr setEmpty
        lda #0
        sta prowpos,y
postop2 lda #0
        sta curwind
        sta subwpos2+0
        sta subwpos3+0
jptret  rts
        .pend

.if (SID_AMOUNT==2)
jmptopt_SID2
        ldy #3
        jsr jmptopt
        ldy #4
        jsr jmptopt
        ldy #5
        jmp jmptopt
.elsif (SID_AMOUNT>=3)
jmptopt_SID23
        ldy #3
        jsr jmptopt
        ldy #4
        jsr jmptopt
        ldy #5
        jsr jmptopt
        ldy #6
        jsr jmptopt
        ldy #7
        jsr jmptopt
        ldy #8
        jmp jmptopt
.fi

setEmpty .proc ;set number of 1st empty-pattern given in Accu
        tax
        inx
        cpx #maxptnamount+1
        bcs +
        cpx keyhandler.firstEmptyPtn
        bcc +
        stx keyhandler.firstEmptyPtn
+       rts
        .pend

setFlwP ldx #0
        stx playadapter.maxrasti ;reset rastertime-measuring
        lda Config.confbits
        and #$04        ;follow-play is default?
        ora cbmsig
        beq +
        ldx #1
+       stx playadapter.followp+1 ;if commodore key is pressed together, follow-playing switches ON, else OFF
        rts


;------------------------------------------------------------------------------
getdelamount .proc      ;gets the amount of data (Y) to delete at pattern-memory position (given in X)
        ldy #1          ;stays 1 if only note column exists, otherwise increases up to 4
        .cerror getdla2 & 0 ;dependency hack
        .pend

getdla2 .proc
        stx ptdelpx+1
        cmp #0
        bpl dodelpt     ;if only note column, delete only one byte
        inx
        iny             ;1/2
        jsr instpt1     ;read and test instrument column
        bpl dodelpt
        inx
        iny             ;2/3
        jsr instpt1     ;read and test fxtype column
        and #$e0
        bne dodelpt
        iny             ;3/4
dodelpt
ptdelpx ldx #selfmod    ;stored X
        rts
        .pend

eorconf .proc
        eor Config.confbits
        sta Config.confbits
        rts
        .pend


calcoct .proc           ;add octave multiplied by 12 to Accu (note in octave)
        ldy jamoctave+1 ;octaves must be added
        beq +
-       clc             ;loop to multiply note-octave by 12
        adc OCTMULS,y ;adc #12
        ;dey
        ;bne -
+       cmp #SWM_NOTE_MAX+1 ;if octave got too big, no note entry
        rts
        .pend


notebit7 ora #$80       ;no second column yet, sign it in note column
        pha
        ldy #1
        lda #0
        jsr insertpt    ;insert data, move rest, check pattern overload
        pla
        rts


        .bend
;==============================================================================
; vim: sw=4 ts=4 syntax=asm:
