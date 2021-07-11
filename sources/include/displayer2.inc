;ident:8,24
; $Id: displayer2.inc 382 2014-06-23 17:53:01Z hermitsoft $

display2 .proc          ;refreshers, called from IRQ
        lda curwind
        cmp #1          ;orderlist
        bne +
        inc playadapter.seqrefr ;refresh also when playing changes SEQCNT,x

+       cmp #2          ;instrument window / chordwindow / tempotable - refresh all
        bmi +
        inc display.insrefr+1
+       rts
        .pend

;-------------------------------------------------------------------------------
jammode = cursord.jammode ;self-written variable ;if 0 - edit, if 1 - jam
stplptr = cursord.stplptr

cursord .proc ;cursor displayer - BE CAREFUL TO KEEP THIS UNDER 3-4 rows' rastertime (called from IRQ bars)

        lda namemode    ;instrument name-typing?
        beq +
        lda #0
        sta cury+1
        lda #32
        clc
        adc compzvar2   ;inamepos ;cursor-position of instrument-naming
        sta curx+1
        lda #$fe
        sta $d01d
        gne otherwi

+       lda curwind
        bne chkwi1      ;patternwindow?
        lda subwpos3+0  ;calculate Y cursorpos. in pattern-window
        sta cury+1
        ldy #$ff
        lda subwpos2+0  ;calculate X cursorpos. in pattern-window
        beq nonotep     ;if cursor in note column
        dey             ;ldy #$fe
        clc
        adc #1
nonotep sty $d01d
        ldy subwpos1+0  ;track number (0..2)
.if (SID_AMOUNT>1)
        pha
        tya
        sec
        sbc trkposi
        tay
        pla
.fi
        sec             ;add plus 1, because values in stplptr are counting from 0
        adc stplptr,y
        sta curx+1
        jmp otherwi
chkwi1  ldy #$fe
        sty $d01d
        cmp #1          ;orderlist?
        bne chkwi2
.if (SID_AMOUNT==1)
        ldy subwpos1+1  ;track number (Y-position) in orderlist
.elsif (SID_AMOUNT>1)
        lda subwpos1+1
        sec
        sbc trkposi
        tay
.fi
        lda PALNTSC
        beq +           ;if NTSC, orderlist cursor-position should be 0
        lda PALseqposy,y ;asl                ; - multiply A by 2
+       sta $d001
        lda subwpos2+1
        sta curx+1
        jmp justxdis
chkwi2  cmp #2          ;instrument window
        bne chkwi3
        ldy subwpos1+2  ;instrument-subwindow (1..4)
        lda inswXbuf,y  ;lda subwpos2+2
        clc
        adc iswposx,y
        cpy #0
        bne noiwin0
        cmp #36         ;check instrument window 0 position X
        bmi noiwin0
        clc
        adc #2
noiwin0 sta curx+1
        lda iswposy,y
        clc
        adc inswYbuf,y  ;adc subwpos3+2
        sta cury+1
        jmp otherwi

        .section data
iswposx .byte 32,31,31,31 ;instrument-window X basepositions (common,wf,pw,ctf)
iswposy .byte 01,6,15,20 ;instrument-window Y basepositions (common,wf,pw,ctf)
PALseqposy .byte 0,1,2 ;0*PALseqdistance, 1*PALseqdistance, 2*PALseqdistance ;PAL orderlist cursor-positions (NTSC handled differently above)
stplptr .byte 0,<(stp2plc-stp1plc),<(stp3plc-stp1plc) ;step-higliht-screen-pointers
        .send data

chkwi3  cmp #3          ;chordtable
        bne chkwi4
        lda subwpos1+3
        clc
        adc #6
        sta cury+1
        lda #38
        clc
        adc subwpos2+3
        sta curx+1
        bit Config.confbits
        bvc +           ;note-mode for chord-table?
        ;cmp #39
        ;beq +
        lda #$ff
        sta $d01d
+       jmp otherwi
chkwi4  lda subwpos1+4  ;tempotable
        clc
        adc #20
        sta cury+1
        lda #38
        clc
        adc subwpos2+4
        sta curx+1

otherwi
cury    lda #1          ;self-written variable ;cursor y coordinate (0..25..28)
        asl
        asl
        asl
        clc
        adc #$22
        sta $d001
justxdis
        ;cursor-sprite display (coordinates,colour)
curx    lda #selfmod    ;cursor x coordinate (0..39)
        asl
        asl
        asl
        php
        clc
        adc #$18
        sta $d000
        lda #$c0
        bcc no9bit1
        ora #$01
no9bit1 plp
        bcc no9bit2
        ora #$01
no9bit2 sta $d010
        ;display cursor colour
        lda #<($100-CursorFlashSpd1)
jammode ldy #1          ;self-written variable ;if 0 - edit, if 1 - jam
        bne +           ;write/jam mode of keyboard
        lda #<($100-CursorFlashSpd2) ;if edit-mode, flash faster
+        clc
        adc curcolc+1   ;added later (rev.108) to make the difference between jam/edit mode more percievable
        sta curcolc+1   ;cursor color flasher
        bpl norescc
        lda #<(curcolend-curcols)-1
        sta curcolc+1
norescc
curcolc ldy #selfmod    ;cursor colour counter
        lda curcols,y
        sta $d027
        
retget rts

curcols .section data
        .byte $00,$0b,$06,$04,$0e,$0c,$03,$0f,$0d,$07,$07,$01;cursor colours to flash
        .byte $07,$01,$01,$07
        .byte $01,$07,$07,$0d,$0f,$0a,$0c,$08,$02,$0b,$09,$00
        .byte $09,$00,$00,$09
curcolend
        .send data

        .pend

;===============================================================================

setrpos .proc           ;set pointers to real ptn-positions of the display-starting positions
        ldy #3-1
        sty zpvari2

realop1 
.if (SID_AMOUNT==1)
        ldy zpvari2
.elsif (SID_AMOUNT>1)
        lda zpvari2
        clc
        adc trkposi
        tay
.fi
        ldx prowpos,y   ;the display position of track Y
        lda curptn1,y   ;the pattern in track Y
        tay
        lda PPTRLO,y    ;set pattern pointer for current pattern
        sta datzptr
        lda PPTRHI,y
        sta datzptr+1

        ldy #0
ydetlp  cpx #0          ;detect memory-index of given row
        beq getnext
        lda (datzptr),y
        bpl decxer0
        iny
        lda (datzptr),y
        bpl decxer0
        iny
        lda (datzptr),y
        and #$e0
        bne decxer0
        iny

decxer0 iny
        dex
        jmp ydetlp

getnext ldx zpvari2
.if (SID_AMOUNT>1)
        txa
        clc
        adc trkposi
        tax
.fi
        tya
        sta ptnMpos,x
        dec zpvari2
        bpl realop1
        rts
        .pend
;===============================================================================
seqdpos = seqdisp.seqdpos;self-written variable ;display-position of orderlist (sequence)

seqdisp .proc           ;display orderlist numbering in 26th row (video ram 2)
seqdpos ldx #selfmod    ;display-position of orderlist (sequence)
        ldy #0
seqdlp  txa
        pha
        #divAby16 ;jsr commonsubs.div16
        cpx playadapter.olstart
        bne +
        ora #$30
+       sta row26,y
        pla
        and #$0f
        ora #$90
        cpx playadapter.olstart
        bne +
        stx sdxstor+1
        and #$0f
        tax
        lda commonsubs.hexchar,x
sdxstor ldx #selfmod    ;X stored before
+       sta row26+1,y
        inx
        iny
        iny
        cpy #$28
        bne seqdlp
        rts
        .pend
;===============================================================================
.if POS_DISP_MOD==2
blinkit = ordldis.blinkit ;self-written - blink or not?
.fi

ordldis .proc           ;display orderlist values in sprite-bitmap ;zeropage: 2
        lda #$34        ;orderlist characters are under IO area
        sta banksel
        ldx #3-1
        stx zpvari7     ;counter for number of track
-       jsr commonsubs.getsubaddr ;get subtune-address position to Y
.if (SID_AMOUNT>1)
        adc trkposi
        adc trkposi
.fi
        adc zpvari7
        adc zpvari7     ;added 2 times
        tay
        lda SUBTUNES,y  ;READ ORDERLIST LO-ADDRESSES FOR SUBTUNE
        sta seqaddr+1   ;SET ORDERLIST LO-ADDRESSES FOR SUBTUNE
        lda SUBTUNES+1,y ;READ ORDERLIST HI-ADDRESSES FOR SUBTUNE
        sta seqaddr+2   ;SET ORDERLIST HI-ADDRESSES FOR SUBTUNE
        jsr dseqrow
        dec zpvari7
        bpl -
        lda #$35        ;orderlist characters are under IO area
        sta banksel
        ldx #0
        stx playadapter.seqrefr
        .if POS_DISP_MOD==2
        stx display.blirefr+1
        .fi
retorli rts

;-------------------------

        .section data
PALolypos  .byte 0*PALseqdistance*3,1*PALseqdistance*3,2*PALseqdistance*3 ;starting memory positions of orderlist rows
NTSColypos .byte 0*NTSCseqdistance*3,1*NTSCseqdistance*3,2*NTSCseqdistance*3 ;the same for NTSC
        .send data
        
dseqrow                 ;displays one sequence-row, Accu holds sprite-characterrow pixelposition
        ldx zpvari7     ;current track-sequence to display
        lda PALolypos,x ;y position adder
        ldy PALNTSC
        bne +
        lda NTSColypos,x
+       sta zpvari6
.if (SID_AMOUNT>1)
        txa
        clc
        adc trkposi
        tax
.fi
        lda playadapter.mul7chn,x ;currently displayed track (0..2 from caller)
        clc
        adc #<player.SEQPOS ;for this, SEQPOS shouldn't be on boundaries
        sta actrack+1
        lda #>ordlspr
        sta datzptr+1   ;sprite pixeldata pointer hi-byte

        lda playadapter.olstart,x
        cmp playadapter.olstart+0
        bne +
        lda #$c0        ;if the same as first track track, avoid underline
+       sta markpos+1

        ldx #0
        stx zpvari2     ;orderlist-displayposition counter 0..20
ordloop ldx zpvari2     ;counts displayed column of displayed 2 hexa digit pattern-number
        cpx #20
        beq retorli     ;check if we exceeded display area
        cpx #12
        bne +           ;if position 12, increase spritedata pointer
        inc datzptr+1
+       lda sprtable,x  ;precalc.table for sprite-addresses of corresponding orderlist x-coordinates (datatables.inc)
        clc
        adc zpvari6     ;sequence display startposition in memory 0 / 18 / 36, depending on which track/row we are displaying
        sta datzptr     ;set sprite-pixeldata lo-pointer
        txa             ;current value position in sequence (0..19)
        clc
        adc seqdpos+1   ;seq display-position
        tay

mrkequa lda #selfmod    ;only the different playpositions get displayed
        ;bne skipmrk
markpos cpy #selfmod
        bne +
        lda #%11001000  ;underline at orderlist-playmarker position
+       sty postore+1
        ldy #15
        sta (datzptr),y ;at playmark-positions
postore ldy #selfmod

skipmrk .if POS_DISP_MOD==1
        ldx #$00
        .fi
actrack cpy player.SEQPOS ;self-written operand lo-byte, sequence played position in Player-routine
        bne noplpos
        .if ( POS_DISP_MOD==1 && GHOSTPIXEL_ON && PTNBOUND==256 && maxptnlen<250 && maxsubtuneamount<18 )
        ldx #$f0        ;self-written - if sequence position is played currently, the character is inverted
        .elsif POS_DISP_MOD==1
        dex             ;$ff   ;invert currently played position
        .elsif POS_DISP_MOD==2
blinkit lda #1          ;self-written - blink or not?
        bne noplpos
        ;lda #0          ;empty value for blinking into orderlist
        ldy #12
-       sta (datzptr),y
        dey
        dey
        dey
        bpl -
        rts
        .fi
noplpos 
        .if POS_DISP_MOD==1
        stx seqposd+1
        .fi
seqaddr lda selfmodA,y  ;reads current displayed orderlist-position's value from memory
        pha             ;accu holds the current seq value here
        bne +           ;if zero, display dots - zero has special meaning now, not used as pattern
        ldy #$10        ;spritedata dot character
        bne ++
+       ldy #$11
        cmp #$f0        ;Orderlist separator NOP with index 0?
        beq +
        and #$0f        ;calculating 2nd digit
        tay
+       lda seqchtb2,y  ;alternative seq-chartable for ghostpixel method (or normal)
        sta wrsp1+1     ;right char
        pla
        #divAby16
;        bne +           ;if 1st digit is 0, a dot is written for better sight of orderlist (less crowded)
;        lda #$10        ;spritedata dot character
+       tay
        lda seqchtbb,y  ;alternative seq-chartable for ghostpixel method (or normal)
        sta wrsp2+1     ;left char

        ldy #12
        ldx #4
chrloop                 ;writing character to pixeldata in sprites
wrsp1   lda seqchr2,x   ;self-written previously
        and #$0f
        sta zpvari3
wrsp2   lda seqchr2,x   ;self-written previously
        and #$f0
        ora zpvari3     ;merge left and right character pixeldata
        .if POS_DISP_MOD==1
seqposd eor #$ff        ;self-written - if sequence position is played currently, the character is inverted
        .fi
        sta (datzptr),y ;puts the graphdata to low-border sprites
        dex
        dey
        dey
        dey
        bpl chrloop     ;end of character-pixeldata displaying loop (for 1 value)
        inc zpvari2     ;holds current hexa number display-column position (0..19)
        jmp ordloop


        .pend

seqchtbl                ;reference table (5X multiplier) for 5-pixel high chardata
        .byte <seqchar+0,<seqchar+5,<seqchar+10,<seqchar+15,<seqchar+20,<seqchar+25
        .byte <seqchar+30,<seqchar+35,<seqchar+40,<seqchar+45,<seqchar+50
        .byte <seqchar+55,<seqchar+60,<seqchar+65,<seqchar+70,<seqchar+75
        .byte <seqchar+80,<seqchar+85

        .if ( GHOSTPIXEL_ON && PTNBOUND==256 && maxptnlen<250 && maxsubtuneamount<18 )
seqchtb2                ;reference table (5X multiplier) for 5-pixel high chardata
         .byte <seqchr2+0,<seqchr2+5,<seqchr2+10,<seqchr2+15,<seqchr2+20,<seqchr2+25
         .byte <seqchr2+30,<seqchr2+35,<seqchr2+40,<seqchr2+45,<seqchr2+50
         .byte <seqchr2+55,<seqchr2+60,<seqchr2+65,<seqchr2+70,<seqchr2+75
         .byte <seqchar+80,<seqchar+85
        .else
seqchtb2 = seqchtbl
        .fi

seqchtbb .byte <seqchar+80,<seqchr2+5,<seqchr2+10,<seqchr2+15,<seqchr2+20,<seqchr2+25
         .byte <seqchr2+30,<seqchr2+35,<seqchr2+40,<seqchr2+45,<seqchr2+50
         .byte <seqchr2+55,<seqchr2+60,<seqchr2+65,<seqchr2+70,<seqchar+85
         .byte <seqchar+80


;---------------------------------------------------------------------------
        .if POS_DISP_MOD==2
blinkpos ;blink current playback-positions in sequences
;        lda blirefr+1 ;prevent waving (switching blink-state while displaying blinking outside irq)
;        bne endblnk ;(blirefr+1 is reset by displayer routines to 0)
        dec blinkcn+1
blinkcn lda #selfmod    ;blink-counter
        bpl +
        lda #PBLINKSPEED ;playback-position blink-speed
        sta blinkcn+1
        bne ++          ;jump
+       sbc #PBLINKSPEED/PBLINKRATIO
        bne endblnk
        .fi
        .if ( POS_DISP_MOD==2 && IDLEBLINK_ON==0)
        ldx playadapter.playmod
        cpx #1
        bne endblnk     ;if patternplay or stop don't blink...
        .fi
        .if POS_DISP_MOD==2
+       sta blinkit+1
        inc display.blirefr+1 ;seqrefr ;tell main loop to display orderlist
endblnk rts
        .fi

;===============================================================================

CTFMETR = upbordisp.CTFMETR;SELF-WRITTEN - A HELP FROM PLAYER, CONTAINS CUTOFF-FREQUENCY HI-BYTE
ENV3sto = upbordisp.ENV3sto;self-written ;ENV3 (envelope of 3rd channel) ;Multiplier
jamoctave = upbordisp.jamoctave ;self-written variable ;octave for jamming and note/typing

upbordisp .proc         ;display upper border data in upper-border sprites

;upper border display data source- and screenpositions
        .section data
posies .byte 0+64*2+4,      0+64*2+5
.if (SID_AMOUNT==1)
       .byte 0+47,     <updispr+64*2+45, <updispr+46
.elsif (SID_AMOUNT>1)
       .byte 64+45,     <updispr+64*2+46, <updispr+47
trkvmL .byte <updispr+46, <updispr+64*2+45, <updispr+64*3+47
trkvmH .byte >updispr+46, >updispr+64*2+45, >updispr+64*3+47
.fi
memiel .byte <(songtime+0), <(songtime+1)
       .byte <curptn3, <curptn2,         <curptn1
ctuvm1 .byte <updispr+64+46, 0+45,          0+64+47
        .send data

        ;need to switch bank with zeropage $01 := $34 to write RAM under IO area
        lda #$34
        sta banksel

.if (SID_AMOUNT>1)
dTrkNum ldx #3-1
-       lda trkvmL,x
        sta vidzptr+0
        lda trkvmH,x
        sta vidzptr+1
        txa
        sec ;+1
        adc trkposi
        jsr uplchar
        dex
        bpl -
posPtnN lda trkposi
        clc
        adc #<curptn1
        ldx #3-1
        clc
-       sta memiel+2,x
        adc #1
        dex
        bpl -
.fi

        ldx #>updispr
        stx vidzptr+1

upbord1 ldx #4          ;display the numbers in the upper border
-       lda posies,x
        sta vidzptr
        cpx #4-2        ;point, where sprite-graphic hi-pointer increases
        bne +
        inc vidzptr+1
+       lda memiel,x
        sta datzptr
        lda #>zerovar   ;memieh,x
        sta datzptr+1
        ldy #0
        lda (datzptr),y
        jsr uphexac
        dex
        bpl -

        lda #0+64*2 + 23
        sta vidzptr
        lda Config.autoadv ;Autoadvance mode sign
        jsr upchar2

        lda #0 + 64*2 + 21 ;rastertime-display (was framespeed-display before)
        sta vidzptr
        lda playadapter.maxrasti ;framesp
        jsr uphexac     ;upchar2

        lda #0 + 64*2 + 3 ;jamming octave display
        sta vidzptr
jamoctave ldy #4        ;self-written variable ;octave for jamming and note/typing
        iny
        tya
        jsr upchar1

        ldx #3-1        ;patternsize displays
-       lda ctuvm1,x
        sta vidzptr
.if (SID_AMOUNT==1)
        ldy curptn1,x
.elsif (SID_AMOUNT>1)
        txa
        clc
        adc trkposi
        tay
        lda curptn1,y
        tay
.fi
        lda ptnsize,y
        sec
        sbc #1
        pha
        jsr uplchar
        dec vidzptr
        cpx #1
        bne +
        dec vidzptr+1
        lda #<updispr+64*2+47
        sta vidzptr
+       pla
        jsr uprchar
        dex
        bpl -
;--------------
        ldx #0          ;track1
        jsr DisPWmt     ;display PW-meter for channel given in X
        ldx #1          ;track2
        jsr DisPWmt     ;display PW-meter for channel given in X
        ldx #2          ;track3
        jsr DisPWmt     ;display PW-meter for channel given in X

CTFMETR lda #selfmod    ;A HELP FROM PLAYER, CONTAINS CUTOFF-FREQUENCY HI-BYTE
        lsr             ;/2 ;0..127
        lsr             ;/4 ;0..63
        lsr             ;/8 ;0..31
        sta scinput+1   ;store 5bit (0..31) input value
        lda #<(updispr+64*5+54)
        ldy #>(updispr+64*5+54)
        jsr dispmtr
;----------------
        .if OSCILLOSCOPE==1 ;display oscilloscope
        ldy #15*3       ;refresh (clear) scope-screen
-       lda #$80        ;left edge of oscilloscope-screen
        sta updispr+64*6+6,y
        lda #5          ;right edge of oscilloscope-screen
        sta updispr+64*6+7,y
        dey
        dey
        dey
        bpl -
;display oscilloscope-pixels in order horizontally
        ldx #12-1       ;width of oscilloscope
ENV3sto lda #selfmod    ;ENV3 (envelope of 3rd channel) ;Multiplier
        beq +           ;if envelope is 0, no need to calculate multiplicaton (ensure zero)
        sta vidzptr     ;retain original base-position in exp-table caused by ENV3
        clc
        adc OSC3Buffer,x ;OSC3 (oscillator of 3rd channel) ;Multiplicand;
        tay
p_EXPT3 lda player.EXPTABH,y ;FREQHI,y ;;;
        ldy vidzptr     ;eliminate base position (shift) in exponent-table
        sec
p_EXPT4 sbc player.EXPTABH+9,y ;adding 9 centers OSC3 produced value (roundabout 0..31)
+       clc
        adc #16-1       ;this shifts the value to positive range for display
        lsr             ;eliminate one pixel difference (on 0..15 scale) of exponent-table if ENV3=0
        and #$0f        ;prevent from graphic-overwriting bugs
        jsr commonsubs.multi3b ;multiply by 3 for column-based sprite-display of new rightmost pixel
        tay
        cpx #12/2       ;treshold of 1 byte display
        bmi +           ;handle second part of the scope
        iny             ;by going to second byte in sprite-display
+       lda updispr+64*6+6,y
        ora OsciPixel,x
        sta updispr+64*6+6,y
        dex
        bpl ENV3sto
        .fi
;--------------
        inc banksel        ;$35
        rts

;------------------------------------
;PW/Cutoff meter scaler - range 0..32 to 0..24 and sort to rough/fine bitfields
        .section data
scalemask1 .byte $ff,$fe,$fc,$f8,$f8,$f0,$e0,$e0,$c0,$80,$80,$00 ;used to display PW/cutoff-meters
pwmeterpos .byte <(updispr+64*5+42),<(updispr+64*5+45),<(updispr+64*5+48)
scalecomp  .byte 11,21,32 ;range-positions for 32 to 24 pixel conversion

;scalemask2 .byte $ff,$fe,$fc,$fc,$f8,$f0,$f0,$e0,$c0,$80,$00,$00 ;used to display PW/cutoff-meters - SMOOTHER
OsciPixel .byte $20,$10,8,4,2,1,$80,$40,$20,$10,8,4;oscilloscope pixel ORs
        .send data

DisPWmt                 ;display PW-meter for channel given in X

        ldy playadapter.mul7chn,x ;transform 0..1..2 to 0..7..14
        lda player.PWLOGHO,y
        cmp #$80        ;set carry flag based on bit7 of pwlogho,x
        lda playadapter.PWHIHELP,y ;PWHIGHO,y ;4 bit (0..15) value
        and #$0f        ;range 0..15
        rol             ;with carry-flag do 5 bit (0..32) value in Accu
        sta scinput+1   ;store 5bit (0..31) input value
        lda pwmeterpos,x
        ldy #>(updispr+64*5+42)
dispmtr                 ;display PW/Cutoff meters (5bit in 'scinput' , A-Y:screen-address)
        sta vidzptr+0   ;set screendata-pointers
        sty vidzptr+1
        ldy #2
-       ldx #0          ;point to $ff if below
        lda scalecomp,y
        sec
scinput sbc #selfmod    ;input value
        bmi +
        tax
        cpx #12
        bmi +
        ldx #11         ;point to $00 if above
+       lda scalemask1,x
        ;lda scalemask2,x
        sta (vidzptr),y
        dey
        bpl -
        rts

;--------------- reserve X here in these routines! Needed for display-indexing
uphexac pha             ;0. display a complete 2 digit hexa number in upper border
        #divAby16 ;jsr commonsubs.div16
        jsr upchar1
        pla
        and #$0f
        gpl upchar2

uplchar and #$0f        ;1. left char
upchar1 tay             ;2. left char
        lda #$0f
        gne chdrcom

uprchar #divAby16 ;jsr commonsubs.div16 ;3. right char
upchar2 tay             ;4. right char
        lda #$f0
chdrcom sta zpvari4
        eor #$ff
        sta zpvari5
        lda seqchtbl,y
        sta wrsp2b+1    ;char
        txa
        pha
        ldy #12
        ldx #4
wrsp2b  lda seqchar,x   ;self-written
        eor #$ff
        ora zpvari4     ;#$f0 / #$0f
        sta zpvari6
        lda (vidzptr),y
        ora zpvari5     ;#$0f / #$f0
        and zpvari6     ;sw
        sta (vidzptr),y
        dey
        dey
        dey
        dex
        bpl wrsp2b
        pla
        tax             ;X reserved
        rts
        .pend
;====================================

mutedis .proc           ;display all mute/solo column colourings
        ldy #0
        jsr mutesolodisp ;darken muted tracks if needed
        ldy #1
        jsr mutesolodisp ;darken muted tracks if needed
        ldy #2
        .pend
mutesolodisp .proc      ;performs colouring of track at mute/solo - tracknumber in y - done outside IRQ
        lda #$ff
        sta display.msorefr+1
        ldx stplptr,y   ;colour positions taken from step-highighting column position table
        inx
        stx msoloop+1
.if (SID_AMOUNT>1)
        tya
        clc
        adc trkposi
        tay
.fi
        lda mutesolo,y  ;mute/solo status byte for track Y
        and #1
        tax
        ldy msolocol,x  ;colour index for on/off
        sty msolocw+1
        bit Config.confbits ;instrument auto-typing enabled?
        bmi +
        ldy msolocol+2,x ;instrument-colour index for on/off
+       sty msoloc2+1

        lda #<COLRPOS
        sta datzptr
        lda #>COLRPOS
        sta datzptr+1

        ldx #23         ;number of rows to change colour of
msoloop ldy #selfmod
msolocw lda #selfmod
        sta (datzptr),y
        iny
        sta (datzptr),y
msoloc2 lda #selfmod
        iny
        sta (datzptr),y
        iny
        sta (datzptr),y
        lda datzptr
        clc
        adc #40
        sta datzptr
        bcc +
        inc datzptr+1
+       dex
        bpl msoloop
        rts
        .pend

        .section data
msolocol .byte mutecol,15, mutecol,3 ;mute/solo colours for note-column and instrument-column
        .send data

;------------------------------------------------------------
selsubt = miscdis.selsubt;self-written variable ;selected subtune ;display subtune number + subtune-speeds
miscdis .proc           ;miscenalleous display - subtune data
selsubt lda #selfmod    ;selected subtune  ;display subtune number + subtune-speeds
        ora #$c0
;        clc             ;more human-centric to count from 1, not 0 (on the other hand everything else is count from 1)
;        adc #1          ;(the only problem is with subtune-jump, which is count from $80 not $81!)
        sta stplace
        lda TEMPOTBL+0  ;jsr calcsubsp1
        pha
singtmp                 ;and #$3f
        ora #$c0
        sta vidram+$3e5
        ldx #$20
        pla
        bmi funk2pl
funk2   lda TEMPOTBL+1  ;subtunes+1,y
        ;and #$3f
        ora #$c0
        tax
funk2pl stx vidram+$3e6
        rts
        .pend

wf1disp ;display 1st frame waveform (parameter $09 of instrument) - NEW FEATURE in SW1.2
        ldy #$0f        ;position of 1st frame waveform in instrument
        lda (inszptr),y
        jsr commonsubs.ihexdis
        sta display.waform1+0
        txa
        ora #$90
        sta display.waform1+1
        rts


;---------------------------------------------------------------------
iniscrn .proc           ;initialize VIC, screen colors and sprite-pointers
        ldy #0          ;copy the RAM $d800..$dc00 to COLOUR-RAM $d800..$dc00
        sty $d020
        sty $d021
        sty $d017       ;robozz at CSDB notified me it's not initialized
        sty $d01c       ;robozz at CSDB notified me it's not initialized

SetScol sty vidzptr+0   ;init pointer to $d800 colour-RAM area ;generate editor screen-colors in $d800 Colour-RAM
        lda #>(COLRPOS) ;$d8
        sta vidzptr+1
        ldx #25-1       ;X counts screen-rows
ColoRAM ldy #40-1
ColoRow lda fxcol
Cpattn  cpy #30
        bmi jWrColo     ;horizontal division - if bigger than 30, colorize instrument-panel, else colorize patterns

SiPanel lda ipacol      ;instrument-panel default colour
        cpx #25-25      ;row 25 - lower border of instrument-panel (subtune-number, tempo)
        beq jWrColo
CiTable cpx #25-6
        bne +           ;row 6 is the header for tables, emphasized colour needed
SiTbHdr lda itahdrcol   ;instrument-table header colour
        cpy #37
        bmi jWrColo
        bpl chChdSu     ;jump

+        bpl CiName     ;vertical division - if bigger than 5, colorize isntrument tables, else colorize instrument-header

Cchdtmp cpx #25-20      ;tempotable-header
        bmi CtempoT
        beq +
        bpl CchordT
+       lda itahdrcol   ;instrument-table header colour
        bne +           ;jmp
CtempoT cpy #37         ;chord/tempotable numbering column
        bmi CiTabl2
        beq +
        lda itacol      ;instrument-table colour
+       bit TEMPOPRGSUPP
FeatCol bmi +           ;test fetched feature and disable color if needed
Disable lda #DISABLECOL
+       jmp WrColor
CchordT cpy #37         ;chord/tempotable numbering column
        bmi CiTabl2
        beq chChdSu
        lda itacol      ;instrument-table colour
chChdSu bit CHORDSUPPORT
        jmp FeatCol     ;jump

CiTabl2 cpy #35         ;3rd inst.table columns
        bmi SiTable
        cpx #25-20      ;Filter-table position
        bpl +
        lda #fltktcol   ;Filter-keyboard-track colour
        bit FILTKBTRACK
        jmp FeatCol     ;jump
+       cpx #25-15      ;PW-table position
        bpl +
        lda #pwtktcol   ;PW-keyboard-track colour
        bit PWKBEYBTRACK
        jmp FeatCol     ;jump
+       lda #detutcol   ;detune-colour
        bit DETUNESUPPORT
jFeatCo jmp FeatCol     ;jump

SiTable lda itacol      ;instrument-table colour
        jmp WrColor

CiName  cpx #25-1
        bne CiHeadr
SiName  lda inacol      ;instrument-name colour
        cpy #32
        bpl WrColor
        lda instnumcol  ;instrument-number colour
jWrColo jmp WrColor

CiHeadr cpy #32         ;instrument-header edges
        bmi WrColor
        cpy #38
        bmi SiHdrTb
        cpx #25-2
        bne +
        bit FRAME1SWITCH
        bpl Disable
+       cpx #25-3
        bne +
        bit HARDRESTYPES
        bpl Disable
+       cpx #25-4
        bne +
        bit VIBRATOTYPES
        bpl Disable
+       cpx #25-5
        bne +
        bit OCTAVESHIFT
        bpl Disable
+       jmp WrColor

SiHdrTb lda itaemphcol  ;instrument-header table emphasized value colour

CiHDSR  cpx #25-3       ;HR-ADSR values
        bne CiSpChd
        bit HARDRESTYPES
        bmi +
        bpl Disabl2

CiSpChd cpx #25-5       ;arp/chordspeed & default-chord values
        bne CiValTi
+       lda itacol      ;instrument-table value colour
CiValTi cpy #36         ;value-titles' column
        bmi WrColor
Disabl2 lda #DISABLECOL

WrColor sta (vidzptr),y
        dey
        bmi +
        jmp ColoRow
+       lda vidzptr+0
        clc
        adc #40         ;next screen-row
        sta vidzptr+0
        bcc +
        inc vidzptr+1
+       dex
        bmi +
        jmp ColoRAM


+       ;sign disabled features in mid/light versions
        lda #DISABLECOL
        ldx itacol      ;pre-set hardrestart-timer colour
        bit HARDRESTYPES
        bmi +
        tax
+       stx COLRPOS+2*40+38 
        bit ARPSPEEDSUPP
        bmi +
        sta COLRPOS+4*40+32
        sta COLRPOS+4*40+33
+       bit CHORDSUPPORT
        bmi +
        sta COLRPOS+4*40+34
        sta COLRPOS+4*40+35

+       ldx #13         ;pre-set detune-table header colour
        bit DETUNESUPPORT
        bmi +
        tax             ;disable-colour
+       stx COLRPOS+5*40+35
        stx COLRPOS+5*40+36
        ldx #14         ;pre-set PW-keyboard-track table header colour
        bit PWKBEYBTRACK
        bmi +
        tax
+       stx COLRPOS+14*40+35
        stx COLRPOS+14*40+36
        ldx #8          ;pre-set Filter-keyboard-track table header colour
        bit FILTKBTRACK
        bmi +
        tax
+       stx COLRPOS+19*40+35
        stx COLRPOS+19*40+36

        ldy #6
        ldx #0
        lda #BORDERSPX  ;starting coordinate of bordersprites
-       pha             ;border-sprite x coordinates
        lda borderspcol
        sta $d028,y
        tya
        clc
        adc #<(ordlspr/$40)
        sta lowvidram+$3f9,y ;lower border graphic sprite-pointers
        pla             ;self-written, bordersprite X-coordinate
        sta $d002,x
        clc
        adc #$30        ;the width of a double-wide sprite - add to coordinate
        inx
        inx
        dey
        bpl -

;        lda #<(orlcurs/$40) ;modulo of VIC-banksize divided by spritesize ;now set in IRQ
;        sta lowvidram+$3f8

        jmp mutedis     ;display all mute/solo column colourings
        .pend

;--------------------------
coloriz .proc           ;based on Accumulator, select a colour-theme
        asl             ;A*=2
        jsr commonsubs.getsubaddr.mul8toY ;A*=8 -> Y   - in commonsubs.inc
;        dec banksel     ;$34
        ldx #0
-       lda ColorThemes,y
        sta ColorSet,x
        iny
        inx
        cpx #(ColorSetEnd-ColorSet)
        bne -
;        inc banksel     ;$35
        jsr menu.dThName ;display colour-theme name
        lda notecolucol
        sta msolocol+1
        lda instcolucol
        sta msolocol+3
        lda itahdrcol
        sta display.conboff+0
        sta display.conboff+1
        rts
        .pend

ColorSet .section data
;---editor user-modifiable colour scheme------------------------
borderspcol .byte 12    ;(default:15) border sprite-colours (orderlist and upper display)

notecolucol .byte 15    ;(default:1) colour of note-column in pattern-editor
instcolucol .byte 3     ;(default:15) colour of instrument-column in pattern-editor
fxcol       .byte 12    ;(default:12) colour of fx-columns in pattern-editor and orderlist-numbering

ptrownumcol .byte 11    ;(default:11) default colour of patternrow-numbering
downbeatcol .byte 12    ;(default:15) step-highlighting (downbeat) colour
playposicol .byte 15    ;(default:1) highlighting colour of played pattern-position 

instnumcol  .byte 13    ;(default:13) instrument-number colour
inacol      .byte 15    ;(default:1) instrument-name, and instrument-tables' headers' colour
ipacol      .byte 12    ;(default:15) instrument-panel default-colour
itahdrcol   .byte 15    ;(default:15) instrument-table header colour
itacol      .byte 3     ;(default:12) instrument-tabledata numbers' colour
itaemphcol  .byte 15    ;(default:3)  emphasized instrument-table numbers' colour
gateoffcol  .byte 11    ;(default:11) gateoff-pointers' colours in instrument-tables
;---menu user-modifiable colour scheme
menubgcol   .byte 14    ;(default:14) menu background colour
menucurcol  .byte 15    ;(default:13) menu cursor/highlight-colour
ColorSetEnd
        .send

;-------------------------predefined editor color-schemes-----------------------
ColorThemes .section data  ;order is based on 'ColorSet' above
        .byte 12,  15, 3,12,  11,12,15,  13,15,12,15, 3,15,11, 14,15
        .byte 15,   1,15,12,  11,15, 1,  13, 1,15,15,12, 3,11, 14,13
        .byte 14,   1, 3, 14, 11,12,15,  13,15,12,15,   4  ,15,11, 14,15       
        .byte  3,  15, 3, 5,  11,12,15,   3,13, 3,13,15,13,13,  3, 1
        .byte 14,  15,14, 6,   6,14,13,  14,15,14, 3,15,13,15, 14, 3
        .byte  4,  10,14, 4,   4,10, 7,  10,12, 4,15,10, 7,10,  4, 7
        .byte  8,  10, 8, 9,   9, 8, 7,   8,10, 8,10,10, 7, 7,  8, 7
        .byte  2,  15,10, 2,  11,12,15,  10,15, 2,10,15, 1,10,  2,10
        .byte 15,  15,12,15,  11,15, 1,  12,15,15,15,15,15,11, 12, 1
ColorThemesEnd
        .send data
ThemeNames .section DataUnderIO
         .enc 'screen'
          .text "DEFAULT "
          .text "BRIGHTER"
          .text "VESSEL  "
          .text "TURKISH "
          .text " BLUISH "
          .text " PURPLE "
          .text "BROWNISH"
          .text "RED&GREY"
          .text "AMISH :)"
         .enc 'none'
ThemeNamesEnd
        .send DataUnderIO

;===============================================================================
; vim: sw=4 ts=4 syntax=asm:
