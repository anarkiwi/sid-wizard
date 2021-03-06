;ident:8,24
;===============================================================================
; $Id: displayer1.inc 360 2014-02-15 14:50:28Z soci $

display .proc           ;display screendata - zeropages used: 2,3,4,5..8,9 ,$fc-$fd,$fe-$ff

syncscr lda #1          ;self-written variable (syncscr) ;VBLANK synchronization, if 1, a drawing is requested from main routine, if drawing finished, sets to 0
        beq nosyncr
        ;syncronize screen content

        lda menumode
        beq +
        jsr menu.dispmenu ;handle menu-display
        lda menu.clearmenu
        beq noptrfr     ;if menu-clear initiated, do it, restore screen colours and content behind menu
        jsr menu.dispmenu
        jsr menu.menuback ;get content under manu back, refresh pattern and seq.highlighting
        jmp noptrfr

+       lda curwind     ;actual window (0-patterneditor,1-orderlist,2-instrum,3-chord,4-tempo)
        beq +           ;if patternwindow, refresh normally (frequently)
.if (SID_AMOUNT>1)
        lda playadapter.seqrefr
        bne +
.fi
        lda playadapter.followp+1
        bne +           ;if followp+1lay, refresh everything everytime
        lda playadapter.playmod
        bne refstep     ;if not patternwindow, but playing state, refresh at least step-highlighting
hilrefr lda #1          ;self-written, checks if highlight-step was adjusted
        beq noptrfr     ;if not played and not patternwindow, and no highlighing change, no need to refresh pattern
        lda #0
        sta hilrefr+1
        beq refstep     ;jump
+       jsr ptndisp     ;display selected patterns
refstep jsr stpdisp     ;display step-highlighted bars for track 1..3

noptrfr lda playadapter.seqrefr ;signs if orderlist needs to be refreshed (change subtune / we're in orderlist)
        .if POS_DISP_MOD==2
blirefr ora #selfmod    ;if orderlist-playposition blinking requested screen-refresh
        .fi
        beq insrefr
titledi lda #selfmod    ;if title should be left in row 26
        bne +
        jsr seqdisp     ;display sequence (orderlist) numbering
+       jsr ordldis     ;display orderlist data (low border spritedata)

insrefr lda #1          ;self-written variable ;signs if instrument table needs to be refreshed (if we-re in instrument window or change instrument)
        beq +
        jsr insdisp     ;display instrument data
        jsr chtmdis

+       jsr miscdis     ;display subtune-specific data
        jsr upbordisp   ;display upper border data in upper-border sprites

msorefr ldy #selfmod    ;signs if refresh of mute/solo is needed
        bmi dfinish
        cpy #$0F        ;$0F means all tracks should be refreshed
        bne +
        jsr mutedis     ;display all mute/solo column colourings
        jmp dfinish
+       jsr mutesolodisp

dfinish lda #0          ;signs (to main loop and IRQ) that drawing has been finished, IRQ will increase this at V-Blank
        sta syncscr+1

nosyncr rts

;------------------------------------------------
        .section data
;bits of instrument controlbyte (0) - screen addresses,on/off values, display lengths
conbits .byte $80          ,$40          ,4                 ,8                 ,8
conadhi .byte >(COLRPOS+$316)  ,>(COLRPOS+$24e)  ,>(vidram+$77)  ,>(vidram+$4e)  ,>(vidram+$4f)
conadlo .byte <(COLRPOS+$316)  ,<(COLRPOS+$24e)  ,<(vidram+$77)  ,<(vidram+$4e)  ,<(vidram+$4f)
conboff .byte 15,           15,            $7F,           $69,          $69
conbons .byte 11,           11,            $79           ;....
waform1 .byte $00,          $00 ;introduced in SW 1.2 - 1st waveform can be set :)
condlen .byte 4,4,0,0,0

vibtype  .byte $6f,$70, $6e,$70, $6e,$71, $6e,$72 ;charactercodes for vibrato types 0...3
imainpos .byte $70,$72,$48,$4a,$98,$9a,$c0,$c2 ;screenpositions of main instrument-settings
        .send data

insdisp lda #0          ;display instrument-data
        sta insrefr+1   ;sign that instrument-refresh happened
selinst lda #1          ;self-written variable, number of selected instrument
        jsr commonsubs.hexdis2
        ldy commonsubs.hexchar,x
        sty vidram+31
        tax
        lda commonsubs.hexchar,x
        sta vidram+30

        jsr commonsubs.setinsptr ;load selected instrumet's basepointer to zeropage pointers
        jsr commonsubs.reftpos ;refresh table-base positions in jmtbase to add them in displayer/datawriter

        lda namemode
        bne +
        ldy #maxinstsize-instnamelength ;display instrument-name
        ldx #0
-       lda (inszptr),y
        sta vidram+$20,x
        iny
        inx
        cpx #8
        bne -
+
        jsr wf1disp     ;display 1st frame waveform (parameter $09 of instrument) - NEW FEATURE in SW1.2
        ldy #0
        lda (inszptr),y ;CONTROL (HRTYPE,RESET,VIB.TYPE)
        sta zpvari2
        ldx #4          ;display instrument-controlbyte bits
-       lda conadlo,x
        sta vidzptr
        lda conadhi,x
        sta vidzptr+1
        ldy conboff,x   ;value if bit is off
        lda zpvari2
        and conbits,x   ;mask of bit
        beq +
        ldy conbons,x   ;value if bit is on
+       tya
        ldy condlen,x
condisa sta (vidzptr),y ;self-written
        dey
        bpl condisa
        dex
        bpl -

        lda zpvari2
        and #3
        ora #$30
        sta vidram+$76  ;HR timer (0..2, 3 is not needed, 2 is enough) - BIT 1-0 (2 , 1)

        lda zpvari2     ;display vibrato-type - BIT 5-4 ($20-$10)
        lsr
        lsr
        lsr
        and #6
        tay
        lda vibtype,y
        sta vidram+$9e
        lda vibtype+1,y
        sta vidram+$9f
;--------------------------numeric main data
        ldy #1          ;display main instrument parameters
-       jsr commonsubs.ihexdis
        sta stxer+1
        txa
        ldx imainpos-2,y
        cpy #7
        bmi +           ;better lookout for distinct parameters
        ora #$10
+       sta vidram+1,x
stxer   lda #selfmod
        sta vidram,x
        cpy #9
        bne -

        ldx #"+"        ;Y is 9 here
        lda (inszptr),y ;OCTAVE
        bpl posoct
        ldx #"-"
        sec
        sbc #1
        eor #$ff
posoct  bne +
        ldx #" "        ;if zero, no +/- sign
+       stx vidram+$c6
        jsr commonsubs.div12 ;divide Accumulator by 12, result is in Y, remainder in Accumulator (commonsubs.inc)
        tya
        ora #$30
        sta vidram+$c7

;--------------------------instrument tables
        ldx #<wfplace   ;Display WF-ARP-DETUNE table
        ldy #>wfplace   ;screen base-position
        lda wfarpos     ;wfarp-table display-poisition
        jsr commonsubs.multip3 ;set table-basepointer, TAX, and #$3f, and multiply accu by 3, then clc
        adc #WFTABLEPOS ;wf-table relative-to-instrument pointer
        sta zpvari2
        ldy #$0c        ;gateoff pointer for WFtable
        lda (inszptr),y
        ldy #1
        sty actbdis+1   ;currently displayed track
        ldy #8
        jsr tablecommon

        ldx #<pwplace   ;Display PW-KT table
        ldy #>pwplace
        lda pwtbpos     ;pwtable display-position
        jsr commonsubs.multip3 ;set table-basepointer, TAX, and #$3f, and multiply accu by 3, then clc
        ldy #$0a
        adc (inszptr),y ;PWtable relative-to-instr. pointer
        sta zpvari2
        ldy #$0d
        lda (inszptr),y
        ldy #2
        sty actbdis+1   ;currently displayed track
        ldy #4
        jsr tablecommon

        ldx #<ctplace   ;display FILTER-KT table
        ldy #>ctplace
        lda ctftpos     ;ctftable display-position
        jsr commonsubs.multip3 ;set table-basepointer, TAX, and #$3f, and multiply accu by 3, then clc
        ldy #$0b
        adc (inszptr),y ;FILTERtable relative-to-instr pointer
        sta zpvari2
        ldy #$0e
        lda (inszptr),y
        ldy #3
        sty actbdis+1   ;currently displayed track
        ldy #4
;----------------------------------------------------
tablecommon             ;common part for wf & pw & ctf table-displays
        sty zpvari3
        ldy actbdis+1   ;actually displayed instrument subwindow
        sec
        sbc commonsubs.jmtbase,y
        jsr commonsubs.div3acu ;divide gateoff-address display by 3
        sta zpvari4
        lda #0          ;zeropage byte 3 contains the height of display
        sta zpvari5     ;5 switches on after reached #$ff endsignal
        sta zpvari6     ;counts rows to display, maximum zpvari3 rows

tablp1  ldy #0          ;display table numbering
        lda vidzptr+1
        sec             ;$fc-$fd contains table-videoram-pointer, $f5-$f6 is instrument-pointer
        sbc #>vidram-COLRPOS
        sta vidzptr+1
        lda ipacol      ;default colour for position numbers
        cpx zpvari4
        bne nogoff1
        lda gateoffcol  ;modified colour for gateoff positions
nogoff1 sta (vidzptr),y
        lda vidzptr+1
        clc
        adc #>vidram-COLRPOS
        sta vidzptr+1
        txa
        ora #$c0
        sta (vidzptr),y ;display tableposition-number
        inx

        ldy zpvari2     ;display column1
        inc zpvari2     ;zeropage byte 2 holds the pointer of actual data to display
        lda zpvari5
        bne +           ;if $ff was reached already, draw dots afterwards
        lda (inszptr),y ;fetching actual data from table
        sta no00c2+1    ;store 1st column data for later decisions on next columns' displays
        bne no00c1      ;if 00 value, draw dots
+       ldy #1
        lda #$61        ;dot character 1
        sta (vidzptr),y
        lda #$62        ;dot character 2
        gne tbc1w

no00c1  cmp #$ff        ;check if the table-data has reached its end, and display dots afterwards
        bne +
        ldy #1
        inc zpvari5     ;signs to draw dots afterwards in the table
        lda #$bb
        sta (vidzptr),y
        lda #$bc
        gne tbc1w

+       pha             ;instant hexa display, no time to call hexdisp subroutine
        #divAby16 ;jsr commonsubs.div16
        ldy #1
        sta (vidzptr),y ;display column1 (hi nybble)
        pla
        and #$0f
        ora #$10
tbc1w   iny
        sta (vidzptr),y ;display column1 (lo nybble)
        ldy zpvari2
        inc zpvari2
        lda zpvari5
        bne +           ;if $ff was reached already, draw dots afterwards
        lda (inszptr),y
        bne no00c2
+       ldy #3
        lda #$61
        sta (vidzptr),y
        lda #$63
        gne tbc2w

no00c2  ldy #selfmod    ;to check column 1
        cpy #$fe
        bne +
actbdis ldy #selfmod    ;currently displayed instrument subwindow
        sec
        sbc commonsubs.jmtbase,y
        jsr commonsubs.div3acu ;divide jump-address display by 3
+       pha
        #divAby16 ;jsr commonsubs.div16
        ldy #3          ;display column2 (hi nybble)
        sta (vidzptr),y
        pla
        and #$0f
        ora #$90
tbc2w   iny
        sta (vidzptr),y ;display column2 (lo nybble)
        ldy zpvari2
        inc zpvari2
        lda zpvari5
        bne last0di     ;if $ff was reached already, draw dots afterwards
        lda (inszptr),y
        bne no00c3
last0di ldy #5
        lda #$61
        sta (vidzptr),y
        lda #$62
        gne tbc3w

no00c3  ldy no00c2+1
        cpy #$fe        ;check, if jump-command in column 1
        beq last0di
        pha
        #divAby16 ;jsr commonsubs.div16
        ldy #5
        sta (vidzptr),y ;display column3 (hi nybble)
        pla
        and #$0f
        ora #$10
tbc3w   iny
        sta (vidzptr),y ;display column3 (lo nybble)
        lda vidzptr
        clc
        adc #$28
        sta vidzptr     ;go to next display-row
        bcc nohinc1
        inc vidzptr+1
nohinc1 inc zpvari6
        lda zpvari6
        cmp zpvari3
        beq endtbld
        jmp tablp1
endtbld rts

;------------------------------------------------------------------------------
chtmdis                 ;display chordnumber + chordtable
        lda #<chplace
        sta vidzptr
        lda #>chplace
        sta vidzptr+1
        lda #<CHORDS
        sta datzptr
        lda #>CHORDS
        sta datzptr+1
        lda #$30        ;BMI
        sta decidtb
selchor lda #1          ;self-written variable ;selected chord
        tay
        ora #$c0
        sta vidram+$ef
        lda chorpos     ;chordtable display-position
        ora #$c0
        tax             ;X=table's display-position
        and #$3f
        clc
        adc CHDPTRLO,y  ;A=chord's startposition
        bit Config.confbits
        bvc + ;note-mode of chord-table?
        jsr commonsubs.NoteArp    
        jmp tmpdisp
+       ldy #$0d        ;columnlength for chordtable
        jsr incptr2

tmpdisp lda #<TMPLACE   ;display tempo-program table
        sta vidzptr
        lda #>TMPLACE
        sta vidzptr+1
        lda #<TEMPOTBL
        sta datzptr
        lda #>TEMPOTBL
        sta datzptr+1
        lda #$10        ;BPL
        sta decidtb
seltemp lda #1          ;self-written variable ;selected tempo-program
        tay
        ora #$c0
        sta vidram+$31f
        lda temppos     ;tempotable display-position
        ora #$c0
        tax
        and #$3f
        clc
        adc TEMPTRLO,y
        ldy #4          ;column length for tempotable
;----------------------------------------------------
incptr2                 ;common point for chord- & tempotable-displays
        sta zpvari2
        sty zpvari3
        lda #0
        sta zpvari4
tablp2  ldy #0          ;display chordtable/tempotable
        txa
        inx
        sta (vidzptr),y
        bit zpvari4
        bmi aftertb
        ldy zpvari2
        inc zpvari2
        lda (datzptr),y
decidtb bmi chNoteM     ;self-written command (bmi/bpl by previous steps)
        cmp #$7e        ;check chordreturn/chordloop signal
        bcc chNoteM
        and #$7f
        dec zpvari4
        jmp dispnum
aftertb lda #$61
        ldy #1
        sta (vidzptr),y
        lda #$63
        jmp dispch2
chNoteM clc            ;self-written command (clc/sec)
        bcc dispnum
disnote clc
BasNote adc #12*4+1    ;self-written operand - the base-note for the chord
        cmp #SWM_NOTE_MAX+1
        bcs aftertb
        tay
        dec banksel ;IO OFF
        lda octachar,y
        pha
        lda notechar,y
        inc banksel ;IO back ON
        ldy #1
        sta (vidzptr),y
        pla
        jmp dispch2
dispnum pha
        #divAby16 ;jsr commonsubs.div16
        ldy #1
        sta (vidzptr),y
        pla
        and #$0f
        ora #$90
dispch2 iny
        sta (vidzptr),y
incvizp lda vidzptr
        clc
        adc #$28
        sta vidzptr
        bcc +
        inc vidzptr+1
+       dec zpvari3
        bne tablp2
        rts
        .pend

;---------------------------------------------------------------------
ptndisp .proc           ;pattern displayer
        .section data
trdposl .byte <trk1plc,<trk2plc,<trk3plc;screen position (lo) to draw patterns
trdposh .byte >trk1plc,>trk2plc,>trk3plc;screen position (hi) to draw patterns
        .send data

        lda ptrdysi
        beq +
        dec ptrdysi     ;pattern-display ready, sign it to key-repeater
+       lda #$34
        sta banksel     ;note character data is in RAM under IO area

        jsr setrpos     ;(in displayer2.inc) set real patternpositions to display

        ldx #3-1
        stx zpvari2     ;counts the three displayed tracks

ptdloop
.if (SID_AMOUNT==1)
        ldx zpvari2
.elsif (SID_AMOUNT>1)
        lda zpvari2
        clc
        adc trkposi
        tax
.fi
        ldy curptn1,x
        lda ptnMpos,x   ;set selected pattern's lo-pointer
        clc
        adc PPTRLO,y
        sta datzptr
        lda PPTRHI,y
        adc #0          ;add Carry-flag as well
        sta datzptr+1   ;calculate selected pattern's hi-pointer
.if (SID_AMOUNT>1)
        ldx zpvari2
.fi
        lda trdposl,x
        sta vidzptr
        lda trdposh,x
        sta vidzptr+1
        ldy #0
        sty zpvari3
        lda #24         ;row counter (backwards to 0)
        sta zpvari4

rowdlop                 ;display note (+fx) column
.if (SID_AMOUNT>1)
        lda zpvari2
        clc
        adc trkposi
        tax
.fi
        ldy zpvari3
        lda #24
        sec
        sbc zpvari4
        cmp subwpos3+0
        bne +
        tya
.if (SID_AMOUNT==1)
        ldx zpvari2
.fi
        sta ptncurs,x   ;memory index under cursor row
+       inc zpvari3
        lda (datzptr),y
        ldy #0
        pha
        and #$7f
        cmp #$7f
        bne noptend
        pla
        gcs ptnend

noptend tax
        lda notechar,x
        sta (vidzptr),y
        lda octachar,x
        ldy #1
        sta (vidzptr),y
        pla
        bpl filrlop     ;only note, go to emptyfill (y=1)
        ldy zpvari3     ;display instrument (+fx) column
        inc zpvari3
        lda (datzptr),y
        tax
        and #$7f
        bne no00ins
        ldy #2
        lda #$bd        ;00 instrument is displayed as space
        sta (vidzptr),y
        gne insdsec

no00ins lsr
        lsr
        lsr
        lsr
        ldy #2
        sta (vidzptr),y
        txa
        and #$0f
        ora #$10
insdsec iny
        sta (vidzptr),y
        txa
        bpl filrlop     ;note+inst. go to emptyfill (y=3)
        ldy zpvari3     ;display fx column
        inc zpvari3
        lda (datzptr),y
        tax
        lsr
        lsr
        lsr
        lsr
        ldy #4
        sta (vidzptr),y
        txa
        and #$0f
        ora #$10
        iny
        sta (vidzptr),y
        txa
        and #$e0
        bne filrlop     ;note+inst+smallfx, goto emptyfill (y=5)
        ldy zpvari3     ;display fx-value column
        inc zpvari3
        lda (datzptr),y
        tax
        lsr
        lsr
        lsr
        lsr
        ldy #6
        sta (vidzptr),y
        txa
        and #$0f
        ora #$10
        iny
        sta (vidzptr),y

nxtrowd lda vidzptr     ;go to next row (in video ram)
        clc
        adc #$28
        sta vidzptr
        bcc nohinc5
        inc vidzptr+1
nohinc5 dec zpvari4
        bmi nextptd
        gpl rowdlop

filrlop iny             ;fill row with empties
        lda #$bd        ;bf ;5d   ( ;first emty-sign (should be adjustable by user))
        cpy #2
        beq nolastc
        lda #$5d        ;00    ( ;first emty-sign (should be adjustable by user))
nolastc sta (vidzptr),y
        iny
        lda #$bd        ;b9 ;5c   ( ;second empty-sign (should be adjustable by user))
        cpy #3
        beq nolstc2
        lda #$5e        ;00 ( ;second empty-sign (should be adjustable by user))
nolstc2 sta (vidzptr),y
        cpy #7
        bne filrlop
        geq nxtrowd

ptnend  ldx #0          ;endmark for pattern
pendlp  lda ptendch,x
        sta (vidzptr),y
        inx
        iny
        cpy #8
        bne pendlp
        jsr commonsubs.afterptn

nextptd dec zpvari2     ;counts tracks 2..0
        bmi pdready
        jmp ptdloop

pdready lda ptrdysi
        beq +
        dec ptrdysi     ;pattern-display ready, sign it to key-repeater
+       lda #$35
        sta banksel

        rts

ptendch .section data
        .byte $be,$be,$be,$bb,$bc,$be,$be,$be;pattern-end string
        .send data

        .pend

;----------------------------------------------------------------------------
stpdisp .proc           ;display step-highlighting
        lda #<stp1plc
        sta vidzptr
        sta datzptr
        lda #>stp1plc
        sta vidzptr+1
        sec
        sbc #>vidram-COLRPOS
        sta datzptr+1

        ldx #3-1        ;X counts number of the currently displayed track, counts down from 2 to 0
stpstlp 
.if (SID_AMOUNT==1)
        lda playadapter.ptrowpos,x ;a help from player-adapter
        sec
        sbc prowpos,x
        sta zparray1,x  ;for later use to display playposition in track
        ldy playadapter.mul7chn,x
        lda player.CURPTN,y ;taken from player
        cmp curptn1,x
        beq +           ;if played pattern is the same as the displayed one
        lda #$ff        ;if not, doesn't show playmarker
        sta zparray1,x
+       lda prowpos,x   ;displayposition of tracks
.elsif (SID_AMOUNT>1)
        stx xstore1+1
        txa
        clc
        adc trkposi
        tay
        lda playadapter.ptrowpos,y ;a help from player-adapter
        sec
        sbc prowpos,y
        sta zparray1,x  ;for later use to display playposition in track
        ldx playadapter.mul7chn,y
        lda player.CURPTN,x ;taken from player
xstore1 ldx #selfmod
        cmp curptn1,y
        beq +           ;if played pattern is the same as the displayed one
        lda #$ff        ;if not, doesn't show playmarker
        sta zparray1,x
+       lda prowpos,y   ;displayposition of tracks
.fi
        sta zpvari2,x   ;set patternpos-counters
chkfwd  cmp stptick     ;get modulo of (position/step-highlight amount)
        bcc modulow
        sec
        sbc stptick
        bcs chkfwd      ;loop to find the modulo
modulow sta zpvari5,x   ;the modulo is stored for tracks 0..2
        dex
        bpl stpstlp

        lda #0          ;24 ;number of rows to display
        sta zpvari8     ;counts actually displayed row

stplop2 ldx #2          ;counts from 2..0, displays steps/rownumbers of the tracks
stplop  ldy stplptr,x
        sty zpvari9     ;store vram/cram index/pointer for later use
        lda zpvari2,x   ;calculated before - starting displaypositions of tracks
        ora #$c0        ;from $c0 the hires numbers are in our charset
        sta (vidzptr),y ;draw the numbering characters
        ldy ptrownumcol ;step-highlight default colour
        lda zpvari5,x   ;counts back always to selected tick-amount
        bne nothick
        ldy downbeatcol ;step-highlight tick/downbeat-colour
nothick lda zparray1,x  ;check playposition
        cmp zpvari8     ;current row
        bne +
pposcol ldy playposicol ;colour to sign playposition in patterns
+       tya
        ldy zpvari9     ;videoram/colourram-index stored before
        sta (datzptr),y ;paste the colour to Colour-RAM
        inc zpvari2,x
        inc zpvari5,x
        lda zpvari5,x
        cmp stptick
        bne nothckr
        lda #0
        sta zpvari5,x
nothckr
        dex
        bpl stplop      ;loops back - counts track 2..0
        lda vidzptr
        clc
        adc #$28
        sta vidzptr
        sta datzptr
        bcc nohinc7
        inc vidzptr+1
        inc datzptr+1
nohinc7
        inc zpvari8
        lda zpvari8
        cmp #25
        bne stplop2
        rts
        .pend

;====================================================================================
; vim: sw=4 ts=4 syntax=asm:
