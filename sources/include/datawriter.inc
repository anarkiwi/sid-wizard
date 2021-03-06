;ident:8,24
; $Id: datawriter.inc 380 2014-04-30 11:36:06Z hermitsoft $

datawriter .block

.if (SID_AMOUNT<3)
JankoNotes ;Janko-keyboard layout equivalents of 2-row SID-Wizard notes 
        .byte 0
        .byte  1, 2, 3, 4, 5,    7, 8, 9,10,11,12,13         ;SW:  1..5 ,  6..12
        .byte 13,14,15,16,17,   19,20,21,22,23,24,25         ;SW: 13..17, 18..24
        .byte 27,28,29,30,31,   33,34,35                     ;SW:25..29,30..32
.else ;to free up some upper code-memory in 3SID version
JankoNotes .section data ;Janko-keyboard layout equivalents of 2-row SID-Wizard notes 
        .byte 0
        .byte  1, 2, 3, 4, 5,    7, 8, 9,10,11,12,13         ;SW:  1..5 ,  6..12
        .byte 13,14,15,16,17,   19,20,21,22,23,24,25         ;SW: 13..17, 18..24
        .byte 27,28,29,30,31,   33,34,35                     ;SW:25..29,30..32
        .send data
.fi

notecol .proc           ;routine for note-keys which are not hexa characters
        lda Config.KeybTyp
        cmp #$2C        ;Janko-keyboard?
        bne noteco2
        lda JankoNotes,x ;Convert 2row SID-Wizard diatonic keyboard-layout into Janko-keyboard piano-key layout
        tax
noteco2 lda curwind
        bne chkchdn     ;check if we're in pattern editor. if not, these keys can still be used for jamming
        lda subwpos2+0
        bne jamming     ;check if we're in note column. if not, these keys can still be used for jamming
        lda jammode+1
        bne jamming     ;if in note-columnm, these keys will be used for jamming
        jsr keyhandler.repeatex ;otherwise they're typed into the pattern
        .cerror noterep & 0
        .pend

noterep .proc
        stx notstor+1   ;X holds the musical keyboardjam note relative to given octave
        stx jammin0+1
        jsr jamming
chNoteW jsr commonsubs.setptnptr ;set pattern pointers in advance based upon current track/pattern position
        jsr commonsubs.compcur ;don't write value if end of pattern, overwrites Y!
        beq nonotew
        jmp notewri
nonotew rts
        .pend

jammin0 .proc
        ldx #selfmod
        .cerror jamming & 0
        .pend

jamming .proc           ;jamming will be in cooperation with Player
        cpx #SWM_NOTE_MAX+1
        bcs +           ;no pitch-change if x is not in note-range
        stx playadapter.jamnote+1
+       rts
        .pend

chkchdn cmp #3 ;chord-window?
        bne jamming
        bit Config.confbits
        bvc jamming
        lda playadapter.followp+1 ;if follow-playing, don't allow typing notes into pattern
        bne noterep.nonotew
        stx chwrchw.notsto2+1
        jsr keyhandler.repeatex
        jsr commonsubs.getctpos ;get chordtable-position under cursor (position in X, value in A)
        jmp chwrchw.ChDigi1 ;notemod


;==========================================================================
writevalue .proc        ;common point to write value in Y to cursor-position, and note in X to cursor position
        lda shiftsi     ;don't allow accidental notes/values if user doesn't know about a combination if Shift or CBM key used
        ora cbmsig
        beq +
        rts
+       ldx NoteOfY,y   ;get corresponding note-value for values 0..F
        lda Config.KeybTyp
        cmp #$2C        ;Janko-keyboard?
        bne +
        cpx #$80
        bcs chkptnw
        lda JankoNotes,x ;convert normal 2-row SID-Wizard layout to Janko chromatic-keyboard layout 
        tax
        jmp chkptnw
+       and #$40 ;check bit6, if FC/GMC layout (with only 1 piano-key octave in the middle)
        beq chkptnw
        cpy #0          ;key 0?
        beq noDMCnt     ;disable note for key '0' in DMC mode 
        cpy #9          ;key 9?
        bne +           ;disable note for key '9' and key '0' in DMC mode 
noDMCnt ldx #$ff        ;disable note for key '9' and key '0' in DMC mode 
+       cpy #$A         ;A key?
        bne +
        inx ;ldx #0+1   ;'C' note in lower octave
+       cpy #$D         ;D key?
        bne +
        inx ;ldx #0+5   ;'E' note in the lower keyboard octave
+       cpy #$E
        bne +
        ldx #0+4        ;'D#' note in the lower keyboard octave
+       cpy #$F         ;F key?
        bne +
        ldx #0+6        ;'F' note in lower octave
+       ;here to solve constant jam-key handling, before repeater starts working


;=========================== PATTERN EDITOR ===================================
chkptnw stx chwrchw.notsto2+1
        lda curwind
        beq ptnwrit     ;patternwindow?
        jmp wrordli     ;if not, go to test orderlist

NoteOfY .byte 12+12+4   ;Y=0 D# in the second higher keyboard octave
        .byte 0         ;Y=1 no note to play, using for NOP placement
        .byte 12+2      ;Y=2 C# in the higher keyboard octave
        .byte 12+4      ;Y=3 D# in the higher keyboard octave
        .byte $80+12+6  ;Y=4 no note for key 4 , except on Janko-keyboard
        .byte 12+7      ;Y=5 F# in the higher keyboard octave
        .byte 12+9      ;Y=6 G# in the higher keyboard octave
        .byte 12+11     ;Y=7 A# in the higher keyboard octave
        .byte $80+26    ;Y=8 no note for key 8 , except on Janko-keyboard
        .byte 12+12+2   ;Y=9 C# in the second higher keyboard octave
        .byte 0         ;Y=A no note assigned for key A, using it for NOP
        .byte 0+8       ;Y=B G in the lower keyboard octave
        .byte 0+5       ;Y=C E in the lower keyboard octave
        .byte 0+4       ;Y=D D# in the higher keyboard octave
        .byte 12+5      ;Y=E E in the higher keyboard octave
        .byte $80+6     ;Y=F no sound produced on F key for jamming, except on DMC/Janko-keyboard

ptnwrit sty commonsubs.wrhxad1+1
        sty commonsubs.wrhxad3+1
        lda Config.KeybTyp
        cmp #$2C ;Janko-layout?
        bne +
        txa
        and #$7F ;get numbers of notes that only exist in Janko-layout
        tax
+       stx notstor+1
        stx jammin0+1
        jsr commonsubs.setptnptr ;set pattern pointers in advance based upon current track/pattern position
        jsr commonsubs.compcur   ;overwrites Y, it holds current pattern number
        bne +
        jmp jammin0     ;nonotew ;don't write value if cursor at the endsignal of pattern
+       lda subwpos2+0  ;column number inside track
        beq +           ;note column?
        jmp isincol
        +
;-----------------------------------Note column
        lda jammode+1
        beq +
        jmp jammin0     ;jamming ;will be used for jamming
+       jsr keyhandler.repeatex ;repeater for value typing/writing
        .cerror notewri & 0 ;dependency hack
        .pend

notstor = notewri.notstor

notewri .proc
        lda playadapter.followp+1 ;if follow-playing, don't allow typing notes into pattern
        bne endnotw
        ;note storing parts, no insertion because this column always exists
        jsr keyhandler.SaveUndo
        jsr commonsubs.getcurnote ;detect vibrato amplitude typing
        and #$70
        cmp #VIBRATOFX ;check, if vibrato existing
        bne notstor
        jsr commonsubs.getcurnote
        ldy commonsubs.wrhxad3+1 ;if value 0, delete vibrato
        bne nodelvi
        and #$80
nodelvi jsr commonsubs.writehex2
        jmp commonsubs.instpt2
notstor lda #selfmod    ;self-written previously - prepare note value in A (from X)
        beq justwrn     ;if note is 0 (key 1 or A), just write NOP     
        bmi endnotw     ;noknote ;>$80 signs if no note for key
        ;and #$7f        ;trust ourselves, we didn't send note (X-reg.) more than $60 from key-subroutines
        cmp #SWM_NOTE_MAX+1 ;special char above notes, no need to add octave info
        bpl justwrn

        bit Config.KeybTyp ;SDI-layout?
        bpl +
        sec
        sbc #12 ;Q..P piano keys are the lower octave in SDI layout
+       jsr commonsubs.calcoct     ;add octave multiplied by 12 to Accu, compare to $5F
        bpl endnotw

        jsr jammin0

justwrn sta astorp1+1
        jsr commonsubs.getcurnote ;notevalue in A, positon in X (from pattern displayposition)
        and #$80        ;delete previous character data (vibrato will be handled differently)
astorp1 ora #selfmod    ;self-written previously from notenumber in X and octave
        jsr commonsubs.instpt2

autoins and #$7F
        beq noteadv
        cmp #SWM_NOTE_MAX+1
        bpl noteadv
        bit Config.confbits ;instrument auto-typing enabled?
        bpl noteadv
        jsr commonsubs.getcurnote
        bmi +           ;if instrument-column already exists
        jsr commonsubs.notebit7 ;set Accu bit7 to 1 and insert place for instrument-selection, Y=FF if pattern-overload
        cpy #$ff        ;if pattern is full
        beq noteadv     ;then no action is taken
        jsr commonsubs.instpt2
+       inx
        jsr commonsubs.instpt1
        and #$80        ;preserve bit7
        ora display.selinst+1 ;current instrument
        jsr commonsubs.instpt2

noteadv lda Config.autoadv ;autoadvance in pattern by pre-adjusted amount
        sta keyhandler.pgdncnt+1
        beq endnotw
advplop jsr keyhandler.godownp ;go down in pattern
        dec keyhandler.pgdncnt+1
        bne advplop
endnotw rts
        .pend


;--------------------------------Instrument column
isincol .proc
        lda playadapter.followp+1 ;if follow-playing, don't allow typing values into pattern
        bne noptnwr
        jsr keyhandler.repeatex ;repeater for value typing/writing
        jsr keyhandler.SaveUndo
        lda subwpos2+0  ;X coordinate in pattern
        cmp #1
        bne noinsle     ;left digit of intrument column?
insapp1 ldy commonsubs.wrhxad1+1 ;restore Y which holds number to type
        cpy #8
        bpl noptnwr
        jsr commonsubs.getcurnote
        bmi +           ;if instrument column exists already
        ldy commonsubs.wrhxad1+1 ;restore Y which holds number to type , instrument column does not exist, but not expanding 0, as that info doesn't need memory
        beq justadv     ;just advance in case of 0
        ora #$80        ;no second column yet, sign it in note column
        pha
        ldy #1
        lda #0
        jsr insertpt    ;insert data, move rest, check pattern overload
        pla
        cpy #$ff        ;if pattern is full
        beq noptnwr     ;then no action is taken
        jsr commonsubs.instpt2 ;sign in note-column
+       inx
        jsr commonsubs.instpt1 ;read instrument-column
        bpl +           ;don't modify bit 7
        jsr commonsubs.writehex1 ;branch 1 - instrument column has bit7 ON
        ora #$80
        jmp aad
+       jsr commonsubs.writehex1 ;branch 2 - instrument column has bit 7 OFF
        bne aad         ;if instrument was zeroed out and no further columns, delete instrument column, move the rest back
        jsr delinst     ;if typed value is 00, delete instrument
aad     jsr commonsubs.instpt2
justadv jmp keyhandler.ptncuri ;jsr ptncuri ;autoadvance to digit 2
noptnwr rts

noinsle cmp #2
        bne fxtypwr     ;right (2nd) digit of instrument column?
insapp2 jsr commonsubs.getcurnote
        bmi +           ;if there is instrument column already, simply overwrite
        ldy commonsubs.wrhxad1+1 ;restore Y which holds number to type  ;not expanding 0, as that info doesn't need memory
        beq noptnwr     ;in case of 0, don't write anything
        jsr commonsubs.notebit7
        cpy #$ff        ;if pattern is full
        beq noptnwr     ;then no action is taken
        jsr commonsubs.instpt2
+       inx
        jsr commonsubs.instpt1
        jsr commonsubs.writehex2
        bne +           ;if 00 typed, delete unneeded instrument data
delinst ldy #1          ;delete instrument column, if $00 typed there and no further columns
        jsr delbackpt   ;backspace pattern from X position by Y bytes
        dex
        jsr commonsubs.instpt1 ;switch off bit7 of note column if instrument column is deleted on typing $00 (zeroing out)
        and #$7f
+       jmp commonsubs.instpt2

;------------------------------FX type column
fxtypwr cmp #3
        bne fxtywri     ;fxtype column 1st (left) digit
        jsr checkinsert ;check if needs to be new column, insert needed extra bytes if not (Y reg)
        cpy #$ff        ;$ff signs that wanted to write new $00 fx
        beq justadv     ;don't write anything just autoadvance if tried to type new zero value
        cpy #0          ;if Y=0, column existed, no need for new bytes by default
        bne chknewf     ;now check if existing value changed to bigfx, and insert or delete value

        inx             ;branch where fx column existed already
        jsr commonsubs.instpt1 ;enough to test here FX column data for big/smallFX at 1st digit typing
        and #$e0
        bne smallfx     ;which type of FX was it before?

        ldy #2          ;2 bytes of bigFX need to be deleted later, if $00 is typed
        lda commonsubs.wrhxad1+1 ;check existing bigfx if it changes to smallfx
        cmp #2
        bmi +           ;no fxtype change, therefore no reduction in pattern
        inx             ;change bigfx to smallFX, remove bigfxvalue from pattern
        ldy #1
        jsr delbackpt   ;backspace pattern from X position by Y bytes
        dex
        jmp decide2

smallfx ldy #1          ;1 byte of smallFX needs to be deleted later, if $00 is typed
        lda commonsubs.wrhxad1+1 ;smallfx, check , if need to change to bigfx
        cmp #2
        bpl +           ;insert value column?
        jsr commonsubs.instpt1
        jsr commonsubs.writehex1 ;check if zeroing out happened
        beq +
        ldy #1          ;yes, changing smallfx to bigfx
        lda #0
        jsr insertpt
        cpy #$ff        ;if pattern is full
        beq noptwr      ;then no action is taken
+       dex
        jmp decided

chknewf lda #$00        ;Y was not 0, checking completely new FX if bigFX value insertion needed
        jsr commonsubs.writehex1
        jsr decideval   ;if bigFX, inserts 1 more byte (sets A=1 before insertptn)
        cpy #$ff        ;if pattern is full
        beq noptwr      ;then no action is taken
        ;.... zeroing out will not be TRUE because this case has already been tested for zero occurence
decided inx
decide2 jsr commonsubs.instpt1
        jsr commonsubs.writehex1 ;X holds position of FX-type column here
        bne +           ;check if the typed value is 0, and if it was previously existing nonzero, delete unneeded space

freemem dex             ;here the fx-type has been zeroed out
        jsr commonsubs.instpt1 ;sign in instrument column that FX column data has been deleted, if instrument is $80, delete it too (iny)
        and #$7f
        beq inszero
        jsr commonsubs.instpt2 ;instrument column was other then $80, simply switch off bit 7
        jmp noizero
inszero dex             ;instrument was $80, so delete it and switch off bit 7 of note column
        jsr commonsubs.instpt1 ;switch off bit 7 of note column, as instrument column was $80 (zero)
        and #$7f
        jsr commonsubs.instpt2
        iny             ;and delete one more byte ($80 instrument column)
noizero inx
        ;if zeroed out, delete the unneeded FX type and value columns, free up unneeded memory space
        jmp delbackpt   ;jsr delbackpt ;delete unneeded outzeroed FX, Y holds the amount of deletion depending on big/smallFX

+       jsr commonsubs.instpt2
        jmp keyhandler.ptncuri ;jsr ptncuri ;autoadvance to digit 2
noptwr rts

fxtywri cmp #4          ;fxtype 2nd (right) digit
        beq +
        jmp fxvawri     ;big FX value typing (if exists)
+       jsr checkinsert
        cpy #$ff        ;$ff signs that user wanted to write new $00 fx to empty place, as no point doing so
        bne +
        jmp justadv     ;don't write anything just autoadvance if tried to type new zero value
+       cpy #0          ;Y=0 if fxtype column existed already
        beq +           ;fxtype existed already?
        lda #$00        ;branch - Y was bigger than 0, was empty, making new fxtype byte, zeroing was checked already
        jsr commonsubs.writehex2
        jsr decideval
        cpy #$ff        ;if pattern is full
        beq noptwr      ;then no action is taken
+       inx             ;Y is not 0 if fxtype existed already, just checking if zeroing out happens
        jsr commonsubs.instpt1
        jsr commonsubs.writehex2
        bne +           ;check if $00 was typed, delete unneeded bytes if it was $00 (1 if smallFX, 2 if bigFX)
        ldy #1          ;by default (smallfx) delete only 1 byte
        jsr commonsubs.instpt1
        and #$e0        ;check if small or big FX
        bne condelp     ;which type of FX was it before?, set Y accordingly
        ldy #2          ;here it was bigFX, delete 2 bytes
condelp jmp freemem
+       jsr commonsubs.instpt2
        and #$e0
        bne noptwr      ;+ ;if smallFX, don't autoadvance to value column
        jmp keyhandler.ptncuri ;jsr ptncuri ;autoadvance to FXvalue - if we have bigFX
;+      rts             ;1 byte optimized

decideval               ;decide if bigfx value is needed to be inserted upon new insert, and insert it
        and #$e0        ;small or big FX?
        beq bigfx
        lda #0          ;should be 1 if bigFX is inserted
        jmp insss
bigfx   lda #1
insss   jmp insertpt    ;jsr insertpt ;insert data, move rest, check pattern overload, amount in Y

checkinsert             ;check and do data in fxtype column, Y holds amount of needed insertion on return
        jsr commonsubs.getcurnote ;get note column data
        inx
        cmp #0
        bmi +           ;test note column data
        jsr commonsubs.chptful ;pre-check if pattern is full, a and x stays preserved
        bcs ptnful3
        ldy commonsubs.wrhxad1+1 ;restore Y which holds number to type  ;not expanding 0, as that info (NOP) doesn't need memory
        bne nozerop     ;just advance in case of 0, don't write anything
        ldy #$ff        ;if tried to write new zero value
        rts
nozerop dex             ;set notecolumn bit 7
        ora #$80
        jsr commonsubs.instpt2 ;bit 7 of note is on
        inx
        ldy #2          ;amount of shifting
        rts
        ;note column was above $80, instrument column exists, no need for inst.insertion
+       jsr commonsubs.instpt1 ;get instrument data
        bmi adjustit ;test instrument column data
        jsr commonsubs.chptful ;pre-check if pattern is full, a and x stays preserved
        bcs ptnful3
        ldy commonsubs.wrhxad1+1 ;restore Y which holds number to type  ;not expanding 0, as that info (NOP) doesn't need memory
        bne inserp1     ;just advance in case of 0, don't write anything
ptnful3 ldy #$ff
        rts
inserp1 ora #$80        ;no 3rd column yet, sign it in instrument column
        jsr commonsubs.instpt2 ;sign in instrument-column
        ldy #1
        rts
adjustit ldy #0         ;both note and instrument column bit7 is ON, FX type column existed already
retpntw rts

;---------------------------------------------FX value column
fxvawri                 ;bigFX value writer in column 5-6
        jsr commonsubs.getcurnote
        bpl retpntw     ;nofxvaw  ;check note column
        inx
        jsr commonsubs.instpt1
        bpl retpntw     ;nofxvaw
        inx
        jsr commonsubs.instpt1
        and #$E0        ;check if bigFX
        bne retpntw     ;nofxvaw
        inx
        jsr commonsubs.instpt1
        ldy subwpos2+0  ;column inside track
        cpy #5
        bne +
        jsr commonsubs.writehex1
        jsr commonsubs.instpt2
        jmp keyhandler.ptncuri
+       jsr commonsubs.writehex2
        jmp commonsubs.instpt2
        .pend


;.section FC3_FIX
;================================ ORDERLIST ===================================
wrordli .proc
        jsr keyhandler.repeatex ;repeater for value typing/writing
        lda curwind
        cmp #1          ;Orderlist?
        beq +
        jmp chwrinw     ;------------ORDERLIST---------------------------
+       sty commonsubs.wrhxad1+1
        sty commonsubs.wrhxad3+1
        jsr keyhandler.SaveUndo
        jsr commonsubs.setseqptr
        lda subwpos2+1  ;cursor X position
        clc
        lsr
        bcs oddposi     ;if the cursor at even or odd position - which nybble to change
        clc
        adc seqdpos+1   ;add displaying position
        tay
        dey
        sty cmpjmpa+1   ;to check, if loop-jumpaddress is inside sequence, don't allow bigger
        jsr commonsubs.seqaddb
        iny
        cmp #$ff        ;check, if we're setting jump-address
        bne lal2
        jsr commonsubs.seqaddb
        jsr commonsubs.writehex1
;        .if SUBTUNEJUMP_ON
        bpl cmpjmpa     ;if below $80 watch for max. seq.length
        cmp #maxsubtuneamount+$80;if above $80, allow typing, because it means subtune-switching
        bmi outseq1     ;if below limit, no modification
        lda #maxsubtuneamount+$80
        bne outseq1     ;jump
;        .fi
cmpjmpa cmp #selfmod    ;check if trying to jump to $FF (would cause endless loop in player -> freeze )
        bcs notwri1     ;no modification if jump wants to point to $FF or above
outseq1 jmp wroldat
lal2    cmp #$fe
        beq noolwr      ;don't write after $fe, even if there's nonzero
        jsr commonsubs.seqaddb ;get the data in A by index prepared in Y
        beq noolwr      ;don't let pattern $00 to be overwritten (it's special)
        cmp #$fe
        bcs chkfeff
        jsr commonsubs.writehex1
        bne +           ;don't let pattern number to be $00 (it's special value)
        lda #$01        ;write 1 if user wants to write 0
        jmp wroldat
+       pha             ;can write above $80 for transposer
        and #$7f        ;bigger than $80 can be used here to expand orderlist-FX,transpose
        tax
        pla
        bpl +           ;don't restrict writing orderlist-FX
        cmp #$fe        ;but don't allow typing $fe/$ff anywhere
        bcs notwri1
	bcc wroldat
+       cpx #maxptnamount ;don't allow bigger pattern number than the allowed
        bcc wroldat
        lda #maxptnamount
wroldat jsr commonsubs.seqaddc ;set/write the data in A to orderlist position
        inc keyhandler.valuecont ;ensure continuous typing at scrolling time
notwri1 jmp keyhandler.seqstpr1 ;step one digit to the right (autoadvance)

chkfeff jsr commonsubs.writehex1
        cmp #$fe
        bcs wroldat
noolwr  rts
oddposi clc             ;cursor at odd position
        adc seqdpos+1   ;add cursor position
        tay
        dey
        sty cmpjmp2+1   ;to check, if loop-jumpaddress is inside sequence, don't allow bigger
        jsr commonsubs.seqaddb
        iny
        cmp #$ff        ;check, if we're setting jump-address
        bne lal3
        jsr commonsubs.seqaddb
        jsr commonsubs.writehex2
;        .if SUBTUNEJUMP_ON
        bpl cmpjmp2     ;if below $80 watch for seq.length
        cmp #maxsubtuneamount+$80;if above $80, allow typing, because it means subtune-switching
        bmi outseq2     ;if below limit, no modification
        lda #maxsubtuneamount+$80
        bne outseq2     ;jump
;        .fi
cmpjmp2 cmp #selfmod    ;check if trying to jump to $FF (would cause endless loop in player -> freeze )
        bcs notwrid     ;no modification if jump wants to point to $FF or above
outseq2 jmp wrolda2
lal3    cmp #$fe
        beq noolwr      ;don't write after $fe
        jsr commonsubs.seqaddb ;get the data in A by index prepared in Y
        beq noolwr      ;don't let pattern $00 to be overwritten (it's special)
        cmp #$fe
        bcs chbfeff
        jsr commonsubs.writehex2
        bne +           ;don't let pattern number to be $00 (it's special value)
        lda #$01        ;write 1 if user wants to write 0
        jmp wrolda2
+       pha
        and #$7f        ;bigger than $80 can be used here to expand orderlist-FX,transpose
        tax
        pla
        bpl +           ;don't restrict writing orderlist-FX
        cmp #$fe        ;but don't allow typing $fe/$ff anywhere
        bcs notwrid
	bcc wrolda2
+       cpx #maxptnamount ;don't allow bigger pattern number than the allowed
        bcc wrolda2
        lda #maxptnamount
wrolda2 jsr commonsubs.seqaddc ;set/write the data in A to orderlist position
        inc keyhandler.valuecont ;ensure continuous typing at scrolling time
        lda Config.autoadv
        beq noolwr
        jmp keyhandler.seqstpr2 ;step one digit to the right (autoadvance)

chbfeff jsr commonsubs.writehex2
        cmp #$fe
        bne +
        jsr commonsubs.seqaddc
        iny
        lda #0
        jmp commonsubs.seqaddc ;delete jumpaddress, if $fe endsignal is typed
+       bcs wrolda2
notwrid rts
        .pend

;============================INSTRUMENT-PANEL==================================
chwrinw .proc
        cmp #2          ;instrument-window?
        beq +
        jmp chwrchw
+       sty commonsubs.wrhxad1+1
        sty commonsubs.wrhxad3+1
        jsr keyhandler.SaveUndo
        jsr commonsubs.setinsptr ;set instrument's memory basepointers
        ldx subwpos1+2  ;current instrument-subwindow
        beq +
        jmp instbl      ;main instrument window or tables?
+       lda inswXbuf,x  ;lda subwpos2+2 ;x coordinate in instrument window
        cmp #4          ;where the full 00..FF value fields are
        bpl chHRtim     ;if column 4 or 5
        lsr
        bcs insmwri     ;if odd or even column
        jsr commonsubs.calcmwpos
        jsr commonsubs.instpt1
        jsr commonsubs.writehex1
        jsr commonsubs.instpt2
        jmp keyhandler.inswiri ;jsr inswiri ;autoadvance
;return1 rts 
insmwri jsr commonsubs.calcmwpos
        jsr commonsubs.instpt1
        jsr commonsubs.writehex2
        jsr commonsubs.instpt2
        ldx subwpos1+2  ;instrument-subwindow
        lda inswXbuf,x  ;lda subwpos2+2
        cmp #3
        bpl wrordli.notwrid
        jmp keyhandler.inswiri ;autoadvance

WF1left ;left nibble of instrument's 1st frame waveform (parameter $0f)
        jsr chkFR1
        jsr commonsubs.writehex1
        beq nowridb     ;don't allow 00 in SW1.x (it will be converted to $09 anyway)
        jsr commonsubs.instpt2
        jmp keyhandler.inswiri

WF1righ ;left nibble of instrument's 1st frame waveform (parameter $0f)
        jsr chkFR1
        jsr commonsubs.writehex2
        beq nowridb     ;don't allow 00 in SW1.x (it will be converted to $09 anyway)
        jmp commonsubs.instpt2

chkFR1  ldx #0          ;don't allow setting when 1st frame waveform disabled for the instrument 
        jsr commonsubs.instpt1
        and #8
        bne +
        pla             ;2 level return
        pla
nowridb rts
+       ldx #$0f        ;1st wavveform parameter position in instrument
        jmp commonsubs.instpt1

chHRtim bne chOctav     ;branch if column 5
        lda inswYbuf,x  ;lda subwpos3+2 ;set HR timer with numer keys 0..2
        beq WF1left
        cmp #1
        bne nowridb
        cpy #3
        bpl nowridb     ;nowrida ;if typed value is too much for HR-timer(more than 2)
        ldx #0
        jsr commonsubs.instpt1
        and #$fc
        ora commonsubs.wrhxad1+1
        jmp commonsubs.instpt2

chOctav lda inswYbuf,x  ;lda subwpos3+2 ;Y coordinate
        beq WF1righ
        cmp #3
        bne nowridb     ;nowrida  ;if not last
        cpy #8          ;maximum octave decrease in instrument
        bpl nowridb     ;nowrida ;don't allow bigger octave shift by typing
        ldx #9          ;Octave-shift amount of instrument
        jsr commonsubs.instpt1
        bmi +           ;negative?
        lda commonsubs.OCTMULS,y ;get octave transposition value
        jmp commonsubs.instpt2
+       lda #0          ;handle neegative
        jsr commonsubs.writehex2
        tay
        lda commonsubs.OCTMULS,y ;get octave transposition value
        sec
        sbc #1
        eor #$ff        ;negative value
        jmp commonsubs.instpt2

instbl                  ;--------------------instrument-tables-----------------------------------
        lda inswXbuf,x  ;lda subwpos2+2 ;x cursor-coordinate in WFARP table
        lsr
        php             ;CARRY BIT WILL SIGN IF EVEN OR ODD COLUMN
        sta xtoadd+1
        jsr commonsubs.getitbi ;get instrument-table position
        ldx subwpos1+2  ;instrument-subwindow
        ldy inswXbuf,x  ;ldy subwpos2+2

        ;common part to add X coordinate into mem.position
calcom  tax             ;memory position under cursor to X, get value to A
        jsr commonsubs.instpt1 ;check if endrow (1st column)
        cmp #$ff
        bne +           ;endrow - don't modify anything
        plp
        rts
+       sta chktjmp+1   ;if 1st row is $FE delimiter, notify to multiply written jump-address by 3
        txa
xtoadd  adc #selfmod
        tax
        jsr commonsubs.instpt1 ;jsr instpt1 ;read data under cursor to Accu

instbcom                ;common place for even row of instrument tables
        plp             ;CARRY bit back
        bcs insbco2     ;woddcol ;even or odd column
        cpy #2
        bpl +           ;check if 1st column
        jsr commonsubs.writehex1 ;merge typed 1st hexa digit with existing data
        cmp #$ff        ;don't allow writing $ff endsignal at 1st column
        beq nowrida
        cmp #$fe        ;if $fe jump command typed to 1st column, zero (init) jump-address
        bne ++++
        jsr commonsubs.instpt2
        inx
        lda #0          ;zero (init) jump-address
        jmp cheatj1
+       cpy #4
        bpl +           ;check if 2nd column
chktjmp ldy #selfmod    ;self-written previously, help to determine if jump-address
        cpy #$fe
        bne ++
        ldy subwpos1+2  ;actual instrument subwindow
        sec
        sbc commonsubs.jmtbase,y
        jsr commonsubs.div3acu
        jsr commonsubs.writehex1 ;merge typed 1st hexa digit with existing data
        jsr commonsubs.multi3b   ;multiply jump-address by 3
cheatj1 ldy subwpos1+2  ;current instrument subwindow
        clc
        adc commonsubs.jmtbase,y
        jmp +++
+       ldy chktjmp+1   ;3rd column ;help to determine if $fe in row
        cpy #$fe
        beq nowrida     ;don't modify third column if $fe jump is in row
+       jsr commonsubs.writehex1 ;merge typed 1st hexa digit with existing data
+       jsr commonsubs.instpt2   ;write data at X index
        jmp keyhandler.inswiri   ;autoadvance
nowrida rts

insbco2 cpy #2
        bpl +           ;check if 1st column
        jsr commonsubs.writehex2 ;merge typed 2nd hexa digit with existing data
        cmp #$ff        ;don't allow writing $ff endsignal at 1st column
        beq nowrida
        cmp #$fe        ;if $fe jump command typed to 1st column, zero (init) jump-address
        bne ++++
        jsr commonsubs.instpt2
        inx
        lda #0          ;zero (init) jump-address
        jmp cheatj2
+       cpy #4          ;check if 2nd column
        bpl +
        ldy chktjmp+1   ;2nd column ;help to determine if $fe in row
        cpy #$fe
        bne ++
        ldy subwpos1+2  ;current instrument subwindow
        sec
        sbc commonsubs.jmtbase,y
        jsr commonsubs.div3acu
        jsr commonsubs.writehex2 ;merge typed 2nd hexa digit with existing data
        jsr commonsubs.multi3b   ;multiply jump-address by 3
cheatj2 ldy subwpos1+2  ;current instrument subwindow
        clc
        adc commonsubs.jmtbase,y
        jmp +++
+       ldy chktjmp+1   ;3rd column ;help to determine if $fe in row
        cpy #$fe
        beq nowrida     ;don't modify third column if $fe jump is in row
+       jsr commonsubs.writehex2 ;merge typed 2nd hexa digit with existing data
+       jsr commonsubs.instpt2   ;write data at X index
        ldx subwpos1+2 ;instrument-subwindow
        lda inswXbuf,x ;lda subwpos2+2 ;auto-advance right. if column 5, no further autoadvance
        cmp #5
        bpl nowrida
        jmp keyhandler.inswiri ;autoadvance
        .pend

;=========================================================================
chwrchw .proc
        cmp #3          ;chordtable-window?
        beq +
        jmp tmptbwr
+       sty commonsubs.wrhxad1+1
        sty commonsubs.wrhxad3+1
        jsr commonsubs.getctpos ;get chordtable-position under cursor (position in X, value in A)
        ldy subwpos2+3
        bne ChDigi2     ;first digit comes
ChDigi1 cmp #$7e        ;don't overwrite end signal 1st digit
        beq chk7e7f
        cmp #$7f        ;don't overwrite return signal 1st digit
        beq chk7e7f
        bit Config.confbits
        bvc nummode
notemod stx xstore+1
notsto2 ldx #selfmod
        jsr jamming
xstore  ldx #0
        lda jammode+1
        bne noctwr
        lda notsto2+1
        jsr commonsubs.calcoct     ;add octave multiplied by 12 to Accu, compare to $5F
        bcs noctwr
ChrdNot sec
        sbc display.BasNote+1
        bmi wrinote
        cmp #$7e        ;filt accidental loop/jump commands
        bcs noctwr
wrinote sta CHORDS,x
        lda Config.autoadv
        beq noctwr
        jmp nextrow
nummode jsr commonsubs.writehex1
        cmp #$7e
        beq noctwr
        cmp #$7f
        beq noctwr
wrctdat sta CHORDS,x
        ldx #3
        jmp keyhandler.chtmrig ;cursor to right (autoadvance)
chk7e7f jsr commonsubs.writehex1
        cmp #$7e
        beq wrctdat
        cmp #$7f
        beq wrctdat
        rts
ChDigi2 cmp #$7e        ;don't overwrite end signal 2nd digit
        beq chb7e7f
        cmp #$7f        ;don't overwrite return signal 2nd digit
        beq chb7e7f
        bit Config.confbits
        bvs notemod
        jsr commonsubs.writehex2
        cmp #$7e
        beq noctwr
        cmp #$7f
        beq noctwr
wrctda2 sta CHORDS,x
nextrow lda #0
        sta subwpos2+3
        jmp keyhandler.norcrdn ;cursor to next row (autoadvance)
chb7e7f jsr commonsubs.writehex2
        cmp #$7e
        beq +
        cmp #$7f
        bne wrctda2
+       sta CHORDS,x
noctwr  rts
        .pend

;=========================================================================
tmptbwr .proc                ;tempotable-window
        sty commonsubs.wrhxad1+1
        sty commonsubs.wrhxad3+1
        jsr commonsubs.gettpos ;get tempotable positison and data under cursor to X and A
        ldy subwpos2+4 ;check horizontal cursor position in tempotable
        bne +
        ldy commonsubs.wrhxad1+1 ;don't allow to type more than 7 into 1st digit
        cpy #8
        bpl chwrchw.noctwr
        cmp #0
        bpl noendco     ;no correction if not end-of-program is under cursor
        pha
        lda commonsubs.wrhxad1+1
        ora #$08
        sta commonsubs.wrhxad1+1
        pla
noendco jsr commonsubs.writehex1 ;left position
        sta TEMPOTBL,x
        ldx #4
        jmp keyhandler.chtmrig ;cursor to right (autoadvance)
+       jsr commonsubs.writehex2
        sta TEMPOTBL,x
        jmp keyhandler.reter.carretn ;cursor to next row (autoadvance)
        .pend
;.send FC3_FIX

;.if (FINAL_CARTRIDGE_3_FIX==0)
;.dsection FC3_FIX
;.fi

        .bend
;==================================================================================================
; vim: sw=4 ts=4 syntax=asm:
