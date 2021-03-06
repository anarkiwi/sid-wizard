;ident:8,24
;===============PATTERN HANDLING (INSERT/DELETE/COPY/CUT/PASTE/TRANSPOSE) ====================
; $Id: insdelcopy.inc 360 2014-02-15 14:50:28Z soci $
;pattern operations

insdelcopy .block       ;private stuff

setptpt jsr commonsubs.setptnptr ;set pattern pointers in advance based upon current track/pattern position
setptp2 lda commonsubs.instpt1+1 ;set pattpt1+1 and pattpt4+1
        sta pattpt1+1
        sta pattpt4+1
        lda commonsubs.instpt1+2
        sta pattpt1+2
        sta pattpt4+2
        rts

CopyIns .proc
        jsr commonsubs.setinsptr
        ldx #maxinstsize-1
-       jsr commonsubs.instpt1
        sta instbuffer,x
        dex
        bpl -
        rts
        .pend

PasteIn .proc
        jsr commonsubs.setinsptr
        ldx #maxinstsize-1
-       lda instbuffer,x
        jsr commonsubs.instpt2
        dex
        bpl -
        rts
        .pend

;=========================================INSERT===============================
insertpt .proc          ;this routine shifts the pattern-data to 1..3 byte higher (Y times)
                        ;in memory from position given in X
                        ;if patternlength exceeds 255 (in ptnlength) no process, and Y=$ff on return
                        ;if ACCU=1, one extra byte at shifting
        stx xposto2+1   ;store current X position for later comparison
        dex
        stx xpostor+1   ;store current X-1 position for later comparison
        sty insamo2+1   ;amount of shift, used to init new empty place,bigFX affects it
        sta zeroval+1
        cmp #1          ;signs that bigFX needs one more byte
        bne +
        iny
+       sty insamou+1   ;insertion amount (how much the rest is moved)
        sty insamo3+1
        jsr setptp2     ;set pattpt1+1 and pattpt4+1
        ;----check if pattern is full or would be overloaded after insertion---
        ldy subwpos1+0  ;current track
        ldx curptn1,y   ;current pattern
        lda ptnsize,x   ;real size of pattern (maintained)
        cmp #maxptnlen+2 ;+2 is important here, as sometimes max. pattern size can be overridden (placing FX when ptn.size is $F6/$F7)
        bcs fullpt2
        clc
insamou adc #1          ;self-written , shifting amount from Y
        bcc +           ;if carry bit reaches 1, end of pattern, no further writing to pattern
fullpt2 ldy #$ff        ;pattern full signal
        rts
+       sta ptnsize,x   ;otherwise change pattern size

        sec
        sbc ptnMpos,y   ;y holds number of track (0..2)
        tax             ;pointer/index to end of pattern - from where to start insertion
        clc
insamo3 adc #1          ;self-modified
        tay             ;pointer/index to the end of pattern plus shifing amount
        jsr movpfwd     ;move pattern-content forward, x and y holds source & target endpositions, X base-position is set in xpostor+1
        inx             ;initial X position
        ;zero out the new places
        inx
        lda #$00        ;the new value is 0 initially
        jsr commonsubs.instpt2
zeroval ldy #selfmod    ;1 if bigfx, zero bigFX value
        beq insamo2
        inx
        jsr commonsubs.instpt2
        dex
insamo2 ldy #selfmod    ;coming from Y
newalop dex
        dey
        beq +           ;no empty $80 to fill?
        lda #$80
        jsr commonsubs.instpt2
        jmp newalop
+
xposto2 ldx #selfmod    ;get back to original position in X
fullptn rts
        .pend

pattpt1 = movpfwd.pattpt1
xpostor = movpfwd.xpostor

movpfwd .proc           ;move pattern-content forward, x and y holds source & target endpositions, X base-position is set in xpostor+1
pattpt1 lda $1111,x     ;self-written , read from patterndata
        sta (ptnzptr),y ;write to shifted data-area
        dey
        dex
xpostor cpx #selfmod
        bne pattpt1
        rts
        .pend

;===================================DELETE=====================================
delbackpt .proc         ;delete Y bytes of pattern data from X position, rest moved back
        ;used to clear up place when e.g. switching back to smallfx or zeroing fx/ins.
        stx xstodel+1
        stx xstode2+1
        sty ystodel+1
        sty ystode2+1
        sty ystode3+1
        jsr setptp2     ;set pattpt1+1 and pattpt4+1
        ldx subwpos1+0  ;current track
        ldy curptn1,x   ;current pattern
        lda ptnsize,y   ;real size of pattern in memory (maintained)
        sec
ystodel sbc #selfmod
        bcc xstode2     ;if zero has been exceeded, no process
        sta ptnsize,y
        ;should check for zero patternlength? maybe enough at delete-key subroutine
        sec
        sbc ptnMpos,x
        clc
ystode3 adc #0          ;set the amount Y bytes bigger to delete the rest after the reduced pattern
        sta lenstor+1

xstodel ldx #selfmod
        txa
        clc
ystode2 adc #selfmod
        tay
        jsr movpbwd     ;move pattern-memory area backwards, X and Y are source/target basepositions, lenstor+1 is set to X endposition
xstode2 ldx #selfmod    ;restore X
        rts
        .pend

lenstor = movpbwd.lenstor
pattpt4 = movpbwd.pattpt4

movpbwd .proc           ;move pattern-memory area backwards, X and Y are source/target basepositions, lenstor+1 is set to X endposition
-       lda (ptnzptr),y
pattpt4 sta selfmodA,x
        iny
        inx
lenstor cpx #selfmod    ;holds real end of pattern in memory
        bne -
        rts
        .pend

;=======================================COPY===================================
copytobuf .proc         ;copy patterndata (from cursorposition) to buffer
        lda playadapter.followp+1 ;don't allow any copying in followplay-mode
        bne copyend
        ;calculate real length (amount of rows) of pattern-data being copied to buffer
        jsr commonsubs.compcur        ;output: Accu holds patternposition under cursor, x-current track, y-current pattern
        beq copyend     ;if cursor at end of pattern, no process
        ldx shiftsi
        bne copyends    ;if shift also pressed for control+C, we define end of copying area
        sta copyrow+1   ;row position inside pattern (relative to patternstart) where copying was initiated
        sty copyptn+1   ;the pattern where copy was initiated
        eor #$ff        ;multiply by -1 to perform a substraction with the addition
        sec
        adc ptnlength,y ;length of the copied pattern
        sta bufflen+1
        lda ptnsize,y
        sta sizesto+1

        jsr setptpt     ;set pattern pointers in advance based upon current track/pattern position - stores & restores Y
        ldx subwpos1+0  ;current track
        lda ptncurs,x   ;pattern memo-position at cursor (relative to patterndisplay)
        tay
        clc
        adc ptnMpos,x
        sta copypos+1   ;the memory position inside pattern (relative to patternstart) where copying was initiated

        eor #$ff        ;multiply by -1 to perform a substraction with the addition
        clc             ;substract 1 too
        adc sizesto+1
        sta sizesto+1 

        ldx #0          ;buffer starting at 0 always
bufloop lda (ptnzptr),y ;adress of current pattern has previously been set by setptpt
        sta patternbuffer,x
sizesto cpx #selfmod    ;self-written before, size of the pattern
        bcs setbsize    ;check if reached end of pattern
        iny
        inx
        bne bufloop     ;possibly never more than max. patternlength, but who knows
setbsize                ;sign copied data size in bytes
        stx bufsize+1
copyend rts

copyends                ;used to sign a wished end of patterndata in the copy-buffer - only if we are in the same pattern as where copy was initiated
copyptn cpy #selfmod    ;(copystartptn) ;the pattern where copy was initiated ;Y is set previously (above in copytobuf) in compcur
        bne cendend     ;if we are not in the same pattern, do nothing, leave the buffer as it is
        jsr commonsubs.compcur ;output: Accu holds pattern-row-position under cursor, x-current track, y-current pattern
        clc             ;adds 1 - don't allow copy-enposition being the same as copy-startposition
copyrow sbc #selfmod    ;(copystartrow) ;row position inside pattern (relative to patternstart) where copying was initiated
        bcc cendend     ;if new copyend position is smaller than initial copying position
        adc #0          ;C is 1 here, so this is addition of 1 actually
        sta bufflen+1
        lda ptncurs,x   ;pattern memo-position at cursor (relative to patterndisplay)
        clc
        adc ptnMpos,x
        sec
copypos sbc #selfmod    ;the memory position inside pattern (relative to patternstart) where copying was initiated
        tay
        sty bufsize+1
cendend rts
        .pend

;=======================================CUT====================================
bufflen = cutandcopy.bufflen

cutandcopy .proc        ;cut patterndata from cursorposition, copy cut data to buffer
        ;patternlength should be intact - or not?
        lda playadapter.followp+1 ;don't allow any cutting in followp-play-mode
        bne cutcend
        jsr copytobuf   ;first copy the data to the buffer
        jsr commonsubs.compcur ;output: Accu holds pattern-rowposition under cursor, x-current track, y-current pattern
        beq cutcend     ;if cursor at end of pattern, no process
        jsr commonsubs.getcurnote ;Y stays intact
        txa             ;memo-position at cursorposition
        clc
        ldx subwpos1+0  ;current track
        adc ptnMpos,x   ;memo-position of pattern at 1st diplayrow
        adc bufflen+1   ;number of empty rows - equal with empty bytes
        adc #1          ;$ff signal's place
        sta ptnsize,y   ;this is the size of data from cursor-position to pattern-endsignal

        ldx subwpos1+0  ;current track
        ldy ptncurs,x   ;pattern memo-position at cursor (relative to patterndisplay)
bufflen ldx #1          ;self-written variable (bufferlength) ;length (amount of rows) of copied buffer
        lda #0
-       sta (ptnzptr),y
        iny
        dex
        bne -
        lda #$ff
        sta (ptnzptr),y
        ;zero out the rest - maybe not needed
cutcend rts
        .pend

;======================================PASTE===================================
bufsize = pastebuf.bufsize

pastebuf .proc
        lda playadapter.followp+1 ;don't allow any pasting in followp+1 play-mode
        beq +
        jmp pastend
+       jsr setptpt
        jsr commonsubs.compcur ;output: Accu holds absolute pattern-rowposition under cursor, x-current track, y-current pattern
        geq pastend     ;pastend ;if cursor at end of pattern, no process
        sta xdetlo2+1
        clc             ;accu holds absolute rownumber of cursor here
        adc bufflen+1   ;Accu holds end of paste in memory here ;sum can be larger than 255! 16 bit addition needed!
        ;sta smpstor+1
        bcc +
        jmp ovrpast     ;if more than 255, overpasting for sure
+       sec             ;needed for 16bit operation
        cmp ptnlength,y ;sbc ptnlength,y ;16 bit comparison if pattern is smaller than end of pasting
        bcc smallpa
        jmp ovrpast     ;if pasting exceeds patternlength, other pasting case & method
;----------------------------
smallpa                 ;branch, where pasting ends before end of pattern -rest needs to be handled (moved in memory)
;smpstor lda #selfmod
        clc
        adc #1          ;?
        ldy subwpos1+0  ;track
        jsr commonsubs.detmemp ;get memory position given (accu) row data to X - relative to displayposition
        txa
        stx intactpos1  ;original intact area mempos - relative to display
        ;inc insrefr+1  -??? why did I do this ???

        ;detect intact area size
        ldx subwpos1+0  ;current track
        ldy curptn1,x   ;current pattern
        lda ptnsize,y
        sec
        sbc intactpos1
        sbc ptnMpos,x
        sta intactsize

        ldx subwpos1+0  ;current track
bufsize lda #1          ;self-written variable (buffersize) ;copied data size in bytes
        sta pastesize+1
        clc
        adc ptncurs,x
        sta intactpos2  ;new position of intact area - relative to display

        ;calculate and check ptnsize = intactpos2+intactsize+ptn1pos,x
        lda intactpos2
        clc
        adc intactsize
        bcs pastend     ;16bit checking
        adc ptnMpos,x
        bcs pastend     ;take 9bit result into consideration
        cmp #maxptnlen+1
        bcs pastend
        sta ptnsize,y

        ;do the moving of intact area - direction depends on difference of pos 1 & 2
        ldx intactpos1
        cpx intactpos2
        beq inpaste     ;if no need to move anything
        dex
        stx xpostor+1   ;sign for mover routine
        bcs movibak
movifwd lda intactpos1
        clc
        adc intactsize
        tax
        lda intactpos2
        clc
        adc intactsize
        tay
        jsr movpfwd     ;move pattern-content forward, x and y holds source & target endpositions, X base-position is set in xpostor+1
        jmp inpaste

pastend rts

movibak ldy intactpos1
        ldx intactpos2
        txa
        clc
        adc intactsize
        sta lenstor+1
        jsr movpbwd

        ;do the copying of pasted area
inpaste ldx subwpos1+0  ;current track
        ldy ptncurs,x
        jsr buftopt
        jmp pastadv

intactpos1 .byte 0      ;original position of intact area
intactpos2 .byte 0      ;new position of intact area
intactsize .byte 0      ;size of intact area in bytes
;----------------------------
ovrpast lda ptnlength,y ;when pasting size is exceeding patternsize
        tay
        ldx #0          ;starts as 0, beginning of patternbuffer, accumulates the mem-amount in X
xdetlo2 cpy #0
        beq dopaste
        lda patternbuffer,x
        bpl decyerb
        inx
        lda patternbuffer,x
        bpl decyerb
        inx
        lda patternbuffer,x
        and #$e0
        bne decyerb
        inx
decyerb inx
        dey
        jmp xdetlo2

dopaste stx pastesize+1
        ldx subwpos1+0  ;current track
        ldy curptn1,x   ;current pattern
        lda ptncurs,x   ;pattern memo-position at cursor (relative to patterndisplay)
        clc
        adc ptnMpos,x
pastesize adc #selfmod
        bcs pastend     ;take 9bit result into consideration
        cmp #maxptnlen
        bcs pastend     ;if pasting would oversize pattern
        adc #1
        sta ptnsize,y

        ldy ptncurs,x
        jsr buftopt     ;copy buffer content into pattern memory position in Y. Pastesize+1 must be precalculated.
        lda #$ff
        sta (ptnzptr),y

pastadv lda Config.autoadv
        beq pastend
        lda bufflen+1   ;go to next position after pasted area - maybe loop slows down, but needed for trackbinding
        cmp #maxadvancesteps ;don't advance for too many steps (shall we?)
        bcs pastend
        sta pascur
-       jsr keyhandler.godownp ;automatically ends when end of pattern
        dec pascur
        bne -
        rts
pascur .byte 0          ;counter for going down after paste

buftopt                 ;copy buffer content into pattern memory position in Y. Pastesize+1 must be precalculated.
        ldx #0
-       lda patternbuffer,x
        ;cmp #$ff ;for safety
        ;beq pastadv
        sta (ptnzptr),y
        iny
        inx
        cpx pastesize+1
        bne -
        rts
        .pend

;---------------------------------------------------------------------------------------------
transptn .proc          ;transpose current pattern from cursorposition up/down, addition (or 2nd complement substraction) amount in X
        stx tranadd+1
        lda playadapter.followp+1 ;don't allow any copying in followp+1lay-mode
        ora curwind     ;if not patternwindow
        bne tranret
        jsr commonsubs.compcur ;output: Accu holds patternposition under cursor, x-current track, y-current pattern
        beq tranret     ;if cursor at end of pattern, no process
        jsr setptpt
        ldx subwpos1+0  ;current track
        ldy ptncurs,x   ;pattern memo-position at cursor (relative to patterndisplay)
translp lda (ptnzptr),y
        cmp #$ff        ;if reached endsignal
        beq tranret
        and #$7f
        beq decidco     ;NOP? Don't disturb
        cmp #SWM_NOTE_MAX+1
        bcs decidco     ;above note-range? - don't disturb
        lda (ptnzptr),y
        clc
tranadd adc #1          ;self-modified , amount to add to note
        sta trstore+1
        and #$7f        ;check if result inside limit
        cmp #$01        ;lower limit of notes
        bcc decidco     ;don't go below/above limit
        cmp #SWM_NOTE_MAX+1 ;upper limit of notes
        bcs decidco
trstore lda #0          ;self-written
        sta (ptnzptr),y
decidco lda (ptnzptr),y
        bpl nextbyt     ;if only a note in that row (no other columns)
        iny
        lda (ptnzptr),y
        bpl nextbyt
        iny
        lda (ptnzptr),y
        and #$e0
        bne nextbyt
        iny
nextbyt iny
        bne translp
tranret rts
        .pend

;-------------------------------------------------------------------------------
copyseq .proc ;copy sequence from cursor-position (in orderlist) to 'seqbuffer'
        jsr commonsubs.setseqptr ;get sequence pointers
        jsr commonsubs.getcurptn ;get orderlist memory index under cursor to A and Y
        dey
        jsr commonsubs.seqaddb
        cmp #$ff        ;don't paste when we're at jump/loop-address
        beq scpyend
        jsr commonsubs.getcurptn
        ldx shiftsi
        bne scopyends   ;if Shift pressed, define the end of clipboard/buffer data
        sta scopypos+1
        ldx #0
-       jsr commonsubs.seqaddb ;load Y-indexed sequence-value
        sta seqbuffer,x
        cmp #$fe        ;copy till $fe or $ff delimiter/endsignal
        bcs + 
        inx
        iny
        cpy #seqlength
        bcc -           ;safety jump
+       stx seqbufs+1
        rts
scopyends ;define the end of clipboard/buffer data
        sec
scopypos sbc #selfmod   ;start-position of seq-copying
        bcc scpyend     ;don't allow 'negative' buffer-size
        cmp seqbufs+1
        bcs scpyend     ;check if accidentally set to bigger than the datasize in buffer
        sta seqbufs+1
scpyend rts
        .pend

;-------------------------------------------------------------------------------
seqbufs = pasteseq.seqbufs
pasteseq .proc ;paste sequence-buffer content into orderlist cursor-position
        lda seqbufs+1   ;if nothing in buffer, do nothing
        beq spstend
        jsr commonsubs.setseqptr ;get & set sequence pointers
        lda commonsubs.seqaddb+1
        sta spasteT+1
        sta spasteS+1
        sta spasteD+1
        sta spasteW+1
        lda commonsubs.seqaddb+2
        sta spasteT+2
        sta spasteS+2
        sta spasteD+2
        sta spasteW+2
        jsr commonsubs.getcurptn ;get orderlist memory index under cursor to A and Y
        sty spstpos+1
        dey
        jsr commonsubs.seqaddb
        cmp #$ff        ;don't paste when we're at jump/loop-address
        beq spstend
seqbufs ldy #selfmod    ;size of data in sequence-buffer
        ldx #seqlength
spasteT lda selfmodA,x  ;test for endsignal in area that's going to be cropped after insertion
        cmp #$fe
        bcs spstend     ;if endsignal found in there, paste wouldn't fit, do not paste anything
        dex
        dey
        bpl spasteT     ;test-loop
        ldy #seqlength
        inx
spasteS lda selfmodA,x  ;move existing area forward to make place for the pasted data
spasteD sta selfmodA,y
        dey
        dex
        bmi +           ;useful if cursor-position is 0
spstpos cpx #selfmod    ;paste start-position 
        bcs spasteS
+       inx
        ldy #0          ;paste the date to the freed area
spastlp lda seqbuffer,y
spasteW sta selfmodA,x
        inx
        iny
        cpy seqbufs+1
        bne spastlp     ;loop
spstend rts
        .pend

        .bend
;==========================================================================================================
; vim: sw=4 ts=4 syntax=asm:
