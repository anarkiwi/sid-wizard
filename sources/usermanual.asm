;===========================================================
;native User Manual for SID-Wizard 1.5 (to be on the floppy)
; $Id: usermanual.asm 382 2014-06-23 17:53:01Z hermitsoft $
;===========================================================

VRAM=$0400
CRAM=$d800
zpvar3=4
zptr3=9
repspd1=$0d ;first keypress repeat delay in frames (x20ms)
repspd2=$02 ;continuous keyrepeat delay (repetition speed) in frames (x20ms)

        *= $0801
        .word ss,1
        .null $9e, format("%d", start)  ;SYS command
ss      .word 0

start   sei
        lda #$35
        sta 1
        lda #<dummirq
        ldx #>dummirq
        sta $fffa
        stx $fffb
        lda #<irq
        ldx #>irq
        sta $fffe
        stx $ffff
        lda #$01
        sta $d01a
        lda #$ff
        sta $d012
        lda #$7f
        sta $dc0d
        ;;; NMI
        lda #$90
        sta $dd0d
        lda $dc0d
        lda $dd0d
        asl $d019

        lda #$03
        sta $dd00
        lda #0
        sta $d020
        sta $d021
        sta $d015
        jsr $1000 ;init tune
        lda #$1b
        sta $d011
        lda #$08
        sta $d016
        lda #$16
        sta $d018
        ldy #0
        lda #5
-       sta CRAM,y
        sta CRAM+$100,y
        sta CRAM+$200,y
        sta CRAM+$300,y
        iny
        bne -

        lda #8 ;set tune volume
        jsr $1009
        
        cli

helploop lda helprefr
        beq +
        jsr dispHelp
+       jmp helploop


;---------------------------------------
irq     pha
        txa
        pha
        tya
        pha
        asl $d019
        
        ;inc $d020
        jsr $1003 ;play music
        ;dec $d020

        jsr keyer

retirq  pla
        tay
        pla
        tax
        pla
        rti

dummirq bit $dc0d
        inc restsig	;signs for keyhandler that restore was pressed. routine zeroes it
        rti

restsig .byte 0

;----------------------------------------
retmkey rts

keyer   lda restsig
        beq +
        lda #0
        sta restsig
        jmp helpgup
+       jsr shcbget
        ldy #$bf
        sty $dc00
        ldy $dc01
+       cpy #$ff-$08 ;home-key?
        bne +
        lda #<HelpText
        ldx #>HelpText
        sta helpposi+0
        stx helpposi+1
        inc helprefr
        rts
+       cpy #$ff-$40 ;up-arrow?
        beq helpgdn
        cpy #$ff-$80 ;slash?
        beq slashjp
        cpy #$ff-($80+$10) ;shift+slash?
        bne +
slashjp lda shiftsi
        bne helpgup
helpgdn	jsr repeatex
        lda helprefr
        bne retmkey
        ldx #HelpTextPageSize ;help page-up/down row amount
        stx zpvar3
-       jsr ScrHlDn
        dec zpvar3
        bne -
        rts
helpgup	jsr repeatex
        lda helprefr
        bne reter1
        ldx #HelpTextPageSize ;help page-up/down row amount
        stx zpvar3
-       jsr ScrHlUp
        dec zpvar3
        bne -
        rts
+       ldy #$fe
        sty $dc00
        ldy $dc01
        cpy #$ff-$80 ;cursor-down key?
        beq +
        jmp nokey
+       jsr repeatex
        lda shiftsi
        bne ScrHlUp
        lda helprefr
        bne retmkey
ScrHlDn	lda helpends ;has end of text been reached? (help from dispHelpTxt)
        bne reter1
        lda helpposi+0
        ldx helpposi+1
        cpx #>(HelpText_end-HelpTextLastPageSize)
        bcc +
        bne reter1
        cmp #<(HelpText_end-HelpTextLastPageSize)
        bcs reter1
+       sta HelpPt2+1
        stx HelpPt2+2
HelpPt2	lda $1111 ;self-written
        cmp #$8d ;$0d ;enter/line-feed?
        beq foundNR
        inc HelpPt2+1
        bne HelpPt2
        inc HelpPt2+2
        bne HelpPt2
foundNR	lda HelpPt2+1 ;found next row
        clc
        adc #1 ;2 ;skip line-feed $0a too
        sta helpposi+0
        lda HelpPt2+2
        adc #0 ;add Carry-flag
        sta helpposi+1
        inc helprefr ;cause refreshment of help-text display
reter1  rts
ScrHlUp	lda #>HelpText
        cmp helpposi+1
        bcc +
        bne rethkey
        lda #<HelpText
        cmp helpposi+0
        bcs rethkey
+       lda helpposi+0
        ldx helpposi+1
        sec
        sbc #2 ;3 ;previous line-feed should be skipped in tester
        sta HelpPt3+1
        txa
        sbc #0 ;substract Carry flag
        sta HelpPt3+2
HelpPt3	lda $1111 ;self-written
        cmp #$8d ;$0d
        beq foundPR
        lda HelpPt3+1
        sec
        sbc #1
        sta HelpPt3+1
        lda HelpPt3+2
        sbc #0 ;substract Carry
        sta HelpPt3+2
        bne HelpPt3
foundPR	lda HelpPt3+1 ;found previous row
        clc
        adc #1 ;2 ;skip line-feed $0a too
        sta helpposi+0
        lda HelpPt3+2
        adc #0 ;add Carry flag
        sta helpposi+1
        inc helprefr
rethkey	rts

nokey	lda #repspd1	;no pressed key, repeatcounter resets
        sta repecnt
        rts

;-----------------------------------------
repeatex                ;repeat-counter routine, return only if repeat can be done (shouldn't affect X or Y registers)
        lda repecnt        ;keyrepeat-counter
        bne norsrep        ;check, if repeat counter is 0
        lda #repspd2	;default repeat-speed 2
        sta repecnt        ;if 0, reset counter to repeatspeed 2
noret	pla         	;getting back Program-counter from stack? (injected by Soci)
        pla                ;causing '2-level rts'?
        rts                ;repeat not allowed here
norsrep cmp #repspd1	;check, of repeat counter is initial value (repeatspeed 1)
        bne repcan        ;if no, then just decrease the repeat counter
        dec repecnt        ;if yes, decrease by one, and reset Z-flag - repeat possible
        rts                ;repeat allowed here
repcan	dec repecnt
        bne noret        ;if reaches 0, a repeat occurs, next round will reset to speed 2
        rts                ;repeat allowed here
repecnt	.byte 0

shcbget	ldx #0        	;get status of SHIFT and CBM keys
        stx shiftsi
        stx cbmsig
        ldy #2
-        lda modkeyc,y
        sta $dc00
        lda $dc01
        and modkeyr,y
        bne +
        inc shiftsi,x
+        dey
        bne +
        inx
+        bpl -
        rts
modkeyc .byte $7f,$bf,$fd ;columns of CBM, Right-SHIFT, Left-Shift
modkeyr .byte $20,$10,$80 ;rows of CBM, Right-SHIFT, Left-Shift
shiftsi	.byte 0
cbmsig  .byte 0


;----------------------------------------
dispHelp lda helpposi+0
        ldx helpposi+1
        sta HelpPtr+1
        stx HelpPtr+2
        lda #<VRAM
        ldx #>VRAM
        sta zptr3+0
        stx zptr3+1
        ldx #25 ;counts rows
HelpRow ldy #0
HlpLoop lda HelpPtr+2
        cmp #>HelpText_end
        bcc HelpPtr
        lda HelpPtr+1
        cmp #<HelpText_end
        bcc HelpPtr
        inc helpends
        lda #0
        sta helprefr
        rts
HelpPtr lda $1111 ;source-byte
        cmp #$8d ;$0d ;Enter?
        bne ShowChr
        lda #" "  ;fill the rest of the line with whitespace
-       cpy #40
        bpl +
        sta (zptr3),y
        iny
        bne -
+       lda #40 ;line-feed
        clc
        adc zptr3+0
        sta zptr3+0
        lda zptr3+1
        adc #0 ;add Carry
        sta zptr3+1
        inc HelpPtr+1
        bne +
        inc HelpPtr+2
+       jmp linefed
ShowChr ;cpy #40  ;only the beginning is visible, if longer than 40...
        ;bcs AdvText
        ;cmp #$1f        ;don't show controller codes
        ;bcc AdvText
        ;cmp #$60        ;input: Accu, output: Accu
        ;bcc +   ;handle lowercase
        ;and #$1f
+       sta (zptr3),y
        iny
AdvText inc HelpPtr+1
        bne +
        inc HelpPtr+2
+       cpy #80 ;safety, allow max. 80 chars/row to prevent endless loops
        bcc HlpLoop
linefed dex
        bne HelpRow
        lda #0
        sta helpends
        sta helprefr
retHelp	rts 

helpposi .word HelpText ;help text's position
helpends .byte 0 ;notifies keyhandler that help reached its end
helprefr .byte 1 ;if help-text needs refresh


;------------------------------------------------------------------------
*=$1000
.binary "../examples/sid-exports/flashitback.sid",$7e


;-----------------------------------------------------------
.byte $8d ;ensure enter before 1st row (for up-scroller)

HelpTextFile .binary "../manuals/SID-Wizard-1.8-UserManual.txt"

HelpTextPageSize=24 ;how much to page-up/down

*=HelpTextFile ;overwrite un-encoded file data with C64-encoded data
HelpText
        .for i = 0, i < size(HelpTextFile), i = i + 1
         .if (HelpTextFile[i]=="@") ;ASCII "@" character
          .byte 0 ;PETSCII "@" character
         .elsif (HelpTextFile[i]==$0d) ;ASCII car-return character
          .byte $8d ;used to tell car-return to C64
         .elsif (HelpTextFile[i]=="[")
          .byte $1b ;PETSCII "[" character
         .elsif (HelpTextFile[i]=="]")
          .byte $1d ;PETSCII "]" character
         ;.elsif (HelpTextFile[i]=="<" && HelpTextFile[i+1]=="-")
         ; .byte $5f ;PETSCII back-arrow character
         ;.elsif (HelpTextFile[i]=="-" && HelpTextFile[i-1]=="<")
         ; .byte $20 ;PETSCII SPACE character
         .elsif (HelpTextFile[i]<$1F)
          ;nothing in place of control-characters
         .else
          .byte (HelpTextFile[i]<$60) ? HelpTextFile[i] : HelpTextFile[i]-$60
         .fi 
        .next
HelpText_end

;j .var HelpText_end-HelpText
;.for i=0,i<25,i=i+1
; .for j=j, HelpText[j]!=$8d, j=j-1
; .next
;.next
HelpTextLastPageSize=875 ;HelpText_end-j


.end ;the real end of the file is the encoded text's end (shrinked from control-characters)
;==========================================================================================

