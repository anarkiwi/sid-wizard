;=======================================================================
;SWFX - Sound-FX tester for SID-Wizard players exported with SFX-support
;Triggers the notes & instruments on 3rd channel with priority over music
; $Id: SWFX.asm 360 2014-02-15 14:50:28Z soci $
;-----------------------------------------------------------------------
PLAYERADDR=$1000 ;the address of the included SID-Wizard music-player
inisub=PLAYERADDR+0
playsub=PLAYERADDR+3
SFXsub=PLAYERADDR+12

varpos=PLAYERADDR+$21 ;variables start here - must be fixed address from now on...
;variables for channel1, (for other channels 7 / 14 must be added to the address):
SPDCNT=varpos+1*3*7+1 ;speed/tempo-counter (to grab finer-grade moments)
SEQPOS=varpos+1*3*7+2 ;sequence-position on channel 1 - useful for demo-timing
CURPTN=varpos+2*3*7+0 ;currently played pattern (for demo maybe)
CURNOT=varpos+2*3*7+1 ;current note
CURIFX=varpos+2*3*7+3 ;current instrument/FX column value
CURINS=varpos+2*3*7+4 ;currently selected instrument, must be changed temporarily
CURFX2=varpos+2*3*7+5 ;small pattern-effect can be used to control volume of SFX

        *= $0801
        .word ss,2012
        .null $9e,^start;Sys 2064
ss      .word 0

        *= $0810
start   sei
        jsr initApp

        lda #0 ;subtune number
        jsr inisub ;init tune
        
rastlp  ;'rastlp' is an example how (in what order) the player & SFX-adapter subroutines should be called
        lda #$fc
        cmp $d012
        bne *-3

        lda #13
        sta $d020
        jsr playsub ;player-code
        lda #11
        sta $d020

        jsr keyer ;It will return the pressed number key in Y (if nothing pressed: Y=$FF)
        cpy #$ff
        beq +
        sty $d020
        tya ;Y=1..10 here depending on the pressed key
        clc
        adc #$0d ;1st SFX instrument in the tune
        tay
        ;Y holds SFX-instrument number
        ldx #1+5*12 ;note
        lda #40 ;length (in frames)
        jsr SFXsub ;the SFX starter code in player - inputs: X:note, Y:instrument (0..$3F), A:length (1..$7F frames)
        +

        jsr dispDebug ;display debug info on screen
        
        jmp rastlp


;-----------------------------------------------------------------------
zpvar1=2
zptr1=4
zptr2=6

keyer   ;It will return the pressed number key in Y (if nothing pressed: Y=$FF)
        ldx #0
        ldy #0 ;init, starting with testing of key "1"
-       lda numkeyrow,x
        sta $dc00
        lda $dc01
        cmp #$fe
        beq dotheFX
        iny
        lda $dc01
        cmp #$f7
        beq dotheFX
+       iny
        inx
        cpx #5
        bne -
nokey   lda #0
        sta oneshot
dontSFX ldy #$ff  ;will tell pressed number key
retkeyr rts
dotheFX lda oneshot
        bne dontSFX     ;if key was pressed in previous frame
        lda #1
        sta oneshot
        rts

numkeyrow .byte $7f,$fd,$fb,$f7,$ef
oneshot	.byte 0

initApp	lda #$35
        sta 1
        lda #3
        sta $dd00
        lda #$17
        sta $d018
        lda #12
        sta $d021
        ldy #0
-       lda #" "
        sta $0400,y
        sta $0500,y
        sta $0600,y
        sta $0700,y
        lda #3
        sta $d800,y
        sta $d900,y
        sta $da00,y
        sta $db00,y
        iny
        bne -
        ldy #39
-       lda titletx,y
        jsr convASCII
        sta $0400,y
        lda #"-"
        sta $0400+40,y
        lda helptxt,y
        jsr convASCII
        sta $0400+40*2,y
        lda usagetx,y
        jsr convASCII
        sta $0400+40*4,y
        lda usaget2,y
        jsr convASCII
        sta $0400+40*5,y
        lda usaget3,y
        jsr convASCII
        sta $0400+40*6,y
        lda usaget4,y
        jsr convASCII
        sta $0400+40*7,y
        lda debugtx,y
        jsr convASCII
        sta $0400+40*(debugTextRow-1),y
        dey
        bpl -
        rts
        
debugTextRow=10 ;debug-text will apear here
valueColumn=21 ;debug-value screen column
dispDebug
        lda #0
        sta zpvar1
-       ldy zpvar1
        lda textlist+0,y
        pha
        ldx textlist+1,y
        tya
        clc
        adc #debugTextRow
        tay
        pla
        beq chDebug
        jsr textdisp
valDisp	ldy zpvar1
        lda valulist+0,y
        ldx valulist+1,y
        sta zptr1+0
        stx zptr1+1
        ldy #0
        lda (zptr1),y
        jsr hexdisp
        ldy #valueColumn
        sta (zptr2),y
        txa
        iny
        sta (zptr2),y
        ldy #7
        lda (zptr1),y
        jsr hexdisp
        ldy #valueColumn+3
        sta (zptr2),y
        txa
        iny
        sta (zptr2),y
        ldy #2*7
        lda (zptr1),y
        jsr hexdisp
        ldy #valueColumn+6
        sta (zptr2),y
        txa
        iny
        sta (zptr2),y
        lda #"$"
        ldy #valueColumn+10
        sta (zptr2),y
        lda zptr1+1
        jsr hexdisp
        ldy #valueColumn+11
        sta (zptr2),y
        txa
        iny
        sta (zptr2),y
        lda zptr1+0
        jsr hexdisp
        ldy #valueColumn+13
        sta (zptr2),y
        txa
        iny
        sta (zptr2),y
        inc zpvar1        
        inc zpvar1
        bne -
chDebug	ldy #40*2-1
-       lda varpos,y
        sta $0400+23*40,y
        dey
        bpl -
        rts
textlist .word tempotx,ordlitx,pattntx,notestx,insfxtx,instrtx,effectx,0
valulist .word SPDCNT, SEQPOS, CURPTN, CURNOT, CURIFX, CURINS, CURFX2, 0

textdisp ;input: A,X: text source-address, Y: screen-row
        sta zptr1+0
        stx zptr1+1
        lda rowindexl,y
        sta zptr2+0
        lda rowindexh,y
        sta zptr2+1
        ldy #0
-       lda (zptr1),y
        beq endtxt
        jsr convASCII
        sta (zptr2),y
        iny
        bne -
endtxt  rts

rowindexl .for i=0,i<25,i=i+1
           .byte <($0400+i*40)
          .next
rowindexh .for i=0,i<25,i=i+1
           .byte >($0400+i*40)
          .next

convASCII
        cmp #$60        ;input: Accu, output: Accu
        bcc +           ;handle lowercase
        and #$1f
+       rts

hexdisp pha                ;input:accu, output: nybbles in Accu and X-register
        and #$0f
        tay
        pla
div16   lsr
        lsr
        lsr
        lsr
        tax
        lda hexchar,x
        ldx hexchar,y
        rts

.enc screen
titletx	.text " SID-Wizard Sound-Effect demonstration: "
helptxt .text "Press keys 1..9 and 0 to play SFX 1..10."
usagetx	.text "Usage:Export tune with SID-Maker-SFX.prg"
usaget2	.text "call LoadAddress+$12 to initiate an SFX "
usaget3 .text "while music plays, but set these before:"
usaget4 .text "A: length, X: channel, Y: SFX-instrument"
debugtx	.text "PlayerCode DebugInfo C1 C2 C3 ADDRESS..."
tempotx	.null "Tempo-counters:"
ordlitx	.null "Orderlist-positions:"
pattntx	.null "Current patterns:"
notestx	.null "Current notes/nFX:"
insfxtx	.null "Current instr/iFX:"
instrtx	.null "Current instruments:"
effectx	.null "Current Effects:"
hexchar	.text "0123456789ABCDEF"
.enc none

;-----------------------------------
*=PLAYERADDR
.binary "pimpnotersfx.prg",2
