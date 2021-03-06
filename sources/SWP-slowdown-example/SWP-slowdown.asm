;-----------------------------SID BULLET TIME -----------------------------------------
;coded by Hermit in 2014 - SID slowdown routine tester (this effect is mainly for games)
;--------------------------------------------------------------------------------------
current_slowdown = 2 ;zeropage-variable

        *= $0801
        .word ss,2013
        .null $9e,^start ;Sys 2064
ss        .word 0

start   sei
        jsr initScreen ;initScreen screen, VIC, etc.

restartune lda #0 ;this way we initScreen SWP relative/relocatable tune-data
        ldx #<SWP_musicdata_address
        ldy #>SWP_musicdata_address
        jsr SWP_player_address+0 ;initialize SID and music-routine

raster  lda #$80  ;wait for raster-beam to arrive at the middle of the screen
        cmp $d012
        bne *-3

        lda current_slowdown       ;A = value set by 'keyhandler' subroutine
        jsr SWP_player_address+15  ;this sets the slowdown of the player

        inc $d020
        jsr SWP_player_address+3  ;playing the musicdata
        dec $d020

        jsr keyhandler
        jsr display
        jmp raster        ;go to wait for next PAL-frame



;-----------------------------------------------------------------------
SWP_player_address=$0900  ;player was relocated here with SID-Maker-SWP
*=SWP_player_address
.binary "SWP-player-0900.drv.prg",2 ;this was created by SID-Maker-SWP 


SWP_musicdata_address=$1789  ;can be at any address, SWP-player conforms to it
*=SWP_musicdata_address
.binary "flashitback.swp.prg",2 ;this was created by SID-Maker-SWP (from .swm)

bultim	.byte 1
bultcnt	.byte 0 ;bullet-time slowdown current_slowdown-counter
bultspd	.byte 2 ;get back tempo
bultspd2 .byte 0 ;slowdown tempo
slowdown .byte 24

keyhandler
        lda #$7f
        sta $dc00
        lda $dc01
        cmp #$ff
        beq chplus
        dec bultcnt
        bpl +
        lda bultspd2
        sta bultcnt
        lda bultim
        cmp slowdown  ;bullet-time slowdown current_slowdown
        beq +
        inc bultim
+       lda bultim
        sta current_slowdown
        rts
chplus  lda #$df
        sta $dc00
        lda $dc01
        cmp #$ff-1 ;+?
        bne chminus
        lda norepe
        bne +
        lda current_slowdown
        cmp #24
        bpl +
        inc current_slowdown
        jmp acksped
chminus cmp #$ff-8 ;-?
        bne chretu
        lda norepe
        bne +
        lda current_slowdown
        cmp #1
        bmi +
        dec current_slowdown
acksped lda current_slowdown
        sta slowdown
        inc norepe
+       rts
chretu  lda #$fe
        sta $dc00
        lda $dc01
        cmp #$ff-02
        bne keyretu
        jmp restartune
keyretu lda #0
        sta norepe
        dec bultcnt
        bpl +
        lda bultspd
        sta bultcnt
        lda bultim
        cmp #0
        beq +
        dec bultim
        lda bultim
        sta current_slowdown
+        rts

norepe	.byte 0




;-------------------------- misc. subroutines ----------------------------------
bank=1 ;bank-selection register
SCINIT = $ff81 ;Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer. Used registers: A, X, Y. (Real address: $FF5B.)
video=$0400

initScreen
        lda #3
        sta $dd00
        lda #$36
        sta bank
        jsr SCINIT
        lda #$16
        sta $d018
        lda #0
        sta $d020
        sta $d021
        rts

displaytexts
        ldy #39
-       lda text1,y
        sta video,y
        lda text1b,y
        sta video+40*1,y
        lda text2,y
        sta video+40*5,y
        lda text3,y
        sta video+40*3,y
        lda text4,y
        sta video+40*7,y
        dey
        bpl -
        rts

decimdi ldy #$2f        ;input:A, output: decimal digits in Y and A
        sec
-       iny
        sbc #10
        bcs -
        adc #$3a
        rts

display	jsr displaytexts 
        lda current_slowdown
        jsr decimdi
        sty video+40*3+7
        sta video+40*3+8
        lda slowdown
        jsr decimdi
        sty video+40*3+37
        sta video+40*3+38
        rts
icnt    .byte 0

.enc screen
text1   .text"PRESS + TO INCREASE, - TO DECR. SLOWDOWN "
text1b  .text"(SETS BULLET-TIME SLOWDOWN TARGET TOO.) "
text2   .text"PRESS AND HOLD SPACE FOR BULLET-TIME FX."
text3   .text"SLOWDN:0  BULLETTIME TARGETSLOWDOWN: 0  "
text4 .text  "    PRESS RETURN TO RESTART THE TUNE     "
.enc none
hexdisp pha             ;input:accu, output: nybbles in Accu and X-register
        and #$0f
        tay
        ldx hexchar,y
        pla
div16   lsr
        lsr
        lsr
        lsr
        tay
        lda hexchar,y
        rts

        .enc screen
hexchar .text "0123456789ABCDEF"
        .enc none

;-------------------------------------------------------------------------------