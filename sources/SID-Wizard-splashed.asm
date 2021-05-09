;ident:8,24
;===============================================
;add SPLASH-SCREEN for SID-Wizard decompression
;-----------------------------------------------
; $Id: SID-Wizard-splashed.asm 382 2014-06-23 17:53:01Z hermitsoft $

;used zeropage-addresses:
banksel=$01             ;C64 bank-selection register at zeropage
sourcePtr=$02           ;zeropage-pointer used as source-pointer
destinPtr=$04           ;zeropage-pointer used as destination-pointer
zptemp=$06              ;temporary zeropage-storage
zptemp2=$07
videoPtr=$08            ;videoRAM pointer

;graphical memory locations:
VideoRAM = $0400
;OverlaySp = $5a00      ;target (displayed) address of overlay-sprites
BitmapCol = $8c00       ;target (displayed) address of bitmap-colours (videoRAM)
Bitmap = $a000          ;target (displayed) address of bitmap including splash-logo

SplashBuffer=$C000
ColMapBuffer=$CF00      ;store colours that might be overwritten
ROMRTI = $ea86          ;the RTI command at the end of $EA31 IRQ routine, 
                        ;used as 'dummyirq' in case of banksel=$36/$37 (KERNAL-ROM turned on)
dummirq= $fff8          ;the same, without ROM

;splash-screen positions and dimensions
screenWidth=40
screenHeight=25
splashWidth=16
splashHeight=15
splashRow=(screenHeight-splashHeight)/2 ;5
splashColu=(screenWidth-splashWidth)/2 ;12

fadestart=16+2          ;where fade starts
fadeend=30              ;peak brightness

.weak
 KERNAL_LOADSAVE=0
.endweak

;-------------------------------------------------------------------------------
        *= $0801

.if (SID_AMOUNT==1)
 .if (KERNAL_LOADSAVE==0)
        .binary "../application/SID-Wizard-packed.prg",2
 .else
        .binary "../application/SID-Wizard-packed-KERNAL.prg",2
 .fi
.elsif (SID_AMOUNT==2)
        .binary "../application/SID-Wizard-2SID-packed.prg",2
.elsif (SID_AMOUNT>=3)
        .binary "../application/SID-Wizard-3SID-packed.prg",2
.fi
endpacked

;-------------------------------------------------------------------------------
;splash-screen displayer (after the included 'SID-Wizard-packed.prg' binary)

SPLASHER lda #$A0
         sta $080d      ;restore exomizer launcher's destructed byte
;--------------------------
        sei
        lda #$35
        sta banksel
        ;NMI (RESTORE key) pointer setting - so pressing RESTORE won't freeze depacking/startup
        lda irqcode
        sta dummirq ;store an RTI to a safe place
        lda #<dummirq
        sta $fffa
        lda #>dummirq
        sta $fffb
        lda #<ROMRTI
        sta $0318
        lda #>ROMRTI
        sta $0319
        lda #$7f
        sta $d019       ;clear possible raster-interrupt-sign flag
        sta $dc0d       ;disable CIA1 interrupts
        sta $dd0d       ;disable CIA2 interrupts
        lda $dc0d       ;clear CIA1 possible IRQ-sign flags
        lda $dd0d       ;clear CIA2 possible IRQ-sign flags
        jmp StorCol
irqcode rti

;-------------- copy splash colour data to a buffer ----------------------------
StorCol ldy #0
-       lda SplashColorMap,y
        sta ColMapBuffer,y
        iny
        bne -

        lda #<SplashBitmap
        ldx #>SplashBitmap
        jsr setSrcPt
        lda #<SplashBuffer
        ldx #>SplashBuffer
        jsr setDesPt
        ldx #>(SplashBitmapEnd-SplashBitmap)
        ldy #0
-       lda (sourcePtr),y
        sta (destinPtr),y
        iny
        bne -
        inc sourcePtr+1
        inc destinPtr+1
        dex
        bpl -

;-------------------------------------------------------------------------------
;Convert VIDEORAM-characters to bitmap ($0400..$0800 --> $6000..$8000)
        lda $d018
        and #$0e
        asl
        asl
        ora #$c0
        sta CASER+1     ;character-ROM address
        lda $d018
        lsr
        lsr
        and #$3c
        sta VMEM+2      ;videoram-hi pointer

CHTOBMP lda #$33        ;switch to Char-RAM
        sta banksel
        lda #<Bitmap
        ldx #>Bitmap    ;base address (hi) of the targeted bitmap
        jsr setDesPt

        ldx #0
LP0     lda #0
        sta sourcePtr+0
        sta sourcePtr+1
VMEM    lda VideoRAM,x  ;self-written  - get the next character videoram
        asl             ;and multiply by 8 to get character's address
        rol sourcePtr+1
        asl 
        rol sourcePtr+1
        asl 
        rol sourcePtr+1
        sta sourcePtr+0
CASER   lda #0          ;self-written - base address of character-ROM
        clc
        adc sourcePtr+1
        sta sourcePtr+1

        ldy #7
-       lda (sourcePtr),y ;copy character from character ROM
        sta (destinPtr),y ;paste character to bitmap
        dey
        bpl -

        lda #8          ;increase bitmap-pointer by 8 (one character in BMP mode)
        jsr addDesPt
        inx             ;increase VideoRAM pointer-lo
        bne LP0
        inc VMEM+2      ;increase VideoRAM pointer-hi
        lda VMEM+2
        and #$03        ;and check, if all VideoRAM has been taken over
        bne LP0         ;if not, just continue

        lda #$35
        sta banksel

;======================= copy splash-screen bitmap-data ========================
;splash-section
        lda #<SplashBuffer
        ldx #>SplashBuffer
        jsr setSrcPt
        lda #<(Bitmap + splashRow*40*8 + splashColu*8)
        ldx #>(Bitmap + splashRow*40*8 + splashColu*8)
        jsr setDesPt
        ldx #splashHeight
SplashL ldy #splashWidth*8-1 ;counts columns
-       lda (sourcePtr),y
        sta (destinPtr),y
        dey
        bpl -
        lda #splashWidth*8
        jsr addSrcPt
        lda #(320-256)  ;add 320-256 to 16bit destination-pointer
        jsr addDesPt
        inc destinPtr+1 ;add 256 to 15bit destination pointer
        dex             ;counts rows
        bne SplashL


;-------------------------------------------------------------------------------
;set the colour of the bitmap (VIDEORAM HI-NYBBLEs) to the selected bordercolour
        lda $D021
        and #$0F
        sta zptemp
        ldx #4
        ldy #0
LP3     lda $D800,y
        asl
        asl
        asl
        asl
        ora zptemp
CMEM    sta BitmapCol,y
        iny
        bne LP3
        inc LP3+2
        inc CMEM+2
        dex
        bne LP3

        lda #fadestart
        sta colcnt      ;darkest brightness
        jsr FadeCol     ;display splash-screen colours

;-------------------------------------------------------------------------------
;copy overlay-sprites to their place, set their pointers, coordinates and colours
;        ldy #0
;        sty $d010
;        sty $d017
;        sty $d01b
;        sty $d01c
;        sty $d01d
;        dey 
;-       lda SplashSpriteOverlay+$100,y
;        sta OverlaySp+$100,y
;        dey
;        cpy #$ff
;        bne -
;-       lda SplashSpriteOverlay,y
;        sta OverlaySp,y
;        dey
;        cpy #$ff
;        bne -


;========================= show the results ====================================
;vblank-synchronized switching from VIDEORAM to BITMAP (prevent glitches)
        lda #$FF
        cmp $D012
        bne *-3
        lda #$3B
        sta $D011       ;set bitmap mode
        lda #((Bitmap & $3800) >> 10) | ((BitmapCol & $3c00) >> 6)
        sta $D018       ;set bitmap and video-ram pointers
        lda $dd00
        and #%11111100
        ora #(BitmapCol >> 14) ^ 3
        sta $dd00 ;set VIC-bank
;        lda #$FF
;        sta $d015       ;switch on sprites


;------------------------ colour fade-in ------------------------
        lda #$d0
        cmp $d012
        bne *-3
        ;fadestart in action for one screen
        .if (fadestart<fadeend)
-       lda #$d0
        cmp $d012
        bne *-3
        inc colcnt
        jsr FadeCol
        lda colcnt
        cmp #fadeend
        bne -
        .fi
-       lda #$d0
        cmp $d012
        bne *-3
        dec colcnt
        jsr FadeCol
        lda colcnt
        cmp #16+1       ;normal fade, no colour-transformation
        bcs -

;------------------start depacker of main app.----------------------------
        lda #$37
        sta banksel
        jmp $080d       ;go to perform decompression

ENDSPLASHER             ;sign end of splash-code


;----------------------subroutines-----------------------------------------
FadeCol ;colour-fade splash-bitmap & sprites

        ;sprite-colours
;        ldy #7
;-       tya             ;set sprite-pointers
;        clc
;        adc #(OverlaySp-$4000)/$40
;        sta BitmapCol+$3F8,y
;        lda OverlayColours,y
;        tax
;        lda ColBright,x
;        clc
;        adc colcnt
;        tax
;        lda FadeInSta,x ;BrightCol,x
;        sta $d027,y     ;set sprite-colours
;        tya
;        asl
;        tax
;        lda OverlayPositions,x ;set sprite-coordinates
;        sta $d000,x
;        lda OverlayPositions+1,x
;        sta $d001,x
;        dey
;        bpl -

        ;splash-bitmap-colours
        lda #<ColMapBuffer
        ldx #>ColMapBuffer
        jsr setSrcPt
        lda #<(BitmapCol + splashRow*40 + splashColu)
        ldx #>(BitmapCol + splashRow*40 + splashColu)
        jsr setDesPt
        lda #splashHeight-1
        sta zptemp

        lda #<FadeInSta ;prepare fade-colour base pointers
        clc
        adc colcnt
        sta fadePt1+1
        sta fadePt2+1
        lda #>FadeInSta
        adc #0 ;add carry
        sta fadePt1+2
        sta fadePt2+2

-       ldy #splashWidth-1
-       lda (sourcePtr),y
        pha
        lsr
        lsr
        lsr
        lsr
        tax
        lda ColBright,x
        tax
fadePt1 lda FadeInSta,x ;self-written code
        and #$F0
        sta zptemp2
        pla
        and #$0F        ;low nybble
        tax
        lda ColBright,x
        tax
fadePt2 lda FadeInSta,x ;self-written code
        and #$0F
        ora zptemp2
        sta (destinPtr),y
        dey
        bpl -

        lda #16
        jsr addSrcPt
        lda #40
        jsr addDesPt
        dec zptemp
        bpl --

        rts

FadeInSta .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;into black
BrightCol .byte $00,$99,$66,$BB,$22,$44,$88,$CC,$EE,$55,$AA,$FF,$33,$DD,$77,$11 ;C64 colours in order of brightness (from darkest to brightest)
FadeInEnd .byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11 ;into white
ColBright .byte 0,15,4,12,5,9,2,14,6,1,10,3,7,13,8,11 ;pace of C64 colours on brightness-scale
colcnt .byte 16         ;color-fader's colour-counter (0..15-darker, 16-normal, 17..31-brighter)


;OverlayColours
;        .byte 0,1,0,1,0,1,0,1
;OverlayPositions
;        olX1=200 ;overlay logo-part 1 coordinates
;        olY1=90
;        olX2=200 ;overlay logo-part 2 coordinates
;        olY2=187
;        .byte olX1,olY1,olX1,olY1, olX1+24,olY1,olX1+24,olY1, olX2,olY2,olX2,olY2, olX2+24,olY2,olX2+24,olY2 

;--------------------------
setSrcPt sta sourcePtr+0
         stx sourcePtr+1
         rts
setDesPt sta destinPtr+0
         stx destinPtr+1
         rts
;--------------------------
addSrcPt clc ;add 8bit value in Accu to 16bit source-pointer
         adc sourcePtr+0
         sta sourcePtr+0
         bcc +
         inc sourcePtr+1
+        rts
addDesPt clc  ;add 8bit value in Accu to 16bit destination-pointer
         adc destinPtr+0
         sta destinPtr+0
         bcc +
         inc destinPtr+1
+        rts

;--------------------------------------------------------
;.cerror *>=OverlaySp,"!!! Sprite-overlay area reached. Too much code below !!!" ;(max.$5A00)
;SplashSpriteOverlay
;        .binary "graph/sw-splash-overlay.prg",2
;--------------------------------------------------------
.cerror *>=BitmapCol,"!!! Colour-map area reached. Too much code below !!!"
SplashColorMap
;        .binary "graphics/sw-splash-hires.colmap",2
        .binary "graphics/sw-splash-hires.prg",2+$1f40+0*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+1*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+2*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+3*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+4*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+5*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+6*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+7*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+8*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+9*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+10*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+11*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+12*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+13*40,16
        .binary "graphics/sw-splash-hires.prg",2+$1f40+14*40,16
SplashColorMapEnd

;--------------------------bitmap-data-------------------
SplashBitmap ;converted from the PNG by enthusi's png2hires.py script
;        .binary "graphics/sw-splash-hires.bitmap",2
        .binary "graphics/sw-splash-hires.prg",2+0*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+1*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+2*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+3*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+4*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+5*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+6*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+7*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+8*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+9*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+10*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+11*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+12*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+13*$140,$80
        .binary "graphics/sw-splash-hires.prg",2+14*$140,$80
SplashBitmapEnd

;-------------------------------------------------------------------------------
;modify Starter to point to splash displayer
        *= $0801
        .word ss,2014
        .null $9e,^SPLASHER;Sys SPLASHER
ss      .word 0
;===============================================================================
; vim: sw=4 ts=4 syntax=asm:
