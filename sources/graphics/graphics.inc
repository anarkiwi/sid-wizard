;ident:8,24
;=========== ALL GRAPHICAL DATA IS HERE (charset,Video-RAM,Colour-RAM, sprites)========
; $Id: graphics.inc 378 2014-04-22 19:42:01Z hermitsoft $
        *= $d000
        .binary "sw-charset.prg",2;loading the charset, 2 byte loadaddress taken out


;===================================================================================================================
octachar .byte $5c        ;right chars of note-columns
         .byte $a0,$b0,$a0,$b0,$a0,$a0,$b0,$a0,$b0,$a0,$b0,$a0
         .byte $a1,$b1,$a1,$b1,$a1,$a1,$b1,$a1,$b1,$a1,$b1,$a1
         .byte $a2,$b2,$a2,$b2,$a2,$a2,$b2,$a2,$b2,$a2,$b2,$a2
         .byte $a3,$b3,$a3,$b3,$a3,$a3,$b3,$a3,$b3,$a3,$b3,$a3
         .byte $a4,$b4,$a4,$b4,$a4,$a4,$b4,$a4,$b4,$a4,$b4,$a4
         .byte $a5,$b5,$a5,$b5,$a5,$a5,$b5,$a5,$b5,$a5,$b5,$a5
         .byte $a6,$b6,$a6,$b6,$a6,$a6,$b6,$a6,$b6,$a6,$b6,$a6
         .byte $a7,$b7,$a7,$b7,$a7,$a7,$b7,$a7,$b7,$a7,$b7
         .byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f;vibrato-amplitude (0..f)
         .byte $00,$00,$00,$00,$00,$00,$00,$00;compressed zero-rows - only in packed tune
         .byte $ba        ;autoportamento signal ($78)
         .byte $ad        ;syncbit on signal ($79)
         .byte $ae        ;syncbit off signal ($7a)
         .byte $ad        ;ringbit on signal ($7b)
         .byte $ae        ;ringbit off signal ($7c)
         .byte $af        ;gateon signal ($7d)
         .byte $60        ;gateoff signal ($7e)
         .byte $bc        ;pattern-end signal

notechar .byte $5b        ;left chars of note-column
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$89,$8b
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$89,$8b
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$89,$8b
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$89,$8b
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$89,$8b
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$89,$8b
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$89,$8b
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$89 ;A is the same for normal & sharp, $8B is occupied by 'Vel'
        .byte $8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f,$8f;vibrato-signal ($60..$6f)
        .byte $00,$00,$00,$00,$00,$00,$00,$00;compressed zero-rows - only in packed tune ($70..$77)
        .byte $aa        ;autoportamento signal ($78)
        .byte $ab        ;syncbit on signal ($79)
        .byte $ab        ;syncbit off signal ($7a)
        .byte $ac        ;ringbit on signal ($7b)
        .byte $ac        ;ringbit off signal ($7c)
        .byte $af        ;gateon signal ($7d)
        .byte $5f        ;gateoff signal ($7e)
        .byte $bb        ;pattern-end signal


;-----------------------------------------------------------------------------------------------------------
        *= $d900        ;extra RAM under IO used for non-graphic, or grahpic-source content

cursprite
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte %11111111,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %11111111,%10000000,%00000000


        *= cursprite+$40        ;upper border sprites - values displayed here by subroutine 'upbordisp'

updispr
.if (SID_AMOUNT==1)
        .binary "sw-header.prg",2 ;sprite-header with the logo in the upper-border
.elsif (SID_AMOUNT==2)
        .binary "sw-2SID-header.prg",2
.elsif (SID_AMOUNT==3)
        .binary "sw-3SID-header.prg",2
.fi

        *= cursprite+$200        ;cursor-sprite in orderlist
olcursp .byte %11111111,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %10000000,%10000000,%00000000
        .byte %11111111,%10000000,%00000000
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0,0,0, 0,0,0,0,0,0


seqchar                	;characters for orderlist patternvalues' display
;normal sprite-charset without idle-ghostpixels

        .byte %11101110 ;0
        .byte %10101010 ;0
        .byte %10101010 ;0
        .byte %10101010 ;0
        .byte %11101110 ;0

        .byte %01000100 ;1
        .byte %11001100 ;1
        .byte %01000100 ;1
        .byte %01000100 ;1
        .byte %11101110 ;1

        .byte %11101110 ;2
        .byte %00100010 ;2
        .byte %01100110 ;2
        .byte %10001000 ;2
        .byte %11101110 ;2

        .byte %11101110 ;3
        .byte %00100010 ;3
        .byte %01100110 ;3
        .byte %00100010 ;3
        .byte %11001100 ;3

        .byte %01000100 ;4
        .byte %10001000 ;4
        .byte %11101110 ;4
        .byte %00100010 ;4
        .byte %00100010 ;4

        .byte %11101110 ;5
        .byte %10001000 ;5
        .byte %11101110 ;5
        .byte %00100010 ;5
        .byte %11001100 ;5

        .byte %01100110 ;6
        .byte %10001000 ;6
        .byte %11001100 ;6
        .byte %10101010 ;6
        .byte %11001100 ;6

        .byte %11101110 ;7
        .byte %00100010 ;7
        .byte %01000100 ;7
        .byte %01000100 ;7
        .byte %01000100 ;7

        .byte %11101110 ;8
        .byte %10101010 ;8
        .byte %11101110 ;8
        .byte %10101010 ;8
        .byte %11101110 ;8

        .byte %11101110 ;9
        .byte %10101010 ;9
        .byte %11101110 ;9
        .byte %00100010 ;9
        .byte %11001100 ;9

        .byte %11101110 ;a
        .byte %10101010 ;a
        .byte %11101110 ;a
        .byte %10101010 ;a
        .byte %10101010 ;a

        .byte %11001100 ;b
        .byte %10101010 ;b
        .byte %11001100 ;b
        .byte %10101010 ;b
        .byte %11001100 ;b

        .byte %01100110 ;c
        .byte %10001000 ;c
        .byte %10001000 ;c
        .byte %10001000 ;c
        .byte %01100110 ;c

        .byte %11001100 ;d
        .byte %10101010 ;d
        .byte %10101010 ;d
        .byte %10101010 ;d
        .byte %11001100 ;d

        .byte %01100110 ;e
        .byte %10001000 ;e
        .byte %11001100 ;e
        .byte %10001000 ;e
        .byte %11101110 ;e

        .byte %11101110 ;f
        .byte %10001000 ;f
        .byte %11001100 ;f
        .byte %10001000 ;f
        .byte %10001000 ;f

        .byte %00000000 ;..
        .byte %00000000 ;..
        .byte %00100100 ;..
        .byte %00000000 ;..
        .byte %00000000 ;..

        .byte %00000000 ;* ;Orderlist NOP separator
        .byte %00000000 ;*
        .byte %11111111 ;*
        .byte %00000000 ;*
        .byte %00000000 ;*


;ghostpixel in settings.cfg masks the low-resolution (X-extended) lower border sprites with $3FFF idle graphics
.if ( GHOSTPIXEL_ON && GHOSTPIXEL_TYPE==1 && PTNBOUND==256 && maxptnlen<250 && maxsubtuneamount<18 ) ;sprite-charset adjusted to idle-ghostpixel trick
;type 1. hermit's ghostpixel-characters, better separation of character-pairs
seqchr2 .byte %01111110 ;0
        .byte %01011010 ;0
        .byte %01011010 ;0
        .byte %01011010 ;0
        .byte %01111110 ;0

        .byte %00100100 ;1
        .byte %01101100 ;1
        .byte %00100100 ;1
        .byte %00100100 ;1
        .byte %01111110 ;1

        .byte %01111110 ;2
        .byte %00010010 ;2
        .byte %00110110 ;2
        .byte %01001100 ;2
        .byte %01111110 ;2

        .byte %01111110 ;3
        .byte %00010010 ;3
        .byte %00110110 ;3
        .byte %00010010 ;3
        .byte %01101110 ;3

        .byte %00100010 ;4
        .byte %01000100 ;4
        .byte %01111110 ;4
        .byte %00010010 ;4
        .byte %00010010 ;4

        .byte %01111110 ;5
        .byte %01001000 ;5
        .byte %01111110 ;5
        .byte %00010010 ;5
        .byte %01101110 ;5

        .byte %00111110 ;6
        .byte %01001000 ;6
        .byte %01101110 ;6
        .byte %01011010 ;6
        .byte %01101110 ;6

        .byte %01111110 ;7
        .byte %00010010 ;7
        .byte %00100010 ;7
        .byte %00100010 ;7
        .byte %00100010 ;7

        .byte %01111110 ;8
        .byte %01011010 ;8
        .byte %01111110 ;8
        .byte %01011010 ;8
        .byte %01111110 ;8

        .byte %01111110 ;9
        .byte %01011010 ;9
        .byte %01111110 ;9
        .byte %00010010 ;9
        .byte %01101110 ;9

        .byte %01111110 ;a
        .byte %01011010 ;a
        .byte %01111110 ;a
        .byte %01011010 ;a
        .byte %01011010 ;a

        .byte %01101100 ;b
        .byte %01011010 ;b
        .byte %01101100 ;b
        .byte %01011010 ;b
        .byte %01101100 ;b

        .byte %00110110 ;c
        .byte %01001000 ;c
        .byte %01001000 ;c
        .byte %01001000 ;c
        .byte %00110110 ;c

        .byte %01101100 ;d
        .byte %01011010 ;d
        .byte %01011010 ;d
        .byte %01011010 ;d
        .byte %01101100 ;d

        .byte %01111110 ;e
        .byte %01001000 ;e
        .byte %01101100 ;e
        .byte %01001000 ;e
        .byte %01111110 ;e

        .byte %01111110 ;f
        .byte %01001000 ;f
        .byte %01101100 ;f
        .byte %01001000 ;f
        .byte %01001000 ;f


.elsif ( GHOSTPIXEL_ON && GHOSTPIXEL_TYPE==2 && PTNBOUND==256 && maxptnlen<250 && maxsubtuneamount<18 ) ;sprite-charset adjusted to idle-ghostpixel trick
;type 2. Soci's ghostpixel-characters, better looking chars but small separation of character-pairs
seqchr2 .byte %01100110 ;0
        .byte %10011001 ;0
        .byte %10011001 ;0
        .byte %10011001 ;0
        .byte %01100110 ;0

        .byte %00100010 ;1
        .byte %01100110 ;1
        .byte %00100010 ;1
        .byte %00100010 ;1
        .byte %01110111	;1

        .byte %01100110 ;2
        .byte %00010001 ;2
        .byte %01100110 ;2
        .byte %10001000 ;2
        .byte %11111111 ;2

        .byte %01100110 ;3
        .byte %00010001 ;3
        .byte %00100010 ;3
        .byte %00010001 ;3
        .byte %11101110 ;3

        .byte %00100010 ;4
        .byte %01000100 ;4
        .byte %11111111 ;4
        .byte %00100010 ;4
        .byte %00100010 ;4

        .byte %11111111 ;5
        .byte %10001000 ;5
        .byte %11101110 ;5
        .byte %00010001 ;5
        .byte %01100110 ;5

        .byte %01100110 ;6
        .byte %10001000 ;6
        .byte %11101110 ;6
        .byte %10011001 ;6
        .byte %01100110 ;6

        .byte %11111111 ;7
        .byte %00010001 ;7
        .byte %00100010 ;7
        .byte %01000100 ;7
        .byte %01000100 ;7

        .byte %01100110 ;8
        .byte %10011001 ;8
        .byte %01100110 ;8
        .byte %10011001 ;8
        .byte %01100110 ;8

        .byte %01100110 ;9
        .byte %10011001 ;9
        .byte %01110111 ;9
        .byte %00010001 ;9
        .byte %01100110 ;9

        .byte %01100110 ;a
        .byte %10011001 ;a
        .byte %11111111 ;a
        .byte %10011001 ;a
        .byte %10011001 ;a

        .byte %11101110 ;b
        .byte %10011001 ;b
        .byte %11101110 ;b
        .byte %10011001 ;b
        .byte %11101110 ;b

        .byte %01110111 ;c
        .byte %10001000 ;c
        .byte %10001000 ;c
        .byte %10001000 ;c
        .byte %01110111 ;c

        .byte %11101110 ;d
        .byte %10011001 ;d
        .byte %10011001 ;d
        .byte %10011001 ;d
        .byte %11101110 ;d

        .byte %11111111 ;e
        .byte %10001000 ;e
        .byte %11101110 ;e
        .byte %10001000 ;e
        .byte %11111111 ;e

        .byte %11111111 ;f
        .byte %10001000 ;f
        .byte %11101110 ;f
        .byte %10001000 ;f
        .byte %10001000 ;f
        .else
seqchr2 = seqchtb2
        .fi

;--------------------------------------------------

sprtable                ;precalc.table for sprite-addresses of corresponding orderlist x-coordinates
        .byte <ordlspr+$00,<ordlspr+$01,<ordlspr+$02,<ordlspr+$40,<ordlspr+$41
        .byte <ordlspr+$42,<ordlspr+$80,<ordlspr+$81,<ordlspr+$82,<ordlspr+$c0
        .byte <ordlspr+$c1,<ordlspr+$c2
        .byte <ordlspr+$100,<ordlspr+$101,<ordlspr+$102,<ordlspr+$140,<ordlspr+$141
        .byte <ordlspr+$142,<ordlspr+$180,<ordlspr+$181


;===================================================================================================================
        .dsection CodeUnderIO        ;around 1k of code parts (which don't use IO area) can be sent here from other sources 
        ;.send CodeUnderIO

        .dsection DataUnderIO
        ;.send DataUnderIO

;===================================================================================================================
;video ram content
        .cerror *>$e000,"TOO MUCH CODE BEFORE VIDEO-RAM ($e000) DOESN'T FIT IN MEMORY. MOVE SOME CODE TO FREE AREAS!"
        *= $e000
        .enc 'screen'
vidram
INSPOS

stp1plc .byte $c0
trk1plc .byte $80,$A0,$00,$10,$00,$10,$00,$10,$bd
stp2plc .byte $c0
trk2plc .byte $80,$A0,$00,$10,$00,$10,$00,$10,$bd
stp3plc .byte $c0
trk3plc .byte $5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd

;*=$E01E
        .text "01INSTRUM."
        *= INSPOS+$28
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $64,$65, $00,$00,$00,$00, $66,$67, $68,$69
        *= INSPOS+$50
        .byte $c2,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c2,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c2,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $77,$78, $00,$00,$00,$00, $78,$6c
        .text "2"
        .byte $79
        *= INSPOS+$78
        .byte $c3,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c3,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c3,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $6a,$6b, $00,$00,$00,$10, $6c,$6d, $6e,$70
        *= INSPOS+$a0
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $73,$74, $00,$10,$00,$10, $75,$76
        .text "+1"
        *= INSPOS+$c8
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .text "WFARPDT"
        .byte $7a,$7b,$c1

        *= INSPOS+$f0
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
wfplace .byte $c1,$00,$10,$00,$90,$00,$10
chplace .byte $c1,$00,$90
        *= INSPOS+$118
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c2,$00,$10,$00,$90,$00,$10,$c2,$00,$90
        *= INSPOS+$140
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c3,$00,$10,$00,$90,$00,$10,$c3,$00,$90
        *= INSPOS+$168
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c4,$00,$10,$00,$90,$00,$10,$c4,$00,$90
        *= INSPOS+$190
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c5,$00,$10,$00,$90,$00,$10,$c5,$00,$90
        *= INSPOS+$1b8
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c6,$00,$10,$00,$90,$00,$10,$c6,$00,$90
        *= INSPOS+$1e0
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c7,$00,$10,$00,$90,$00,$10,$c7,$00,$90
        *= INSPOS+$208
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c8,$00,$10,$00,$90,$00,$10,$c8,$00,$90


        *= INSPOS+$230
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .text "PULSEKT"
        .byte $c9,$00,$90


        *= INSPOS+$258
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
pwplace .byte $c1,$00,$10,$00,$90,$00,$10,$ca,$00,$90
        *= INSPOS+$280
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c2,$00,$10,$00,$90,$00,$10,$cb,$00,$90
        *= INSPOS+$2a8
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c3,$00,$10,$00,$90,$00,$10,$cc,$00,$90
        *= INSPOS+$2d0
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c4,$00,$10,$00,$90,$00,$10,$cd,$00,$90

        *= INSPOS+$2f8
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .text "FILT.KT"
        .byte $7c,$7d,$c1

        *= INSPOS+$320
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
ctplace .byte $c1,$00,$10,$00,$90,$00,$10
TMPLACE .byte $c1,$00,$90
        *= INSPOS+$348
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c2,$00,$10,$00,$90,$00,$10,$c2,$00,$90
        *= INSPOS+$370
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c3,$00,$10,$00,$90,$00,$10,$c3,$00,$90
        *= INSPOS+$398
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $c4,$00,$10,$00,$90,$00,$10,$c4,$00,$90

        *= INSPOS+$3c0
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$80,$A0,$00,$10,$00,$10,$00,$10,$bd
        .byte $c1,$5D,$5C,$5D,$5C,$5D,$5C,$5D,$5C,$bd
        .byte $8c,$8d,$8e
stplace .byte $c1
        .text "-"
        .byte $7c,$7d,$c8,$c4,$7e
        .enc 'none'

        *= INSPOS+$3f8	;sprite-pointers
        .byte <(cursprite / $40) ;divided by spritesize, 256 sprites in a bank
        .byte <(updispr / $40 + range(7))

;===================================================================================================================================
; vim: sw=4 ts=4 syntax=asm:
