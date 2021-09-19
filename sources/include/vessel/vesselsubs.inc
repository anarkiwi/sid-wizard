; vesselsubs.inc 2021 m64 
.dsection VESSEL
.section VESSEL
VESSELSTART

VMODE_OFF           .var 0
VMODE_EXT_MIDI_SYNC .var 1
VMODE_NMI_SYNC      .var 2

VESSELMODE      .byte 0         ; 0 = off 
                                ; 1 = vessel sync playing mode 
                                ; 2 = vessel NMI sync

VESSEL_OVERLAY_PTN .byte 0

DISP_SLOW_SKIP_SPEED = 1        ; every X loops: ptndisp (+everything slow) 
                                ; will be called,
                                ; else skipped 

DISP_SLOW_SKIP_CNT .byte DISP_SLOW_SKIP_SPEED


; main loop 

vessel  lda MIDIdev     ; by checking for MIDI dev present, no further need to check VESSELMODE
        bne +
        jmp goback

+       lda NMI_MIDI_STOP
        beq + ; continue, if no STOP
        jmp VESSEL_NMISTOP
        
+       lda playadapter.playmod
        bne _playing

_not_playing
        lda VESSELMODE
        cmp #VMODE_NMI_SYNC
        bne goback
        jmp _vnmi_play

; PLAYING HERE        
_playing lda VESSELMODE
        cmp #VMODE_EXT_MIDI_SYNC
        beq wait4play           ; ext sync will play 

        cmp #VMODE_NMI_SYNC
        bne goback
        
_vnmi_play        
        dec DISP_SLOW_SKIP_CNT
        bne +

        jsr keyhandler.keyer
        
        lda #1
        sta display.syncscr+1

        lda #DISP_SLOW_SKIP_SPEED
        sta DISP_SLOW_SKIP_CNT

+       ; jsr ptndisp
        jsr display  
        jsr vessel_overlay_disp ; switched to inst window
        jsr playadapter.jamplay
        jsr cursord     ;time:3 rasterlines; put cursor to coordinate & flash colour
        jsr display2
        jsr playadapter.dispScope

        jmp vessel      ; NMI will play

wait4play lda #1
        beq vessel_read_midi   ; first run toggle

        lda #0
        sta wait4play+1 ; 1st run: wait4play: wait for incoming MIDI sync messages

        ; disable raster irq    
        lda $d01a
        and #$fe
        sta $d01a

        jsr vesseldisp

vessel_read_midi  
        jsr MIDIC64.GetData.vesselReadMIDI
        cpy #MIDI.StopSeqPlay
        beq midistop
        cpy #MIDI.TimingClock
        bne vessel_read_midi

do_sync jsr playadapter.playsubb        
        
        jsr vesseldisp
        jsr keyhandler.keyer
        jmp vessel

        ; stop all else and go back
midistop lda #1
        sta wait4play+1

        ; re init vessel disp
        lda #1
        sta vesseldisp+1

        ; in case of MIDI stop
        lda #0
        sta playadapter.playmod

        ; re-enable raster irq
        lda $d01a
        ora #$1
        sta $d01a
        sei
        lda #$35
        sta banksel 
        jmp menu.retoedi 

goback  jsr display     ;considers menu-display over patterns (based on 'menumode' variable) 
        ; jsr vessel_statusdisp
        lda menu.menupoint ;check selected menupoint (value is 0 if no menupoint was launched)
        beq +           ;if no menu was launched, main loop continues
        jmp menu.runmenup
+       jmp mainloop

; display code 

vesseldisp lda #1 ; selfmod 1st chk
        beq notfirst 
        lda #0 
        sta vesseldisp+1
        sta $d020
        sta $d015
         
        ; clrscr
        ldy #0
-       lda #$20        
        sta $0400,y
        sta $0500,y
        sta $0600,y
        sta $0700,y 
        lda #0
        sta $d800,y
        sta $d900,y
        sta $da00,y
        sta $db00,y
        iny
        bne -
    
        ; colors for numbers  
        lda #$0b ; SEQPOS
        sta $d800+40*(3)+5+0
        sta $d800+40*(3)+5+1

        lda #$0e ; PTNPOS
        sta $d800+40*(5+9)+30+0
        sta $d800+40*(5+9)+30+1

        ; PTN 1-3
        lda #1
        sta $d800+40*(5+9)+4+0+1
        sta $d800+40*(5+9)+4+1+1

        sta $d800+40*(5+9)+7+0+1
        sta $d800+40*(5+9)+7+1+1

        sta $d800+40*(5+9)+10+0+1
        sta $d800+40*(5+9)+10+1+1

        ; INS 1-3
        lda #1
        sta $d800+40*(5+11)+4+0+1
        sta $d800+40*(5+11)+4+1+1

        sta $d800+40*(5+11)+7+0+1
        sta $d800+40*(5+11)+7+1+1

        sta $d800+40*(5+11)+10+0+1
        sta $d800+40*(5+11)+10+1+1

notfirst ldx #40
-       lda vessellogo,x
        cmp #$20
        beq +
        sta vidram+40*5-1,x
        inc $d800+40*5-1,x        
+       dex
        bne -

        ; disp positions
part3   lda player.SEQPOS
        and #$f0
        lsr
        lsr
        lsr
        lsr 
        sta vidram+40*(3)+5+0
        lda player.SEQPOS
        and #$0f
        sta vidram+40*(3)+5+1

        lda player.PTNPOS
        and #$f0
        lsr
        lsr
        lsr
        lsr
        sta vidram+40*(5+9)+30+0

        lda player.PTNPOS
        and #$0f
        sta vidram+40*(5+9)+30+1

        ; disp 3x pat nr
part4   lda player.CURPTN+0
        and #$f0
        lsr
        lsr
        lsr
        lsr 
        sta vidram+40*(5+9)+4+0+1
        lda player.CURPTN+0
        and #$0f
        sta vidram+40*(5+9)+4+1+1

        lda player.CURPTN+7
        and #$f0
        lsr
        lsr
        lsr
        lsr 
        sta vidram+40*(5+9)+7+0+1
        lda player.CURPTN+7
        and #$0f
        sta vidram+40*(5+9)+7+1+1

        lda player.CURPTN+14
        and #$f0
        lsr
        lsr
        lsr
        lsr 
        sta vidram+40*(5+9)+10+0+1
        lda player.CURPTN+14
        and #$0f
        sta vidram+40*(5+9)+10+1+1

; disp 3x ins
part5   lda player.CURINS+0
        and #$f0
        lsr
        lsr
        lsr
        lsr 
        sta vidram+40*(5+11)+4+0+1
        lda player.CURINS+0
        and #$0f
        sta vidram+40*(5+11)+4+1+1

        lda player.CURINS+7
        and #$f0
        lsr
        lsr
        lsr
        lsr 
        sta vidram+40*(5+11)+7+0+1
        lda player.CURINS+7
        and #$0f
        sta vidram+40*(5+11)+7+1+1

        lda player.CURINS+14
        and #$f0
        lsr
        lsr
        lsr
        lsr 
        sta vidram+40*(5+11)+10+0+1
        lda player.CURINS+14
        and #$0f
        sta vidram+40*(5+11)+10+1+1

; disp 3x gate
part6   lda mutesolo
        and #1
        bne +
        lda #$20
        ldx #15
        jmp stap1

+       lda player.WFGHOST+0
        and #1
        beq +
        
        lda #$20
        ldx #$a
        jmp stap1
           
+       lda #0
        ldx #0
stap1   sta vidram+40*(5+10)+4+0+1
        sta vidram+40*(5+10)+4+1+1
        txa 
        sta $d800+40*(5+10)+4+0+1
        sta $d800+40*(5+10)+4+1+1

        lda mutesolo+1
        and #1
        bne +
        lda #$20
        ldx #15
        jmp stap2

+       lda player.WFGHOST+7
        and #1
        beq +
        lda #$20
        ldx #$a
        jmp stap2
           
+       lda #0
        ldx #0
stap2   sta vidram+40*(5+10)+7+0+1
        sta vidram+40*(5+10)+7+1+1
        txa 
        sta $d800+40*(5+10)+7+0+1
        sta $d800+40*(5+10)+7+1+1

        lda mutesolo+2
        and #1
        bne +
        lda #$20
        ldx #15
        jmp stap3

+       lda player.WFGHOST+14
        and #1
        beq +
        lda #$20
        ldx #$a
        jmp stap3
           
+       lda #0
        ldx #0
stap3   sta vidram+40*(5+10)+10+0+1
        sta vidram+40*(5+10)+10+1+1
        txa 
        sta $d800+40*(5+10)+10+0+1
        sta $d800+40*(5+10)+10+1+1
        
        lda #4
        sta $d020
        rts

; used as overlay in main loop
vessel_statusdisp
        lda VESSELMODE
        sta vidram+29
        lda playadapter.playmod
        sta vidram+29+40
        inc vidram+29+40+40+40
        rts

.enc 'screen'
PT .text "PT"      
.enc 'none'



vessel_overlay_disp
        lda VESSEL_OVERLAY_PTN
        bne +
        rts

+       jsr do_v_scrn 
        jsr v_orderlist_disp

        rts

do_v_scrn
        lda player.CURPTN+0
        ldx #<(vidram+30+40*8+4)
        ldy #>(vidram+30+40*8+4)
        jsr print_hex_fat

        lda player.CURPTN+7
        ldx #<(vidram+30+40*8+4+2)
        ldy #>(vidram+30+40*8+4+2)
        jsr print_hex_fat

        lda player.CURPTN+14
        ldx #<(vidram+30+40*8+4+4)
        ldy #>(vidram+30+40*8+4+4)
        jsr print_hex_fat

        
        
        lda player.CURINS+0
        ldx #<(vidram+30+40*6+4)
        ldy #>(vidram+30+40*6+4)
        jsr print_hex

        lda player.CURINS+7
        ldx #<(vidram+30+40*6+4+2)
        ldy #>(vidram+30+40*6+4+2)
        jsr print_hex

        lda player.CURINS+14
        ldx #<(vidram+30+40*6+4+4)
        ldy #>(vidram+30+40*6+4+4)
        jsr print_hex



        lda player.WFGHOST
        ldx #<(vidram+30+40*5+4)
        ldy #>(vidram+30+40*5+4)
        jsr print_hex

        lda player.WFGHOST + 7
        ldx #<(vidram+30+40*5+4+2)
        ldy #>(vidram+30+40*5+4+2)
        jsr print_hex

        lda player.WFGHOST + 14
        ldx #<(vidram+30+40*5+4+4)
        ldy #>(vidram+30+40*5+4+4)
        jsr print_hex



        lda player.PTNPOS
        ldx #<(vidram+30+40*7+4)
        ldy #>(vidram+30+40*7+4)
        jsr print_hex

        lda player.PTNPOS + 7
        ldx #<(vidram+30+40*7+4+2)
        ldy #>(vidram+30+40*7+4+2)
        jsr print_hex

        lda player.PTNPOS + 14
        ldx #<(vidram+30+40*7+4+4)
        ldy #>(vidram+30+40*7+4+4)
        jsr print_hex

        jsr v_disp_trackrow1
        jsr v_disp_trackrow2
        jsr v_disp_trackrow3


        rts


v_orderlist_disp

        jsr v_disp_trackrow1

        rts

v_disp_trackrow1
        ; init vidramptr to write to 
        lda # <(vidram+30+40*9+4) 
        sta v_ptr+0
        lda # >(vidram+30+40*9+4) 
        sta v_ptr+1

        lda #16 - 1
        sta vdisp_rowctr        ; max 16 rows

        ldy player.SEQPOS ; ORDERLIST SEQUENCE PLAYPOSITION
        iny

-       tya
        pha

        lda SEQUENCES, y ; ORDERLIST T1
        ldx v_ptr
        ldy v_ptr+1
        jsr print_hex
        
        clc
        lda #40
        adc v_ptr+0
        bcc +
        inc v_ptr+1
+       sta v_ptr+0

        pla
        tay
        ; check for FE
        lda SEQUENCES, y
        cmp #$FE
        beq _pr_spc

+       iny 
        cpy #seqbound
        bne +

_pr_spc jsr print_spc_border
        rts

+       dec vdisp_rowctr
        bne -

        rts

v_disp_trackrow2
        ; init vidramptr to write to 
        lda # <(vidram+30+40*9+4+2) 
        sta v_ptr+0
        lda # >(vidram+30+40*9+4+2) 
        sta v_ptr+1

        lda #16 - 1
        sta vdisp_rowctr        ; max 16 rows

        ldy player.SEQPOS+7 ; ORDERLIST SEQUENCE PLAYPOSITION
        iny

-       tya
        pha

        lda SEQUENCES+seqbound, y ; ORDERLIST T1
        ldx v_ptr
        ldy v_ptr+1
        jsr print_hex
        
        clc
        lda #40
        adc v_ptr+0
        bcc +
        inc v_ptr+1
+       sta v_ptr+0

        pla
        tay
        ; check for FE
        lda SEQUENCES, y
        cmp #$FE
        beq _pr_spc

        iny 
        cpy #seqbound
        bne +
_pr_spc jsr print_spc_border
        rts

+       dec vdisp_rowctr
        bne -

        rts

v_disp_trackrow3
        ; init vidramptr to write to 
        lda # <(vidram+30+40*9+4+4) 
        sta v_ptr+0
        lda # >(vidram+30+40*9+4+4) 
        sta v_ptr+1

        lda #16 - 1
        sta vdisp_rowctr        ; max 16 rows

        ldy player.SEQPOS+14 ; ORDERLIST SEQUENCE PLAYPOSITION
        iny

-       tya
        pha

        lda SEQUENCES+2*seqbound, y ; ORDERLIST T1
        ldx v_ptr
        ldy v_ptr+1
        jsr print_hex
        
        clc
        lda #40
        adc v_ptr+0
        bcc +
        inc v_ptr+1
+       sta v_ptr+0

        pla
        tay
        ; check for FE
        lda SEQUENCES, y
        cmp #$FE
        beq _pr_spc
        iny 
        cpy #seqbound
        bne +
_pr_spc jsr print_spc_border
        rts

+       dec vdisp_rowctr
        bne -

        rts

v_ptr   .byte 00, 00

print_spc
        ldx v_ptr+0
        ldy v_ptr+1
        stx storespc1+1
        sty storespc1+2
        stx storespc2+1
        sty storespc2+2
        inc storespc2+1
        bne +
        inc storespc2+2
+       ; lda #$BD
        lda #$61
storespc1 sta vidram+30+40*6+4
        lda #$62
storespc2 sta vidram+30+40*6+4

        rts

print_spc_border
        ldx v_ptr+0
        ldy v_ptr+1
        stx storespc3+1
        sty storespc3+2
        stx storespc4+1
        sty storespc4+2
        inc storespc4+1
        bne +
        inc storespc4+2
+       ; lda #$BD
        lda #$61

storespc3 sta vidram+30+40*6+4
        ; lda #$90
        lda #$63
storespc4 sta vidram+30+40*6+4

        rts


print_hex
        stx storehi2+1
        sty storehi2+2
        stx storelo2+1
        sty storelo2+2

        inc storelo2+1
        bne +
        inc storelo2+2
+       tax
        and #$f0
        lsr
        lsr
        lsr
        lsr

storehi2 sta vidram+30+40*6+4  
        txa
        and #$0f
        ora #$90

storelo2 sta vidram+30+40*6+4+1
        rts


print_hex_fat
        stx storehi+1
        sty storehi+2
        stx storelo+1
        sty storelo+2
        inc storelo+1
        bne +
        inc storelo+2
+       tax
        and #$f0
        lsr
        lsr
        lsr
        lsr
        cmp #10
        bcc +
        clc
        adc #55
        jmp storehi

+       clc
        adc #$30

storehi sta vidram+30+40*6+4
        txa
        and #$0f
        cmp #10
        bcc +
        clc
        adc #55
        jmp storelo

+       clc
        adc #$30

storelo sta vidram+30+40*6+4+1
        rts


clr_v_scrn
        lda # <(vidram+30) 
        sta smod00+1
        lda # >(vidram+30) 
        sta smod00+2

        lda #24
        sta vdisp_rowctr

        ldx #0
-       ldy #0
-       lda v_scrn, x
smod00  sta vidram+30
        inx
        iny

        inc smod00+1
        bne +
        inc smod00+2

+       cpy #10
        bne -
        clc
        lda #30
        adc smod00+1
        bcc +
        inc smod00+2

+       sta smod00+1
        dec vdisp_rowctr
        bne --

_do_colors
        lda # <($d800 + 30) 
        sta smod01+1
        lda # >($d800 + 30) 
        sta smod01+2

        lda #24
        sta vdisp_rowctr

        ldx #0
-       ldy #0
-       lda v_colors, x
smod01  sta $d800 + 30
        inx
        iny

        inc smod01+1
        bne +
        inc smod01+2

+       cpy #10
        bne -
        clc
        lda #30
        adc smod01+1
        bcc +
        inc smod01+2

+       sta smod01+1
        dec vdisp_rowctr
        bne --

        rts
vdisp_rowctr .byte 00

v_scrn_save
        ; init pointers
        lda # <(vidram+30) 
        sta lda_vid+1
        lda # >(vidram+30) 
        sta lda_vid+2

        lda # <($d800+30) 
        sta lda_color+1
        lda # >($d800+30) 
        sta lda_color+2

        ; copy screen
        lda #24
        sta vdisp_rowctr

        ldx #0
-       ldy #0
lda_vid lda vidram+30
        sta v_scrn_bak, x
        inx
        iny

        inc lda_vid+1
        bne +
        inc lda_vid+2

+       cpy #10
        bne lda_vid
        clc
        lda #30
        adc lda_vid+1
        bcc +
        inc lda_vid+2

+       sta lda_vid+1
        dec vdisp_rowctr
        bne -
        
        ; copy colors
        lda #24
        sta vdisp_rowctr

        ldx #0
-       ldy #0
lda_color lda $d800+30
        sta v_colors_bak, x
        inx
        iny

        inc lda_color+1
        bne +
        inc lda_color+2

+       cpy #10
        bne lda_color
        clc
        lda #30
        adc lda_color+1
        bcc +
        inc lda_color+2

+       sta lda_color+1
        dec vdisp_rowctr
        bne -
        rts

v_scrn_restore
        ; init pointers
        lda # <(vidram+30) 
        sta sta_vid+1
        lda # >(vidram+30) 
        sta sta_vid+2

        lda # <($d800+30) 
        sta sta_color+1
        lda # >($d800+30) 
        sta sta_color+2

        ; copy colors
        lda #24
        sta vdisp_rowctr

        ldx #0
-       ldy #0
-       lda v_colors_bak, x
sta_color sta $d800+30
        inx
        iny

        inc sta_color+1
        bne +
        inc sta_color+2

+       cpy #10
        bne -
        clc
        lda #30
        adc sta_color+1
        bcc +
        inc sta_color+2

+       sta sta_color+1
        dec vdisp_rowctr
        bne --
        
        ; copy screen
        lda #24
        sta vdisp_rowctr

        ldx #0
-       ldy #0
-       lda v_scrn_bak, x
sta_vid sta vidram+30
        inx
        iny

        inc sta_vid+1
        bne +
        inc sta_vid+2

+       cpy #10
        bne -
        clc
        lda #30
        adc sta_vid+1
        bcc +
        inc sta_vid+2

+       sta sta_vid+1
        dec vdisp_rowctr
        bne --

        rts

v_scrn
        .enc 'screen'
        
        .text "VESSEL NMI"
        .text "          "
        .text "          "
        .text "          "
         
v_scrn_osc_text
        .text "OSC 010203"
        .text " WF 404189"
        .text "INS 010204"
        .text "POS 101010"
        .text "PTN 102030"

        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63
        .byte $20,$00,$10,$00,$61,$63,$61,$63,$61,$63        
        ; .byte $20,$00,$10,$00,$bd,$bd,$bd,$bd,$bd,$90
        ;.byte $20,$00,$10,$00,$00,$10,$00,$10,$00,$90
        .enc 'none'

v_colors
        .byte $01,$01,$01,$01,$01,$01,$0f,$0f,$0f,$0f

        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        
        .byte $0b,$0b,$0b,$0b,$0b,$0b,$0c,$0c,$0f,$0f ; OSC
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f ;  WF
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f ; INS
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f ; POS
        .byte $04,$04,$04,$04,$04,$04,$06,$06,$0e,$0e ; PTN

        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f
        .byte $00,$00,$00,$00,$0b,$0b,$0c,$0c,$0f,$0f

v_scrn_bak
        .byte $01,$01,$01,$01,$01,$01,$0b,$0b,$0b,$0b
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f 
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f 
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f 
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f 
        .byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04 
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0c,$0c,$0c,$0c,$0c,$0c
        .byte $00,$00,$00,$00,$0b,$0b,$0b,$0b,$0b,$0b
v_colors_bak
        .byte $01,$01,$01,$01,$01,$01,$0b,$0b,$0b,$0b
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f 
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f 
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f 
        .byte $0b,$0b,$0b,$00,$0b,$0b,$0c,$0c,$0f,$0f 
        .byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04 
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0f,$0f,$0f,$0f,$0f,$0f
        .byte $00,$00,$00,$00,$0c,$0c,$0c,$0c,$0c,$0c
        .byte $00,$00,$00,$00,$0b,$0b,$0b,$0b,$0b,$0b

VESSELEND

.include "vesselnmi.inc"
.include "vesselsubmenu.inc"

.send VESSEL
.section data
    .enc 'screen'
    vessellogo .text "             <-VESSEL/SYNC=>             "
.send data

.warn format(" vessel TMP WARNING IGNORE: Vesselsubs end: $%04x ($%04x remaining)", *, $D000 - *)