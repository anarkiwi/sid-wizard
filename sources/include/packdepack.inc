;ident:8,24
;==============================================================================================================
;SID-WIZARD fast packer/depacker (called by load/save menu and SID-Maker app.)
; $Id: packdepack.inc 378 2014-04-22 19:42:01Z hermitsoft $
;------------------------------------------------------------------------------

;packed .SNG file-format structure: -in-place compression/decompression necessary due to small memory
;--------------------------------- -inbetween empties must be stored
;1.Header: $40 bytes - basic tune-settings, used data amount, author-info (CONSTANT MEMORY LOCATIONS. DON'T MODIFY!)
;2.Orderlist sequences in 3's groups, all sequences end with $FE-$00 or $FF-$xx sequence
;3.Patterns
;4.Instruments
;5.Chords
;6.Tempoprograms
;7.Subtune-funktempos (from SUBTUNES table)


;======================================================================================
        .if COMPILEDAPP==1;coming part (packer) is only needed for editor
;=======================================IN-PLACE FORWARD PACKER======================================================
;---pack subtune-orderlists (in-place compression by omitting empty space between sequences)-------------------------
;we rely on boundary size given by SWM-spec.src / settings.cfg to seek uncompressed sequence beginnings
packtun sei
        lda #$35
        sta banksel
        lda #<SEQUENCES ;set zeropage pointers to beginning of music data
        ldx #>SEQUENCES
        jsr inicptr
        ldx #0          ;counts number of sequences
        stx TUNEHEADER+SEQAMOPOS
        stx TUNEHEADER+PTAMOUPOS ;will be set to maximum number of pattern
seqcolp ldy #0          ;pointer inside sequences
-       lda (decozptr),y ;read data from uncompressed area
        cmp #$FE        ;hardwired delimiter value - absolute end of sequence
        beq foundli
        cmp #$FF        ;hardwired delimiter value - loop-command of sequence - 1 byte follows it
        beq ++          ;if found loop-delimiter
        sta (compzptr),y ;write data to possibly compressed (back-skewed) area
        cmp #maxptnamount+1
        bcs +           ;if SEQ-FX or illegal pattern, don't count it
        cmp TUNEHEADER+PTAMOUPOS ;check and find maximal referred pattern-number for pattern-compressor
        bcc +
        sta TUNEHEADER+PTAMOUPOS
+       iny
        cpy #seqlength  ;seqbound ;safety check, if reached max. length
        bne -           ;if reached max. seqlength without delimiter, correct data with artificial $fe
        lda #$FE        ;safety delimiter (if there was data corruption it might help to restore orderlists)
        bne foundli     ;jump
+       sta (compzptr),y ;write data to possibly compressed (back-skewed) area
        iny             ;get jumpaddress after $ff
        lda (decozptr),y ;read data from uncompressed area
foundli sta (compzptr),y ;sequence-delimiter found
        iny
        tya
        sta (compzptr),y ;length of sequence stored after each sequence to ease later backwards-decompression
        lda #seqbound   ;go to next sequence
        jsr incptrs
        cpy #0+1+1      ;check if sequence was empty ($fe delimiter found at position 0)
        beq +           ;+ ;if empty sequence, the counter stays 0
        stx TUNEHEADER+SEQAMOPOS 
        jsr ctotmpz     ;store pointer of data after last non-empty sequence
+       cpx #(maxsubtuneamount+1)*(CHN_AMOUNT) ;One subtune's Orderlist contains min. 3 sequences (3 tracks)
        bcc seqcolp     ;if not yet reached maximum sequence-amount
        jsr ctempzp

;---pack pattern data (in-place compression by omitting empty space between patterns) and rle-compressing continuous zero notes
;--last referred pattern searched previously in sequence-packer
;(-but the last pattern should be which contains other than zeroes before delimiter (not to lose not used patterns with content))
        ldx #0
ptnclop cpx TUNEHEADER+PTAMOUPOS ;loop ends after last used (referenced) pattern reached (if 0, no process at all)
        bne +
        jmp doneptc
+       lda PPTRLO+1,x  ;set pattern-pointers according to number of pattern (pattern 0 ignored)
        sta decozptr
        lda PPTRHI+1,x
        sta decozptr+1
        ldy #0          ;pointer inside pattern
        sty compzvar    ;source-pointer index
        sty compzvar2   ;target-pointer index
        sty previns+1   ;previously selected instrument - used to check duplicated instrument
        sty optimiz+1   ;optimization amount
        
ptpcklp jsr ldsourc     ;ldy compzvar ;lda (decozptr),y;load & check actual value
        .if ENABLEPACKEDNOP==1 ;if NOP packing enabled
        bne wrcmptr     ;check for 2 successive zeroes (1 NOP is sure)
        cpy #0
        beq putcbyt     ;if position is 0 (very 1st data of pattern), don't do packed NOP
        dey             ;to point to previous value
        cmp (decozptr),y ;check if previous value is 0 too - a 0 always has to preceed packed NOP (helps for depacker, prevents counting bigFX 0-value)
        bne putcbyt     ;if zero too, we reached a NOP, start counting NOPs
        lda #PACKEDMIN-1 ;init 'packed rest' counter to hardwired minimum ($70 = 2 packed NOPs) ;from here we count successive NOPs
        sta compzvar3   ;counts successive NOPs to determine packed rest
-       ldy compzvar
        iny             ;check next item if NOP
        lda (decozptr),y
        bne putprst     ;other than NOP?
        inc compzvar
        inc compzvar3
        lda compzvar3
        cmp #PACKEDMAX  ;max. value of packed rest ($77 = 9 packed NOPs)
        bne -           ;jump
putprst lda compzvar3
        cmp #PACKEDMIN-1 ;if only one NOP, don't pack anything
        bne putcbyt
        lda #$00        ;simple NOP in case of only 2 consequent zeroes
        .fi
wrcmptr 
        .if OPTIMIZEPTN==1
        bit Config.confbits ;if instrument auto-typing enabled, optimize, otherwise allow user to optimize by hand only
        bpl putcbyt
        cmp #0          ;check in note-data if instrument-column exists
        bpl putcbyt
        cmp #$FF        ;end of pattern?
        bpl putcbyt
        jsr rolpdat     ;copy Accu to target and increase pointers, then load next source-data to A
        pha
        and #$7F
        beq optimer     ;accidental standalone instrument-column NOPs should be optimized, no instr. 0 to be selected
        cmp #$3F        ;don't disturb legato and instr.effects 
        bpl +
previns cmp #selfmod    ;previously selected instrument - used to check duplicated instrument
        beq NoNewIn     ;check if instrument-change happens
newinst sta previns+1
        jmp +           ;if new instrument selected, don't disturb it
NoNewIn tay ;no inst.change but also check if PW/CTF non-reset instrument where instrument-selection also resets PW/filterprogram
        lda INSPTLO,y
        sta chIctrl+1
        lda INSPTHI,y
        sta chIctrl+2
chIctrl bit selfmodA    ;reads selected instrument's control-byte
        bvs +           ;check PW/CTF reset bytes
        bmi +           ;do not optimize anything if instrument is 'non-reset' type
optimer pla
        bpl shrinki     ;go to shrink unneeded bytes of unneeded instrument-selection
        lda #$80        ;zero unneeded instrument selection (doesn't mean memory reductiod but better for the eye)
        jmp writifx
shrinki ldy compzvar2
        dey             ;index to previous target-byte
        lda (compzptr),y
        and #$7F        ;disable bit7 of note before deleted instrument-selection
        sta (compzptr),y
        inc compzvar    ;omit repeated/redundant instrument-selection event (the optimization happens here)
        inc optimiz+1   ;will be substracted from pattern-size
        jmp chkpend
+       pla
        bpl putcbyt     ;check in instrument-column if effect-column exists
writifx jsr rolpdat     ;copy Accu to target and increase pointers, then load next source-data to A
        cmp #$20
        bcs putcbyt     ;check in effect-column if effect-value exists (Big-Fx)
        jsr rolpdat     ;copy Accu to target and increase pointers, then load next source-data to A
        .fi
putcbyt 
        jsr rolpdat
chkpend lda compzvar    ;check source end-index
        cmp ptnsize+1,x
        bcs +
        jmp ptpcklp
+       ldy compzvar2
        dey
        sec
optimiz sbc #selfmod    ;amount of optimized bytes
        sta (compzptr),y ;transform endsignal to patternsize indicator for depacker
        iny
        lda ptnlength+1,x ;patternlength-indicator for depacker
        sta (compzptr),y
        jsr incptrs     ;counts patterns (DEX)
        beq doneptc
        jmp ptnclop     ;loop
doneptc

;---pack instrument-data (in-place compression by omitting empty space between instruments)---------
        lda #<INSTRUMENTS ;set zeropage pointers to beginning of instrumental data
        ldx #>INSTRUMENTS
        jsr inidptr
        jsr ctotmpz
        ldx #0          ;counts number of instruments
        stx TUNEHEADER+INSTAMPOS ;init instrument-counter
insclop jsr packinst    ;pack actual instrument referred by pointers decozptr and compzptr (X counts inside)
        cpx #maxinstamount-1 ;-1: don't check very last unseen instrument (it's a copybuffer)
        bne insclop
        jsr ctempzp

;---copy chordtable,cut by last (no need to crop between chords, because packed in realtime by tracker)
        ldy #0
        sty chcount+1   ;safety counter for chords
        sty TUNEHEADER+CHRDLEPOS ;init
-       lda CHORDS+1,y  ;start with chord1, as chord 0 is always empty (used as NOP)
        sta (compzptr),y
        bmi ++
        cmp #$7E        ;hardwired chord delimiter ($7e..$7f)
        bmi ++          ;if read value is not delimiter, just look forward
        inc chcount+1
prevdat lda CHORDS,y    ; - previous value
        bmi +
        cmp #$7E        ;hardwired chord delimiter
        bpl ++          ;if previous value is not delimiter
+       tya             ;register end-position of non-empty chord
chcount ldx #selfmod
        cpx #MAXCHORDAMOUNT-1
        bpl donepch     ;safety test, if corrupt end of table no more than the allowed amount of chords saved
        clc
        adc #1
        sta TUNEHEADER+CHRDLEPOS
+       iny
        .if (ChordTableLen<$100)
        cpy #ChordTableLen
        .fi
        bne -
donepch ldy TUNEHEADER+CHRDLEPOS ;if no chord was found, don't set pointers
        beq +
nextcpt jsr incompt     ;increase target-pointer with Y
+
;---copy tempotable,cut by last (no need to crop between tempoprograms, because packed in realtime by tracker)
        ldy #0
        sty tmcount+1   ;safety counter for tempo-programs
        sty TUNEHEADER+TMPLENPOS ;init tempoprogram length-counter
-       lda RESTEMP+1,y ;base-pointer of tempo-programs' tables (others not needed to be saved)
        sta (compzptr),y
        bpl ++          ;check if delimiter bit7=ON
        inc tmcount+1
        cmp #$80        ;empty delimiter
        bne +           ;if not empty delimiter, no matter what previous value is, tempoprogram is valid, should be counted
        cpy #0          ;delimiter at position 0 always means empty table, expect if delimiter is not $80
        beq ++
        lda RESTEMP,y   ; - previous value
        bmi ++          ;if current value is $80 and previous value is >=$80 it's an empty program
+       tya
tmcount ldx #selfmod
        cpx #MAXTEMPOPRAMOUNT-1
        bpl doneptm     ;safety test, if corrupt end of table no more than the allowed amount of tempo-programs saved
        clc
        adc #1
        sta TUNEHEADER+TMPLENPOS
+       iny
        cpy #(TempoTableLen-(RESTEMP-TEMPOTBL)) ;check till address $100 after TEMPOTBL (tempotable must be addressable on 1 byte index by player)
        bne -
doneptm ldy TUNEHEADER+TMPLENPOS ;if no tempoprogram was found, don't set pointers
        beq +
        jsr incompt
+

;--Subtune-tempos---
        ldy #0          ;if no pattern/sequence counted, subtune-tempo is still possible for 1st empty subtune
-       tya
        asl             ;x4
        asl             ;x8
.if (SID_AMOUNT>=2)
        asl ;*16
.fi
.if (SID_AMOUNT>=3)
        asl ;*32
.fi
        tax
.if (SID_AMOUNT==1)
        lda SUBTUNES+6,x
.elsif (SID_AMOUNT==2)
        lda SUBTUNES+14,x
.elsif (SID_AMOUNT==3)
        lda SUBTUNES+30,x
.fi
        sta (compzptr),y
        iny
.if (SID_AMOUNT==1)
        lda SUBTUNES+7,x
.elsif (SID_AMOUNT==2)
        lda SUBTUNES+15,x
.elsif (SID_AMOUNT==3)
        lda SUBTUNES+31,x
.fi
        sta (compzptr),y
        iny
        tya             ;start multiplying A by 1.5
        sta compzvar
        lsr
        clc
        adc compzvar    ;compare sequence multiplied by 3 to amount of counted sequences (subtune*3)
.if (SID_AMOUNT==2)
        asl ;*2 ;if 2SID, multiplying A by 2*1.5==3 instead
.elsif (SID_AMOUNT==3)
        sta compzvar
        asl ;if 3SID, multiply A*1.5 by 3 (Y by 4.5)
        clc
        adc compzvar
.fi
        cmp TUNEHEADER+SEQAMOPOS
        bcc -
        jsr incompt
        rts

;----------------------------shared packer-subroutines-------------------------
rolpdat ldy compzvar2   ;copy Accu to target and increase pointers, then load next source-data to A
        sta (compzptr),y
        inc compzvar2
pacptfw inc compzvar    ;increase (only) source pointer, then load next source-data to A
ldsourc ldy compzvar
        lda (decozptr),y ;load current value
        rts

packinst                ;pack instrument -source given in decozptr, destination in compzptr
        ldy #maxinstsize-instnamelength-1;search $ff backwards - requires clean unused instrument area
-       lda (decozptr),y
        cmp #$FF        ;hardwired delimiter
        beq foundil
        dey
        cpy #$0F        ;hardwired table-beginning safety-test jump (if not found delimiter above) - should create delimiters / empty instr.
        bne -           ;!!!what to do if there's an irrelevant instrument without $ff
foundil tya
        sta (compzptr),y ;change last delimiter to length of effective instrument-size
        sty compzvar
        ldy #0
-       lda (decozptr),y
        sta (compzptr),y
        iny             ;better increase than decrease, because source & target may overlap if music memory is fully utilized
        cpy compzvar    ;end of instrument-data before name
        bne -           ;and Y holds that here
        lda #maxinstsize-instnamelength
        jsr incptrs     ;increase source-pointer to instrument-name
        dex             ;back-correction
        ldy #selfmod
-       lda (decozptr),y
        sta (compzptr),y
        iny
        cpy #instnamelength
        bne -           ;Y is instrument-name length here
        lda #instnamelength
        jsr incopt2
        ldy compzvar
        cpy #(endummyih-dummyihdr)-1 ;last $ff - if this is true, instrument is empty
        beq +
        stx TUNEHEADER+INSTAMPOS ;count last used instrument
        jsr ctotmpz     ;store pointer of data after last non-empty instrument
+        rts
;=====================================end of packer================================================================
        .fi                ;end of part only needed for editor



;===================================================================================================================
;============================DEPACK/INIT loaded tune-data in place (backwards)======================================
depackt                 ;depack-tune-data (backward in-place decompression with filling up empty place (init))
        ;compzptr must be set to 1st byte after packed/loaded tune-data
        sei
        lda #$35
        sta banksel     ;ensure consistent variable and tune-data area
        ;!!order of these calls is important!! don't modify!
        jsr depktempo   ;depack/init subtune-tempos and tempotable
        jsr depkinsch   ;depack/init instruments and chords
        jmp depkptseq   ;depack/init patterns/sequences

;---depack/init subtune tempos and tempotable -------------------------------------------------------------------------
depktempo               ;depack/init sudtune-tempos and tempotable
        ldy TUNEHEADER+SEQAMOPOS
        beq +           ;treat 0 the same as 1/2/3 - one subtune-tempo must always exist
        dey             ;set treshold boundary from 3..6..9..etc. to 4..7..10..etc.
+       tya
        ldy #0          ;counts result
-       sec
        sbc #CHN_AMOUNT ;divide by 3 loop (or 6 for 2SID version)
        bcc +
        iny
        bne -           ;max. 255 - avoid endless loop
+       iny             ;shift from 0..1..2..etc. counting to 1..2..3..etc. counting of subtunes (no such like 0 subtunes, on always has to exist)
       .if COMPILEDAPP==2
        sty SIDstamount+1 ;set number of subtunes in SID-header
       .fi
        tya             ;Y contains amount of subtunes (3 * amount of sequences)
        asl             ;*2: adjust to 2 byte data-indexing
        sta compzvar    ;ending of use contains 2 * (number of subtunes)
        jsr deccptr     ;set pointers back by twice the amount of counted subtunes
       .if COMPILEDAPP==2
        lda compzptr+0
        clc
        adc #(RESTEMP-TEMPOTBL) ;space for tempoprogram-beginning (selected subtune-tempos)
        sta subtuneadd+0
        tax
        lda compzptr+1
        adc #0          ;add Carry flag
        sta subtuneadd+1
        tay

        txa
        clc
        adc #(SID_AMOUNT!=3)? (8*SID_AMOUNT-2)-4*SID_AMOUNT : 30-16
        sta setstp1+1   ;subtune-tempo pointer setting for depacker
        tya
        adc #0
        sta setstp1+2
        txa
        clc
        adc #(SID_AMOUNT!=3)? (8*SID_AMOUNT-1)-4*SID_AMOUNT : 31-16
        sta setstp2+1
        tya
        adc #0
        sta setstp2+2
       .fi

        jsr dpstemp

       .if COMPILEDAPP==2
        lda compzvar    ;set tempo-pointers' base address after subtune-pointers' area (subtunes * 8 or 16)
        asl ;*4
        asl ;*8
        .if (SID_AMOUNT>=2)
         asl ;*16
        .fi
        .if (SID_AMOUNT>=3)
         asl ;*32
        .fi
        clc
        adc subtuneadd+0
        sta tempoptadd+0
        php
        adc #1
        sta temptad+1
        lda subtuneadd+1
        adc #0          ;add Carry flag
        sta temptad+2
        plp
        lda subtuneadd+1
        adc #0          ;add Carry flag
        sta tempoptadd+1
       .fi

;---depack tempotable-----------------------------It must be forward-motion!!! (poor chance of overlap with packed data btw.)
        ldy TUNEHEADER+TMPLENPOS
        jsr decptrs     ;set read-pointer to beginning of packed tempotable
        .if COMPILEDAPP==2
        lda compzptr
        sta tempotbadd
        lda compzptr+1
        sta tempotbadd+1
        .fi
        ldx #0          ;tempoprogram-counter (pointer-position)
        ldy #0
-       lda (compzptr),y
        cpy TUNEHEADER+TMPLENPOS ;length of tempoprogram
        bcc +           ;check if end of loaded table reached
        .if COMPILEDAPP==2
        rts
        .elsif COMPILEDAPP==1 ;fill rest of allowed tempos with $80 endsignals
        lda #$80        ;hardwired delimiter for filler
        .fi
+       cmp #0
        bpl ++          ;tempoprogram-endsignal?
        inx             ;pointer 0 and 1 need not be set (they're always the same)
        .if COMPILEDAPP==1
        cpx #MAXTEMPOPRAMOUNT-1
        bcc +           ;if no more tempoprograms allowed, fill rest with zeroes
        lda #0          ;table-end filler
        beq ++          ;jump
        .fi
+       pha
        tya
        clc
        .if COMPILEDAPP==1
        adc #1+1+(RESTEMP-TEMPOTBL)
        .elsif COMPILEDAPP==2
        adc #1+(RESTEMP-TEMPOTBL) ;don't add +1 - omitting tempoprogram-0 for exported tune
        .fi
temptad sta TEMPTRLO+1,x ;set pointer to next tempoprogram
        pla
+
        .if COMPILEDAPP==1
        sta RESTEMP+1,y ;base-pointer of tempo-programs' tables (others not needed to be saved)
        .fi
        iny
        cpy #(TempoTableLen-1)-(RESTEMP-TEMPOTBL)
        bne -
donedpt rts

;------------------
dpstemp
       .if COMPILEDAPP==1
        ldy #(maxsubtuneamount+1)*2-1 ;*2: adjust to 2 byte data-indexing
        .elsif COMPILEDAPP==2
        ldy compzvar    ;last subtune *2 stored here
        dey
       .fi
isttemp tya             ;x2 (double-incremented)
        asl             ;x4
        asl             ;x8
       .if (SID_AMOUNT>=2)
        asl ;*16
       .fi
       .if (SID_AMOUNT>=3)
        asl ;*32
       .fi
        tax
        lda (compzptr),y
       .if COMPILEDAPP==1
        cpy compzvar    ;check if unused subtunes reached
        bcc setstp2     ;if current pointer is bigger than used
        lda #defsubtempo2 ;default subtune-tempo 2
       .fi
setstp2 sta (SID_AMOUNT!=3)? SUBTUNES+(8*SID_AMOUNT-1)-4*SID_AMOUNT,x : SUBTUNES+31-16,x ;self-written by SID-Maker
        dey
        lda (compzptr),y
       .if COMPILEDAPP==1
        bcc setstp1     ;use previous Carry-bit result
        lda #defsubtempo1 ;default subtune-tempo 1
       .fi
setstp1 sta (SID_AMOUNT!=3)? SUBTUNES+(8*SID_AMOUNT-2)-4*SID_AMOUNT,x : SUBTUNES+30-16,x ;self-written by SID-Maker
        dey
        cpy #$ff
        bne isttemp
        rts

;---depack/init chordtable and chords -------------------------------------------------------------------------
depkinsch               ;depack/init chords -  It's forward-motion!!! (poor chance of overlap with packed data btw.)
        ldy TUNEHEADER+CHRDLEPOS
        jsr decptrs     ;set read-pointer to beginning of packed chordtable
        .if COMPILEDAPP==2
        lda compzptr
        sta chordtbadd
        lda compzptr+1
        sta chordtbadd+1
        .fi
        ldx #0
        ldy #0
-       lda (compzptr),y
        cpy TUNEHEADER+CHRDLEPOS ;check depack/fill treshold
        bcc +           ;if end of loaded table reached,fill rest of allowed tempos with endsignals
        .if COMPILEDAPP==2
        bcs donedpc
        .elsif COMPILEDAPP==1 ;fill rest of allowed tempos with $80 endsignals
        lda #$7F
        .fi
+       cmp #$7E        ;hardwired delimiter
        beq inchdpt
        cmp #$7F        ;hardwired delimiter
        bne ++          ;tempoprogram-endsignal? - if not, just write value to chordtable
inchdpt inx             ;pointer 0 need not be set
        .if COMPILEDAPP==1
        cpx #MAXCHORDAMOUNT
        bcc +           ;if no more tempoprograms allowed, fill rest with zeroes
        lda #0          ;table-end filler
        beq ++          ;jump
        .fi
+        pha
        tya
        clc
        .if COMPILEDAPP==1
        adc #1+1
        .elsif COMPILEDAPP==2
        adc #1          ;compensate with pointers the lack of chord 0 in chordtable
        .fi
chdptad sta CHDPTRLO+1,x ;set pointer to next tempoprogram (pointer 0 & 1 not be set)
        pla
+
        .if COMPILEDAPP==1
        sta CHORDS+1,y  ;base-pointer of chord-program table (chord 0 not touched)
        .fi
        iny
        cpy #ChordTableLen-1 ;don't write to next page (tempotable)
        bne -
donedpc
        .if COMPILEDAPP==2
        ldy #1          ;set chord 1 pointer (not done by depacker)
        lda #1-1        ;0, compensate missing chord 0
        sta (chordptadd),y
        inx             ;add 1 more byte after chordpointer
        inx             ;!!! for advice of Ian Coog, have pointer for (dummy) instrument 0!
        txa             ;X is amount of chords
        clc
        adc chordptadd+0 ;set instrument-lo pointer table base address
        sta insptloadd+0
        lda chordptadd+1
        adc #0          ;add Carry flag
        sta insptloadd+1
        lda TUNEHEADER+INSTAMPOS ;amount of instruments
        clc
        adc #1          ;!!! for advice of Ian Coog, have pointer for (dummy) instrument 0!
        adc insptloadd+0 ;set instrument-hi pointer table base address
        sta inspthiadd+0
        lda insptloadd+1
        adc #0          ;add Carry flag
        sta inspthiadd+1
        .fi

depackins               ;depack existing and clear non-existing instruments (if INSTAMPOS=0, init all instruments)
        .if COMPILEDAPP==2
        lda compzptr+0  ;set target pointer to source pointer (inst.names will be wiped out)
        sta decozptr+0
        lda compzptr+1
        sta decozptr+1
        ldy TUNEHEADER+INSTAMPOS ;init inst.counter
        beq donedpi     ;fix: No instrument -> no depacking :)
        sty compzvar2   ;counts instruments
ininilp jsr depkins     ;depack one (currently counted) instrument or init above last
        .elsif COMPILEDAPP==1
        ldy TUNEHEADER+INSTAMPOS
        cpy #maxinstamount-1 ;check if more instruments than currently supported
        bcc storins     ;if all instruments fit, store them normally
        sty compzvar2
skiplop ldy #maxinstamount-1
        cpy compzvar2
        bcs storins
skipins lda #>(lowvidram+$200) ;a safe temporary depack-area for instruments that doesn't fit in memory
        ldx #>(lowvidram+$200)
        jsr inidptr
        jsr depkins
        dec compzvar2
        jmp skiplop
storins ldy #maxinstamount-1 ;start to init instrument-area from last unused (before clipboard/buffer) instrument
        sty compzvar2   ;counts instruments
        jsr setidpt     ;set decozptr to Y instrument-pointer
ininilp ldy TUNEHEADER+INSTAMPOS
        cpy compzvar2
        bcs indepak     ;depack or generate instrument?
        jsr EmptIns     ;create empty instrument
        jmp adviptr     ;jump
indepak jsr depkins     ;depack one (currently counted) instrument or init above last
adviptr lda #maxinstsize
        jsr decdptr
        .fi
        dec compzvar2   ;count instruments
        bne ininilp
donedpi
        .if COMPILEDAPP==2
;       ;done depacking instruments 1..xx - creating pointer for dummy instrument 0 (Ian Coog's advice)
        lda #<normalPlayer.player.VARIABLES ;safely points inside player-code
        ldx #>normalPlayer.player.VARIABLES ;safely points inside player-code
        ldy TUNEHEADER+INSTAMPOS
        beq +
        ldy #1          ;read instrument 1 lo & hi pointer
        lda (inspthiadd),y
        tax
        lda (insptloadd),y
+       ldy #0
        sta (insptloadd),y ;link instrument 0 (dummy) to instrument 1 or safe stagnant area inside player
        txa
        sta (inspthiadd),y
        .fi
        rts

depkins                 ;depack one (currently counted) instrument - compzptr should point to end of source (packed) instrument, decozptr should point to beginning of target instrument
        .if COMPILEDAPP==1
        lda #maxinstsize-instnamelength-1
        ldy #0
        jsr incopt2     ;set target to byte before inst.name
        .fi
        ldy #instnamelength+1 ;+1: last value is effective size of current instrument
        jsr decptrs
        .if COMPILEDAPP==1
-       lda (compzptr),y ;copy instrument-name
        sta (decozptr),y
        dey
        bne -           ;stop at 0
        .elsif COMPILEDAPP==2
        ldy #0
        .fi
        lda (compzptr),y ;effective inst.size
        .if COMPILEDAPP==2
        pha
        lda #$ff        ;inst.table hardwired endsignal - restore length-number back to this delimiter
        sta (compzptr),y
        pla
        .fi
        tay
        jsr decptrs     ;to beginning of instrument in source (compzptr)
        .if COMPILEDAPP==2
        iny
        iny
        tya
        jsr decdptr     ;set target pointers too
        dey
        jsr depcopy     ;shift instrument to 'nameless' destination
        ldy compzvar2   ;number of current instrument
        lda decozptr+0  ;set instrument-pointer
.if(SWP_EXPORT!=0)
        sec
        sbc SWPdatL+1
.fi
        sta (insptloadd),y
        lda decozptr+1
.if(SWP_EXPORT!=0)
        sbc SWPdatH+1
.fi
        sta (inspthiadd),y
        .elsif COMPILEDAPP==1
        lda #maxinstsize-instnamelength-1 ;to beginning of instrument in target (decozptr)
        jsr decdptr
        iny
        lda #maxinstsize-instnamelength-1 ;fill rest with zero!
        jsr clrupdp     ;clear up remaining space (starting value in A, end value in Y (also output) )
        lda #$ff        ;hardwired delimiter value
        sta (decozptr),y ;change size-signture to $ff delimiter
        dey
        jsr depcopy      ;copy instrument to depack-destination
        .fi

;trick for SW 1.x version to keep compatibility with old tunes. in SWM2 / SW 2.x versions this won't be needed and $00 will be valid 1st waveform
        ldy #$0f        ;1st waveform position in instrument
        lda (decozptr),y
        bne +
        lda #$09        ;default 1st waveform in SWM1
        sta (decozptr),y
        +

        rts             ;maybe check for max. inst.amount and size to be on the safe side


EmptIns ldy #(endummyih-dummyihdr)-1;($0f+3) ;generate empty instrument
-       lda dummyihdr,y  ;default hard-restart timer value
        sta (decozptr),y
        dey
        bpl -
        ldy #maxinstsize-1
-       lda #"."        ;dummy instrument-name
        cpy #maxinstsize-instnamelength
        bcs +
        lda #0          ;init instrument-tables' area
+       sta (decozptr),y
        dey
        cpy #(endummyih-dummyihdr)-1 ;($0f+3) ;(hardwired) instrument-header size
        bne -
        rts

dummyihdr               ;empty instrument-header to initialize all instruments by depacker
instbase .byte %00011010;0 CONTROL REGISTER (HR TIMER,HRTYPE ,VIBRATO-TYPE,TIED PW/CTF)
        .byte >DEFAULTHRADSR,<DEFAULTHRADSR ;1 HR-ADSR
        .byte $00,$F0    ;3 ADSR ON NOTESTART (GATE-ON)
        .byte $00        ;5 VIBRATO (CALCULATED) FREQ+AMP (amplitude:high nibble)
        .byte $00        ;6 VIBRATO DELAY OR AMPLITUDE-INCREMENT SPEED
        .byte $00        ;7 ARPEGGIO AND CHORD-SPEED (0=1X)
        .byte $01        ;8 DEFAULT CHORD FOR INSTRUMENT
        .byte $00        ;9 OCTAVE SHIFT (2's complement) - it's transpose essentially but in 12 steps by editor
        .byte <(pulsprog - instbase) ;PWTBL1-INST1 ;A PW-TABLE POINTER RELATIVE TO INSTRUMENT-ADDRESS
        .byte <(filtprog - instbase) ;FLTBL1-INST1 ;B FILTER-TABLE POINTER RELATIVE TO INSTRUMENT-ADDRESS
        .byte $00        ;C GATEOFF POINTER FOR WFARP-TABLE RELATIVE TO WF-TABLE POSITION
        .byte $00        ;D GATEOFF POINTER FOR PW-TABLE RELATIVE TO PW-TABLE POSITION
        .byte $00        ;E GATEOFF POINTER FOR FILTER-TABLE RELATIVE TO FILTER-TABLE POS.
        .byte $09        ;F 1ST FRAME WAVEFORM
        .byte $ff        ;WF-PROGAM ;hardwired position at $10 (relative to instrument-base)
pulsprog .byte $ff       ;PW-PROGRAM
filtprog .byte $ff       ;FILTER-PROGRAM
endummyih                ;end of dummy-header


;---depack patterns and sequences----------------------------------------------------------------------
depkptseq               ;depack/init patterns & sequences
        .if COMPILEDAPP==1      
        lda TUNEHEADER+PTAMOUPOS
        sec
        sbc #maxptnamount+1 ;check if more pattern than supported
        sta overptn+1     ;will count down non-fitting patterns
        lda #maxptnamount ;start to init pattern-area after used patterns
        .elsif COMPILEDAPP==2
        lda TUNEHEADER+PTAMOUPOS
        .fi
        sta compzvar3   ;counts patterns
iniptlp
        .if COMPILEDAPP==1
        ldy compzvar3
        lda PPTRLO,y
        ldx PPTRHI,y
        jsr inidptr     ;set pattern depack-pointer
        .fi
        ldx compzvar3
        .if COMPILEDAPP==1
        cpx TUNEHEADER+PTAMOUPOS
        bcc ptdepak     ;if X <
        beq ptdepak     ;if X =
emptypt ldy #$ff
        lda #PTNBOUND-1
        jsr clrupdp     ;wipe unused patterns  --- why last one isn't initialized?
        lda defptlen
        sta ptnlength,x
        tay
        clc
        adc #1
        sta ptnsize,x
        lda #$ff        ;hardwired delimiter
        sta (decozptr),y
        jmp dpnextp
        .fi
ptdepak ldy #2          ;depack used patterns
        jsr decptrs
        .if COMPILEDAPP==1
        lda (compzptr),y ;get pattern-length
        sta ptnlength,x
        .fi
        dey
        lda (compzptr),y ;get pattern-size
        .if COMPILEDAPP==1
        sta ptnsize,x
        .elsif COMPILEDAPP==2
        pha
        lda #$ff        ;pattern delimiter (hardwired)
        sta (compzptr),y ;turn patternsize value back to delimiter value
        pla
        .fi
        tay
        dey
        .if COMPILEDAPP==1
        lda #PTNBOUND-1
        jsr clrupdp     ;wipe out remaining part of pattern
        lda #$ff        ;pattern delimiter (hardwired)
        sta (decozptr),y ;turn patternsize value back to delimiter value
        .fi
        dey             ;depack pattern (checking packed NOPs)
-       sty compzvar2
        ldy #1
        jsr decptrs     ;decrease compressed source data-pointer by 1, DEY (y=0)
        lda (compzptr),y
        ldx #0          ;counts packed NOPs
;.if ENABLEPACKEDNOP==1 ;if consequent NOP compression was enabled
        cmp #PACKEDMIN  ;lowest packed NOP value
        bcc wrnorpt     ;if A < PACKEDMIN
        cmp #PACKEDMAX+1
        bcs wrnorpt     ;if A > PACKEDMAX
        pha             ;Handle INS/PTFX $70..$78 differenty.
        ldy #1          ;amount of pointer-decrease
        jsr decptrs     ;decrease compressed source data-pointer by 1, DEY (y=0)
        lda (compzptr),y ;read value before suspected packed-NOP
        beq depack0     ;if data before packed NOP is 0, it's really a packed nop, not a chord-FX or effect-value
        bmi nopack0     ;not packed-NOP if data before has bit7 set 1
        cmp #PACKEDMIN  ;lowest packed NOP value
        bmi nopack0     ;if data before < PACKEDMIN (only another packed NOP or a $00 can be before a packed NOP)
        cmp #PACKEDMAX+1
        bpl nopack0     ;if data before > PACKEDMAX
depack0 ldy #1          ;it turned out that it's packed NOP, so depack it
        jsr incompt     ;set compressed-data pointer back to point to current value
depak0b pla             ;restore Accu containing current packed-NOP value
        sec             ;A is in packed NOP range
        sbc #PACKEDMIN-1 ;change range $70..$77 to $1..$8
        tax             ;store repeater amount in X
        lda #$00        ;put NOP into Accu.
        jmp wrnorpt
nopack0 ldy #1          ;it turned out that it's not packed NOP, so simply copy the value to destination
        jsr incompt     ;set compressed-data pointer back to point to current value
        pla             ;restore original value
        ldx #0          ;reset X
;.fi ;if want to load tunes with packed NOP in any case, this .IF must not be used
wrnorpt ldy compzvar2
wrpckpt
        .if COMPILEDAPP==1
        sta (decozptr),y
        .fi
        dey
        dex             ;count remaining packed NOPs
        bpl wrpckpt     ;do packed NOPs
        cpy #$ff        ;if place 0 was copied (reached), end of loop
        bne -
        .if COMPILEDAPP==1
overptn lda #selfmod
        bmi dpnextp
        dec overptn+1
        jmp iniptlp     ;do all the non-fitting patterns at the last position
        .elsif COMPILEDAPP==2
        ldy compzvar3   ;current pattern
        lda compzptr+0  ;set pointer of current pattern in pattern-pointer table
.if(SWP_EXPORT!=0)
        sec
        sbc SWPdatL+1
.fi
        sta (ptnptloadd),y
        lda compzptr+1
.if(SWP_EXPORT!=0)
        sbc SWPdatH+1
.fi
        sta (ptnpthiadd),y
        .fi
dpnextp dec compzvar3
        beq donedpp
        jmp iniptlp
donedpp

;---depack sequences----------------------------------------------
        .if COMPILEDAPP==1
        lda #<(SEQUENCES+((maxsubtuneamount+1)*(CHN_AMOUNT)-1)*seqbound)
        ldx #>(SEQUENCES+((maxsubtuneamount+1)*(CHN_AMOUNT)-1)*seqbound)
        jsr inidptr     ;set decozptr to last sequence
        ldx #((maxsubtuneamount+1)*(CHN_AMOUNT)) ;by default, start filling up sequences from last to 1st
        cpx TUNEHEADER+SEQAMOPOS
        bcs + ;check if more subtunes (orderlists/sequences) than supported
        ldx TUNEHEADER+SEQAMOPOS
+       dex
decoseq cpx TUNEHEADER+SEQAMOPOS
        bcc +           ;check, whether stored, or unused sequence instead
unused  lda #seqbound-1
        ldy #0
        jsr clrupdp     ;clear unused sequence area
        lda #$fe        ;hardwired delimiter to 1st position of unused sequence
        sta (decozptr),y
        jmp ++          ;jump
        .elsif COMPILEDAPP==2
        ldx TUNEHEADER+SEQAMOPOS
        beq donedps     ;if no sequence, nothing to be set
        dex             ;make range 1...x to 0...x-1
decoseq
        .fi
+       ldy #1          ;used sequence, storing it
        jsr decptrs     ;move source pointer to length-info
        lda (compzptr),y;Y is 0 here
        tay             ;length of sequence (amount to copy)
        jsr decptrs
.if COMPILEDAPP==1
        cpx #((maxsubtuneamount+1)*(CHN_AMOUNT))
        bcs nextseq     ;skipping spare (not fitting) sequences
        lda #seqbound-1
        jsr clrupdp
        jsr depcopy
+       lda #seqbound
        jsr decdptr     ;get to beginning of next decompressed sequence (backwards)
.elsif COMPILEDAPP==2
        txa             ;sequence-number to divide by 3 (or 6 or 9) to get number of current subtune
        ldy #0          ;counts result
-       cmp #CHN_AMOUNT ;'divide A by 3 or 6' loop
        bcc +
        sec
        sbc #CHN_AMOUNT
        iny
        bne -           ;max. 255 - avoid endless loop
+                       ;result of division in Y, remainder in A
        asl             ;multiply remainder by 2, sequence-pointer address inside subtune-data
        sta compzvar2
        tya             ;result of division
        asl             ;multiply by 8 to get to subtune-data base-address for current subtune
        asl
        asl
        .if (SID_AMOUNT>=2)
        asl ;*16 for 2SID version
        .fi
        .if (SID_AMOUNT>=3)
        asl ;*32 for 3SID version
        .fi
        clc
        adc compzvar2
        tay
        lda compzptr+0  ;get base-address of current sequence
.if(SWP_EXPORT!=0)
        sec
        sbc SWPdatL+1
.fi
        sta (subtuneadd),y
        iny
        lda compzptr+1
.if(SWP_EXPORT!=0)
        sbc SWPdatH+1
.fi
        sta (subtuneadd),y
.fi
nextseq dex
        cpx #$ff
        bne decoseq
donedps rts

;=====================================end of tune-depacker/initer==========================================


;--------------------------common subroutines for the main menu routines -------------------------
;!these pointer routines are used by many other parts of the code , don't touch them, not even rearrange them!!!
inicptr                 ;lo-pointer in A, hi-pointer in X, set zeropage pointers - order of entry points is important!
        sta compztmp
        stx compztmp+1
ctempzp lda compztmp
        sta compzptr
        ldx compztmp+1
        stx compzptr+1
inidptr sta decozptr
        stx decozptr+1
        rts

ctotmpz lda compzptr
        sta compztmp
        lda compzptr+1
        sta compztmp+1
        rts

incptrs                 ;increase source zeropage pointers by boundary size in A, and target pointer by Y value
        iny
incopt2 clc
        adc decozptr
        sta decozptr
        bcc incompt
incptnp inc decozptr+1
incompt tya
incmpt2 clc
        adc compzptr
        sta compzptr    ;set compressed data pointer to next position
        bcc +
        inc compzptr+1
+       inx
        rts

decdptr sta compzvar
        lda decozptr
        sec
        sbc compzvar
        sta decozptr
        bcs +
decptnp dec decozptr+1
+       rts

decptrs tya
deccptr sta compzvar
        lda compzptr
        sec             ;input: Accu -decrease 16bit tunedata-pointers by value given in a
        sbc compzvar
        sta compzptr
        bcs +
dechptr dec compzptr+1
+       dey
        rts

depcopy                 ;depack-copier, Y is amount, copies content from compzptr to decozptr
-       lda (compzptr),y
        sta (decozptr),y
        dey
        cpy #$ff        ;if place 0 is copied, end of loop
        bne -
        rts

clrupdp                 ;clear up remaining space (starting value in A, end value in Y (also output) )
        sty dpkYsto+1
        tay
-       lda #0
        sta (decozptr),y
        dey
dpkYsto cpy #selfmod
        bne -
        rts

divideX stx compzvar2   ;divide A by divisor X, remainder is in A for further division, Y gets charcode to display
        ldx #0
-       sec
        cmp compzvar2   ;compare divisor
        bcc +
        sbc compzvar2   ;substract divisor
        inx
        bne -           ;jump
+        
        .if COMPILEDAPP==1
        ldy commonsubs.hexchar,x
        .else
        ldy hexchar,x
        .fi
        rts

        .if COMPILEDAPP==1
setidptr                ;set decozptr to current instrument, set bank, disable IRQ
        sei             ;no clue why needed here, hopefully not a stacking/recursive bug
        lda #$35        ;selinst+1 might reside in RAM under KERNAL
        sta banksel
        ldy display.selinst+1 ;set starting source-address in decozptr
        .fi
setidpt lda INSPTLO,y
        ldx INSPTHI,y
        jmp inidptr

;=========================================================================================================
; vim: sw=4 ts=4 syntax=asm:
