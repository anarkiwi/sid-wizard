;ident:8,24
;==========================================KEYBOARD HANDLER=====================================
; $Id: keyhandler.inc 385 2014-07-01 12:59:56Z hermitsoft $
;handle the keypresses - the callback subroutine-names below are found in the datatables.inc file

keyhandler .block

morekey = keyer.morekey
retukey = keyer.retukey
        .if COMPILEDAPP==1 ;next part needed for editor only
restsig = keyer.restsig
        .fi

keyer .proc ;C64 keyboard handler (scanner) routine  ;zeropage variables can be seen in main source
        lda #0
.if (MIDI_support!=0)
midstrt ldy #selfmod    ;check if sequence-start command received through MIDI
        beq +
        sta midstrt+1
        jmp keyhandler.f1er.PlayBeg
+ .fi
        sta zpkeysi     ;keypress sign (nonzero, if key was pressed, Shift & C= are exceptions)
        sta zpkeyco
        lda #$fe
        sta zpkcsel     ;will rotate through keymatrix columns
        jsr shcbget     ;shift & CBM key requester
keylp2  lda zpkcsel     ;check keymatrix column 7..0
        sta $dc00
        ldy #0
        sty zpkeyro
        iny
        sty zpkyand
keylp1  lda $dc01
        cmp #$ff        ;much faster pre-testing, why should we examine column, if no keypress at all there?
        beq colcach
        and zpkyand     ;check keymatrix row 7..0
        bne morekey
        ;a key was pressed, branch to its routine
        lda zpkeyco     ;calculate jump-address from row & column
        asl
        asl
        asl             ;..x8
        asl             ;x2 (word-iterator)
        sta adder+1     ;self writing code to store accu
        lda zpkeyro
        asl             ;jump-addresses are given in words, so multiply by 2
        clc
adder   adc #selfmod    ;(accu restore)
        tay             ;Y is keycode here
        cpy #30         ;Left-Shift?   -not to count shift/cbm keys now, done already in 'shcbget'
        beq morekey     ;check next
        cpy #104        ;Right-Shift?
        beq morekey     ;check next
        cpy #122        ;C= key?
        beq morekey     ;check next
        cpy zprvkey     ;compare previous key to current, if different, reset repeat-counter
        beq +           ;if the same key is pressed, no counter-reset
        ldx #repspd1    ;no pressed key, repeatcounter resets
        stx repecnt     ;using X to preserve A
+       sty zprvkey

        .if COMPILEDAPP==1 ;next part needed for editor only
        ldx namemode    ;name-typing mode?
        beq +
        jmp menu.nametyper

+       ldx menumode
        beq +           ;if menu, just a list of some keys are allowed (cursor u/d, +/-, ESC/F8, RETURN, etc.)
        ldx #size(menukeys)-1
        tya
-       cmp menukeys,x
        beq +
        dex
        bpl -
        rts

+       stx display.titledi+1 ;author-info/title display off when any key pressed
        lda keyjmp+1,y  ;(if different jumptable for Shift/CBM, examine them here - not yet considered)
        pha
        lda keyjmp,y
        pha
        rts             ;jump to the given address from keytable (pushing the desired address to top of stack before rts) 

keyjmp .section data ;keyboard-jumptable                                     ;- 01 - 02 - 04 - 08 - 10 - 20 - 40 - 80 ---
 .rta deleter ,reter   ,cursori ,f7er    ,f1er    ,f3er    ,f5er    ,cursrdn ; del |retu|crri| F7 | F1 | F3 | F5 |crdn|fe
 .rta key3sub ,wkeysub ,keyAsub ,key4sub ,zkeysub ,skeysub ,keyEsub ,morekey ;  3  | W  | A  | 4  | Z  | S  | E  |lSH |fd
 .rta key5sub ,rkeysub ,keyDsub ,key6sub ,keyCsub ,keyFsub ,tkeyer  ,xkeysub ;  5  | R  | D  | 6  | C  | F  | T  | X  |fb
 .rta key7sub ,ykeysub ,gkeysub ,key8sub ,keyBsub ,hkeysub ,ukeysub ,vkeysub ;  7  | Y  | G  | 8  | B  | H  | U  | V  |f7
 .rta key9sub ,ikeysub ,jkeysub ,key0sub ,mkeysub ,kkeyer  ,okeysub ,nkeysub ;  9  | I  | J  | 0  | M  | K  | O  | N  |ef
 .rta pluser  ,pkeysub ,lkeysub ,minuser ,dotter  ,coloner ,atkeysub,commasub;  +  | P  | L  | -  | .  | :  | @  | ,  |df
 .rta inskeye ,asterisk,semicol ,homer   ,morekey ,equaler ,uparrow ,slasher ;pound| *  | ;  |HOME|rSH | =  | up | /  |bf
 .rta key1sub ,bckarrow,ctrler  ,key2sub ,spacer  ,morekey ,qkeysub ,stopper ;  1  |back|CTRL| 2  |SPC | C= | Q  |stop|7f
 .send data

menukeys .section data  
              ;ret,  F7,  curdn,stop, +  ,  -  ,  F1 ,  F3 ,  <-  , /  ,  Home, uparrow (page-down)
        .byte $01*2,$03*2,$07*2,$3F*2,$28*2,$2B*2,$04*2,$05*2,$39*2,$37*2,$33*2,$36*2 ;allowed key-codes in menu (multplied by 2)
        .send data

        .else
        ldx namemode    ;name-typing mode?
        jmp menu.nametyper
        .fi

;-----------------------------------------------------------
dummykey
        ;inc $d021       ;dummy handler for unused keys

retukey inc zpkeysi     ;keypress sign (counts simultaneously pressed keys)

morekey inc zpkeyro     ;check for other keys if not found pressed
        asl zpkyand
        bcc keylp1

colcach inc zpkeyco
        sec
        rol zpkcsel
        bcs keylp2

        .if COMPILEDAPP==1 ;next part needed for editor only
restsig lda #selfmod    ;signs that there was restore pressed
        beq nokey       ;check if restore key was pressed, and perform Page-UP if was
        lda #0
        sta restsig+1
        inc shiftsi
        jmp slasher.norSlas
        .fi

nokey lda zpkeysi
        bne retkey
        lda #repspd1    ;no pressed key, repeatcounter resets
        sta repecnt
        .if COMPILEDAPP==1 ;next part needed for editor only
        lda #0
        sta playadapter.jamnote+1
        .fi
retkey rts              ;return from key-handler routine

        .pend

;-----------------------------------------------------------
repeatex .proc          ;repeat-counter routine, return only if repeat can be done (shouldn't affect X or Y registers)
        lda repecnt     ;keyrepeat-counter
        bne norsrep     ;check, if repeat counter is 0
        lda #repspd2    ;default repeat-speed 2
        sta repecnt     ;if 0, reset counter to repeatspeed 2
noret   pla             ;getting back Program-counter from stack? (injected by Soci)
        pla             ;causing '2-level rts'?
        rts             ;repeat not allowed here
norsrep cmp #repspd1    ;check, of repeat counter is initial value (repeatspeed 1)
        bne repcan      ;if no, then just decrease the repeat counter
        lda ptrdysi     ;should wait for pattern-redraw?
        bne noret       ;if nonzero, repeater should wait for pattern-redrawing
        dec repecnt     ;if yes, decrease by one, and reset Z-flag - repeat possible
        rts             ;repeat allowed here
repcan  lda ptrdysi     ;should wait for pattern-redraw?
        bne noret       ;if nonzero, repeater should wait for pattern-redrawing
        dec repecnt
        bne noret       ;if reaches 0, a repeat occurs, next round will reset to speed 2
        rts             ;repeat allowed here
        .pend

;=================================

shcbget .proc
        ldx #0          ;get status of SHIFT and CBM keys
        stx shiftsi
        stx cbmsig
        ldy #2
-       lda modkeyc,y
        sta $dc00
        lda $dc01
        and modkeyr,y
        bne +
        inc shiftsi,x
+       dey
        bne +
        inx
+       bpl -
        rts

;maybe separate table for shift / cbm keys would be useful, when keyboard-layout-change support is planned
;-------------------------------------------------------------------------------
        .section data
modkeyc .byte $7f,$bf,$fd ;columns of CBM, Right-SHIFT, Left-Shift
modkeyr .byte $20,$10,$80 ;rows of CBM, Right-SHIFT, Left-Shift
        .send data
        .pend

        .if COMPILEDAPP==1 ;next part needed for editor only


;*************************************KEYBOARD-INITIATED ROUTINES********************************


;===================================HEXA KEYS==================================
;hexa-key handler routines - datawriter subroutine is in datawriter.inc

key0sub lda shiftsi
        bne solochn
        jmp nor0key
solochn jsr repeatex ;solo/unsolo current channel
        lda #$fe
solosta ldx #selfmod
        beq +
        lda #$ff
+       ldy #SID_AMOUNT*3-1
-       sta mutesolo,y
        dey
        bpl -
        lda solosta+1
        eor #1
        sta solosta+1
        ldy subwpos1+0 ;current track
        lda #$ff
        sta mutesolo,y
        jmp dAllMut

key9sub ldy #9-1        ;handle key '9' - value-typing (9), note-entry (C#), jamming (C#)
        .byte $2c       ;skip
key8sub ldy #8-1        ;handle key '8' - value-typing (8), note-entry (no note), jamming (no note)
        .byte $2c       ;skip
key7sub ldy #7-1        ;handle key '7' - value-typing (7), note-entry (A#), jamming (A#)
        .byte $2c       ;skip
key6sub ldy #6-1        ;handle key '6' - value-typing (6), note-entry (G#), jamming (G#)
        .byte $2c       ;skip
key5sub ldy #5-1        ;handle key '5' - value-typing (5), note-entry (F#), jamming (F#)
        .byte $2c       ;skip
key4sub ldy #4-1        ;handle key '4' - value-typing (4), note-entry (no note), jamming (no note)
        .byte $2c       ;skip
key3sub ldy #3-1        ;handle key '3' - value-typing (3), note-entry (D#), jamming (D#)
        .byte $2c       ;skip
key2sub ldy #2-1        ;handle key '2' - value-typing (2), note-entry (C#), jamming (C#)
        .byte $2c       ;skip
        .cerror key1sub & 0

key1sub .proc           ;handle key '1' - value-typing (1), note-entry (no note for this), jamming (no note)
        ldy #1-1
        lda shiftsi     ;shift key signal (00- no shift, 01- l/r shift was pressed)
        bne setmute
        lda cbmsig      ;C= key signal (00-no C=, 01- C= pressed )
        beq chOctaS
        lda Config.KeybTyp ;SDI layout? ($88)
        bpl +
        cpy #CHN_AMOUNT
        bmi setmute
+       sty jamoctave+1
        rts
chOctaS lda curwind     ;only use GMC octave selection when in pattern note-column (and there's also note-vibrato!)
        ora subwpos2+0
        bne nor1key
        bit Config.KeybTyp     ;if GMC layout, no need for C= key to select octave
        bvc nor1key
        jsr commonsubs.getcurnote
        and #$70
        cmp #VIBRATOFX
        beq nor1key
setocta sty jamoctave+1
        ;in DMC mode set octave of existing notes
        jsr commonsubs.setptnptr ;set pattern pointers in advance based upon current track/pattern position
        jsr commonsubs.compcur   ;check if not end of pattern ;overwrites Y, it holds current pattern number
        beq endOctS     ;nonotew ;don't write value if cursor at the endsignal of pattern
        lda jammode+1
        ora playadapter.followp+1 ;if follow-playing, don't allow typing notes into pattern
        bne endOctS     ;jamming ;will be used for jamming
        jsr commonsubs.getcurnote
        pha
        and #$80
        sta SavBit7+1   ;preserve bit7
        pla
        and #$7f        ;bit7 elimination
        beq endOctS     ;NOP?
        cmp #SWM_NOTE_MAX+1 ;data not in note range?
        bcs endOctS
        sec
        sbc #1          ;in SW note-numbering starts from 1
        jsr commonsubs.div12 ;divide Accumulator by 12, result (octave) is in Y, remainder (note) in Accumulator (commonsubs.inc)
        clc
        adc #1          ;add 1 - in SW note-numbering starts from 1
        sta playadapter.jamnote+1 ;make the octave-changed sound audible
        jsr commonsubs.calcoct    ;add octave multiplied by 12 to Accu, compare to $5F
        bpl endOctS
SavBit7 ora #selfmod    ;restore note with bit7
        jsr commonsubs.instpt2 ;write to pointer, using X-indexing
endOctS rts
setmute cpy #CHN_AMOUNT ;mute track 1-3 (y=0-2)
        bge nor1key
        jsr repeatex
        lda mutesolo,y  ;mute/solo track1
        eor #1
        sta mutesolo,y
.if (SID_AMOUNT>1)
        tya
        sec
        sbc trkposi
        bmi retnumk     ;don't allow other than 0..2
        cmp #3
        bcs retnumk
        tay
.fi
        sty display.msorefr+1 ;refresh mute-solo colouring for track given in Y
retnumk rts
nor1key iny             ; 1-8
        .byte $2c
        .pend

nor0key ldy #$00        ;handle key '0' - value-typing (2), note-entry (D#), jamming (D#)

norAkey
norBkey
norCkey
norDkey
norEkey
norFkey
        jmp datawriter.writevalue

keyAsub .proc           ;handle key 'A' - value-typing (A), note-entry (no note), jamming (no note)
        ldy #$0A        ;value 2
+       lda shiftsi
        bne +
        lda cbmsig
        beq norAkey
        jmp zkeysub.decAdva
+       jsr repeatex
        lda Config.autoadv ;toggle autoadvance mode
        cmp #$0f
        beq +
        inc Config.autoadv
+       rts
        .pend

keyBsub .proc           ;handle key 'B' - value-typing (B), note-entry (G), jamming (G)
        ldy #$0B        ;value 2
        lda cbmsig      ;shiftsi ;changed, because on real C64 shift+B could be pressed accidentally
        beq norBkey
        jsr repeatex
        lda #1          ;toggle track-binding
        jmp commonsubs.eorconf
        .pend

keyCsub .proc           ;handle key 'C' - value-typing (C), note-entry (E), jamming (E)
        ldy #$0C        ;value 2
        lda cbmsig      ;check CTRL+C - copy
        ora shiftsi     ;if shift+C, that also Copy setting - in copier it signs end of selection
        beq norCkey     ;note
cpyStrt jsr repeatex
        lda curwind
        bne +           ;if not patternwindow
        jmp copytobuf   ;copy patterndata (from cursorposition) to buffer
+       cmp #1
        bne +           ;orderlist?
        jmp copyseq     ;copy sequence from cursor-position (in orderlist) to 'seqbuffer'
+       cmp #2
        bne retCkey
        jmp CopyIns     ;copy instrument if it's instrument window
retCkey rts
        .pend

keyDsub .proc           ;handle key 'D' - value-typing (D), note-entry (D#), jamming (D#)
        ldy #$0D        ;value 2
        lda shiftsi
        bne +
        lda cbmsig
        beq norDkey     ;note
        jsr repeatex
togPoly lda #8          ;polyphonic-mode flag in Config.confbits
        jmp commonsubs.eorconf
+       jsr repeatex
        lda #$10 ;toggledovetailing
        jmp commonsubs.eorconf
        .pend

keyEsub .proc
        ldy #$0E        ;handle key 'E' - value-typing (E), note-entry (E), jamming (E)
        lda cbmsig
        beq norEkey
        jmp findUnused
        .pend

keyFsub .proc           ;F key handler
        ldy #$0f
        lda shiftsi     ;check SHIFT key
        bne +
        lda cbmsig
        bne PWreset
        jmp norFkey
+       jsr repeatex    ;check & handle repeat counter
        lda Config.KeybTyp     ;SDI layout?
        bpl DecFspd
goFilWi lda #3
goInsWi sta subwpos1+2  ;filter-table
        jmp f7er.setInsW
DecFspd ldy framesp     ;decrease frame-speed, if gone zero roll to max
        dey
        bne +           ;was it 1, the minimal?
        ldy #PALmaxframespeed
+       sty framesp     ;C= + F - decrease framespeed
keyret8 rts
PWreset ;toggle PW-table reset on/off
        lda Config.KeybTyp     ;SDI layout?
        bpl +
        jmp xkeysub.CbmXkey
        +
;        .if FILTRESETSW_ON
        jsr repeatex
        jsr commonsubs.setinsptr ;toggle filter-reset on instrument-notestart
        ldx #0
        jsr commonsubs.instpt1
        eor #$80
        jsr commonsubs.instpt2
        jmp commonsubs.instinit.goirefr
;        .else
;        rts
;        .fi
        .pend


;================================NOTE KEYS======================================
toggIns=ikeysub.toggIns ;toggle auto-instrument typing mode on/off
dAllMut=ikeysub.dAllMut
ikeysub .proc
        lda shiftsi
        beq norIkey
        jsr repeatex
toggIns lda #$80        ;instrument auto-type on/off setting (in tune-header)
        jsr commonsubs.eorconf
dAllMut ldy #$0F
        sty display.msorefr+1 ;refresh mute-solo colouring for all tracks
        rts
norIkey bit Config.KeybTyp     ;check if FC/GMC layout (with only 1 piano-key octave in the middle)
        bvs gkeysub.noincfs ;no audible sound in GMC mode
+       ldx #12+12+1    ;'C' note in 2nd upper octave
        jmp datawriter.notecol
        .pend

kkeyer .proc            ;K key handler
        lda cbmsig
        beq +
        jsr repeatex
        jmp f5er.swtochd
+       lda shiftsi     ;check SHIFT key
        bne dechorx
        lda Config.KeybTyp     ;check if FC/GMC layout (with only 1 piano-key octave in the middle)
        bpl +
        jmp skeysub.normalS
+       cmp #$2C        ;Janko-layout?
        bne +
        ldx #12+2 ;'C#' note in upper octave at lower row
        jmp datawriter.notecol.noteco2
+       and #$40 ;check bit6 (DMC-layout?)
        beq keyretk
        jmp norComma ;12+1 ;'C' note in upper octave at lower row
keyretk rts
dechorx jsr repeatex
        .cerror decchor & 0 ;dependency hack
        .pend
decchor .proc
        lda display.selchor+1
        cmp #1
        bne +
        rts
+       dec display.selchor+1 ;Shift + K - decrease chordnumber
        jmp chdzero
        .pend


IncFspd=gkeysub.IncFspd
gkeysub .proc
        lda Config.KeybTyp     ;SDI-layout?
        bpl +
        jmp gkeySDI
+       ldx #0+7        ;'F#' note in lower octave
        lda shiftsi
        bne +           ;check SHIFT
        jmp norGkey
+       jsr repeatex    ;check & handle repeat counter
IncFspd ldy framesp     ;increase frame-speed, roll over if max. passed
        cpy #PALmaxframespeed ;(8)
        bcc +
        ldy #0          ;(minimal framespeed 1 will be set with iny)
        sty IRQ.fspdctr+1
+       iny
        sty framesp     ;SHIFT+G - increase fremespeed
noincfs rts
        .pend

hkeysub .proc
        lda Config.KeybTyp     ;SDI layout?
        bpl +
        jmp findUnused
+       ldx #0+9        ;'G#' note in lower octave
        lda shiftsi
        bne +
        jmp norHkey
+       jsr repeatex
        lda stptick     ;decrease highlight step
        cmp #1
        beq retpnt2
        dec stptick
        inc display.hilrefr+1 ;refresh step-highlighting
retpnt2 rts
        .pend

jkeysub .proc
        ldx #0+11       ;'A#' note in lower octave
        lda shiftsi
        bne +
        jmp norJkey
+       jsr repeatex
        lda stptick     ;increase highlight step
        cmp #$10
        beq nothinc
        inc stptick
        inc display.hilrefr+1 ;refresh step-highlighting
nothinc rts
        .pend

incchor=lkeysub.incchor
lkeysub .proc
        ldx #12+2       ;'C#' note in upper octave at lower row
        lda shiftsi
        bne +++         ;check SHIFT
        lda Config.KeybTyp     ;SDI layout?
        bmi ++
chJanko cmp #$2C
        bne +
        ldx #12+4
+       jmp norLkey
+       jsr repeatex
        jmp ctrler.norctrl
+       lda Config.KeybTyp     ;SDI layout?
        bpl incchor
        lda #4          ;"load tune" menupoint
        sta menu.menucurpos
        jmp f7er.menuer
incchor jsr repeatex    ;check & handle repeat counter
incchr2 lda display.selchor+1
        cmp #MAXCHORDAMOUNT-2 ;$3e ;to be safe about insertion length in last chord, it's $3E not $3F
        bne +
        rts
+       inc display.selchor+1 ;SHIFT+K - increase chordnumber
        .cerror chdzero & 0 ;dependency hack
        .pend
tblhome=chdzero.tblhome
chdzero .proc
        lda #3          ;chordwindow number
tblhome ldx #0
        jmp chthome
        .pend

nkeysub .proc
        lda Config.KeybTyp     ;SDI-layout?
        bmi +
        ldx #0+10       ;'A' note in lower octave
        lda shiftsi
        ora cbmsig
        beq norNkey
+       jsr repeatex
        lda cbmsig
        beq InsName
        inc display.insrefr+1 ;display setting/result
        lda #$40        ;toggle note/number-mode of chord-table
        jmp commonsubs.eorconf
InsName ;rename instrument
        inc namemode    ;tell keyhandler it's instrument naming mode
        lda #instnamelength-1
        sta compzvar4
        lda #2 ;instrument-window undo-type
        sta UndoType
        jsr SaveUndo.SavInsU
        lda #<vidram+$20
        ldx #>vidram+$20
        jmp menu.setyfl2
        .pend


;value entry-point for normal simple notes
;------------------------------------------
okeysub .proc
        ldx #12+12+3    ;'D' note in 2nd upper octave
        lda #12+2       ;'C#' note in upper octave at lower row
        bne chkDMCk     ;jump
        .pend

norGkey lda #0+8        ;'G' note in the lower keyboard octave
        .byte $2c       ;skip
norHkey lda #0+10       ;'A' note in lower octave
        .byte $2c       ;skip
norJkey lda #0+12       ;'H' note in lower octave
        .byte $2c       ;skip
norLkey lda #12+3       ;'D' note in upper octave at lower row
        .byte $2c       ;skip
norPkey lda #12+4       ;'D#' note in upper octave at lower row
        .byte $2c       ;skip
norRkey lda #$ff        ;no sound for key R in DMC mode
        .byte $2c       ;skip
norSkey lda #0+3        ;'D' note in lower octave
        .byte $2c       ;skip
norTkey lda #0+7        ;'F#' note in lower octave 
        .byte $2c       ;skip
norWkey lda #0+2        ;'C#' note in lower octave - for DMC mode
        .byte $2c       ;skip
norYkey lda #0+9        ;'G#' note in lower octave - for DMC mode
        .byte $2c       ;skip
norSdot lda #12+5       ;Y=E E in the higher keyboard octave

chkDMCk bit Config.KeybTyp     ;check if FC/GMC layout (with only 1 piano-key octave in the middle)
        bvc +           ;if normal 2-row keyboard (not DMC), don't disturb X
        tax             ;but in DMC mode overwrite X with the alternative in Accu
+       jmp datawriter.notecol

ukeysub .proc
norUkey ldx #12+12      ;'H' (B) note in upper octave
        lda #0+11       ;'A#' note in lower octave
        bne chkDMCk     ;jump
        .pend

;------------------------
norMkey ldx #0+12       ;'H' note in lower octave
norNkey 
norXkey 
norZkey 
norNote jmp datawriter.notecol
;------------------------------------------
mkeysub lda Config.KeybTyp     ;SDI layout?
        bpl norMkey
        lda cbmsig
        bne +
        jmp keyCsub.cpyStrt
+       jmp vkeysub.paste
        
xkeysub .proc
        ldx #0+3        ;'D' note in lower octave
        lda cbmsig
        beq norXkey
CbmXkey jsr repeatex
        jsr SaveUndo
        lda curwind
        bne +           ;pattern-window?
        jmp cutandcopy  ;cut patterndata from cursorposition, copy cut data to buffer
+       cmp #2          ;cut instrument? (copy to buffer, then delete)
        bne keyxret     ;instrument-window?
        jsr CopyIns
        lda inszptr+0
        sta decozptr+0  ;won't interfere, because decozptr is only used when compressing/decompressing
        lda inszptr+1
        sta decozptr+1
        jsr EmptIns
        jsr commonsubs.instinit
keyxret rts
        .pend

inctmpp=ykeysub.inctmpp
ykeysub .proc
        ldx #12+10      ;'A' note in upper octave
        lda shiftsi
        beq norYkey
        jsr repeatex
inctmpp lda display.seltemp+1 ;increment tempoprg number
        cmp #MAXTEMPOPRAMOUNT-2;$3e ;just $3e, $3f wouldn't be well recognized in length at insertion
        bne +
        rts
+       inc display.seltemp+1
        .cerror tmpzero & 0 ;dependency hack
        .pend
tmpzero .proc
        lda #4          ;tempowindow number
        jmp tblhome
        .pend

zkeysub .proc
        ldx #0+1        ;'C' note in lower octave
        lda cbmsig
        beq +
        jsr repeatex
        jmp Undo
+       lda shiftsi
        bne decAdva
        lda Config.KeybTyp     ;SDI layout? ($88)
        bpl norZkey
        jsr repeatex
        jmp spacer.shifSpc
decAdva jsr repeatex
        lda Config.autoadv
        beq zkeyret
        dec Config.autoadv
zkeyret rts
        .pend

pkeysub .proc
        lda cbmsig
        beq +
;        bit PWRESETSW
;        bpl zkeysub.zkeyret
        jsr repeatex
        jsr commonsubs.setinsptr ;toggle pulse-reset on instrument-notestart
        ldx #0
        jsr commonsubs.instpt1
        eor #$40
        jsr commonsubs.instpt2
        jmp commonsubs.instinit.goirefr
+       ldx #12+12+5    ;'E' note in 2nd upper octave
        lda shiftsi
        bne +
        jmp norPkey
+       lda Config.KeybTyp     ;SDI-Layout?
        bmi goPulWi
        lda curwind
        beq +           ;pattern-window?
goPulWi lda #2          ;pulsewidth-table
        jmp keyFsub.goInsWi
+       ldx #PORTAMFX   ;autoportamento in note column
        jmp datawriter.notecol
        .pend

qkeysub .proc
        ldx #$ff        ;preset X - amount to add to note (decrease by 1)
        lda shiftsi
        bne transdn
        ldy cbmsig
        beq norQkey
        ldx #$f4        ;if CBM/Control key is on, transpose a whole octave down
transdn jsr repeatex    ;check & handle repeat counter
        jsr SaveUndo
        jmp transptn    ;transpose current pattern from cursorposition down (insdelcopy.inc)
norQkey bit Config.KeybTyp     ;check if FC/GMC layout (with only 1 piano-key octave in the middle)
        bvc +
        ldy #1          ;in DMC mode Q key places 'NOP'
        jmp datawriter.writevalue
+       ldx #12+1       ;'C' note in upper octave
        jmp datawriter.notecol
        .pend

rkeysub .proc
        ldx #RINGONFX   ;RING-BIT ON to note-column
        lda shiftsi
        bne wrRing
        lda cbmsig
        beq +
        ldx #RINGOFFX   ;RING-BIT OFF to note-column
wrRing  jmp datawriter.notecol
+       ldx #12+6       ;'F' note in upper octave
        jmp norRkey
        .pend

skeysub .proc
        ldx #SYNCONFX   ;SYNC-BIT ON to note-column
        lda shiftsi
        beq normalS
        lda curwind
        beq +           ;pattern-window?
        lda #0          ;main instrument-setting panel
        jmp keyFsub.goInsWi
+       lda Config.KeybTyp     ;SDI layout?
        bpl wrSync
        lda #3          ;"save tune" menupoint
        sta menu.menucurpos
        jmp f7er.menuer
normalS lda Config.KeybTyp     ;SDI layout?
        bpl +
        jsr repeatex
        jmp ctrler.shiCtrl
+       lda cbmsig
        beq +
        ldx #SYNCOFFX   ;SYNC-BIT OFF to note-column
wrSync  jmp datawriter.notecol
+       ldx #0+2        ;'C#' note in lower octave
        jmp norSkey
        .pend

dectmpp=tkeyer.dectmpp
tkeyer .proc            ;T key handler
        ldx #12+8       ;'G' note in upper octave
        lda cbmsig      ;if cbm+F, toggle single/funktempo of subtune
        beq +
;        bit TEMPOPRGSUPP
;        bpl zkeysub.zkeyret ;keyrett
        jsr repeatex    ;check & handle repeat counter
        jsr commonsubs.calcsubsp1
        eor #$80        ;toggle subtune-funktempo
        jmp setstm2
+       lda shiftsi
        bne +
        jmp norTkey
+       jsr repeatex
        lda Config.KeybTyp     ;SDI layout?
        bpl dectmpp
        jmp f7er.swtotmp
dectmpp lda display.seltemp+1
        cmp #1
        bne +
        rts
+       dec display.seltemp+1
        jmp tmpzero
        .pend

wkeysub .proc
        ldx #1          ;preset X - amount to add to note (increase by 1)
        lda shiftsi
        beq +
        lda Config.KeybTyp     ;SDI-layout?
        bmi goWfTab     ;if shift pressed, transpose up halfnote
        lda curwind
        beq transup     ;patternwindow?
goWfTab lda #1          ;waveform-arp. table
        jmp keyFsub.goInsWi
+       lda cbmsig
        beq +           ;if cbm-key pressed, transpose up 1 octave
        ldx #12         ;amount to add to note - if CBM pressed, an octave (12 halftones)
transup jsr repeatex    ;check & handle repeat counter
        jsr SaveUndo
        jmp transptn    ;transpose current pattern from cursorposition down (insdelcopy.inc)
+       ldx #12+3       ;'D' note in upper octave
        jmp norWkey
        .pend

vkeysub .proc
        lda cbmsig
        beq norVkey
paste   jsr repeatex
        jsr SaveUndo
        lda curwind
        bne +           ;if not patternwindow
        jmp pastebuf    ;jsr pastebuf  ;paste buffer data starting with cursorposition in target pattern
+       cmp #1
        bne +           ;orderlist?
        jmp pasteseq    ;paste sequence-buffer content into orderlist cursor-position
+       cmp #2
        bne vkeyret     ;paste instrument if it's instrument window
        jsr PasteIn
        jmp commonsubs.instinit
vkeyret rts
norVkey ldx #0+6        ;'F' note in lower octave
        lda shiftsi
        beq +
        ldx #$66        ;Vibrato into note column
+       jmp datawriter.notecol
        .pend

atkeysub ldx #12+12+6   ;'F' note in 2nd upper octave
        lda #$ff        ;no sound for @ key in DMC mode
        jmp chkDMCk
        
asterisk ldx #12+12+8   ;'G' note in 2nd upper octave
        lda #12+7       ;'F#' note in upper octave at lower row in DMC mode
        jmp chkDMCk

findUnused .proc ;find and enter 1st unused empty pattern after used patterns
        jsr keyhandler.repeatex
        lda curwind
        cmp #1
;       bmi PtEmpty     ;pattern-window?
        bne retEmpt     ;orderlist?
        jsr commonsubs.cSeqLop ;check if loop-position under cursor, Carry=1 if it is
        bcs retEmpt
        jsr commonsubs.seqaddb
        cmp #$fe        ;sequence-delimiter?
        bcc +
        jsr commonsubs.seqInst ;insert one place before delimiter (without advancing cursor)
+       jsr commonsubs.getcurptn
        cpy #seqlength-1 ;don't allow setting pattern-number automatically at the very end of the sequence
        bcs retEmpt
        lda firstEmptyPtn
        jsr commonsubs.seqaddc
        jmp commonsubs.setEmpty ;set number of 1st empty-pattern given in Accu
retEmpt rts
;PtEmpty lda firstEmptyPtn
;        ldy subwpos1+0
;        jmp commonsubs.seleptn
        .pend

        .section data
firstEmptyPtn .byte 4   ;set after loading/depacking/initializing a tune, refreshed when patterns are disturbed
        .send
        

;=============================CONTROLLER KEYS===================================
keyrett rts

pluser .proc            ;plus (+) key handler
        jsr repeatex    ;check & handle repeat counter
        lda menumode
        beq norplus
        lda menu.menucurpos ;signs pattern-length setting
        cmp #menu.ptlenmenupt ;pattern-length setter menupoint's number
        bne +     ;if no patternlength-setting
        jmp menu.IncPtln ;increase pattern-length
+       .if (SID_AMOUNT>=2)
        cmp #menu.SID2Amenupt ;SID2 address setter menupoint's number
        bne + ;norplus
        jmp menu.SID23set.IncSID2address
        .fi
+       .if (SID_AMOUNT>=3)
        cmp #menu.SID3Amenupt ;SID3 address setter menupoint's number
        bne norplus
        jmp menu.SID23set.IncSID3address
        .fi

norplus lda shiftsi
        beq +          ;check SHIFT
        lda curwind
        bne incinst     ;if pattern-window, inc/dec pattern-number
        ldx subwpos1+0  ;current track
        lda curptn1,x
        cmp #maxptnamount
        bcs kyrettb 
        inc curptn1,x
kyrettb rts
+       lda cbmsig
        beq defaPlus
        lda Config.KeybTyp     ;SDI layout?
        bpl +
        jmp dotter.incSubt
+       jmp commonsubs.incOctave ;f8er.incOctave
defaPlus lda curwind
        cmp #3
        bmi incinst     ;if ptn/orderlist/instrument window, no need for shift to +/- to select instrument
        bne +           ;if chordwindow
        jmp lkeysub.incchr2 ;incchor        ;increment chordnumber

+       cmp #4
        bne keyrett
        jmp inctmpp     ;increment tempoprogram

incinst lda display.selinst+1
        cmp #maxinstamount-1  ;minus 1, because last instrument is the buffer for instrument copying
        beq keyrett           ;nothinc ;noincis
        inc display.selinst+1 ;SHIFT + PLUS - increase inst.number
        jmp commonsubs.instinit    ;jsr instinit

        .pend


;----------------------
minuser .proc         ;minus (-) key handler
        jsr repeatex  ;check & handle repeat counter
        lda menumode
        beq norminu
        lda menu.menucurpos ;signs pattern-length setting
        cmp #menu.ptlenmenupt ;pattern-length setter menupoint's number
        bne +
        jmp menu.DecPtln ;decrease default pattern-length
+       
        .if (SID_AMOUNT>=2)
        cmp #menu.SID2Amenupt ;SID2 address setter menupoint's number
        bne + ;norminu
        jmp menu.SID23set.DecSID2address
        .fi
+       .if (SID_AMOUNT>=3)
        cmp #menu.SID3Amenupt ;SID3 address setter menupoint's number
        bne norminu
        jmp menu.SID23set.DecSID3address
        .fi

norminu lda shiftsi
        beq +           ;check SHIFT
        lda curwind
        bne decinst     ;if pattern-window, inc/dec pattern-number
        ldx subwpos1+0  ;current track
        lda curptn1,x
        cmp #1+1
        bcc kyrettc 
        dec curptn1,x
kyrettc rts
+       lda cbmsig
        beq defMinu
        lda Config.KeybTyp     ;SDI layout?
        bpl +
        jmp commasub.decSubt
+       jmp commonsubs.decOctave ;f7er.decOctave
defMinu lda curwind
        cmp #3
        bmi decinst
        bne +
        jmp decchor

+       cmp #4
        bne kyrettc     ;pluser.kyrettb ;retpntm ;nodecis
        jmp dectmpp

decinst lda display.selinst+1
        cmp #1
        beq kyrettc     ;pluser.kyrettb; retpntm ;nodecis
        dec display.selinst+1 ;SHIFT + MINUS - decrease inst.number
        jmp commonsubs.instinit
        .pend


;-------
equaler .proc           ;negate value in instrument-tables or chordtable
NegateVal
        jsr keyhandler.repeatex
        lda curwind
        cmp #2          ;instrument-window?
        bne NegChrd
NegItbl ldx subwpos1+2  ;current instrument-subwindow
        beq endequa
        jsr commonsubs.setinsptr ;set instrument's memory basepointers
        jsr commonsubs.getitbi   ;get instrument-table position to X
        jsr commonsubs.instpt1   ;read instr.table data
        cmp #$FF        ;don't disturb table-delimiter
        bcs endequa
        inx
        jsr commonsubs.instpt1 ;read instr.table data 2nd column
        eor #$FF
        ;clc            ;C was set to 0 previously by CMP command
        adc #1
        jsr commonsubs.instpt2 ;write instr.table data 2nd column
        jmp commonsubs.instinit.goirefr
NegChrd cmp #3          ;chord-window?
        bne endequa
        jsr commonsubs.getctpos ;get chordtable-position under cursor (position in X, value in A)
        bmi +
        cmp #$7E        ;don't disturb end-signals
        bcs endequa
+       eor #$FF        ;negate chord-value (2's complement conversion)
        ;clc            ;C was set to 0 previously by CMP command
        adc #1
        sta CHORDS,x
endequa rts
        .pend


;----------------------
spacer .proc            ;SPACE key handler
        jsr repeatex
        lda shiftsi
        bne shifSpc
        jmp cbmspc      ;if SHIFT not pressed
shifSpc lda curwind
        bne chorder     ;patternwindow? play patterns from cursorposition
        jsr f3play      ;activate pattern-playing
        jsr playadapter.continue ;restore played instruments
playpos ldx #CHN_AMOUNT-1
rowplop lda prowpos,x   ;first displayed pattern-row in track
        clc
        adc subwpos3+0  ;cursor Y position in pattern-editor
        ldy curptn1,x   ;currently selected pattern on track
        cmp ptnlength,y
        bcs nocurp
        sta playadapter.ptrowpos,x
.if (SID_AMOUNT==1)
        lda ptncurs,x   ;-Not from 0 position, but from cursor position
        clc
        adc ptnMpos,x
.elsif (SID_AMOUNT>=2)
        cmp #0
        beq endrowg
        sta zpvari0
        lda PPTRLO,y ;refresh memory positions (ptncurs,ptnMpos) on track (not from displayer)
        sta ptnzptr+0
        lda PPTRHI,y
        sta ptnzptr+1
        ldy #0
-       lda (ptnzptr),y ;check note-column
        bpl +
        iny
        lda (ptnzptr),y ;check instrument-column
        bpl +
        iny
        lda (ptnzptr),y ;check FX-number column
        and #$e0
        bne +
        iny
+       iny
        dec zpvari0
        bne -
        tya
endrowg 
.fi
        ldy playadapter.mul7chn,x
        sta player.PTNPOS,y
nocurp  dex
        bpl rowplop
        rts

chorder cmp #1          ;orderlist?
        beq olspace     ;if orderlist
        cmp #2          ;instrument-window?
        bne keyret3
;        bit GATEOFFPTR  ;.if GATEOFFPTR_ON
;        bpl keyret3
        jsr commonsubs.setinsptr
        jsr commonsubs.getitbi ;get current instrument table row pointer to X
        tay
        lda subwpos1+2  ;instrument-subwindow
        beq keyret3     ;if main instrument settings not tables
        clc
        adc #$0c-1      ;GATEOFF pointers in instrument-data
        tax             ;position
        jsr commonsubs.instpt1 ;check current gate-off pointer to decide on action
        sta icurpst+1
        beq +           ;if gateoff pointer was zero, set it to current position
        ;gateoff-pointer nonzero: zero if cursor is at it, otherwise modify
icurpst cpy #selfmod    ;position of current data under cursor
        bne +
        ldy #0
+       tya
        jmp commonsubs.instpt2

olspace                 ;set playing startpositions in orderlist
        lda cbmsig      ;check if both Shift and CBM are pressed
        beq allmark
plymark ldx #CHN_AMOUNT-1 ;set playing startpositions to currently played positions
-       ldy playadapter.mul7chn,x
        lda player.SEQPOS,y
        sta playadapter.olstart,x
        dex
        bpl -
        inc playadapter.seqrefr
        rts
allmark jsr commonsubs.getcurptn ;get orderlist memory index under cursor to A and Y
        ldx #CHN_AMOUNT-1
-       sta playadapter.olstart,x
        dex
        bpl -
        rts

cbmspc  lda cbmsig
        beq chSDIspc    ;if SHIFT and CBM both 'unpressed'
        jsr commonsubs.getcurptn ;get orderlist memory index under cursor to A and Y
trkmark ldx subwpos1+1  ;track number (Y-position) in orderlist
        sta playadapter.olstart,x
        inc playadapter.seqrefr
        rts

chSDIspc lda Config.KeybTyp    ;SDI layout?
        bpl normspc
        jmp deleter.delerow

normspc lda jammode+1   ;toggle edit/jam mode
        eor #1
        sta jammode+1
keyret3 rts
        .pend


;----------------------
coloner .proc           ; (:) colon key handler
        ldx #12+4       ;'D#' note in upper octave at lower row
        lda shiftsi
        bne ++
        lda Config.KeybTyp
        cmp #$2C ;Janko-layout?
        bne +
        ldx #12+6
        jmp datawriter.notecol.noteco2
+       jmp norSdot     ;musical note for : key
+       jsr repeatex
        jsr commonsubs.calcsubsp1 ;decrement subtune-funktempo (the right one)
        bmi semicol.retpnts ;if not funktempo
        lda SUBTUNES+1,y
        sec
        sbc #1
        bvs semicol.retpnts
        jmp setstmp
        .pend

;----------------------
setstmp=semicol.setstmp
semicol .proc           ; (;) semicolon key handler
        lda shiftsi
        beq norScol
        jsr repeatex
        jsr commonsubs.calcsubsp1 ;increment subtune-funktempo (the right one)
        bmi retpnts     ;+ ;if not funktempo
        lda SUBTUNES+1,y
        clc
        adc #1
        cmp #$c0
        bcs retpnts     ;+ ;second tempo cannot be smaller than $80
setstmp sta SUBTUNES+1,y
        sta TEMPOTBL+1
retpnts rts
norScol bit Config.KeybTyp     ;DMC mode?
        bvc retpnts
        ldx #12+6       ;F in the higher keyboard octave
        jmp datawriter.notecol
        .pend

;----------------------
setstm2=commasub.setstm2
commasub .proc          ; (,) comma key handler
        lda shiftsi
        beq cbmcomma
        lda Config.KeybTyp     ;SDI-layout?
        bpl defComa
        lda curwind
        beq +           ;pattern-window?
        jsr repeatex
        jmp decchor
+       ldx #$ff        ;preset X - amount to add to note (decrease by 1)
        jmp qkeysub.transdn
defComa jsr repeatex
        jsr commonsubs.calcsubsp1 ;decrement subtune-tempo (the left one)
        sec
        sbc #1
        bcc retpntQ
        bvs retpntQ
setstm2 sta SUBTUNES,y
        sta TEMPOTBL+0
retpntQ rts

cbmcomma lda cbmsig
        beq chkSDIcomma ;norComma ; musical note for , key?
        jsr repeatex
decSubt lda selsubt+1   ;decrement subtune
        beq retpntQ
        dec selsubt+1
        jmp playadapter.resetune

chkSDIcomma lda Config.KeybTyp ;SDI layout?
        bpl norComma
        jsr repeatex
        inc shiftsi
        jmp slasher.norSlas
        .pend

;----------------------
dotter .proc            ; (.) fullstop key handler
        lda shiftsi
        beq cbmdot
        lda Config.KeybTyp     ;SDI-layout?
        bpl defaDot
        lda curwind
        beq +
        jmp lkeysub.incchor
+       ldx #1         ;preset X - amount to add to note (increase by 1)
        jmp wkeysub.transup
defaDot jsr repeatex
        jsr commonsubs.calcsubsp1 ;increment subtune-tempo (the left one)
        and #$3f
        cmp #$3f
        beq commasub.retpntQ
        lda SUBTUNES,y
        clc
        adc #1
        jmp setstm2
cbmdot  lda cbmsig
        beq chSDIdot    ;normDot        ; musical note for . key?
        jsr repeatex
incSubt lda selsubt+1   ;increment subtune
        cmp #maxsubtuneamount ;maximum number of subtunes it $1f80 / ($80 * 3) = $15 = 21 , $80 left is sizelist
        beq commasub.retpntQ
        inc selsubt+1
        jmp playadapter.resetune
chSDIdot lda Config.KeybTyp ;SDI layout?
        bpl normDot
        jsr repeatex
        jmp slasher.norSlas
        .pend

norComma ldx #12+1      ;'C' note in upper octave at lower row
        lda Config.KeybTyp
        cmp #$2C ;Janko-layout?
        bne +
        ldx #12+3
+       jmp datawriter.notecol

normDot ldx #12+3       ;'D' note in upper octave at lower row
        lda Config.KeybTyp
        cmp #$2C ;Janko-layout?
        bne +
        ldx #12+5
+       jmp datawriter.notecol


;==============================================================================
norcrle=cursori.norcrle
cursori .proc           ;cursor-right key handler
        jsr shcbget     ;get shift/cbm key status (to make sure VICE cursorkeys work all times)
        jsr repeatex    ;check & handle repeat counter
        lda shiftsi
        beq norcrri     ;check SHIFT

norcrle lda curwind     ;-----------------------CURSOR-TO-LEFT------------------
        bne noptcle
        lda subwpos2+0  ;window 0 (ptn) branch
        beq +
        dec subwpos2+0
        rts
+       .if (SID_AMOUNT==1)
        lda subwpos1+0
        beq keyrtbb
        .fi
        lda #6
        sta subwpos2+0        
        jmp tabptbk

noptcle cmp #1
        bne noseqle     ;window 1 (orderlist) branch
        lda subwpos2+1
        beq +
        dec subwpos2+1
        rts
+       lda seqdpos+1
        beq +
        dec seqdpos+1
+       rts

noseqle cmp #2          ;instrument window branch
        bne chktmpl
        ldx subwpos1+2
        dec inswXbuf,x  ;dec subwpos2+2 ;window 2 (inst)
        bpl nodecle
        lda #0
        sta inswXbuf,x  ;sta subwpos2+2
        .if (SID_AMOUNT==1)
        jmp f5er.ptnwsel
        .fi
keyrtbb rts

chktmpl tax             ;X=window 3 or 4 (chord/tempotable)
        lda subwpos2,x
        beq tabtoin
        dec subwpos2,x
nodecle rts
tabtoin txa ;3/4
        asl ;6/8, carry=0
        sbc #5-1 ;1/3
        jmp keyFsub.goInsWi
        .pend

valuecont=norcrri.valuecont
seqstpr1=norcrri.seqstpr1
seqstpr2=norcrri.seqstpr2
norcrri .proc
        lda curwind     ;---------------------------------CURSOR-TO-RIGHT------
        beq ptncuri
        cmp #1
        bne noseqri     ;Orderlist is the current window?
        jsr commonsubs.setseqptr ;set the datapointer for current orderlist position
        lda subwpos2+1  ;test if $ff is at previous place or $fe is in current place in orderlist
        clc             ;shouldn't be after lsr?
        lsr
        adc seqdpos+1   ;add cursor position
        cmp #seqlength+1
        bcs endolri
        tay
seqstpr1 dey
seqstpr2 ldx #1
-       jsr commonsubs.seqaddb ;get data under orderlist cursor into A, index in orderlist is Y
        cmp #$FE        ;orderlist-end signal
        beq endolri
        dey             ;lets accessing the number (jump position) after $ff
        dex
        bpl -
        cmp #$ff        ;orderlist jumper loop-signal, jump position given after
        beq endolri
noseqend                ;orderlist (window 1) branch
        lda subwpos2+1
        cmp #39         ;check end of orderlist-display, and if reached maximum of orderlist
        beq +
        inc subwpos2+1
        rts
+                       ;lda seqdpos+1
        ;cmp #$ec        ;no need to check, insertion already limits patternlength
        ;beq endolri
        inc seqdpos+1
        ldy valuecont   ;to check whether the cursor advance was initiated from typing - provide continous typing
        beq endolri
        lda #38
        sta subwpos2+1
        lda #0
        sta valuecont
endolri rts
valuecont .byte 0       ;signs, if this is an autoadvance (seqstpr1) initiated from datawriter
        .pend

noseqri .proc
        cmp #2
        beq inswiri     ;instrument window

        tax             ;X=window 3 or 4 (chord/tempotable)
        .cerror chtmrig & 0 ;reference hack
        .pend

chtmrig .proc
        lda subwpos2,x
        cmp #1
        beq keyretb
        inc subwpos2,x
keyretb rts
        .pend

inswiri .proc
        ldx subwpos1+2
        lda inswXbuf,x  ;lda subwpos2+2 ;check window2 x-coord
        cmp #5
        beq +           ;jptowin  ;switch window with cursor movement, not needed
        inc inswXbuf,x  ;inc subwpos2+2
-       rts
+       cpx #0
        beq -
        cpx #3
        beq +
        jmp f5er.swtochd
+       jmp f7er.swtotmp
        .pend

ptncuri .proc
        lda subwpos2+0  ;window 0
        cmp #6          ;width of pattern
        bne noincle
        .if (SID_AMOUNT==1)
        lda subwpos1+0
        cmp #3-1
        bcc +
        jmp f7er.setInsW
        .fi
+       lda #0
        sta subwpos2+0
        jmp tabptfw

noincle inc subwpos2+0
        rts
        .pend

;==============================================================================
menucup .proc
        lda menu.helpmode+1 ;not going when help is selected
        bne endmcup
        ldx menu.menucurpos ;menu cursor goes up
        dex
        cpx #menu.firstcpos
        bcs +
        ldx #menu.lastcrpos
+       stx menu.menucurpos
endmcup rts
        .pend

cursrdn .proc           ;cursor-down key handler
        jsr shcbget
        jsr repeatex    ;check & handle repeat counter
        lda shiftsi
        bne norcrup     ;check SHIFT
        jmp norcrdn
        .pend

;------------------------------------------------Cursor UP branch--------------
ptcurup=norcrup.ptcurup
norcrup .proc
        lda menumode
        bne menucup
        lda curwind
        bne noptcup
        ;ptn-window branch
ptcurup lda subwpos3+0  ;common point to go up in pattern, used by pageup too
        beq nodecpu
        dec subwpos3+0
        rts
        .pend

upinptn .proc           ;go up in pattern (called from data-modifiers)
        lda #2          ;or 3?
        sta ptrdysi     ;notifies key-repeater to wait for pattern-redraw
        gne ptcurup     ;jump
        .pend

nodecpu .proc
        lda #2          ;or 3?
        sta ptrdysi     ;notifies key-repeater to wait for pattern-redraw
        ldx subwpos1+0  ;current tracknumber
        lda Config.confbits
        and #1          ;track-binding?
        beq ptscrup     ;if binding is off, scroll only current track
bindupp ldx #CHN_AMOUNT-1
-       jsr ptscrup
        dex
        bne -           ;X is 0 here, to do 1st track too...
ptscrup lda prowpos,x
        beq +
        dec prowpos,x
+       rts
        .pend

noptcup .proc
        cmp #1
        bne noupseq     ;up-key in orderlist
        lda subwpos1+1
.if (SID_AMOUNT==1)
        bne +
.elsif (SID_AMOUNT>1)
        beq inicrup
        sec
        sbc trkposi
        bne +
        dec trkposi
        jsr dAllMut ;refresh mute-solo colouring
        jmp +
inicrup rts ;lda #CHN_AMOUNT-3
        ;sta trkposi
.fi
        lda #CHN_AMOUNT-1
        sta subwpos1+1
        jmp commonsubs.sseqend
+       dec subwpos1+1
        jmp commonsubs.sseqend ;check if zero under cursor, set position to writable area

noupseq cmp #2          ;up in instrument-window?
        bne noiscup
        ldx subwpos1+2
        lda inswYbuf,x  ;lda subwpos3+2
        beq nodeciu
        dec inswYbuf,x  ;dec subwpos3+2 ;if cursor Y is not zero, simply go up in instrument subwindow
        rts
nodeciu ldx subwpos1+2  ;instrument-subwindow
        beq + ;isupret     ;if main instrument settings, scroll nothing
        lda wfarpos-1,x ;X is 1..3 here, this trick solves it to be 0..2 index
        beq isubwup ;isupret
        dec wfarpos-1,x ;scroll up corresponding instrument-table
isupret rts
+       jmp nkeysub.InsName
isubwup lda inswXbuf,x
        cpx #1
        bne +
        cmp #0
        beq +
        sec
        sbc #1
        cmp #4
        bne +
        sbc #1
+       sta inswXbuf-1,x
        jmp ctrler.tbinsbk

noiscup tax             ;X is 3 or 4 here (chordtable/tempotable)
        lda subwpos1,x  ;up in tempo/chordwindow
        beq chscrup
        dec subwpos1,x
        rts
chscrup                 ;scroll up in chord-window
        lda chorpos-3,x
        beq + ;retscru
        dec chorpos-3,x
retscru rts
+       dec curwind ;4->3, 3->2
        cpx #3
        bne +
        lda #4
        sta inswXbuf+0 
        lda #0
        sta subwpos1+2 ;subwindow
+       rts
        .pend

;-----------------------------------------------Cursor-Down Branch-------------
norcrdn .proc
        lda menumode
        bne menucdn
        lda curwind     ;no shift for CURSOR-Down
        bne noptcdn     ;if not ptn-window, go testing which
        ;geq godownp    ;no-op
        .pend

        ;pattern window
ptcurdn .proc
        jsr commonsubs.compcur ;common point to go down in pattern, used by pagedown,return,etc.
        beq +           ;if cursor is at last row of pattern
        lda subwpos3+0  ;Y position in pattern-window
        cmp #25-1       ;max. amount of pattern-rows
        beq noincpu     ;if cursor is at last row of pattern-screen
        inc subwpos3+0
+       rts
        .pend

godownp .proc ;go down in pattern (called from data-modifiers)
        lda #2          ;or 3?
        sta ptrdysi     ;notifies key-repeater to wait for pattern-redraw
        gne ptcurdn     ;jump
        .pend

menucdn .proc
        lda menu.helpmode+1 ;not going when help is selected
        bne endmcdn
        ldx menu.menucurpos
        inx
        cpx #menu.lastcrpos+1
        bcc +
menpini ldx #menu.firstcpos
+       stx menu.menucurpos
endmcdn rts
        .pend

noincpu .proc           ;scroll down in current track
        lda #2          ;or 3?
        sta ptrdysi     ;notifies key-repeater to wait for pattern-redraw
        ldx subwpos1+0  ;current tracknumber to X
        lda Config.confbits
        and #1          ;track-binding?
        beq ptscrdn     ;if no binding, scroll track individually
        ldx #CHN_AMOUNT-1
-       jsr ptscrdn
        dex
        bne -           ;leave X 0 here, and do the job for track 1 too
ptscrdn ldy curptn1,x   ;scroll down indexed patten (if possible)
        lda ptnlength,y
        cmp prowpos,x
        beq notrscr
        inc prowpos,x
notrscr rts
        .pend

noptcdn .proc
        cmp #1
        bne noseqdn     ;orderlist window
olidown lda subwpos1+1
.if (SID_AMOUNT>1)
        sec
        sbc trkposi
.fi
        cmp #3-1
        bcc +
.if (SID_AMOUNT>1)
        lda subwpos1+1
        cmp #CHN_AMOUNT-1
        bcs retptcd
        inc trkposi
        jsr dAllMut ;refresh mute-solo colouring
        jmp +
retptcd rts ;lda #0
        ;sta trkposi
.fi
        lda #0
        sta subwpos1+1
        jmp commonsubs.sseqend
+       inc subwpos1+1
        jmp commonsubs.sseqend ;check if zero under cursor, set position to writable area

noseqdn cmp #2
        bne noiscdn     ;instrument window here
        jsr commonsubs.setinsptr
        jsr commonsubs.getitbi ;get current inst.table row memory-position index to X
        jsr commonsubs.instpt1
        ldx subwpos1+2  ;current subwindow
        beq +
        cmp #$ff        ;table endsignal under cursor!!!
        beq isubwdn ;isdnret     ;if endsignal, not going further
+       lda inswYbuf,x  ;lda subwpos3+2 ;cursor-Y coordinate in instrument-window
        cmp iswmaxy,x   ;detect current instrument-subwindow cursor-Y end-reach
        beq noinciu
        inc inswYbuf,x  ;inc subwpos3+2
-       rts
isubwdn cpx #3
        beq -
        lda inswXbuf,x
        cpx #0
        bne +
        clc
        adc #1
+       sta inswXbuf+1,x
        jmp ctrler.tbinsfw

iswmaxy .section data
        .byte 3,7,3,3   ;maximum Y-coordinates of instrument-windows
        .send data

noinciu ldx subwpos1+2  ;current instrument-subwindow
        bne +           ;check if main instrument settings
        lda inswXbuf,x
        cmp #4
        bcc isubwdn
        jmp f5er.swtochd
+       inc wfarpos-1,x ;X is 1..3 here, this trick solves it to be 0..2 index
isdnret rts

noiscdn cmp #3
        bne nochddn     ;chord window
        jsr commonsubs.getctpos ;check if chordlength has been reached
        bmi +           ;in chordtable, more than $80 is allowed
        cmp #$7e        ;if chord end/loop, no advance
        bcc +
        jmp f7er.swtotmp
+       lda subwpos1+3
        cmp #12
        beq chscrol
        inc subwpos1+3
        rts
chscrol inc chorpos     ;scroll down chord window
        rts

nochddn cmp #4
        bne keyretc
        jsr commonsubs.gettpos
        bmi keyretc     ;if tempoprogram endsignal, no advance
        lda subwpos1+4  ;tempowindow
        cmp #3
        beq tmscrol
        inc subwpos1+4
        rts
tmscrol inc temppos     ;scroll down tempo wondow
keyretc rts
        .pend

;==============================================================================
ctrler .proc            ;CONTROL (TAB) key handler
        jsr repeatex    ;check & handle repeat counter
        lda shiftsi
        beq norctrl     ;check SHIFT
shiCtrl lda curwind
        beq tabptbk
        cmp #2          ;check for another window
        bne noitabb
tbinsbk dec subwpos1+2  ;tabulate back instrument-window
        bpl nonegit
        lda #3
        sta subwpos1+2
nonegit rts ;jmp tabinsw

noitabb cmp #3          ;chordtable/tempotable
        bmi nonegit
        tax
        jmp cursori.tabtoin

norctrl lda curwind     ;normal (without Shift) CTRL/TAB key branch
.if (SID_AMOUNT==1)
        beq tabptfw
.elsif (SID_AMOUNT>=2)
        bne chisctl
        lda cbmsig
        beq tabptfw
chngSID lda #0
        ldx subwpos1+0 ;/trkposi
 .if (SID_AMOUNT>=3)
        cpx #3*2
        bcs setcurs
        lda #3*2
 .fi
        cpx #3
        bcs setcurs
        lda #3
setcurs sta trkposi
        clc
        adc #1
        sta subwpos1+0
        inc playadapter.seqrefr
        jmp commonsubs.chkcury
.fi
chisctl cmp #2          ;TAB in instrument-window?
        bne noistab
tbinsfw ldx subwpos1+2  ;instrument-subwindow
        cpx #3
        bne nomaxit
        ldx #$ff
nomaxit inx
        stx subwpos1+2  ;tabulate instrument-window
        rts ;jmp tabinsw

noistab cmp #3
        bmi keyretd     ;chord-/tempotable
        lda #0
        sta curwind     ;simply go back to patternwindow
keyretd rts
        .pend

tabptbk .proc
.if (SID_AMOUNT==1)
        dec subwpos1+0  ;tabulate back pattern 1..3
        bpl nonegpt
        lda #3-1
        sta subwpos1+0
.elsif (SID_AMOUNT>1)
        lda subwpos1+0
        beq toend
        sec
        sbc trkposi
        bne +
        dec trkposi
        inc playadapter.seqrefr
        jsr dAllMut ;refresh mute-solo colouring
+       dec subwpos1+0
        jmp nonegpt
toend   lda #(SID_AMOUNT*3-3)
        sta trkposi
        inc playadapter.seqrefr
        jsr dAllMut ;refresh mute-solo colouring
        lda #CHN_AMOUNT-1
        sta subwpos1+0
.fi
nonegpt jmp commonsubs.chkcury ;check if cursor in pattern display-area
        .pend

tabptfw .proc
.if (SID_AMOUNT==1)
        ldx subwpos1+0  ;track in patternwindow
        cpx #3-1
        bcc nomaxpt
        ldx #$ff
nomaxpt inx
        stx subwpos1+0  ;tabulate pattern 1..3
.elsif (SID_AMOUNT>1)
        lda subwpos1+0
        sec
        sbc trkposi
        cmp #3-1
        bcc nomaxpt
        ldx subwpos1+0
        cpx #SID_AMOUNT*3-1
        bcs +
        inc trkposi
        inc playadapter.seqrefr
        jsr dAllMut ;refresh mute-solo colouring
        jmp nomaxpt
+       lda #0
        sta trkposi
        sta subwpos1+0
        inc playadapter.seqrefr
        jsr dAllMut ;refresh mute-solo colouring
        jmp +
nomaxpt inc subwpos1+0
.fi     
+       jmp commonsubs.chkcury ;check if cursor in pattern display-area, if outside, move it inside
        .pend

tabinsw .proc           ;instrument-window subwindows
;        lda #0         ;always go to zero position in table, safely in writeable part
;        sta subwpos3+2
;        sta subwpos2+2
        rts
        .pend
;-------------------------------------------------------------------
homer .proc             ;HOME key handler
        lda menumode
        beq +
        jmp menucdn.menpini
+       jsr repeatex
        ldx #0          ;prepare 0 value for cusror- and scroll position setting
        lda shiftsi
        beq +
        lda #1          ;orderlist
        sta curwind
fulhome jsr homeOl1
        jmp homeOl2
+       lda curwind     ;check which window we are in
        bne iwinhom     ;are we in pattern-window?
        lda subwpos2+0
        ora subwpos3+0  ;check both positions if zero
        bne smallho     ;if any cursorposition is not zero, small home (no scrolling up)
        ldy subwpos1+0  ;current track
        lda #0
        sta prowpos,y
        lda Config.confbits
        and #1          ;track-binding?
        beq +
        lda #0
        ldy #CHN_AMOUNT-1
-       sta prowpos,y
        dey
        bpl -
+       rts
smallho stx subwpos3+0
        stx subwpos2+0
homret1 rts

iwinhom cmp #1
        bne noseqho     ;are we in sequence/orderlist window?
        lda subwpos2+1
        beq homeOl2
homeOl1 lda #0
        sta subwpos2+1
        rts
homeOl2 lda #0
        sta seqdpos+1
        rts

noseqho cmp #2
        bne chthome     ;are we in instrument window (2)
        lda #0
        ldy subwpos1+2
        beq +           ;are we in instrument-main table?
        sta wfarpos-1,y ;zero instr.table scroll-position  (-1 is trick to lower X=1..3 to X=0..2)
+       sta inswYbuf,y  ;stx subwpos3+2 ;zero cursor Y-position in window 2 (instr.window)
        sta inswXbuf,y  ;stx subwpos2+2 ;zero cursor X-position in window 2
        rts
        .pend

chthome .proc
        tay
        txa
        sta subwpos1,y  ;zero chordtable cursor-position
        sta chorpos-3,y ;zero chordtable scroll-position (-3 lowers X indexing range)
        inc display.insrefr+1 ;to display it
        rts
        .pend

;-----------------------------------------------------------------------
autoFlw=bckarrow.autoFlw
bckarrow .proc
        lda cbmsig
        beq toggflw
        jsr repeatex
autoFlw lda #$04        ;toggle if follow-play is the default (setting gets saved to worktune)
        jmp commonsubs.eorconf
toggflw lda shiftsi
        beq ffwd
        jsr repeatex
        lda playadapter.playmod
        beq +
        lda playadapter.followp+1 ;don't set follow-play mode while stopped, it would disable pattern-editing
        eor #1
        sta playadapter.followp+1
+       rts
ffwd    cmp playadapter.playmod ;don't allow fast-forward if not playing the tune (jamming mode)
        beq + 
        lda PALNTSC
        eor #$ff
        and #1
        ;clc             ;Carry has been set in cmp
        adc #ffwdspeed ;how many times faster when fast-forward used
        sta IRQ.fastfwd+1 ;fast-forward ;cause irq to play single-speed routine more times in a frame
+       rts
        .pend

f7er .proc              ;F7 key handler
        jsr repeatex
        lda Config.KeybTyp     ;SDI-layout? ($88)
        bpl normaF7
        lda shiftsi
        bne +
        jmp commonsubs.incOctave 
+       jmp commonsubs.decOctave  
normaF7 lda menumode
        beq +
        inc menu.clearmenu ;initiate a menu-clearance (sign to main loop)
        rts
+       lda cbmsig
        bne swtotmp
        lda shiftsi
        bne menuer      ; go to MENU
        ;ldx #0
        ;lda curwind
        ;cmp #2
        ;beq swtoins
setInsW ldx #2
swtoins stx curwind
        rts
swtotmp lda #4          ;switch to tempotable
        sta curwind
        rts

menuer  lda #1          ;jmp packdepack
        sta menumode
        lda #0
        sta menu.menupoint ;init menu (nothing pressed yet)
        jmp menu.dispaut ;display author-info also when menu is invoked (added in SW-1.5)

        .pend

;-----------------------
f1er .proc              ;F1 key handler (reset and play whole tune from beginning)
        jsr repeatex
        lda Config.KeybTyp     ;SDI-layout? ($88)
        bpl + ;PlayBeg
        lda shiftsi
        beq + ;PlayBeg
        jmp spacer.shifSpc
+
.if (MIDI_support!=0)
        ; M64 NOTE: MAKE THIS CONFIGURABLE
        jsr midisubs.sendSt ;start-command to other MIDI-controlled sequencer
.fi
PlayBeg ldx #0 ;reset tune and play from beginning
        stx IRQ.fspdctr+1 ;start with 1st raster of course
        stx playadapter.maxrasti ;reset rastertime-measuring
        lda Config.confbits
        and #$04        ;follow-play is default?
        ora cbmsig
        beq +
        ldx #1
+       stx playadapter.followp+1 ;if commodore key is pressed together, follow-playing switches ON, else OFF
        lda #1
        sta playadapter.playmod
        jsr playadapter.ptploff ;switch off pattern-playing in player-code (playadapter.inc)
        lda shiftsi
        bne f2er
        lda Config.KeybTyp     ;SDI-layout? ($88)
        bmi playMrk
        inc inirequ+1
        rts
f2er ;F2 (shift+F1) key handler (play tune from orderlist markers)
playMrk ;sei
        inc player.StrtPos      ;tell initer it's not the beginning of the tune
        jsr playadapter.inisubb ;playadapter's init routine - sets subtune too in case there was subtune-jumping meanwhile :)
        dec player.StrtPos      ;set initer back to normal
        ;lda #1
        ;sta $d019
        rts
        .pend
;-----------------------
f4er=f3er.f4er
f3er .proc              ;F3 key handler (play current pattern from its beginning)
        jsr repeatex
        lda Config.KeybTyp     ;SDI-layout? ($88)
        bpl +
        lda shiftsi
        beq ++ ;pauseco
        jmp spacer.normspc ;toggle edit/jam
+       lda shiftsi
        beq f3play
f4er    ;F4 (Shift+F3) key handler (stop playback / ? continue (play/pause))
+
.if (MIDI_support!=0)
        lda #MIDI.StopSeqPlay
        jsr midisubs.sendMID ;start-command to other MIDI-controlled sequencer
.fi
pauseco jsr commonsubs.setFlwP
        ldx pmodstore   ;defualt X - value for possible continuation
        lda playadapter.playmod
        beq +
        sta pmodstore   ;store if 1 or 2 (song/patternplay) was previously, to resume at continuation
        ldx #0          ;altered X to 0 - value for stopping playback
        stx playadapter.jambuf+1
        stx playadapter.jamnote+1
        stx playadapter.followp+1
+       stx playadapter.playmod
        lda playadapter.playmod
        beq +           ;if playback is stopped
        .if (SID_AMOUNT>=2)
        jsr menu.WrSID2add
        .fi
        .if (SID_AMOUNT>=3)
        jsr menu.WrSID3add
        .fi
        jmp playadapter.continue ;continue paused playback, restore played instruments
+       jmp playadapter.pause    ;pause playback, store numbers of played instruments
        ;(these routines are in playadapter.inc)
pmodstore .byte 0       ;used to remember what playmode was before stopping, so continuation will be the same
        .pend

f3play .proc
        .if (SID_AMOUNT>=2)
        jsr menu.WrSID2add
        .fi
        .if (SID_AMOUNT>=3)
        jsr menu.WrSID3add
        .fi
        jsr commonsubs.setFlwP
        jsr commonsubs.resetimer
        lda #2
        sta playadapter.playmod
        jsr playadapter.ptplyon ;switch ON pattern-playing mode in player-code
        ldx #CHN_AMOUNT-1
-       ldy playadapter.mul7chn,x
        lda #0
        sta player.PTNPOS,y
        sta playadapter.skiptrk,x ;allow playing selected patterns
        ;lda #$ff
        sta playadapter.ptrowpos,x
        sta player.SPDCNT,y ;jmp INICNTR ;init speedcounters - ensure that player starts with rowread
        lda curptn1,x
        sta player.CURPTN,y ;jsr PTNPTR ;set played patterns to selected patterns
        bit TEMPOPRGSUPP
        bpl +
        lda player.TMPPTR,y
        sta player.TMPPOS,y ;so funktempo and tempoprogram would start deterministically on its 1st tempo
+       dex
        bpl -
        rts
        .pend
;-----------------------
f5er .proc              ;F5 key handler
        jsr repeatex
        lda cbmsig
        beq swtoseq
swtochd lda curwind
        sta reter.prevwin+1
        lda #3
        sta curwind
        rts
swtoseq lda shiftsi
        beq ptnwsel
        ;lda curwind
        ;cmp #1
        ;bne goorlis
        ;lda #0 ;prevwin
        ;jmp +
goorlis lda #1
+       sta curwind
.if (SID_AMOUNT>1)
        ldx trkposi
        inx
        stx subwpos1+1
.fi
        rts
ptnwsel ;lda menumode
        ;bne f5reter    ;don't select pattern-window, if menu is on
        lda curwind
        beq goorlis     ;pattern-window?
        cmp #2 ;inst./chord/tempo-table?
        bpl +
.if (SID_AMOUNT>1)
        ldx trkposi
        inx
        stx subwpos1+0
.fi
+       ldx #0          ;for pattern-editor
        stx curwind
        rts
        .pend
;=================================================================================
gkeySDI .proc
        jsr repeatex
        lda curwind     ;if pattern-window
        ora menumode    ;and menu not brought up
        bne retgSDI
        lda shiftsi
        ora cbmsig
        beq +
        jmp reter.carretn ;will put 'gate-on'
+       jmp reter.ptnRETU ;will put 'gate-off'
retgSDI rts
        .pend

reter .proc             ;return (enter) key handler
        jsr repeatex
        lda menumode
        beq +
        lda menu.menucurpos
        sec
        sbc #menu.firstcpos-1
        sta menu.menupoint ;will launch a menupoint to main loop (if other than 0)
retreter rts
+       lda curwind
        cmp #2          ;instrument window or above?
        bpl +
        jmp goposit     ;if not pattern or orderlist,simply a carreturn (except instrument main data)
+       bne carretn     ;if instrument-window, check, if last column in main instrument data

        ;instrument-window branch
        jsr commonsubs.setinsptr ;set instrument base/memory position to instpt1 and instpt2
        ldy subwpos1+2
        bne carretn     ;if not main instrument data
        lda inswXbuf,y  ;lda subwpos2+2
        cmp #4
        bmi jmpchrd     ;carretn ;if not last 2 columns in main instrument data
        ldx #0          ;set index to 1st byte of instrument basedata (control reg.)
        jsr commonsubs.instpt1
        ldx subwpos1+2
        ldy inswYbuf,x  ;ldy subwpos3+2 ;action depending on y coordinate
        ldx #0
        cpy #0
        bne +           ;if row 0
        eor #$08
        jmp commonsubs.instpt2 ;jsr instpt2
+       cpy #2
        bpl +           ;if not row 1
        eor #$04
        jmp commonsubs.instpt2
+       bne +           ;if not row 2
        and #$cf
        sta wrvibty+1
        jsr commonsubs.instpt1
        and #$30        ;mask out vibrato type bits ($10 and $20)
        clc
        adc #$10        ;roll vibrato type
        cmp #$40
        bmi wrvibty
        lda #0
wrvibty ora #selfmod
retiwr  jmp commonsubs.instpt2
+                       ;row4  - octave
        ldx #9
        jsr commonsubs.instpt1
        beq retreter    ;if zero (+0), don't change direction
        sec
        sbc #1
        eor #$ff
        jmp commonsubs.instpt2

jmpchrd cmp #2          ;column 2/3?
        bmi carretn
        lda inswYbuf,y  ;jump to chord when cursor is over instrument's default chord-number
        cmp #3
        bne carretn
        ldx #SWI_DEFCHORD_POS
        jsr commonsubs.instpt1
goChrdA beq retucho ;chord 0 shouldn't be allowed
        cmp #MAXCHORDAMOUNT
        bcc +
retucho rts
+       sta display.selchor+1
        lda curwind
        sta prevwin+1
        lda #3
        sta curwind     ;chord-window
        jmp keyhandler.chdzero

        ;simple carreturn
carretn ldy curwind     ;shift+return - a cursor position down
        bne retdown
        lda subwpos2+0
        bne retdown     ;patternwindow note column?
        ldx #$7d        ;gateon
        lda jammode+1
        bne retdown
        jmp datawriter.noterep

retdown lda #0
        cpy #2          ;instrument-window?
        bne +
        ldx subwpos1+2  ;instr.subwindow
        sta inswXbuf,x
+       cpy #3          ;chord-window?
        bne +
prevwin lda #selfmod 
        sta curwind
        rts
+       sta subwpos2,y
        jmp keyhandler.norcrdn


ptninsr lda shiftsi     ;insert gateon/gateoff into pattern
        bne carretn
        lda cbmsig
        beq +
        jmp cbmretu
+       lda Config.KeybTyp     ;SDI-layout? ($88)
        bpl ptnRETU
        jmp f3play      ;if SDI-layout, 'Return' in pattern-window is 'pattern-play'
ptnRETU lda subwpos2+0  ;column in track
        bne +           ;note-column?
        ldx #$7e        ;gateoff
        lda jammode+1
        bne retdown
        jmp datawriter.noterep
+       cmp #3          ;instrument-column?
        bmi goInsCh
        jsr commonsubs.setptnptr  ;prepare pattern pointers
        jsr commonsubs.getcurnote ;get note-data in current track/pattern at cursor position to A
        bpl rerete2     ;check if no second column
        inx
        jsr commonsubs.instpt1
        bpl rerete2     ;check if no third (fx) column
        inx
        jsr commonsubs.instpt1 ;fx-number
        bmi ptCaret
        cmp #$07        ;chord-BigFX?
        bne +
        inx
        jsr commonsubs.instpt1 ;fx-value (chord-number)
        jmp goChrdA
+       sec
        sbc #$70        ;is it chord-smallFX?
        bcc ptCaret
        jmp goChrdA     ;call chord
ptCaret lda #3
ptCare2 sta subwpos2+0  ;car-return in effect-columns
        jmp norcrdn
goInsCh jsr commonsubs.setptnptr  ;prepare pattern pointers
        jsr commonsubs.getcurnote ;get note-data in current track/pattern at cursor position to A
        bpl rerete2     ;check if no second column
        inx             ;second column
        jsr commonsubs.instpt1
        and #$7f
        geq rereter     ;if instrument is 0
        cmp #maxinstamount ;max number of instrument. Don't select instrument if bigger than $3e.
        bmi goInstA
chGoChd sec
        sbc #$70        ;is it chord-InstFX?
        bcc +
        jmp goChrdA     ;call chord
+       lda #1
        jmp ptCare2
rerete2 rts
goInstA ldx #2
        stx curwind
        sta display.selinst+1
        jmp commonsubs.instinit ;zero out instrument coordinates at instrument-change

goposit                 ;go to corresponding inst/tableposition
        cmp #1          ;orderlist?
        beq +
        jmp ptninsr     ;check if not orderlist
+       lda shiftsi
        beq +           ;if SHIFT pressed, all tracks are selected for orderlist position
        ldy #0
        jsr commonsubs.jmptopt
        ldy #1
        jsr commonsubs.jmptopt
        ldy #2
        jsr commonsubs.jmptopt
.if (SID_AMOUNT==2)
        jsr commonsubs.jmptopt_SID2
.elsif (SID_AMOUNT>=3)
        jsr commonsubs.jmptopt_SID23
.fi
        ldy subwpos1+1
        jmp postopt

+       lda cbmsig
        beq +
cbmretu ldx #CHN_AMOUNT-1          ;cbm+return - go to currently played patterns
-       ldy playadapter.mul7chn,x
        lda player.CURPTN,y ;help from player
        beq invptn
        cmp #maxptnamount+1
        bcs invptn      ;don't allow invalid patterns
        ldy playadapter.followp+1 ;if followplay is on, don't disturb pointers
        bne invptn
        sta curptn1,x   ;go to currently played patterns
        lda #0
        sta prowpos,x
invptn  dex
        bpl -
        jmp commonsubs.postop2

+       ldy subwpos1+1  ;orderlist row (track 1..3)
        jsr commonsubs.jmptopt ;jump to pattern under orderlist cursor
        ldy subwpos1+1
postopt sty subwpos1+0  ;track in patternwindow
rereter rts

        .pend


;=================================================================================
stopper .proc           ;RUN/STOP (ESC in VICE) handler
        jsr repeatex
        lda menumode
        beq +
        .if (SID_AMOUNT>=2)
        jsr menu.WrSID2add
        .fi
        .if (SID_AMOUNT>=3)
        jsr menu.WrSID3add
        .fi
        inc menu.clearmenu ;initiate a menu-clearance (sign to main loop)
        rts
+       lda Config.KeybTyp     ;SDI layout?
        bpl norStop
        lda curwind
        bne +
        jmp f7er.setInsW
+       ldx #0          ;in SDI mode return to pattern-window from anywhere with STOP (Esc)
        stx curwind
        rts
norStop ldx #0
        lda cbmsig
        beq +
        ldx #1
+       stx playadapter.followp+1 ;if commodore key is pressed together, follow-playing switches ON, else OFF
        jmp f4er

        .pend

;-----------------------
pgdncnt = slasher.pgdncnt
uparrow = slasher.uparrow

slasher .proc           ;slash (/) handler (page-up/dn)
        jsr repeatex
        lda Config.KeybTyp     ;SDI layout?
        bpl norSlas
        lda shiftsi
        beq +
        jmp IncFspd
+       ldx prevWin
        lda curwind
        cmp #2          ;instrument-window we are in?
        beq +
        sta prevWin
        ldx #2
+       stx curwind
        rts
prevWin .byte 0         ;window before entering instrument-editor
uparrow jsr repeatex 
norSlas ldx curwind
        lda #8          ;default page-up/dn amount is 8
        cpx #1
        bne pgupndn
        lda #4
        sta pgricnt+1
        sta pglecnt+1
        lda shiftsi
        beq pgrilp
pglelp  jsr norcrle
        dec pglecnt+1
pglecnt lda #0
        bne pglelp
        rts
pgrilp  jsr norcrri     ;orderlist-fast navigation
        dec pgricnt+1
pgricnt lda #0
        bne pgrilp
        rts

pgupndn ldy menumode
        bne ++
        cpx #4
        beq ++
+       cpx #2
        bne ++
        ldx subwpos1+2  ;check subwindow in instrument-window
        cpx #2
        bmi ++
+       lda #4          ;page-up/dn amount is 4 in short windows
+       sta pgdncnt+1
        sta pgupcnt+1
        lda shiftsi
        bne pgupper
        ;page-dn
pgdnlop jsr norcrdn
        dec pgdncnt+1
pgdncnt lda #0
        bne pgdnlop
        rts
pgupper ;page-up
pguplop jsr norcrup
        dec pgupcnt+1
pgupcnt lda #0
        bne pguplop
slasret rts
        .pend

;==============================================================================
deleter .proc           ;INST/DEL key handler
        lda cbmsig
        beq cInsert
        jsr repeatex
delerow lda curwind     ;C= + DEL - deletion (zeroing out) of note/ins&fx/entire row?
        beq +
        jmp inskeye.olinser ;insert entire column in orderlist
+       ora playadapter.followp+1 ;don't allow any deletions in followplay-mode
        bne delrret     ;only allow in patternwindow
        jsr SaveUndo
        jsr commonsubs.setptnptr ;set pattern pointers in advance based upon current track/pattern position
        jsr commonsubs.compcur   ;overwrites Y!
        beq delrret
        jsr commonsubs.getcurnote ;notevalue in A, positon in X (from pattern displayposition)
        ldy shiftsi
        bne +           ;if shift also pressed, zero out note column too
        ldy subwpos2+0  ;column in pattern
        bne ++          ;not note column?
        and #$80        ;delete note
        jsr commonsubs.instpt2
        jmp advdelr
+       and #$80
        jsr commonsubs.instpt2
        bpl advdelr     ;if note column only
+       ldy #0          ;delete nothing more if only note column exists
        jsr commonsubs.getdla2 ;getdelamount subroutine entry without Y setting
        jsr commonsubs.instpt1
        and #$7f
        jsr commonsubs.instpt2
        inx             ;deletion pointer after note column
        jsr delbackpt   ;deletes current patternrow, amunt given in Y, position in X
advdelr lda Config.autoadv
        beq delrret
        jmp keyhandler.godownp ;jsr godownp ;go down in pattern if autoadvance mode is ON
delrret rts

        ;ldy subwpos2+0  ;pattern-column
        ;beq advdelr     ;not note-column? Shift pressed?

cInsert lda shiftsi     ;check if 'Insert' instead
        beq +
        jmp inskeye

+       jsr repeatex
        lda curwind     ;branch is here when no shift or c= pressed
        bne delordl
;patternwindow delete row, rest moves back:
        lda playadapter.followp+1 ;don't allow any deletions in pattern in followplay-mode
        bne delret
        jsr SaveUndo
        jsr commonsubs.setptnptr ;set pattern pointers in advance based upon current track/pattern position
        jsr commonsubs.compcur   ;output: Accu holds pattern-rowposition under cursor, x-current track, y-current pattern
        beq +
delinpt ;delete one row in pattern, then append end of pattern to keep patternlength
        jsr commonsubs.getcurnote ;notevalue in A, positon in X (from pattern displayposition)
        jsr commonsubs.getdelamount ;to Y
        jsr delbackpt   ;deletescurrent patternrow, amunt given in Y, position in X
        ldy subwpos1+0  ;put a NOP at the end of pattern
        ldx curptn1,y   ;current pattern
        lda PPTRLO,x    ;set pattern-pointers to its beginning
        sta commonsubs.instpt2+1
        lda PPTRHI,x
        sta commonsubs.instpt2+2
        lda ptnsize,x   ;increase patternlength by 1 (size was increased already in insertptn)
        inc ptnsize,x
        tax
        dex
        lda #0          ;append zero to the end of the pattern
        jsr commonsubs.instpt2
        lda #$ff        ;append $ff endsignal to end of pattern
        inx
        jmp commonsubs.instpt2
+       ;end-of-pattern - reduce patternsize, delete last row
        ;ldy subwpos1+0 ;current track
        ;ldx curptn1,y  ;current pattern number to X
        lda ptnlength,y ;Y calculated in compcur already
        cmp #minptnlen  ;1
        beq delret      ;minptnr ;don't allow zero-length pattern
        lda subwpos3+0  ;if cursor at zero dislpay-position, don't delete, because no ptncurs calculated
        beq delret      ;minptnr
        jsr deleter.dellast
        ldy subwpos1+0
        ldx curptn1,y   ;current pattern number to X
        dec ptnlength,x ;decrease patternlength
        lda prowpos,y
        beq +
        lda subwpos3+0  ;cursor Y position in patternwindow
        cmp #13
        bpl +           ;minptnr ;if at position 0 or 1, don't perform decrease -focus should follow fast instead
        jmp nodecpu     ;scroll up pattern if possible to keep focus
+       jmp upinptn     ;and follow endsignal up with cursor and scroll
delret  rts
dellast                 ;delete last row of pattern
        jsr commonsubs.getlast ;get memo-position of last row to X
        jsr commonsubs.instpt1
        jsr commonsubs.getdelamount  ;get amount of data in that row
        jmp delbackpt ;jsr delbackpt ;delete data last row from X position with Y amount
        .pend

delordl .proc
        cmp #1
        beq +
        jmp delitbl     ;delete-key subroutine in orderlist
+       jsr SaveUndo
        jsr commonsubs.setseqptr ;get sequence pointers
        lda commonsubs.seqaddb+1
        sta oldelet+1
        sta oldeleb+1
        lda commonsubs.seqaddb+2
        sta oldelet+2
        sta oldeleb+2
        ldy #0
        jsr commonsubs.seqaddb
        cmp #$fe        ;check if endsignal at position 0
        bcs delret      ;don't allow lower than zero length sequence
        jsr commonsubs.cSeqLop ;check if loop-position under cursor, Carry=1 if it is
        bcs delret      ;don't allow deletion in loop-jumpvalue position
        jsr commonsubs.seqaddb ;get data under cursor
        cmp #$fe        ;check if endsignal
        bcc jusdele     ;if $fe/$ff under cursor, not delete, but backspace
        lda subwpos2+1
        cmp #20         ;below half of the screen - delete with seqdisplay movement to stay in scope
        bcs no0bspc     ;if cursor below position 20, get orderlist into focus
        lda seqdpos+1   ;if display position zero, no need to follow focus
        beq no0bspc
        dec seqdpos+1
        jmp backdel
no0bspc lda subwpos2+1
        sec
        sbc #2
        sta subwpos2+1
backdel dey             ;backspace delete position
jusdele tya
        tax
        inx
oldelet lda selfmodA,x  ;deletion loop
oldeleb sta selfmodA,y
        inx
        iny
        cpy #seqlength+1
        bne oldelet

chkjadd ldy #0          ;check if loop-address stays in range, decrease it if not
-       jsr commonsubs.seqaddb ;means: lda sequence,y
        cmp #$fe
        beq delret      ;if ends with $FE don't care about any jump-address
        cmp #$ff
        bne +           ;if $FF, check and set loop-address if needed
        tya
        sec
        sbc #1
        sta cmpjmp3+1
        iny             ;point to jump-address
        jsr commonsubs.seqaddb
        bmi delret      ;subtune-jumping doesn't cause endless loop
cmpjmp3 cmp #selfmod
        bcc delret      ;if jump-address in range
        lda cmpjmp3+1
        jsr commonsubs.seqaddc ;write sequence data at Y index with A
        rts
+       iny
        cpy #seqlength
        bne -
delret  rts

delitbl cmp #2
        beq +
        jmp delchrd     ;delete-key subroutine in instrument window
+       jsr SaveUndo
        jsr commonsubs.setinsptr ;set instrument's memory basepointers
        lda subwpos1+2  ;current instrument-subwindow
        beq delret      ;if main instrument settings, no insertion possible
        jsr commonsubs.getitbi
        sta stoinsp+1
        clc
        adc #tblwidth
        tay
        ;delete from instrument-table , 3 bytes at one step
        jsr commonsubs.instpt1
        cmp #$ff
        beq delret      ;if $ff under cursor, don't delete any further
-       lda (inszptr),y ;read
        jsr commonsubs.instpt2 ;write
        inx
        iny
        cpy #lastinstpt+1
        bne -
        ldx #2
        lda #0
-       dey
        sta (inszptr),y ;the 0 for the last place
        dex
        bpl -
;decrement PW/CTF table pointers if needed
        ldy #$0b        ;rel.address of CTF-table
        ldx subwpos1+2  ;subwindow (3..1)
-       cpx #tblamoun
        beq +
        lda (inszptr),y
        sec
        sbc #tblwidth
        sta (inszptr),y
        inx
        dey
        bne -
;shift gateoff-pointers
+       ldy #$0c        ;1st gateoff-pointer
-       lda (inszptr),y ;modify tablepointers in batch
        cmp stoinsp+1
        beq +           ;=?
        bcc +           ;<?
        sec
        sbc #tblwidth   ;3
        sta (inszptr),y
+       iny
        cpy #$0c+tblamoun ;+1? corrected bug since v1.2!
        bne -
;correct jump-addresses (substract 3 from $fe addresses which point after insertion cursor-position)
+       ldy stoinsp+1
-       lda (inszptr),y ;read 1st column
        cmp #$fe        ;check if $fe at 1st column, if $fe, correct jump-address
        bne +
        iny
        lda (inszptr),y
        cmp stoinsp+1   ;check if target of jump is smaller or bigger than cursor-data-index (insertion-position)
        beq ++          ;=?
        bcc ++          ;<?
        sec
        sbc #tblwidth   ;increase pointer in case
        sta (inszptr),y
        jmp ++
+       cmp #$ff
        beq ++          ;in case of $ff endsignal step only 1 byte, as $ff is not followed by other bytes in particular tables
        iny
+       iny
+       iny
        cpy #lastinstpt ;+1? ;loop checking until end of table is reached
        bcc -
deret2  rts

deltmpt jsr commonsubs.gettpos ;here comes deletion in tempotable
        bmi deret2      ;delret ;if end of tempoprogram
-       lda TEMPOTBL+1,x
        sta TEMPOTBL,x
        inx
        cpx #TempoTableLen-1
        bne -
        ldx display.seltemp+1
        inx
-       dec TEMPTRLO,x
        inx
        cpx #$40
        bne -
        rts

delchrd cmp #3
        bne deltmpt     ;delete-key subroutine in chordtable
        jsr commonsubs.getctpos ;get chordtable-position under cursor (position in X, value in A)
        bmi +           ;in chordtable more than $80 is allowed to delete
        cmp #$7e
        bcs deret2      ;delret ;if $7e or $7f endsignal reached, no process
+
-       lda CHORDS+1,x  ;delete back one byte in chordtable
        sta CHORDS,x
        inx
        cpx #ChordTableLen-1
        bne -
        ldx display.selchor+1 ;adjust back rest of chordpointers by 1
        inx
-       dec CHDPTRLO,x
        inx
        cpx #$40
        bne -
        rts

        .pend

;------------------------------------------------------------------------------
stoinsp = inskeye.stoinsp

inskeye .proc           ;insertion handler
        jsr repeatex
        lda curwind
        beq +
        jmp olinser     ;is it pattern-window?
+       lda playadapter.followp+1 ;don't allow any insertions into pattern in followplay-mode
        bne retpnto     ;+
        jsr SaveUndo
        jsr commonsubs.setptnptr ;set pattern pointers in advance based upon current track/pattern position
        jsr commonsubs.compcur   ;check, if cursor is at pattern-endposition , Y holds current pattern's number
        bne insinpt     ;branch if we are inside pattern
        ;branch when we're at the pattern endsignal to increase patternlength
        ldy subwpos1+0  ;current track
        ldx curptn1,y   ;current pattern
        beq retpnto     ;don't allow insert in pattern 0 in any case (e.g. after SYS2061 restart)!
        lda PPTRLO,x    ;set pointer to beginning of pattern
        sta commonsubs.instpt2+1
        lda PPTRHI,x
        sta commonsubs.instpt2+2
        lda ptnsize,x   ;if insertion is succesful, increase pattern/size (insert pattern-NOP)
        cmp #maxptnlen
        bcs retpnto     ;+   ;if patternsize is full do nothing
        inc ptnsize,x
        inc ptnlength,x
        tax
        dex
        lda #0
        jsr commonsubs.instpt2 ;end-of-pattern - increase patternsize, insert empty row
        inx
        lda #$ff
        jsr commonsubs.instpt2
        jmp godownp

insinpt                 ;insert one row into pattern, delete last row to keep patternsize constant
        jsr deleter.dellast
        jsr commonsubs.getcurnote ;notevalue in A, positon in X (from pattern displayposition)
        dex
        ldy #1
        lda #0
        jmp insertpt ;jsr insertpt
retpnto rts

olinser cmp #1         ;-------------------------------------------------------------
        bne insinst     ;is it orderlist?
        lda cbmsig
        beq +
        jmp commonsubs.moreSeqInst ;non-Undoable! (no memory to store all sequences)
+       jsr keyhandler.SaveUndo
        jsr commonsubs.seqInst
        jmp commonsubs.moreSeqInst.adjust

insinst cmp #2          ;-----------------------------------------------------------
        bne inschrd     ;is it instrument-window?
        jsr SaveUndo
        jsr commonsubs.setinsptr ;set instrument's memory basepointers
        lda subwpos1+2  ;current instrument-subwindow
        beq deleret     ;if cursor at main instrument settings, no insertion possible
        jsr commonsubs.getitbi ;get instrument-data-pointer under cursor to X
insitbl                 ;insert into instrument-table , 3 bytes at one step
        stx stoinsp+1
        ldy #lastinstpt ;points to 1st column of last possible row in instrument
        lda (inszptr),y
        bne deleret     ;if pattern is full (its very last row's 1st column is non-zero, but $ff)
        ldx #lastinstpt-tblwidth ;move data
-       jsr commonsubs.instpt1 ;read data to move
        sta (inszptr),y ;write data to new place
        dey
        dex
stoinsp cpx #selfmod
        bcs -
        inx
        ldy #2
        lda #0
-       jsr commonsubs.instpt2 ;fill inserted row with zero
        inx
        dey
        bpl -
;shift tablepointers
        ldy #$0b        ;rel.address of CTF-table
        ldx subwpos1+2  ;subwindow (3..1) ;cycle through table-pointers from current table
-       cpx #tblamoun
        beq +           ;check if reached last table
        lda (inszptr),y ;modify tablepointers in batch
        clc
        adc #tblwidth ;3
        sta (inszptr),y
        inx
        dey
        bne -
;shift gateoff-pointers
+       ldy #$0c        ;1st gateoff-pointer
-       lda (inszptr),y ;modify tablepointers in batch
        cmp stoinsp+1
        bcc +
        clc
        adc #tblwidth ;3
        sta (inszptr),y
+       iny
        cpy #$0c+tblamoun ;+1? corrected bug since v1.2
        bne -
;correct jump-addresses (add 3 to $fe addresses which point after insertion cursor-position)
+       ldy stoinsp+1
-       lda (inszptr),y ;read 1st column
        cmp #$fe        ;check if $fe at 1st column, if $fe, correct jump-address
        bne +
        iny
        lda (inszptr),y
        cmp stoinsp+1   ;check if target of jump is smaller or bigger than cursor-data-index (insertion-position)
        bcc ++
        clc
        adc #tblwidth   ;increase pointer in case
        sta (inszptr),y
        jmp ++
+       cmp #$ff
        beq ++          ;in case of $ff endsignal step only 1 byte, as $ff is not followed by other bytes in particular tables
        iny
+       iny
+       iny
        cpy #lastinstpt ;+1? ;loop checking until end of table is reached
        bcc -
deleret rts

inschrd cmp #3          ;-------------------------------------------------------------
        beq inschd2     ;is it chordwindow?
instmpt                 ;insert into tempotable? ;------------------------------------------
        lda TEMPOTBL+TempoTableLen-2
        bne deleret     ;if tempotable is full (nonzero at it's end)
        ldx display.seltemp+1 ;adjust back rest of tempoprogram-pointers by 1
        lda TEMPTRLO+1,x
        sec
        sbc TEMPTRLO,x
        sbc #maxtempolength ;max length of a tempoprogram
        bcs deleret     ;if current tempoprogram's length reached $40
        inx
-       inc TEMPTRLO,x
        inx
        cpx #MAXTEMPOPRAMOUNT;max. number of tempoprograms
        bne -
        jsr commonsubs.gettpos ;get tempotable-position under cursor (position in X, value in A)
        stx tminsxs+1   ;storing X for bit later use
        ldx #TempoTableLen-1
-       lda TEMPOTBL-1,x ;move tempotable up by 1 byte
        sta TEMPOTBL,x
        dex
tminsxs cpx #1          ;self-written, holds current position
        bne -
        lda #$00
        sta TEMPOTBL,x  ;empty inserted area under cursorposition
        rts
inschd2 lda CHORDS+ChordTableLen-2
        bne deleret     ;if there's other than $00 at chordtable's last byte, it's full
        ldx display.selchor+1 ;adjust back rest of chordpointers by 1
        lda CHDPTRLO+1,x
        sec
        sbc CHDPTRLO,x
        sbc #maxchordlength ;max length of a chord
        bcs deleret     ;if current chord's length reached $40
        inx
-       inc CHDPTRLO,x
        inx
        cpx #MAXCHORDAMOUNT ;max. number of chords
        bne -
        jsr commonsubs.getctpos ;get chordtable-position under cursor (position in X, value in A)
        stx chinsxs+1   ;storing X for bit later use
        ldx #ChordTableLen-1
-       lda CHORDS-1,x  ;move chordtable up by 1 byte
        sta CHORDS,x
        dex
chinsxs cpx #1          ;self-written, holds current position
        bne -
        lda #$00
        sta CHORDS,x    ;empty inserted area under cursorposition
delrret rts
        .pend


;-------------------------------------------------------------------------------
Undo .proc
        ldx UndoItemNumber
        lda UndoType
        bne chOrdrL
UndoPat lda playadapter.followp+1
        bne endUndo
        lda PPTRLO,x
        sta patAddr+1
        lda PPTRHI,x
        sta patAddr+2
UndoLen lda #selfmod
        sta ptnlength,x
UndoSiz lda #selfmod
        sta ptnsize,x
        tay
-       lda UndoBuffer,y
patAddr sta selfmodA,y
        dey
        cpy #$ff
        bne -
        lda #0 ;refresh pattern-display
        sta curwind
        jmp commonsubs.chkcury ;check if cursor in pattern display-area, if outside, move it inside
endUndo rts
chOrdrL cmp #2
        bcs chInstr
        ldx #0
-       lda UndoBuffer,x
UndSeqP sta selfmodA,x
        inx
        cpx #seqlength+1
        bne -
        jsr commonsubs.sseqend ;check if zero under cursor, set position to writable area
        inc playadapter.seqrefr ;display orderlist (handy in case we're in other window)
        rts
chInstr bne endUndo
UndoIns jsr commonsubs.setinsptr.setinp2 ;input:X (set above)
        ldx #maxinstsize-1
-       lda UndoBuffer,x
        jsr commonsubs.instpt2
        dex
        bpl -
        jmp commonsubs.instinit
        .pend

UndoItemNumber .byte 0 ;the number of pattern/instrument/sequence where undo was saved
UndoType .byte $ff ;the type of undo: 0=pattern, 1=orderlist, 2=instrument (identical to window-numbers)

SaveUndo .proc
;        sta astore+1
        lda repecnt
        cmp #repspd1-1    ;Undo only for the first time when keys pressed long & repeated
        bne retSaveUndo
SaveUn2 stx xstore+1
;        sty ystore+1
        ldx subwpos1+0  ;current track
        lda curwind
        sta UndoType
        bne chOrdrL
SavPatU ldy curptn1,x   ;current pattern
        lda PPTRLO,y
        sta sUndoLp+1
        lda PPTRHI,y
        sta sUndoLp+2
        sty UndoItemNumber
        lda ptnlength,y
        sta Undo.UndoLen+1
        ldx ptnsize,y
        stx Undo.UndoSiz+1
sUndoLp lda selfmodA,x
        sta UndoBuffer,x
        dex
        cpx #$ff
        bne sUndoLp
retSaveUndo
;astore  lda #selfmod
xstore  ldx #selfmod
;ystore  ldy #selfmod
        rts
chOrdrL cmp #2
        bcs chInstr
SavSeqU jsr commonsubs.setseqptr ;get sequence pointers
        lda commonsubs.seqaddb+1
        sta UndSeqP+1
        sta Undo.UndSeqP+1
        lda commonsubs.seqaddb+2
        sta UndSeqP+2
        sta Undo.UndSeqP+2
        ldx #0
UndSeqP lda selfmodA,x
        sta UndoBuffer,x
        inx
        cpx #seqlength+1
        bne UndSeqP
        beq retSaveUndo ;jump
chInstr bne retSaveUndo
SavInsU lda display.selinst+1
        sta UndoItemNumber
        jsr commonsubs.setinsptr
        ldx #maxinstsize-1
-       jsr commonsubs.instpt1
        sta UndoBuffer,x
        dex
        bpl -
        jmp retSaveUndo
        .pend

;-------------------------------------------------------------------------------


        .fi             ;end of part needed only for editor

        .bend
;==========================================================================================================
; vim: sw=4 ts=4 syntax=asm:
