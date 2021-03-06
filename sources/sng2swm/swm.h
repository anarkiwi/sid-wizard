//================SID-Wizard SWM1 workfile format description===================
//(unfortunately this format cannot be used by 64tass at the same time...that's why SWM-spec.src is still used in SID-Wizard)
// $Id: swm.h 360 2014-02-15 14:50:28Z soci $

#include <map>
#include <vector>
//#include "SWM-spec.src" - might be read as constants (without type)

#define SID_CHNAMOUNT 3
#define SW1_TAG_SIZE 4
#define SW1_HEADER_SIZE 64
#define SW1_AUTHORINFO_SIZE 40
#define SW1_PATTERNLENGTH_MAX 0xF8
#define SW1_PATTERNSIZE_MAX   0xFA

#define SW1_SEQUENCE_TRANS_MIN 0x80
#define SW1_SEQUENCE_TRANS     0x90
#define SW1_SEQUENCE_TRANS_MAX 0x9F
#define SW1_SEQUENCE_VOLUME    0xA0
#define SW1_SEQUENCE_TEMPO     0xB0
#define SW1_SEQUENCE_ENDSONG   0xFE
#define SW1_SEQUENCE_LOOPSONG  0xFF

//pattern-data note-column values - these are described in SWM-spec.src too...
#define SW1_PTNCOL1_REST 0x00
#define SW1_PTNCOL1_NOTE 0x01
#define SW1_NOTE_MAX     0x5F
#define SW1_PTNCOL1_VIBAMP 0x60 //..0x6F = 0..F vibrato-amplitude setting in note-column
#define SW1_PTNCOL1_PACKMIN 0x70 //multiple rests can be compressed with $70..$77 values (a $00 always preceeds, $70 means 2 zeroes)
#define SW1_PTNCOL1_PACKMAX 0x77
#define SW1_PTNCOL1_PORTAM 0x78
#define SW1_PTNCOL1_SYNCON 0x79
#define SW1_PTNCOL1_SYNCOFF 0x7A
#define SW1_PTNCOL1_RINGON 0x7B
#define SW1_PTNCOL1_RINGOFF 0x7C
#define SW1_PTNCOL1_KEYON 0x7D
#define SW1_PTNCOL1_KEYOFF 0x7E
#define SW1_PTNCOL1_END 0xFF  //0x7F can't be used as it's 0xFF without bit7 set

//pattern-data instrument-column values
#define SW1_INSTCOLUMN_NOP 0x00
#define SW1_INSTRUMENT_MIN 0x01
#define SW1_INSTRUMENT_MAX 0x3E
#define SW1_LEGATO         0x3F
//values above are small-effects till $7F, bit7 means the same but fx-column follows

//SWM1 small effect base-values (2nd nybble is fx-value 0..F)
#define SW1_SMALLFX_MIN 0x20
#define SW1_SMALLFX_ATTACK     0x20
#define SW1_SMALLFX_DECAY      0x30
#define SW1_SMALLFX_WAVEFORM   0x40
#define SW1_SMALLFX_SUSTAIN    0x50
#define SW1_SMALLFX_RELEASE    0x60
#define SW1_SMALLFX_SETCHORD   0x70
#define SW1_SMALLFX_VIBAMP     0x80
#define SW1_SMALLFX_VIBFREQ    0x90
#define SW1_SMALLFX_MAINVOL    0xA0
#define SW1_SMALLFX_FILTBAND   0xB0
#define SW1_SMALLFX_CHORDSPD   0xC0
#define SW1_SMALLFX_DETUNE     0xD0
#define SW1_SMALLFX_WAVEREGC   0xE0
#define SW1_SMALLFX_RESONANCE  0xF0
//SWM1 big-effects (followed by 1 byte value)
#define SW1_BIGFX_PORTUP       0x01
#define SW1_BIGFX_PORTDOWN     0x02
#define SW1_BIGFX_TONEPORT     0x03
#define SW1_BIGFX_WAVEREG      0x04
#define SW1_BIGFX_AD           0x05
#define SW1_BIGFX_SR           0x06
#define SW1_BIGFX_SETCHORD     0x07
#define SW1_BIGFX_VIBRATO      0x08
#define SW1_BIGFX_WAVETBL      0x09
#define SW1_BIGFX_PULSETBL     0x0a
#define SW1_BIGFX_FILTERTBL    0x0b
#define SW1_BIGFX_CHORDSPEED   0x0c
#define SW1_BIGFX_DETUNE       0x0d
#define SW1_BIGFX_PULSEWIDTH   0x0e
#define SW1_BIGFX_FILT_CUTOFF  0x0f
#define SW1_BIGFX_TEMPO        0x10
#define SW1_BIGFX_FUNKTEMPO    0x11
#define SW1_BIGFX_TEMPOPROG    0x12
#define SW1_BIGFX_TRACKTEMPO   0x13
#define SW1_BIGFX_TRACKFUNKT   0x14
#define SW1_BIGFX_TRKTEMPOPRG  0x15
#define SW1_BIGFX_VIBRATYPE    0x16
//some space for expansion. 0x17..0x1b
#define SW1_BIGFX_FILTERSHIFT  0x1c
#define SW1_BIGFX_DELAYTRACK   0x1d
#define SW1_BIGFX_DELAYNOTE    0x1e
#define SW1_BIGFX_FILT_CONTROL 0x1f

//instrument main data defines
#define SW1_INSTRUMENT_PARAMSIZE 0x10 //header
#define SW1_INSTRUMENT_NAMESIZE 8 //name (after the wf/pulse/filter tables)
//instrument table-data defines
#define SW1_ARP_REL_MAX   0x7E  //positive relative-pitches range from $00..$7E in SW
#define SW1_ARP_CHORDCALL 0x7F
#define SW1_ARP_NOP       0x80
#define SW1_ARP_ABS_MIN   0x81 //from C-0 note onwards (displayed as C-1 in SW)
#define SW1_ARP_ABS_MAX   0xDF //last absolute note
#define SW1_ARP_REL_MIN   0xE0  //negative relative-pitches range from $E0..$FF in SW
#define SW1_TABLE_JUMP    0xFE
#define SW1_TABLE_END     0xFF

const char SWM_DEFAULT_FILENAME[] = "output.swm";
const char SW1_TAG[] = "SWM1";


//SID-Wizard module data in the order they appear in the .SWM file

//-------header----------
struct swm_header
{
    char tag[SW1_TAG_SIZE]; //0
    unsigned char framespd; //4  - 1 means single-framespeed
    unsigned char highlight; //5 - downbeat/step-highlight amount, default is 4
    unsigned char auto_advance; //6 - how much the cursor jumps after typing a note
    
    struct swm_header_conf_bits //7 -configuration bits with various editor-settings, default is 0
    { //be careful about endianness when saving this byte
	    unsigned char binding     :1; //bit0 on C64, 1=non-binding mod, tracks can be scrolled separately
	    unsigned char hiderasters :1; //bit1 on C64, 1=hiding rasterbars in editor
	    unsigned char follow_play :1; //bit2 on C64, 1=follow-play by default (no need to press C= key)
	    unsigned char not_defined :4; //bit3..6 on C64, ...for later expansions
	    unsigned char auto_instru :1; //bit7 on C65, 1=type instrument automatically after note, delete repeats on saving
    } conf_bits;
    
    unsigned char mutesolo[SID_CHNAMOUNT]; //8..A values are $FE for 'mute', $FF for 'unmute' for tracks
    unsigned char default_ptn; //B
    unsigned char sequence_count; //C -amount of sequences. 3 times the number of subtunes, should be minimum 1 
    unsigned char pattern_count; //D -amount of patterns (the last used pattern), minimum 1
    unsigned char instrument_count; //E
    unsigned char chordtable_length; //F
    unsigned char tempotable_length; //$10
    unsigned char colour_theme; //$11 - editor-setting, default is 0
    unsigned char keyboard_type; //$12 - editor-setting (GT or DMC note/jamming-keyboard), default is 0 (GT 2-row)
    unsigned char driver_type; //$13 - just an information, which driver/player (normal/light/etc.), default is 0
    unsigned char not_defined[(SW1_HEADER_SIZE-SW1_AUTHORINFO_SIZE)-0x14]; //.....for later expansions
    char authorinfo[SW1_AUTHORINFO_SIZE]; //$18th position
};

//-------subtunes/orderlists------------
struct swm_sequence //3 of them are grouped into an orderlist (subtune, so to speak)
{ //swm_header.sequence_count gives how many sequences exist
    std::vector<unsigned char> data; //containing end/loop delimiters ($FE or $FF with loop-index)
    unsigned char size;  //size of the whole data (including end-signals)
};

struct swm_subtune
{
    swm_sequence *track1, *track2, *track3;
};

//----------patterns------------
/*struct swm_pattern_row
{
    unsigned char note;
    unsigned char instrument;
    unsigned char command;
    unsigned char parameter;
    //unsigned char * data; //variable size - min.1, max.4 elements, bit7 shows if next column coming (except FX, which has value if >=$20)
    //if only note column:   note:$00..$7F (if $00, it is RLE-compressed with value $70..$77, but not necessary, only to reduce size
    //if note+instr column:  note:$80..$ff , instr:$00..$7F
    //if note+instr+smallFX: note:$80..$ff , instr:$80..$ff, smallFX: $20..$$FF
    //if note+instr+bigFX:   note:$80..$ff , instr:$80..$ff, big-FX : $01..$1F , FX-value: $00..$ff
}; */

struct swm_pattern
{ //swm_header.pattern_count gives how many patterns exist
    std::vector<unsigned char> data; //the $FF pattern-endsignal shouldn't be included in this (turns to 'size' in .SWM file)
                           //not necessary to compress multiple empty rows (that contain only $00)
    unsigned char size; //size in memory - not to be confused with length which is the number of pattern-rows
    unsigned char length; //number of pattern-rows
};

//--------instrumens-------------
/*struct swm_waveitem
{
       unsigned char command;
       unsigned char value;
       unsigned char detune;   // always zero!
};

struct swm_pulseitem
{
       unsigned char command;
       unsigned char value;
       unsigned char kt;   // always zero!
};

struct swm_filteritem
{
       unsigned char command;
       unsigned char value;
       unsigned char kt;   // always zero!
};*/

struct swm_instrument
{
    struct swm_instrument_params
    {
        struct swm_instrument_flags //position 0 - instrument configuration bits
        { //be careful about endianness of this bit-field structured instrument-configuration byte
         unsigned char hrtimer : 2; //bit0,1 on C64, values 0..2 are used only in player (might be 3 for NTSC support)
         unsigned char staccato: 1; //bit2 on C64, if set to 1 it's staccato (waveform is $18 in HR-phase - testbit&mute)
         unsigned char wframe1 : 1; //bit3 on C64, if 1st waveform should be $09 testbit - will be deprecated in SWM2, inst.parameter $F will be 1st waveform
         unsigned char vibtype : 2; //bit4,5 on C64, $00:increasing   $10:normal  ,  $20:down-oriented  , $30:up-oriented
         unsigned char pwreset : 1; //bit6 on C64, if value is 1, pulsewidth-table-reset will be disabled until an instrument-selection command
         unsigned char flreset : 1; //bit7 on C64, if value is 1, filtertable-reset will be disabled until an instrument-selection command
        }flag;
        
        unsigned char hr_ad, hr_sr; //1,2 
        unsigned char ad, sr; //3,4 
        unsigned char vibrato; //5 - high nybble:amplitude (utilizing exponential scale of freq.table),   low nybble:frequency 
        unsigned char vibrato_delay; //6 - if increasing vibrato (vibtype:$00), it's amplitude-increase-speed
        unsigned char arpchord_speed; //7 - for multispeed PW it bit6 should be set, for multispeed PW&filter bit7 should be set
        unsigned char default_chord; //8 - should be 1 at least as chord0 doesn't exist
        unsigned char octave_shift; //9 - to be easier processable by player, it's given in half-tones, not in octaves!
        unsigned char pulsetb_index;  //$A - pulse-table-beginning index - relative to instrument-base
        unsigned char filtertb_index; //$B - filtertable-beginning index - relative to instrument-base
                                       //all index-values point directly to the memory what they refer to
        unsigned char wfgoff,pwgoff,fltgoff; //$C,$D,$E - gate-off pointer indexes - relative to instrument-base
                                              //always 0 for GT as there's no substitute in GT
        unsigned char frame1_waveform; //$F - used as 1st-frame waveform (only effective if 'wframe1' is set to 1
    } parameter;

    std::vector<unsigned char> tables; //wave-table,pulse-table,filter-table

    /*std::vector<swm_waveitem> wave_sequence; //$10th position - relative to instrumet-base
     std::vector<swm_pulseitem> pulse_sequence; //table-start given by pulsetb_index (directly)
     std::vector<swm_filteritem> filter_sequence; //table-start given by filtertb_index (directly) */

    //the last $FF delimiter should be turned into size of the instrument (without name) when saving to SWM module

    char name[SW1_INSTRUMENT_NAMESIZE];
};

//-----------chords-------------------
//no chords as such in GT, but arpeggios using the same instrument-data or wavearp-table jumps could be detected
struct swm_chord
{ //size of chord-table (in bytes) is given in swm_header.chordtable_length
    unsigned char * pitch_shift; //relative pitch-shifts
    unsigned char delimiter; //$7e or $7f
};

struct swm_chordtable
{
    swm_chord * data;
};

//---------tempo-programs-------------
//no tempo-programs in GT...as this is the case, swm_header.tempotable_length should be 0 and tempotable is empty...
struct swm_tempoprogram
{ //size of tempoprogram-table (in bytes) is given in swm_header.tempotable_length
    unsigned char * rowtempo; //relative pitch-shifts
    unsigned char delimiter; //the last tempo-value with bit7 set ($80..$ff)
};

struct swm_tempotable
{
    swm_tempoprogram * data;
};

//--------subtune-tempos/funktempos-----
struct swm_funktempo
{
    unsigned char tempo1,tempo2; //funktempo. alternated row-by-row when subtune played.  
                                  //it works with the same principle as tempo-programs, if tempo1>$80 it's single-tempo
};

//-------------misc----------------------
unsigned char SWexpTabH[]= { //values taken from 'player.asm' of SID-Wizard 1.2
                  0,0,0,0,0,0,0,0, 0,0,
                  0,			//FOR UNCALCULATED ZERO VIBRATO
                  0x01,0x01,0x01, 0x01,0x01,0x01,   //;0  ;13  ;16TH ELEMENT IS 0x01 IN EXP.TABLE
                  0x01,0x01,0x01,0x01,0x01,0x01,    //;6   ;19
                  0x02,0x02,0x02,0x02,0x02,0x02,    //;12  ;25
                  0x02, 0x03,0x03,0x03,0x03,0x03,   //;18 ;31  ;32ND ELEMENT IS 0x03 IN EXP.TABLE
                  0x04,0x04,0x04,0x04,0x05,0x05,    //;24  ;37  ;!!! 0x04 IS 37TH ELEMENT IN EXP.TABLE
                  0x05,0x06,0x06,0x06,0x07, 0x07,   //;30 ;43  ;48TH ELEMENT IS 0x07 IN EXP.TABLE
                  0x08,0x08,0x09,0x09, 0x0a,0x0a,   //;36 ;49  ;!!!! 0x0A IS 53TH ELEMENT IN EX.TABLE
                  0x0b,0x0c,0x0d,0x0d,0x0e, 0x0f,   //;42 ;55  ;0x0F IS 60TH ELEMENT IN EXP.TABLE
                  //-------------------------------------
                  0x10,0x11,0x12, 0x13,0x14,0x15,   //;48 ;61 ;!!!64TH (72-8) ELEMENT IS 0x13 (0x1F-0x0C) IN EXP.TABLE
                  0x17, 0x18,0x1a,0x1b,0x1d, 0x1f,  //;54;67 ;!!!! 0x1F (0x2E-0x0F) IS 72ND (64+8) ELEMENT IN EXP.TABLE,!!!0x18 (0x0A+0x0E) IS 68TH
                  0x20,0x22, 0x24,0x27,0x29,0x2b,   //;60 ;73  ;0x24 IS 75TH ELEMENT IN EXP.TABLE
                  0x2e,0x31, 0x34,0x37,0x3a, 0x3e,  //;66;79 ;!!!0x2E (0x1F+0x0F) IS 79TH (72+7), 0x3E IS 84TH, 80TH ELEMENT IS 0x31 IN EXP.TABLE
                  0x41,0x45,0x49,0x4e,0x52,0x57,    //;72  ;85
                  0x5c,0x62,0x68,0x6e,0x75,0x7c,    //;78  ;91
                  0x83,0x8b,0x93,0x9c,0xa5,0xaf,    //;84  ;97
                  0xb9,0xc4,0xd0,0xdd,0xea,0xf8,    //;90  ;103
                  };
#define FREQTBH_POS 11 //position of freq.table inside SWexpTabH[]
#define FREQTB_SIZE 96
#define EXPTRESHOLD 107 //FREQTB_SIZE+FREQTBH_POS //(size of exp.table array)

unsigned char SWexpTabL[]= { //values taken from 'player.asm' of SID-Wizard 1.2
                  0x07,0x16,0x27,0x38,0x4b,0x5e,
                  0x73,0x89,0xa1,0xba,0xd4,0xf0,
                  0x0d,0x2c,0x4e,0x71,0x96,0xbd,
                  0xe7,0x13,0x42,0x74,0xa8,0xe0,
                  0x1b,0x59,0x9c,0xe2,0x2c,0x7b,
                  0xce,0x27,0x84,0xe8,0x51,0xc0,
                  0x36,0xb3,0x38,0xc4,0x59,0xf6,
                  0x9d,0x4e,0x09,0xd0,0xa2,0x81,
                  //---------------------------------------------
                  0x6d,0x67,0x70,0x88,0xb2,0xed,
                  0x3a,0x9c,0x13,0xa0,0x44,0x02,
                  0xda,0xce,0xe0,0x11,0x64,0xda,
                  0x75,0x38,0x26,0x40,0x89,0x04,
                  0xb4,0x9c,0xc0,0x22,0xc8,0xb4,
                  0xeb,0x71,0x4c,0x80,0x12,0x08,
                  0x68,0x38,0x80,0x45,0x90,0x68,
                  0xd6,0xe3,0x98,0x00,0x24,0x10
                  };
//vibrato-amplitude (FREQMODL,FREQMODH) calculation in SID-Wizard 1.2 (if non-zero): the same as slide but Input value is halved

                            //0,1,2,3,4,5,6,7,   8,  9, 0xA,0xB,0xC,0xD,0xE,0xF
unsigned char SWvibFreq[] = { 2,4,5,6,7,8,9,0xB,0xC,0xD,0xE,0xF,0xF,0xF,0xF,0xF }; //SW equivalents of GT vibrato-frequencies 0..F


//And that's it, EOF :)
//=============================================================================
