//==============================================================================
// SID-Wizard's SWM v1 module converter application (2026 Hermit Software)
//==============================================================================

//-----------------source-library inclusions------------------------------------
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>  //for surely 64bit variables (int64_t, uint64_t) (e.g. XM SampleIndex)
#include <ctype.h>
#include <math.h>
#include <time.h>    //for date in copyright/release-info
#include <libgen.h>  //for basename()?


//----------------------literate constant definitions---------------------------
#include "SWM-spec.src"


//Configuration before building:
#define SNG_OPTIMIZE_SPEEDTABLE 1  //if editing after conversion is desired, speedtable is better unoptimized (but some tunes will overflow speedtable with many identical entries)
#define SNG_OPTIMIZE_WF_PW_FILT_TABLES 1  //Turns on looking for fully/partly identical table-chunks and linking to existing instead of creating new entries, might avoid table-overflows for some tunes (e.g. cloudlessrain, gardenparty), but if no overflow, GT exporter might optimize out repetitions anyway. (Row-omissions might also need to account for when performing internal/from-pattern table-jumps.)
#define SWM_FREQ_EXPTABLE_BASED_SLIDES_THRESHOLD 0x60  //below this SWM slide-speed value we're converting to calculated GT slides (speedtable left-value bit $80 set), above it we're adjusting slide-speed in speedtable by note-pitch  //0x60: congabeat.swm orderlistpos. $26, //0x70: oakyardmemo.swm bass octave-slides //0x80, //0x98,  //GT calculated slides has a plateau, so above this input-value we're using non-calculated slide/portamento instead of the calculated which has a speed/slidesize limit


#define FILENAME_LENGTH_MAX 256
#define C64_FILENAME_LENGTH_MAX 16
#define TUNEDATA_SIZE_MAX 65536
#define EXT_LENGTH_MAX 4
#define TYPE_ID_LENGTH_MAX 32
#define XM_PATTERNS_MAX 256
#define XM_INSTRUMENTS_MAX 128
#define MIDI_CHANNELS_MAX 64  //maximum allowed number of MIDI channels (tracks in MIDI format 1)
#define MIDI_TRACKLEN_MAX 65536 //maximum length of one MIDI-track
#define MIDI_VLV_LEN_MAX 8 //maximum length of variable-length variables in MIDI
#define SID_CHANNELCOUNT 3   //SID/SWM maximal amount of channels - hardwired
unsigned int XM_CHANNELCOUNT=18; //number channels to represent converted SWM channels in XM
#define XM_COL_PER_TRK 5 //number of (byte) columns in one XM track (note,inst.,vol.,fx,fx-value
#define XM_SAMPLING_FREQUENCY 48000.00
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define CHORD_SILENCER_RATIO 2 //can be 1..4 - how much the chord-note loudness is smaller
#define VICE_P00_OFFSET 0x1A //vice program-offset (not containing PRG load-address following after)
#define VICE_P00_FILENAME_OFFSET 0x8


//SWM format descriptor constants ===========================================

 char* SWM_ID="SWM1";
 char* SWS_ID="SWMS";
#define SWS_HILI_POS 0x15

//#include "swm.h"
enum { //the others (most of them) are taken from SWM-spec.src
 SWM_AUTHORINFO_SIZE = 40,

 SWM_VALUE__PLAYERTYPE_NORMAL = 0, SWM_VALUE__PLAYERTYPE_MEDIUM = 1, SWM_VALUE__PLAYERTYPE_LIGHT = 2,
 SWM_VALUE__PLAYERTYPE_EXTRA = 3, SWM_VALUE__PLAYERTYPE_BARE = 4, SWM_VALUE__PLAYERTYPE_DEMO = 5, //SWM_VALUE__PLAYERTYPE_GOATTRACKER = 6, //?

 SWM_BITVALUE__TEMPOTABLE_END = 0x80, SWM_BITVALUE__TEMPOTABLE_MASK = (0xFF-SWM_BITVALUE__TEMPOTABLE_END), SWM_VALUE__SUBTUNE_TEMPO_DEFAULT = 0x06,

 SWM_VALUE__SEQUENCE_TRANSPOSE_MIN = 0x80, SWM_VALUE__SEQUENCE_TRANSPOSE_MID = 0x90, SWM_VALUE__SEQUENCE_TRANSPOSE_MAX = 0x9F,
 SWM_VALUE__SEQUENCE_VOLUME_MIN = 0xA0, SWM_VALUE__SEQUENCE_VOLUME_MAX = 0xAF,
 SWM_VALUE__SEQUENCE_TEMPO_MIN = 0xB0, SWM_VALUE__SEQUENCE_TEMPO_MAX = 0xEF,
 SWM_VALUE__SEQUENCE_NOP_MIN = 0xF0, SWM_VALUE__SEQUENCE_NOP_MAX = 0xFD,
 SWM_VALUE__SEQUENCE_END = 0xFE, SWM_VALUE__SEQUENCE_LOOP = 0xFF,
  SWM_VALUE__SEQUENCE_SUBTUNEJUMP_MIN = 0x80, SWM_VALUE__SEQUENCE_SUBTUNEJUMP_MAX = 0x9F, //0x87, //theoretical maximum, real is given by 'maxsubtuneamount'

 SWM_NOTE_NUMBER_NONE = 0x00, SWM_NOTE_NUMBER_UNDETECTED = SWM_NOTE_NUMBER_NONE, //denotes yet undetected notes in pattern-statistics
 SWM_NOTE_NUMBER_MIN = 0x01,
 SWM_INSTRUMENT_NUMBER_NONE = 0x00, SWM_INSTRUMENT_NUMBER_UNDETECTED = SWM_INSTRUMENT_NUMBER_NONE,  //denotes yet undetected or unused instruments in pattern-statistics (in that case default instrument 1 will be selected)

 SWM_INSTRUMENT_AMOUNT_MAX = 64,  //theoretical maximum for safe array-sizing, 'maxinstamount' in SWM-spec.src tells the max instrument-value used by SW player/editor
 SWM_INSTRUMENT_SIZE_MAX = 256,  //theoretical maximum for safe array-sizing, 'maxinstsize' in SWM-spec.src tells the max instrument-size used by SW player/editor
 SWM_INSTRUMENT_NUMBER_MIN = 1, SWM_INSTRUMENT_NUMBER_MAX = 0x3E, //(SWM_LEGATO_INSFX - 1),  //theoretical maximum, real is given by 'maxinstnamount'
  SWM_INSTRUMENT_NUMBER_DEFAULT = SWM_INSTRUMENT_NUMBER_MIN, //if instrument not given, player uses instrument 1
 SWM_INSTRUMENT_TABLE_ROWSIZE = 3 /*bytes*/, SWM_INSTRUMENT_TABLE_SIZE_MAX = 0x80-24,  //common for all tables, given by 'maxinstsize' minus instrument-header size
 SWI_CONTROL_POS = 0x00, SWI_FILTERTBPT_POS = 0x0B, SWI_FRAME1WAVEFORM_POS = 0x0F,
 SWI_GATEOFFINDEX__WFTABLE = 0x0C, SWI_GATEOFFINDEX__PWTABLE = 0x0D, SWI_GATEOFFINDEX__FILTERTABLE = 0x0E,
 SWM_VALUE__FRAME1WAVEFORM__DEFAULT = 0x09, SWM_VALUE__FRAME1WAVEFORM__FALLBACK = SWM_VALUE__FRAME1WAVEFORM__DEFAULT,
 SWM_BITVALUE__INSTRUMENT_ARPSPEED__MULTISPEED_PW_PROGRAM = 0x40, SWM_BITVALUE__INSTRUMENT_ARPSPEED__MULTISPEED_FILTER_PROGRAM = 0x80,
  SWM_MASK__INSTRUMENT_ARPSPEED = 0x3F,

 SWM_VALUE__WFTABLE__REPEAT_MIN = 0x00, SWM_VALUE__WFTABLE__REPEAT_MAX = 0x0F,
 SWM_VALUE__WFTABLE__WAVECONTROL_MIN = 0x10, SWM_VALUE__WFTABLE__WAVECONTROL_MAX = 0xFD,
 SWM_VALUE_MIN__ARPTABLE__POSITIVE_SHIFT = 0x00, SWM_VALUE__ARPTABLE__NO_SHIFT = SWM_VALUE_MIN__ARPTABLE__POSITIVE_SHIFT,
  SWM_VALUE_MAX__ARPTABLE__POSITIVE_SHIFT = 0x5F,
 SWM_VALUE__ARPTABLE__CALL_CHORD = 0x7F, SWM_VALUE__ARPTABLE__NOP = 0x80,
 SWM_VALUE__ARPTABLE__ABSOLUTE_NOTE_MIN = 0x81, SWM_VALUE__ARPTABLE__ABSOLUTE_NOTE_MAX = 0xDF, //C-0..A#7 (GT starts from C#0!)
 SWM_VALUE_MIN__ARPTABLE__NEGATIVE_SHIFT = 0xE0, SWM_VALUE_MAX__ARPTABLE__NEGATIVE_SHIFT = 0xFF, //lowest value gives max. negative shift

 SWM_VALUE_MIN__PWTABLE_COMMAND__SET_PULSEWIDTH = 0x80, SWM_VALUE_MAX__PWTABLE_COMMAND__SET_PULSEWIDTH = 0xFD, //theoretical max, usually using only $8X
 SWM_VALUE__PWTABLE_COMMAND__SWEEPTIME_MIN = 0x00, SWM_VALUE__PWTABLE_COMMAND__SWEEPTIME_MAX = 0x7F, //parameter is 8bit 2nd-complement up/down add-value

 SWM_VALUE_MIN__FILTERTABLE_COMMAND__SET_FILTER = 0x80, SWM_VALUE_MAX__FILTERTABLE_COMMAND__SET_FILTER = 0xFD,
 SWM_VALUE__FILTERTABLE_COMMAND_IN_1ST_ROW__PASSIVE = 0x00,  //filter-switch turned on on channel but it doesn't control filter
  SWM_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MIN = 0x00, SWM_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MAX = 0x7F,
   SWM_VALUE__FILTERTABLE_COLUMN3__FILTERSWITCH_OVERRIDE_MIN = 0x80, SWM_VALUE__FILTERTABLE_COLUMN3__FILTERSWITCH_OVERRIDE_MAX = 0x8F,
 SWM_VALUEMASK__FILTERTABLE_COMMAND__SET_RESONANCE = 0x0F, SWM_BITPOS__FILTERTABLE_COMMAND__SET_RESONANCE = 0,
  SWM_VALUEMASK__FILTERTABLE__FILTERSWITCH_OVERRIDE = 0xF,  //maskfor the most GoatTracker-compatible filter-switch setting (anno created for sng2swm)

 SWM_VALUE_INSTRUMENT_TABLE_JUMP = 0xFE, SWM_VALUE_INSTRUMENT_TABLE_END = 0xFF, SWM_SIZE__INSTRUMENT_TABLE_ENDMARK = 1,
  SWM_VALUE__WFTABLE_JUMP_PARAMETER__SELFJUMP_MIN = 0x40,  //if the operand of $FE is minimum $40, it jumps to itself ($FF 00 is probably the GT equivalent)

 SWM_CHORD_NUMBER_NONE = 0x00, SWM_CHORD_NUMBER_UNDETECTED = SWM_CHORD_NUMBER_NONE,
 SWM_CHORD_NUMBER_MIN = 0x01, SWM_CHORD_AMOUNT_MAX = 32, //for safe array-sizing, real is MAXCHORDAMOUNT in SWM-Spec.src
 SWM_VALUE_CHORDTABLE__END__RETURN = 0x7E, SWM_VALUE_CHORDTABLE__END__LOOP = 0x7F,

 SWM_PATTERN_AMOUNT_MAX = 256,  //a safe theoretical maximum of available patterns for array-sizing (real is given by 'maxptnamount' in SWM-spec.src)
 SWM_PATTERN_SIZE_MAX = 256, //a safe theoretical maximum of pattern-size for array-sizing (real is given by 'maxptnlen' in SWM-spec.src)
 SWM_PATTERN_NUMBER_MIN = 1, SWM_PATTERN_NUMBER_MAX = 0x7F,  //theoretical maximum of orderlist-callable patterns, real is given by 'maxptnamount'
 SWM_VALUE__PATTERN_COLUMN1__NOP = 0x00,  //pattern-data note-column values begin here
 SWM_VALUE__PATTERN_COLUMN1__NOTE_MIN = SWM_NOTE_NUMBER_MIN /*0x01*/, SWM_VALUE__PATTERN_COLUMN1__NOTE_MAX = 0x5F,  //'SWM_NOTE_MAX' in SWM-spec.src
  SWM_NOTE_NUMBER_DEFAULT = SWM_VALUE__PATTERN_COLUMN1__NOTE_MAX / 2, // - SWM_VALUE__PATTERN_COLUMN1__NOTE_MIN
 SWM_VALUE__PATTERN_COLUMN1__VIBRATO_AMPLITUDE_MIN = 0x60, SWM_VALUE__PATTERN_COLUMN1__VIBRATO_AMPLITUDE_MAX = 0x6F,  //'VIBRATOFX' in SWM-spec.src
 SWM_VALUE__PATTERN_COLUMN1__PACKED_NOP_MIN = 0x70, SWM_VALUE__PATTERN_COLUMN1__PACKED_NOP_MAX = 0x77,  //'PACKEDMIN' & 'PACKEDMAX' in SWM-spec.src //multiple empty-rows (NOPs) can be compressed with $70..$77 values (a $00 always preceeds, $70 means 2 zeroes)
 SWM_VALUE__PATTERN_COLUMN1__PORTAMENTO = 0x78,  //'PORTAMFX' in SWM-spec.src
 SWM_VALUE__PATTERN_COLUMN1__SYNCON = 0x79, SWM_VALUE__PATTERN_COLUMN1__SYNCOFF = 0x7A,  //'SYNCONFX' & 'SYNCOFFX' in SWM-spec.src
 SWM_VALUE__PATTERN_COLUMN1__RINGON = 0x7B, SWM_VALUE__PATTERN_COLUMN1__RINGOFF = 0x7C,  //'RINGONFX' & 'RINGOFFX' in SWM-spec.src
 SWM_VALUE__PATTERN_COLUMN1__GATEON = 0x7D /*start 'rest'*/, SWM_VALUE__PATTERN_COLUMN1__GATEOFF = 0x7E /*restart note*/,  //'GATEONFX' & 'GATEOFFX' in SWM-spec.src  //value 0x7F can't be used as it's 0xFF without bit7 set
 SWM_BITVALUE__PATTERN_COLUMN1__NEXTCOLUMN = 0x80, SWM_MASK__PATTERN_COLUMN1 = (0xFF - SWM_BITVALUE__PATTERN_COLUMN1__NEXTCOLUMN),  //if bit7 is 1, a next (instrument)column follows
 SWM_VALUE__PATTERN_COLUMN1__PATTERN_END = 0xFF,
 SWM_VALUE__PATTERN_COLUMN2__NOP = 0x00, //(only happens when BigFX follows)  //pattern-data instrument-column values begin here
 SWM_VALUE__PATTERN_COLUMN2__INSTRUMENT_MIN = SWM_INSTRUMENT_NUMBER_MIN /*0x01*/,
  SWM_VALUE__PATTERN_COLUMN2__INSTRUMENT_MAX = SWM_INSTRUMENT_NUMBER_MAX /*0x3E*/,  //theoretical max, players support less
 SWM_VALUE__PATTERN_COLUMN2__LEGATO = 0x3F,  //'SWM_LEGATO_INSFX' in SWM-spec.src (same as 3FF max.portamento)  //values above are small-effects till $7F, bit7 means the same but fx-column follows
 SWM_VALUE__PATTERN_COLUMN2_SMALLFX__MIN = 0x40, SWM_VALUE__PATTERN_SMALLFX__MIN = 0x20,  //Instrument-column/FXcommand-column small effects start here (2nd nybble is fx-value 0..F)

 SWM_MASK__INSTRUMENT_SMALLFX_BASE = 0x70, SWM_MASK__SMALLFX_BASE = 0xF0,  //masks for FX-parts of smallFX in instrument or FX-column
  SWM_MASK__SMALLFX_PARAMETER = 0x0F,  //mask for parameter-part of smallFX in instrument or FX-column
 SWM_VALUE__PATTERN_SMALLFX_BASE__ATTACK = 0x20, SWM_VALUE__PATTERN_SMALLFX_BASE__DECAY = 0x30,
 SWM_VALUE__PATTERN_SMALLFX_BASE__WAVEFORM = 0x40,  //set only waveform-nybble of Wave-control register
 SWM_VALUE__PATTERN_SMALLFX_BASE__SUSTAIN = 0x50, //'SWM_VOLUME_SMALLFX' in in SWM-spec.src //sustain sets note-volume
 SWM_VALUE__PATTERN_SMALLFX_BASE__RELEASE = 0x60, SWM_VALUE__PATTERN_SMALLFX_BASE__CHORD = 0x70,  //the last smallFX in instrument-column (as bit7 means next FX-column)
  SWM_BITVALUE__PATTERN_COLUMN2__NEXTCOLUMN = 0x80, SWM_MASK__PATTERN_COLUMN2 = (0xFF - SWM_BITVALUE__PATTERN_COLUMN2__NEXTCOLUMN), //if bit7 is 1, a next (FX)column follows
 SWM_VALUE__PATTERN_SMALLFX_BASE__VIBRATO_AMPLITUDE = 0x80, SWM_VALUE__PATTERN_SMALLFX_BASE__VIBRATO_PERIOD = 0x90,
 SWM_VALUE__PATTERN_SMALLFX_BASE__MAIN_VOLUME = 0xA0,  //'SWM_MAIN_VOLUME_SMALLFX' in SWM-spec.src
 SWM_VALUE__PATTERN_SMALLFX_BASE__FILTERBAND = 0xB0, SWM_VALUE__PATTERN_SMALLFX_BASE__CHORDSPEED = 0xC0,
 SWM_VALUE__PATTERN_SMALLFX_BASE__DETUNE = 0xD0,
 SWM_VALUE__PATTERN_SMALLFX_BASE__WAVECONTROL = 0xE0,  //sets Control-nybble of waveform-control register (test/ring/sync)
 SWM_VALUE__PATTERN_SMALLFX_BASE__RESONANCE = 0xF0,  //Filter-resonance setting

 //SWM_VALUE__PATTERN_BIGFX__NOP = 0x00,  //doesn't happen  //Big-effects come (followed by 1 byte parameter):
 SWM_VALUE__PATTERN_BIGFX__PITCHSLIDE_UP = 0x01, SWM_VALUE__PATTERN_BIGFX__PITCHSLIDE_DOWN = 0x02,
 SWM_VALUE__PATTERN_BIGFX__PORTAMENTO = 0x03, SWM_VALUE__PATTERN_BIGFX__WAVECONTROL = 0x04,  //Set Waveform-control register
  SWM_VALUE__PATTERN_BIGFX_PARAMETER__LEGATO = 0xFF,
 SWM_VALUE__PATTERN_BIGFX__ATTACK_DECAY = 0x05, SWM_VALUE__PATTERN_BIGFX__SUSTAIN_RELEASE = 0x06,
 SWM_VALUE__PATTERN_BIGFX__CHORD = 0x07, SWM_VALUE__PATTERN_BIGFX__VIBRATO = 0x08,  //'SWM_VIBRATO_BIGFX' in SWM-spec.src, Set Vibrato Amplitude & Rate
 SWM_VALUE__PATTERN_BIGFX__WFTABLE_JUMP = 0x09, SWM_VALUE__PATTERN_BIGFX__PWTABLE_JUMP = 0x0A,  //Go to WF-ARP-table / PW-table-position
  SWM_VALUE__PATTERN_BIGFX__FILTERTABLE_JUMP = 0x0B,  //Go to Filter table-position
 SWM_VALUE__PATTERN_BIGFX__CHORDSPEED = 0x0C,  //Set Chord-speed (or arp.speed)
 SWM_VALUE__PATTERN_BIGFX__NOTE_DETUNE = 0x0D,  //'SWM_DETUNE_FX' in SWM-spec.src, Detune current note (upwards)
 SWM_VALUE__PATTERN_BIGFX__PULSEWIDTH = 0x0E, SWM_VALUE__PATTERN_BIGFX__FILTER_CUTOFF_FREQ = 0x0F,  //Set Filter Cutoff-freq. hi-byte
 SWM_VALUE__PATTERN_BIGFX__TEMPO = 0x10, SWM_VALUE__PATTERN_BIGFX__FUNKTEMPO = 0x11,  //swing-tempo  //Set Main (on-all-channels) tempo
  SWM_VALUE__PATTERN_BIGFX__TEMPO_PROGRAM = 0x12,  //Launch Main (on-all-channels) Tempo-program
 SWM_VALUE__PATTERN_BIGFX__TRACK_TEMPO = 0x13, SWM_VALUE__PATTERN_BIGFX__TRACK_FUNKTEMPO = 0x14,  //Set Track's single/individual tempo
  SWM_VALUE__PATTERN_BIGFX__TRACK_TEMPO_PROGRAM = 0x15,  //Launch Tempo-program for track
 SWM_VALUE__PATTERN_BIGFX__VIBRATO_TYPE = 0x16,  //Select Vibrato-type (0/10/20/30)  //SWM_VALUE__PATTERN_BIGFX__RESERVED = 0x17..0x1B,
 SWM_VALUE__PATTERN_BIGFX__FILTER_SHIFT = 0x1C,  //Shift Cutoff hi-byte (permanent)
 SWM_VALUE__PATTERN_BIGFX__TRACK_DELAY = 0x1D,  //Delay track by $00..$ff frames
 SWM_VALUE__PATTERN_BIGFX__NOTE_DELAY = 0x1E, //'SWM_NOTEDELAY_FX' in SWM-spec.src, Delay note by max=tempo-3 frames
 SWM_VALUE__PATTERN_BIGFX__FILTER_CONTROL = 0x1F  //Set FiltSw/Reso register ($d417)
};

typedef struct /*SWMinstrumentControlFlags*/ { //at instrument-header position 0 - instrument configuration bits
 //be careful about endianness of this bit-field structured instrument-configuration byte
 unsigned char HRtimer  : 2; //bit0,1 on C64, only values 0..2 are used in player (might be 3 for NTSC support)
 unsigned char Staccatto: 1; //bit2 on C64, if set to 1 it's staccatto (waveform is $18 in HR-phase - testbit&mute)
 unsigned char WFrame1  : 1; //bit3 on C64, if 1 then 1st-frame waveform is set, otherwise WF is not disturbed on frame1 (inst.parameter $F is 1st waveform since SID-Wizard v1.2, workaround for old tunes: if $00 at $F then set it to $09 )
 unsigned char VibType  : 2; //bit4,5 on C64, $00:increasing   $10:normal  ,  $20:down-oriented  , $30:up-oriented
 unsigned char PWreset  : 1; //bit6 on C64, if value is 1, pulsewidth-table-reset will be disabled until an instrument-selection command
 unsigned char FILTreset: 1; //bit7 on C64, if value is 1, filtertable-reset will be disabled until an instrument-selection command
} SWMinstrumentControlFlags;  //more info can be seen in SIDwizard 'displayer1.inc' 'conbits' data  (and this info is from sng2swm's swm.h)

typedef struct /*SWMvibratoAmpAndSpeed*/ {
 unsigned char Period:4, Amplitude:4;
} SWMvibratoAmpAndSpeed;

//static const unsigned char SWMtempoPatternTemplate [] = {
// (SWM_VALUE__PATTERN_COLUMN1__NOP | SWM_BITVALUE__PATTERN_COLUMN1__NEXTCOLUMN),
// (SWM_VALUE__PATTERN_COLUMN2__NOP | SWM_BITVALUE__PATTERN_COLUMN2__NEXTCOLUMN),
// SWM_VALUE__PATTERN_BIGFX__TEMPO  /*SWM_VALUE__PATTERN_BIGFX__FUNKTEMPO*/, SWM_VALUE__SUBTUNE_TEMPO_DEFAULT,
// SWM_VALUE__PATTERN_COLUMN1__PATTERN_END
//};

typedef struct /*SWMstatistics*/ {  //filled out by statistic-maker function once which scans through the whole tune
 //The whole struct and code uses 1-based SWM numbering: first valid instrument or note is at index 1
 int Instrument_NoteCounts [SWM_INSTRUMENT_AMOUNT_MAX] [SID_CHANNELCOUNT];  //how many times instruments are used on different channels
 int FilterControllerInstrumentSelection_Counts [SID_CHANNELCOUNT];  //to determine if any filter happens on the channel (will help to decide on setting filter-off program for non-filtered SWM instruments on the same channel as a filtered instrument, like e.g. unfiltered snare between filtered basses)
 //Registered instrument/note/etc. references in patterns by scanned-through 'played' tune.
 //If patterns are used in different orders/channels in orderlist sometimes, and their note/instrument is not set before the slides/tablejumps of interest,
 //the note/instrument can be different on different calls of the same pattern, in that case 1st one wins (better having the unavoidable related problems at end of tune instead of the beginning)
 //(To tell this situation, these arrays are set to $00 (undefined/invalid note/instrument) before filling them by the statistics-routine.
 unsigned char CurrentNotes [SWM_PATTERN_AMOUNT_MAX] [SWM_PATTERN_SIZE_MAX];  //1-based numbering //used note on every pattern-row, usable for note-dependent calculated slide pitch-calculations (which SID-Wizard normal/extra/medium player does)
 unsigned char CurrentInstruments [SWM_PATTERN_AMOUNT_MAX] [SWM_PATTERN_SIZE_MAX];  //1-based numbering //used instrument on every pattern-row, usable to see which instrument should a WF/PW/Filter-table jump patternFX should point into
 unsigned char CurrentChords [SWM_PATTERN_AMOUNT_MAX] [SWM_PATTERN_SIZE_MAX];  //1-based numbering //used chord on every pattern-row, usable to see which wavetable-position for the current instrument should a WF/PW/Filter-table jump patternFX should point into
} SWMstatistics;

const unsigned char SWexpTabH []= { //values taken from 'player.asm' of SID-Wizard 1.2
 0, //this extra 0 is added here (compared to SW or sng2swm table) to have nice multiple-of-12 table-size for slide-calculations
 0,0,0,0,0,0,0,0, 0,0,
 0, //FOR UNCALCULATED ZERO VIBRATO  //end of first 12byte section (1 dummy octave with all zeros)
 //frequency-high-byte table ('FREQTBH') starts here
 0x01,0x01,0x01, 0x01,0x01,0x01,   //;0  ;13  ;SWM C-1 note... ;16TH ELEMENT IS 0x01 IN EXP.TABLE  SWM_FREQTBH_POS = 11
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
 //0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,0xFF  //could be added as an extra imprecise slide-calculation range instead of simple clipping
};
const unsigned char SWexpTabL []= { //values taken from 'player.asm' of SID-Wizard 1.2
 0x07,0x16,0x27,0x38,0x4b,0x5e, 0x73,0x89,0xa1,0xba,0xd4,0xf0, 0x0d,0x2c,0x4e,0x71,0x96,0xbd, 0xe7,0x13,0x42,0x74,0xa8,0xe0,
 0x1b,0x59,0x9c,0xe2,0x2c,0x7b, 0xce,0x27,0x84,0xe8,0x51,0xc0, 0x36,0xb3,0x38,0xc4,0x59,0xf6, 0x9d,0x4e,0x09,0xd0,0xa2,0x81,
 //---------------------------------------------
 0x6d,0x67,0x70,0x88,0xb2,0xed, 0x3a,0x9c,0x13,0xa0,0x44,0x02, 0xda,0xce,0xe0,0x11,0x64,0xda, 0x75,0x38,0x26,0x40,0x89,0x04,
 0xb4,0x9c,0xc0,0x22,0xc8,0xb4, 0xeb,0x71,0x4c,0x80,0x12,0x08, 0x68,0x38,0x80,0x45,0x90,0x68, 0xd6,0xe3,0x98,0x00,0x24,0x10
};
enum { SWM_FREQTBH_POS = 12, //11, //position of freq.table inside SWexpTabH[]
       SWM_EXPTHRESHOLD = sizeof(SWexpTabH), //108=12*(8+1), dummy octave and 8 octaves, //(SWM_FREQTBH_POS + SWM_FREQTB_SIZE) /*107*/  //(size of exp.table array)
        SWM_FREQTB_SIZE = (SWM_EXPTHRESHOLD - SWM_FREQTBH_POS) /*96=12*8, 8 audible octaves*/ };


//GoatTracker SNG format descriptor constants - in order they appear in the SNG file ==============
//#include "sng.h"
enum {  //sng2swm swm.h contains similar data, but this here is more recent and more organized (and built-in)
 SNG_ORDERLIST_SEQUENCE_SIZE_MAX = 256,
 SNG_VALUE__SEQUENCE_TRANSPOSE_MIN = 0xE0, SNG_VALUE__SEQUENCE_TRANSPOSE_MID = 0xF0, SNG_VALUE__SEQUENCE_TRANSPOSE_MAX = 0xFE,
 SNG_VALUE__SEQUENCE_LOOP = 0xFF, //always followed by the loop-position

 SNG_INSTRUMENT_AMOUNT_MAX = 0x3F, SNG_INSTRUMENT_NAME_SIZE_MAX = 16,
 SNG_INSTRUMENTTABLE_LENGTH_MAX = 256, SNG_INSTRUMENT_NUMBER_DEFAULT = 0x00,  //in sng-file intrument starts from 0
 SNG_VALUE__INSTRUMENT_FRAME1_WAVECONTROL_SKIP = 0x00,  //this causes inaudible sound as it doesn't set gate to 1 and seemingly doesn't start wavetable execution
  SNG_VALUE__INSTRUMENT_FRAME1_WAVEFORM_SKIP_MIN = 0xFE, //skips waveform-setting, but sets gate according to bit1 (this value is used as a mask in the player to the running waveform-control register)
   SNG_VALUE__INSTRUMENT_FRAME1_WAVEFORM_SKIP_GATE_OFF = (SNG_VALUE__INSTRUMENT_FRAME1_WAVEFORM_SKIP_MIN | 0) /*0xFE*/,
   SNG_VALUE__INSTRUMENT_FRAME1_WAVEFORM_SKIP_GATE_ON = (SNG_VALUE__INSTRUMENT_FRAME1_WAVEFORM_SKIP_MIN | 1) /*0xFF*/,
 SNG_VALUE__INSTRUMENT_VIBRATO_DELAY__DISABLE_VIBRATO = 0x00,  //delay 0 disables instrument-based vibrato according to GT docs (but vibrato pattern effect can still bring vibrato)
  SNG_VALUE__INSTRUMENT_VIBRATO_DELAY_VALID_MIN = 0x01, //smallest delay for an enabled instrument-based vibrato
 SNG_BITVALUE__INSTRUMENT_HRTIMER_NOHR = 0x80, SNG_BITVALUE__INSTRUMENT_HRTIMER_NO_HRGATEOFF = 0x40,
  SNG_MASK__HARDRESTART_FLAGS = (SNG_BITVALUE__INSTRUMENT_HRTIMER_NOHR | SNG_BITVALUE__INSTRUMENT_HRTIMER_NO_HRGATEOFF),
  SNG_VALUE__HARDRESTART_TIMER_MIN = 0x01, //GoatTracker UI doesn't allow $00/$80 (if got in there by conversion it bugged the UI and playback slowed down)
 SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE = 0x00, SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN = 0x01, //jump-targets account for this
 SNG_VALUE_INSTRUMENTTABLE_EMPTY = 0x00,

 SNG_VALUE__WFTABLE__REPEAT_MIN = 0x00, SNG_VALUE__WFTABLE__REPEAT_MAX = 0x0F,
 SNG_VALUE__WFTABLE__WAVECONTROL_MIN = 0x10, SNG_VALUE__WFTABLE__WAVECONTROL_MAX = 0xEF,
 SNG_VALUE__WFTABLE__RUN_FX__MIN = 0xF0, SNG_VALUE__WFTABLE__RUN_FX__MAX = 0xFE,  //WF-colum calls pattern-FX 1..D (0,8,E are illegal), arp-column is its parameter (might be solution to smallFX beside bigFX)
 SNG_VALUE_MIN__ARPTABLE__POSITIVE_SHIFT = 0x00, SNG_VALUE__ARPTABLE__NO_SHIFT = SNG_VALUE_MIN__ARPTABLE__POSITIVE_SHIFT,
  SNG_VALUE_MAX__ARPTABLE__POSITIVE_SHIFT = 0x5F,
 SNG_VALUE_MIN__ARPTABLE__NEGATIVE_SHIFT = 0x60, SNG_VALUE_MAX__ARPTABLE__NEGATIVE_SHIFT = 0x7F, //lowest value gives max. negative shift
 SNG_VALUE__ARPTABLE__NOP = 0x80,  //leaves current frequency unchanged
 SNG_VALUE__ARPTABLE__ABSOLUTE_NOTE_MIN = 0x81, SNG_VALUE__ARPTABLE__ABSOLUTE_NOTE_MAX = 0xDF, //C#0..B-7 (SWM starts from C-0!)

 SNG_VALUE_MIN__PWTABLE_COMMAND__SET_PULSEWIDTH = 0x80, SNG_VALUE_MAX__PWTABLE_COMMAND__SET_PULSEWIDTH = 0xFD, //theoretical max, usually using only $8X
  SNG_VALUE__PWTABLE_COMMAND__STOP,  //Not documented in GT readme, but '$00' in left table column stops execution
 SNG_VALUE__PWTABLE_COMMAND__SWEEPTIME_MIN = 0x01, SNG_VALUE__PWTABLE_COMMAND__SWEEPTIME_MAX = 0x7F, //parameter is 8bit 2nd-complement up/down add-value.

 SNG_VALUE_MIN__FILTERTABLE_COMMAND__SET_BAND_RESO_SWITCH = 0x80, SNG_VALUE_MAX__FILTERTABLE_COMMAND__SET_BAND_RESO_SWITCH = 0xF0, //filterband is in the command high-nybble, resonance+channelswitches are in the parameter
  SNG_CHANNEL__FILTERSWITCH_FALLBACK = 1, SNG_VALUE__FILTERSWITCH_FALLBACK = (1 << SNG_CHANNEL__FILTERSWITCH_FALLBACK),
 SNG_VALUE__FILTERTABLE_COMMAND__SET_CUTOFF = 0x00, //parameter is 8bit cutoff-frequency value
 SNG_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MIN = 0x01, SNG_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MAX = 0x7F, //8bit filter-cutoff in GT? or just the slide-value?
  SNG_VALUEDIFF__FILTERTABLE_COMMAND__SWEEPTIME_MIN = (SNG_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MIN - SWM_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MIN),
 SNG_VALUEMASK__FILTERTABLE_COMMAND__SET_BAND = 0xF0,
 SNG_VALUEMASK__FILTERTABLE_PARAMETER__SET_RESONANCE = 0xF0, SNG_BITPOS__FILTERTABLE_COMMAND__SET_RESONANCE = 4,
  SNG_SHIFTS__FILTERTABLE__SET_RESONANCE = (SNG_BITPOS__FILTERTABLE_COMMAND__SET_RESONANCE - SWM_BITPOS__FILTERTABLE_COMMAND__SET_RESONANCE),

 SNG_VALUE_INSTRUMENT_TABLE_JUMP = 0xFF, SNG_VALUE_INSTRUMENT_TABLEJUMP_PARAMETER_END = 0x00,

 SNG_BITVALUE__SPEEDTABLE_PERIOD__CALCULATED_VIBRATO = 0x80,  //if bit turned on, GT has calculated vibrato (for normal/extra/medium SW players)
 SNG_VALUE__SPEEDTABLE__SLIDE_TOTAL_RANGE = 0x10000,  //can be entered into speed-table, but much more than needed/supported
 SNG_VALUE__SPEEDTABLE__NONCALCULATED_SLIDE_RANGE = 0x1000,  //about above $1000..$1800 portamento GT produces strange results
  SNG_VALUE__SPEEDTABLE__NONCALCULATED_SLIDE_MAX = (SNG_VALUE__SPEEDTABLE__NONCALCULATED_SLIDE_RANGE-1) /*0xFFF*/,
  SNG_VALUE__SPEEDTABLE__NONCALCULATED_SLIDE_DIV = (SNG_VALUE__SPEEDTABLE__SLIDE_TOTAL_RANGE / SNG_VALUE__SPEEDTABLE__NONCALCULATED_SLIDE_RANGE),
 SNG_VALUE__SPEEDTABLE__NONCALCULATED_PORTAMENTO_RANGE = 0x800,  //about above $D00 portamento GT produces strange results
  SNG_VALUE__SPEEDTABLE__NONCALCULATED_PORTAMENTO_MAX = (SNG_VALUE__SPEEDTABLE__NONCALCULATED_PORTAMENTO_RANGE-1) /*0x7FF*/,

 SNG_PATTERN_COUNT_MAX = 256 /*theoretical, orderlist can have less*/,
 SNG_PATTERN_LENGTH_MAX = 128, //256,  //GoatTracker doesn't allow longer than 128-row patterns (e.g. 'Magyar Nepzenek' last subtune sucks)
 SNG_VALUE__PATTERN_COLUMN1__NOTE_MIN = 0x60, SNG_VALUE__PATTERN_COLUMN1__NOTE_MAX = 0xBC,
 SNG_VALUE__PATTERN_COLUMN1__NOP = 0xBD,
 SNG_VALUE__PATTERN_COLUMN1__GATEOFF = 0xBE /*start 'rest'*/, SNG_VALUE__PATTERN_COLUMN1__GATEON = 0xBF /*restart note*/,
 //SNG_VALUE__PATTERN_COLUMN1__PACKED_NOP_MIN = 0xC0, SNG_VALUE__PATTERN_COLUMN1__PACKED_NOP_MAX = 0xFE,  //no need to pack sng (it's only used to reduce size of exported SNG tunes)
 SNG_VALUE__PATTERN_COLUMN1__PATTERN_END = 0xFF, SNG_LENGTH__PATTERN_ENDMARK = 1 /*row with 4 bytes: $FF $00 $00 $00 */,
 SNG_VALUE__PATTERN_COLUMN2__NOP = 0x00,
 SNG_VALUE__PATTERN_COLUMN2__INSTRUMENT_MIN = 0x01, SNG_VALUE__PATTERN_COLUMN2__INSTRUMENT_MAX = 0x3F,
 SNG_VALUE_MIN__PATTERN_FX = 0x00,
  SNG_VALUE__PATTERN_FX__NOP = 0x00,  //Do nothing. parameter will always be $00
  SNG_VALUE__PATTERN_FX__PITCHSLIDE_UP = 0x01, SNG_VALUE__PATTERN_FX__PITCHSLIDE_DOWN = 0x02,  //parameter is an index to a 16-bit speed value in the speedtable
  SNG_VALUE__PATTERN_FX__PORTAMENTO = 0x03,  //parameter is an index to a 16-bit speed value in the speedtable
   SNG_VALUE__PATTERN_FX_PARAMETER__LEGATO = 0x00,  //portamento with parameter=$00 is legato
   SNG_VALUE__PATTERN_FX_PARAMETER__NOTECOLUMN_PORTAMENTO = 0x70,  //a middle-ish value like in SID-Wizard
  SNG_VALUE__PATTERN_FX__VIBRATO = 0x04,  //parameter is an index to the speedtable, where left side determines how long until the direction changes (speed), and right side determines the amount of pitch change on each tick (depth)
  SNG_VALUE__PATTERN_FX__ATTACK_DECAY = 0x05, SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE = 0x06,
  SNG_VALUE__PATTERN_FX__WAVECONTROL = 0x07,  //Set waveform register to value XY. If a wavetable is actively changing the channel's waveform at the same time, will be ineffective.
  SNG_VALUE__PATTERN_FX__WFTABLE_JUMP = 0x08,  //Set wavetable pointer. $00 stops wavetable execution.
  SNG_VALUE__PATTERN_FX__PWTABLE_JUMP = 0x09,  //Set pulsewidth-table pointer. $00 stops wavetable execution.
  SNG_VALUE__PATTERN_FX__FILTERTABLE_JUMP = 0x0A,  //Set filter-table pointer. $00 stops wavetable execution.
  SNG_VALUE__PATTERN_FX__FILTER_CONTROL = 0x0B,  //BXY: Set filter control. X is resonance and Y is channel bitmask (as in $d417 register), $00 turns filter off and also stops filtertable execution.
   SNG_VALUE__PATTERN_FX__FILTER_CONTROL__STOP = 0x00, SNG_VALUE__PATTERN_FX__FILTER_CONTROL__MIN = 0x10,
  SNG_VALUE__PATTERN_FX__FILTER_CUTOFF_FREQ = 0x0C,  //Set filter cutoff to XY. Can be ineffective if the filtertable is active and also changing the cutoff.
  SNG_VALUE__PATTERN_FX__MAINVOLUME_OR_TIMINGMARK = 0x0D,  //DXY: Set mastervolume to Y, if X is $0. If X is not $0, value XY is copied to the timing mark location, which is playeraddress+$3F.
  SNG_VALUE__PATTERN_FX__FUNKTEMPO = 0x0E,  //Funktempo. parameter is an index to the speedtable, tempo will alternate between left side value and right side value on subsequent pattern steps. Sets the funktempo active on all channels, but you can use the next command ($0F) to override this per-channel.
  SNG_VALUE__PATTERN_FX__SET_TEMPO = 0x0F,  //Set tempo. Values $03-$7F set tempo on all channels, values $83-$FF only on current channel (subtract $80 to get actual tempo). Tempos $00-$01 recall the funktempo values set by EXY command.
   SNG_VALUE__PATTERN_FX__TEMPO__MIN = 3, SNG_BITVALUE__PATTERN_FX__TRACK_TEMPO = 0x80,
    SNG_MASK__PATTERN_FX__TEMPO = (0xFF - SNG_BITVALUE__PATTERN_FX__TRACK_TEMPO),
 SNG_VALUE_MAX__PATTERN_FX = 0x0F
};

struct /*SNGheader*/ {
 const char    MagicString [4];  // "GTS5" for GoatTracker2
 unsigned char TuneTitle [32], AuthorName [32], ReleaseInfo [32];
 unsigned char SubtuneCount;
} SNGheader = { .MagicString = "GTS5",
                .TuneTitle = "", .AuthorName = "", .ReleaseInfo = "",
                .SubtuneCount = 1 };

typedef struct /*SNGorderListSequence*/ {  //sequence for an orderlist
 unsigned char Size; unsigned char Data [SNG_ORDERLIST_SEQUENCE_SIZE_MAX]; unsigned char RestartPosition;
} SNGorderListSequence;

typedef struct /*SNGsubtuneOrderList*/ {  //orderlist for a subtune
 SNGorderListSequence Sequences [SID_CHANNELCOUNT];
} SNGsubtuneOrderList;

SNGsubtuneOrderList* SNGsubtuneOrderLists = NULL;

typedef struct /*SNGinstrumentHeader*/ {
 unsigned char AttackDecay, SustainRelease,
               WaveformArpTableIndex, PulseWidthTableIndex, FilterTableIndex,
               Vibrato_SpeedTableIndex, VibratoDelay, HardRestartTime, Frame1WaveForm;
 char Name [SNG_INSTRUMENT_NAME_SIZE_MAX];
} SNGinstrumentHeader;

typedef struct /*SNGinstrumentTable*/ {
 unsigned char Length;
 unsigned char Columns [2] [SNG_INSTRUMENTTABLE_LENGTH_MAX];
} SNGinstrumentTable;

struct /*SNGinstrumentData*/ {
 unsigned char InstrumentCount;
 SNGinstrumentHeader InstrumentHeaders [SNG_INSTRUMENT_AMOUNT_MAX];
 SNGinstrumentTable WaveformArpTable, PulseWidthTable, FilterTable, SpeedTable;
} SNGinstrumentData;

typedef struct /*SNGpatternRow*/ {
 unsigned char Note, Instrument, FX, FXparameter;
} SNGpatternRow;

enum { SNG_PATTERNLENGTH_SAFETY_MULTIPLIER = 2 /*times*/ };
typedef struct /*SNGpattern*/ {
 unsigned char Length;  //GT2 patterns start with their length (number of rows including $FF end-row) and each line is a pattern-row structure...
 SNGpatternRow Rows [ SNG_PATTERN_LENGTH_MAX * SNG_PATTERNLENGTH_SAFETY_MULTIPLIER ];  //the last line of the patterns is the 0xFF (followed by 3 zeroes in the file)
} SNGpattern;

struct /*SNGpatternData*/ {
 unsigned char PatternCount;
 SNGpattern Patterns [SNG_PATTERN_COUNT_MAX];
} SNGpatternData;

const unsigned char GTexpTabH [] = {  //array generated from binary by bin2array 1.1 (made in 2013 by Hermit)
 0, //this extra 0 is added here (compared to SW or sng2swm table) to have nice multiple-of-12 table-size for slide-calculations
 0,0,0,0,0,0,0,0, 0,0,
 0, //FOR UNCALCULATED ZERO VIBRATO  //end of first 12byte section (1 dummy octave with all zeros)
 //frequency-high-byte table ('FREQTBH') starts here
 0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,
 0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x03,0x03,0x03,0x03,0x03,
 0x04,0x04,0x04,0x04,0x05,0x05,0x05,0x06,0x06,0x06,0x07,0x07,
 0x08,0x08,0x09,0x09,0x0A,0x0A,0x0B,0x0C,0x0D,0x0D,0x0E,0x0F,
 0x10,0x11,0x12,0x13,0x14,0x15,0x17,0x18,0x1A,0x1B,0x1D,0x1F,
 0x20,0x22,0x24,0x27,0x29,0x2B,0x2E,0x31,0x34,0x37,0x3A,0x3E,
 0x41,0x45,0x49,0x4E,0x52,0x57,0x5C,0x62,0x68,0x6E,0x75,0x7C,
 0x83,0x8B,0x93,0x9C,0xA5,0xAF,0xB9,0xC4,0xD0,0xDD,0xEA,0xF8,
 //0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,0xFF
};
const unsigned char GTexpTabL [] = {
 0x07,0x16,0x27,0x38,0x4B,0x5E,0x73,0x89,0xA1,0xBA,0xD4,0xF0,
 0x0D,0x2C,0x4E,0x71,0x96,0xBD,0xE7,0x13,0x42,0x74,0xA8,0xE0,
 0x1B,0x59,0x9C,0xE2,0x2C,0x7B,0xCE,0x27,0x84,0xE8,0x51,0xC0,
 0x36,0xB3,0x38,0xC4,0x59,0xF6,0x9D,0x4E,0x09,0xD0,0xA2,0x81,
 0x6D,0x67,0x70,0x88,0xB2,0xED,0x3A,0x9C,0x13,0xA0,0x44,0x02,
 0xDA,0xCE,0xE0,0x11,0x64,0xDA,0x75,0x38,0x26,0x40,0x89,0x04,
 0xB4,0x9C,0xC0,0x22,0xC8,0xB4,0xEB,0x71,0x4C,0x80,0x12,0x08,
 0x68,0x38,0x80,0x45,0x90,0x68,0xD6,0xE3,0x98,0x00,0x24,0x10,
};

//to get the roundabout SW-vs-GT portamento/slide curve (non-calculated: no note-dependent freq-correction, SWM is for light/bare/demo playertype):
/*int SNGslideSpeeds [] = {  //adjusted by ears on my 'End of the World' cover (not as precise as SW)
 0, //the 'no slide' is at 1st position,    //GT calculated portamento-value equivalents of SW portamento/8 (0..1F)
 0x4, //the minimum nonzero slide is at 2nd position (used by code to overcome resolution-decrease)
 0x08,0x0C,0x10,0x14,0x18,0x20, 0x30,0x50,0x70,0xB0,0xF0,0x130,0x170,0x1C0,
 0x200,0x240,0x280,0x2C0,0x300,0x340,0x380,0x3C0, 0x400,0x440,0x480,0x4C0,0x500,0x540,0x580,0x5C0  //above about $1000..$1800 GT portamento goes haywire
};*/
const int SNGslideSpeeds [] = {  //adjusted by ears on my 'End of the World' cover (not as precise as SW)
 0, //the 'no slide' is at 1st position,    //GT calculated portamento-value equivalents of SW portamento/8 (0..1F)
 0x4, //the minimum nonzero slide is at 2nd position (used by code to overcome resolution-decrease)
 0x08,0x0C,0x10,0x14,0x18,0x20, 0x30,0x50,0x70,0xB0,0x130,0x200,0x400,0x600,
 0x800,0xC00,0x1000,0x1400,0x1800,0x1C00,0x2000,0x2400, 0x2800,0x3000,0x3800,0x4000,0x4800,0x5000,0x5800,06000  //above about $1000..$1800 GT portamento goes haywire
};

const int SNGportamentoSpeeds [] = {  //adjusted by ears on my 'End of the World' cover (not as precise as SW)
 0, //the 'no slide' is at 1st position,    //GT calculated portamento-value equivalents of SW portamento/8 (0..1F)
 0x8, //8, //the minimum nonzero slide is at 2nd position (used by code to overcome resolution-decrease)
 0x10,0x18,0x20,0x28,0x34,0x40, 0x50,0x68,0x88,0xC0,0x100,0x180,0x200,0x280,
 0x300,0x380,0x400,0x480,0x500,0x580,0x600,0x700, 0x800,0x900,0xA00,0xC00,0xE00,0x1000,0x1200,0x1400  //above about $1000..$1800 GT portamento goes haywire
};

const unsigned char SNGcalculatedPortamentoSpeedDivisors [] = {  //adjusted by ears on my 'End of the World' cover (not as precise as SW)
 4, //the 'no slide' is at 1st position,
 3, //the minimum nonzero slide is at 2nd position (used by code to overcome resolution-decrease)
 3,3,3,2,2,1, 1,1,1,1,1,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0  //GT calculated slide divisor equivalents of SW slide/8 (0..1F)
};

const unsigned char SNGcalculatedSlideSpeedDivisors [] = {  //adjusted by ears on my 'End of the World' cover (not as precise as SW)
 5, //the 'no slide' is at 1st position,
 4, //the minimum nonzero slide is at 2nd position (used by code to overcome resolution-decrease)
 4,4,4,3,3,3, 2,2,2,1,1,1,1,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0  //GT calculated slide divisor equivalents of SW slide/8 (0..1F)
};

const unsigned char SNGcalculatedVibratoAmplitudeDivisors [] = {  //adjusted by ears based on my vibrato.swm testcase (cycling through vibrato amplitude 0..F(
 7,6,6,5,5,5,4,4, 4,3,3,3,2,2,1,0  //GT calculated vibrato amplitude-divisor equivalents of SW vibrato-amplitudes 0..F
};

const unsigned char GTvibRates [] = { 0x7F,0,1,1,2,2,3,4, 5,6,7,8,0xA,0xC,0x10,0x14 }; //GT equivalents of SW vibrato-periods 0..F, adjusted by ears with my 'vibrate.swm' testfile


//XM format descriptor constants - in order they appear in XM file==============
#define XM_ID_OFFSET 0
 char* XM_ID="Extended Module: ";
 #define XM_ID_SIZE 18
#define XM_TITLE_OFFSET 17
 #define XM_TITLE_SIZE 20 //length of title is 20 characters
#define XM_$1A_OFFSET 37
#define XM_TRACKERNAME_OFFSET 38 //Digitracker:Author-name, FastTracker:"FastTracker II"
 char* XM_TRACKERNAME="SID-Wizard";
 #define XM_TRACKERNAME_SIZE 20 //lenth of trackername is 20 characters
#define XM_VERSION_OFFSET 58  //little endian word - $04 then $01 ($0104) for this specification
 unsigned int XM_VERSION=0x0104;
#define XM_HEADERSIZE_OFFSET 60 //at offset 60 the header-size of the module is a double-word
 unsigned char XM_HEADERSIZE[4]={0x14,0x01,0,0}; //e.g. $14 $01 $00 $00
#define XM_SONGLENGTH_OFFSET XM_HEADERSIZE_OFFSET+4 //(l.e.word) Song length (in patten order table)
 unsigned int XMsongLength=0;
#define XM_RESTARTPOS_OFFSET XM_HEADERSIZE_OFFSET+6 //(l.e.word) Restart position
 unsigned int XMrestartPos=0;
#define XM_CHANNELCOUNT_OFFSET XM_HEADERSIZE_OFFSET+8 //(l.e.word) Number of channels (2,4,6,8,10,...,32)
#define XM_PTNAMOUNT_OFFSET XM_HEADERSIZE_OFFSET+10 //(l.e.word) Number of patterns (max 256)
 unsigned int XMpatternCount;
#define XM_INSAMOUNT_OFFSET XM_HEADERSIZE_OFFSET+12 //(l.e.word) Number of instruments (max 128)
#define XM_FREQTYPE_OFFSET XM_HEADERSIZE_OFFSET+14 //(l.e.word) Flags:
 unsigned int XM_FREQTYPE=1; //bit 0: 0 = Amiga frequency table (see below); 1 = Linear frequency table
#define XM_DEFTEMPO_OFFSET XM_HEADERSIZE_OFFSET+16 //(l.e.word) Default tempo
#define XM_DEFAUBPM_OFFSET XM_HEADERSIZE_OFFSET+18 //(l.e.word) Default BPM
 unsigned int XM_DEFAUBPM=0x7d;
#define XM_ORDERLIST_OFFSET XM_HEADERSIZE_OFFSET+20 //256 bytes (fix size) Pattern order table
 #define XM_ORDERLIST_SIZE 256
//XM PATTERN FORMAT - pattern starts with 4byte  (dword) Pattern header-length value
#define XM_PATTERNS_OFFSET XM_ORDERLIST_OFFSET+XM_ORDERLIST_SIZE
 unsigned char XM_PATTERN_HEADER_SIZE[4]={0x09,0,0,0}; //(dword) Pattern header length
#define XM_PACKTYPE_OFFSET 4 //(byte) Packing type (always 0)
 #define XM_PACKTYPE 0
#define XM_PTNROWS_OFFSET 5 //(word) Number of rows in pattern (1..256)
#define XM_PTNSIZE_OFFSET 7 //(word) Packed patterndata size - zero if pattern is empty
//XM_PTNDATA_OFFSET=XM_PATTERN_HEADER_SIZE here starts the packed patterndata
 //unsigned char XM_PACKSIGN[6]={0x80,0x81,0x82,0x84,0x88,0x90}; //NOP,NOTE,INST,VOL,FX,FXVALUE
 #define XM_NOP_PACKSIGN 0x80
 #define XM_NOTE_PACKSIGN 0x81
 #define XM_INST_PACKSIGN 0x82
 #define XM_VOL_PACKSIGN 0x84
 #define XM_FX_PACKSIGN   0x88
 #define XM_FXVAL_PACKSIGN 0x90
 #define XM_GATEOFF_VALUE 97
 #define XM_PORTAMENTO_FX 3
 #define XM_VIBRATO_FX 4
 #define XM_NOTE_VOL_FX 0xC
 #define XM_MAIN_TEMPO_FX 0xF
 #define XM_MAIN_VOL_FX 0x10
 #define XM_LEGATO_FXVALUE 0xff //used together with portamento
 #define XM_LEGATO_MIN_VAL 0xf0 //used for XM-to-SWM conversion to reduce patternsize by making these $3F legato command
 unsigned char BigFxConvTable[0x20]={0, 1,2,3,0,0,XM_NOTE_VOL_FX,0,XM_VIBRATO_FX,0,0,0,0,0xE,0,0, XM_MAIN_TEMPO_FX,0,0,0,0,0, 0,0,0,0,0,0,0, 0,0xE,0 }; //SWM-bigFX - to - XM Fx
 unsigned char SmallFxConvTable[0x10]={0, 0,0,0, 0,XM_NOTE_VOL_FX,0,0, XM_VIBRATO_FX,XM_VIBRATO_FX,XM_MAIN_VOL_FX,0,0,0,0,0}; //SWM-smallFX - to - XM FX conversion table
 unsigned char SWM_bigFX_Oneshot[0x20]={0, 0,0,0, 0,0,1, 0,0,0,0,0,0,0,0,0, 1,0,0,0,0,0, 0,0,0,0,0,0,0, 0,1,0}; //if 1, the corresponding SWM FX is of one-shot (non-continuous) type
 unsigned char SWM_bigFX_NoteResets[0x20]={0, 1,1,1,0,0,0,0,1, 0,0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0,0, 0,0,0}; //if the effect should be reset/deleted at new note...
//XM INSTRUMENT FORMAT - instrument starts with 4byte (dword) Instrument-header size
#define XM_INST_HEADER_SIZE_OFFSET 0
 unsigned char XM_INST_HEADER_SIZE[4]={0x07,0x01,0,0};  //{0x1D,0,0,0};
#define XM_INSNAME_OFFSET 4  // 22 bytes (char) Instrument name
 #define XM_INSNAME_SIZE 22
#define XM_INSTYPE_OFFSET 26 //(byte) Instrument type (random value, but should always be zero)
 #define XM_INSTYPE 0
#define XM_SAMPLEAMOUNT_OFFSET 27 //(word) Number of samples in instrument
 #define XM_SAMPLE_HEADER_SIZE 0x0028 //dword

#define XM_C4_NOTE_PERIOD 64.00 //default sample-period (for default C-5 SWM-note / C-4 XM-note) 49)
#define SWM_C5_NOTE_VALUE  49 //default note value for C-5 SWM note / C-4 XM_NOTE
#define XM_1X_PERIOD (16.35*XM_C4_NOTE_PERIOD/2) //number of sample-periods in 1x (50Hz) framespeed
 unsigned int XM_PERIOD_PER_FRAME; //will be used to calculate periods
 unsigned int ATTACK_times[16]={2,8,16,24,38,56,68,80,100,250,500,800,1000,3000,5000,8000};
 unsigned int DECAY_RELEASE_MaxTimes[16]={6,24,48,72,114,168,204,240,300,750,1500,2400,3000,9000,15000,24000};
#define ADSR_time_ScaleRate 32


//MIDI format descriptor constants======================================
//Header-chunk (every value is in big-endian format)
#define MIDI_ID_OFFSET 0
char* MIDI_ID="MThd";
#define MIDI_ID_SIZE 4
#define MIDI_HEADERSIZE_OFFSET 4 //the header-size of the MIDI is a big-endian double-word
unsigned char MIDI_HEADERSIZE[4]={0,0,0,0x6}; //always 00 00 00 06 for standard MIDI files
#define MIDI_VERSION_OFFSET 8  //big endian 2-byte word
unsigned int InputMIDIver; //(0:single-track,1:multiple-track,2:multiple song)
#define MIDI_TRACK_AMOUNT_OFFSET 0xA //2 byte big-endian word containing number of tracks
unsigned int MIDI_TRACK_AMOUNT;
#define MIDI_DIVISION_OFFSET 0xC //2 byte - If the value is positive, then it represents the units per beat. For example, +96 would mean 96 ticks per beat.
signed int MIDI_DIVISION;
#define MIDI_TRACKS_OFFSET 0xE //here tracks are starting
 //Track-chunk - offset values here are relative to track-chunks' beginnings
 #define MIDI_TRACK_ID_OFFSET 0
 char* MIDI_TRACK_ID="MTrk"; //4 bytes - the literal string MTrk. This marks the beginning of a track. 
 #define MIDI_TRACK_SIZE_OFFSET 4 //4 bytes - the number of bytes in the track chunk following this number. 
 unsigned long MIDI_TRACK_SIZE[64]; //collecting track-sizes here
 unsigned long MIDI_TRACK_OFFSET[64]; //calculating from sizes
 #define MIDI_TRACK_EVENT_OFFSET 8 //a sequenced track event
  //Track-event format (consists of a delta time since the last event, and one of three types of events.)
  #define MIDI_DELTA_TIME_OFFSET 0 //variable-length big-endian value, last databyte has bit7 cleared (others have it set)
  #define MIDI_EVENT_OFFSET 0
  #define MIDI_META_EVENT_OFFSET 0
  #define MIDI_SYSEX_EVENT_OFFSET 0
   //Meta-event format
   #define MIDI_META_TYPE_OFFSET 0
   #define MIDI_META_LENGTH_OFFSET 1 //length of meta event data expressed as a variable length value. 
   #define MIDI_META_EVENT_DATA_OFFSET 0
   //System-exclusive event format
    //A system exclusive event can take one of two forms:
    //sysex_event = 0xF0 + <data_bytes> 0xF7 or sysex_event = 0xF7 + <data_bytes> 0xF7
    //In the first case, the resultant MIDI data stream would include the 0xF0. In the second case the 0xF0 is omitted.  



//-----------------global variable declarations---------------------------------
char InputFileName[FILENAME_LENGTH_MAX]; //input-file described as 1st command-line parameter
char InputTypeID[TYPE_ID_LENGTH_MAX]; //type identifier string inside the source file
char OutputFileName[FILENAME_LENGTH_MAX]; //output-file described as 2nd command-line parameter
FILE *InputFile, *OutputFile; //in/out tune-file descriptors
unsigned char TUNEDATA[TUNEDATA_SIZE_MAX]; //tune-file-data loaded in memory for conversion
unsigned int InputSize=0,OutputSize=0;  //detected size of the loaded/saved tune
unsigned int TYPE_ID_OFFSET,TYPE_ID_LENGTH; //offset & length in file where the file-type ID can be found
unsigned char SWMcalculatedVibSlideON, SWMfilter11bit;

unsigned int InstAmount,PatternAmount,SequenceAmount,SubtuneAmount,ChordTableSize,ChordAmount=0;
unsigned int XMinstAmount;
unsigned long int SampleLength,FrameSampleLength,SampleLoopPos,LoopLength;
char AUTHORINFO[256]; //array to hold tune author-info
unsigned char SEQUENCES[32*SID_CHANNELCOUNT*2] [256]; //array to hold orderlist (3/6 sequences)
unsigned char SWM_SEQ_SIZE[256]; //array of SWM sequence-sizes
unsigned int SWM_SEQ_TOP;
unsigned char TUNETEMPO[32][2]; //subtune-tempos
unsigned char PATTERNS [SWM_PATTERN_AMOUNT_MAX] [SWM_PATTERN_SIZE_MAX]; //array to hold SWM pattern-data
unsigned char PATTERNSIZES [SWM_PATTERN_AMOUNT_MAX]; //array of effective SWM pattern-sizes
//unsigned char PACKEDSIZES[SWM_PATTERN_AMOUNT_MAX]; //compressed pattern-sizes
unsigned char PATTERNLENGTHS [SWM_PATTERN_AMOUNT_MAX]; //array of musical lengths of patterns
static SWMstatistics SWMtuneStatistics;  //better being at global scope so pre-processed instrument/note-position arrays can be reached directly by slide-calculator/etc

 unsigned char XMorderlist[256]; //result pattern-numbers of 3-to-1 orderlist conversion / imported XM orderlist
 unsigned long XMpatternOffset[XM_PATTERNS_MAX]; //offsets of patterns in XM-file
 unsigned int XMpatternLen[XM_PATTERNS_MAX];  //array of musical lengths (number of rows) of XM patterns
 unsigned int XMpatternSize[XM_PATTERNS_MAX]; //array of effective XM pattern-sizes
 unsigned char XMseqTemp[SID_CHANNELCOUNT][256]; //groups of 3 SWM patterns put into XM patterns
 unsigned char XMseqTranspose[SID_CHANNELCOUNT][256]; //transpose-value of sequence-positions
 unsigned char XMseqPos[256]; //skew register for XM-orderlist, where seq-effects are left out
 unsigned char XMpattRef[SID_CHANNELCOUNT][256]; //XM pattern-bunch references to the original SWM pattern-numbers
 unsigned char XMpattTranspose[SID_CHANNELCOUNT][256]; //to detect if pattern-notes need to be transposed
 unsigned char XMpattData[256*32]; //temporary place for one pattern, before writing it to file
 unsigned char XMpattTemp[SID_CHANNELCOUNT][256*XM_COL_PER_TRK]; //holds three temporarily read vertical unpacked XM patterns
 unsigned char XMPATTERNS[XM_PATTERNS_MAX][256*XM_COL_PER_TRK]; //holds registered (non-duplicated) vertical unpacked XM patterns
 unsigned char XMinstName[XM_INSTRUMENTS_MAX][XM_INSNAME_SIZE]; //XM instrument-names
 unsigned char XMinsVibDelay[XM_INSTRUMENTS_MAX],XMinsVibAmp[XM_INSTRUMENTS_MAX],XMinsVibFreq[XM_INSTRUMENTS_MAX];
 unsigned char XMinsSustain[XM_INSTRUMENTS_MAX];
 unsigned int XMinsAttack[XM_INSTRUMENTS_MAX], XMinsDecay[XM_INSTRUMENTS_MAX], XMinsRelease[XM_INSTRUMENTS_MAX];
 signed char XMinsTransp[XM_INSTRUMENTS_MAX];

 unsigned char SWMpattPointer[SID_CHANNELCOUNT]={0,0,0}; //init non-linear SWM data pointer for SWM source
 unsigned char ResetFx[SID_CHANNELCOUNT]={0,0,0}; //signs if an effect should be reset at note-start

unsigned char INSTRUMENTS [SWM_INSTRUMENT_AMOUNT_MAX/*64*/] [SWM_INSTRUMENT_SIZE_MAX/*256*/]; //array to hold instruments
unsigned char INSTRUMENTSIZES [SWM_INSTRUMENT_AMOUNT_MAX/*64*/];

unsigned char ChordIns[64]; //0 if instrument is not chord, otherwise the default chord-number
unsigned char INSTNAMES[SWM_INSTRUMENT_AMOUNT_MAX/*64*/][32]; //array of instrument-names (32 is safe for all formats, in SW it's 'instnamelength' given in SWM-spec.src)
unsigned char CHORDS[256]; //array of chords
unsigned char ChordLen[64]; //length of chords
unsigned char ChordIndex[64]; //base-indexes/positions of chords inside chordtable
unsigned char SNGchordWFtableIndexes [SWM_INSTRUMENT_AMOUNT_MAX+1] [SWM_CHORD_AMOUNT_MAX+1]; //WFtable-chunks of 'chords' are registered here for use by chord-change pattern-FXes
//unsigned char SNG_WFtablePosition;  //follows expansion of WF-table so WFtable patternFX-jumps are corrected accordingly
unsigned char SNG_WFtablePositions [SWM_INSTRUMENT_AMOUNT_MAX+1] [SWM_INSTRUMENT_TABLE_SIZE_MAX];
unsigned char SNG_PWtablePositions [SWM_INSTRUMENT_AMOUNT_MAX+1] [SWM_INSTRUMENT_TABLE_SIZE_MAX];
unsigned char SNG_FilterTablePositions [SWM_INSTRUMENT_AMOUNT_MAX+1] [SWM_INSTRUMENT_TABLE_SIZE_MAX];

unsigned int ActSWMptn, ColumnCount, PackSignPos, ADSRpoint,SWMpatternCount,XMpattPos,SWMseqLength[SID_CHANNELCOUNT];
unsigned char ActNote[64], ActInsFx[64], ActIns[64], ActVol[64], ActFx[64], ActFxVal[64], ActChord[64]; 
unsigned char XM_Note,XM_Ins,XM_Vol,XM_Fx,XM_FxVal;

unsigned char WFchar[16]={'0','T','S','3','P','5','6','7','N','9','a','b','c','d','e','f'}; //WAVEFORM-NIBBLE display (in sample-name)
unsigned char OctavePeriod[16]={16,16,16,16,16,8,4,2,1,1,1,1,1,1,1,1}; //divide framespeed for deeper octave instruments

unsigned long MIDIdeltas[MIDI_CHANNELS_MAX][MIDI_TRACKLEN_MAX], ChannelDelta[MIDI_CHANNELS_MAX]; //midi delta-time-data for different channels
unsigned char TimeSignature[8]={4,2,18,8}; //set default MIDI time-signature
unsigned char MIDInotes[MIDI_CHANNELS_MAX][MIDI_TRACKLEN_MAX]; //midi note-data for different channels
unsigned char MIDIvelos[MIDI_CHANNELS_MAX][MIDI_TRACKLEN_MAX]; //midi velocity-data for different channels
unsigned int MIDItrkIndex[MIDI_CHANNELS_MAX], OKtrk[MIDI_CHANNELS_MAX], TuneBPM=120, MIDItempo=500000;
unsigned char Digits[MIDI_VLV_LEN_MAX]; //stores variable length value digits temporarily when processing MIDI
unsigned char CurrentTrack; //temp for multitrack MIDI's currently processed track-number

unsigned char SWMtoXMvibAmp[16]= {0x0,0x1,0x2,0x3,0x4,0x4,0x5,0x6,0x6,0x6,0x7,0x8,0x9,0xA,0xC,0xF}; //XM amplitude values for SWM amplitude values 0..15
unsigned char SWMtoXMvibFreq[16]={0xF,0xF,0xF,0xE,0xA,0x8,0x7,0x6,0x5,0x4,0x4,0x3,0x3,0x3,0x3,0x3}; //XM frequency values for SWM frequency values 0..15


unsigned char XMtoSWMvibAmp(unsigned char XMamp) {
 int i;
 for (i=0; i<=15; i++) if(SWMtoXMvibAmp[i]>=XMamp) break; //find the matching SW-amplitude in the array
 return i;
}
unsigned char XMtoSWMvibFreq(unsigned char XMvib) {
 int i;
 for (i=0; i<=15; i++) if(SWMtoXMvibFreq[i]<=XMvib) break; //find the matching SW-amplitude in the array
 if (i==16) i=4; //default vibrato for a zero-case
 return i;
}



//-----------------function-prototype declarations------------------------------
void ProcessInput(); char* FilExt(char *filename); void ChangeExt(char *filename,char *newExt);
void ProcessSWM(char stereo); void ProcessXM(); void ProcessMIDI(); void ConvertTune (char* exename);
void GenerateXM(); void GenerateMIDI(); void GenerateSWM();
void GenerateSWS(); void SWStoSWM();  void GenerateSNG();
void ProcessSWMver1(char stereo); void ProcessXM204(); void WaitKeyPress(); void DisplayInputInfo();
char LowByte(int num); char HiByte(int num); void CutExt(char *filename);
int between (int value, int min, int max);
void removeTrailingSpaces (char* string);
int getArrayMaxValueIndex (int* array, int count);
int getArraySingleNonzeroIndex (int* array, int count);
int LEwordToFile(unsigned int DataToWrite); int BEwordToFile(unsigned int DataToWrite);
int writeToFile (char* buffer, int size); int byteToFile (unsigned char data);
unsigned int LEwordFromFile(); unsigned int BEwordFromFile();
unsigned char SWMtoXMslide(unsigned char SWMslide); unsigned char XMtoSWMslide(unsigned char XMslide);
unsigned int ATTACKtoXMtime(unsigned int AttackNibble); unsigned char ATTACKtoSWMtime(unsigned int XMattack);
unsigned int DECAYtoXMtime(unsigned int DecayNibble, float SustainNibble);
unsigned char DECAYtoSWMtime(unsigned int XMdecay, unsigned char XMsustain);
unsigned int RELEASEtoXMtime(unsigned int ReleaseNibble, float SustainNibble);
unsigned char RELEASEtoSWMtime(unsigned int XMrelease, unsigned char XMsustain);
void WriteP00header(); //write 'C64File' header to file;
unsigned long ReadVarLenNum();
void ProcessMIDItrack(); void SetChordInstr();

int signed_unity (int value) { return value >= 0 ? 1 : -1; }
int abs (int value) { return value >= 0 ? value : -value; }
int between (int value, int min, int max) { return (min <= value && value <= max); }
int stringMatches (char* string1, char* string2) { return !strcmp( string1, string2 ); }



//==============================================================================

int main(int argc, char *argv[]) {
 printf ("\n====================================\n"
         "SID-Wizard SWM1 converter (app v1.9)\n"
         "====================================\n");

 if ( ( argc==2 && stringMatches( argv[1], "-help" ) ) || argc==1 ) {
  printf ("Converts to/from XM or MIDI formats,\n"
          "between .prg & .P00, and S00 to SID.\n"
          "And .swm to/from .sws (2SID/stereo) \n"
          "And .swm to .sng (GoatTracker tune) \n"
          "Usage syntax is pretty simple:      \n"
          "SWMconvert <inputfile> [outputfile] \n"
          "------------------------------------\n"
          "(Formats will depend on extensions.)\n"
          "If you don't specify an output-file,\n"
          "name of the input file will be used \n"
          "as output-filename. Existing files  \n"
          "will be overwritten automatically.  \n"
          "------------------------------------\n"
          " 2026 Hermit Soft. (Mihaly Horvath) \n"
          "====================================\n");
  printf("\nPress ENTER key...\n\n");
  getchar();
  exit(1);
 }
 else {
  InputFile = fopen(argv[1],"rb"); //!!!important "rb" is for binary (not to check text-control characters)
  if ( InputFile == NULL ) {
   printf( "Could not open input-file...\n" );
   WaitKeyPress();exit(2);
  }
  else {
   strcpy(InputFileName,argv[1]);
   if (argc==3) strcpy(OutputFileName,argv[2]);
   ProcessInput(); printf("\n\n**** Module processed. Going to Convert ****.\n---------------------------------------------\n");
   fclose(InputFile);
   ConvertTune( basename( argv[0] ) );
  }
 }
 WaitKeyPress();
 return 0;
}

//=================================== FUNCTIONS =======================================


void ProcessInput() {
 printf("Input-file name: %s \n",InputFileName);

 printf("Input-file extension: %s \n",FilExt(InputFileName));

 //read extension and decide if input is SWM or XM
 if ( stringMatches(FilExt(InputFileName),".SWM") || stringMatches(FilExt(InputFileName),".PRG") || stringMatches(FilExt(InputFileName),".swm") || stringMatches(FilExt(InputFileName),".prg") ) {
  TYPE_ID_OFFSET=2; TYPE_ID_LENGTH=4;
  ProcessSWM(0);
 }
 else if ( stringMatches(FilExt(InputFileName),".P00") || stringMatches(FilExt(InputFileName),".p00") ) {
  TYPE_ID_OFFSET=VICE_P00_OFFSET+2; TYPE_ID_LENGTH=4;
  if ( strlen(OutputFileName)==0 || strcmp(FilExt(OutputFileName),".prg") ) ProcessSWM(0); //only call ProcessSWM if the given output-file is not .prg
 }
 else if ( stringMatches(FilExt(InputFileName),".S00") ||  stringMatches(FilExt(InputFileName),".s00") ) {
  printf("Converting .S00 format to .SID format.");
 }
 else if ( stringMatches(FilExt(InputFileName),".XM") || stringMatches(FilExt(InputFileName),".xm")) {
  TYPE_ID_OFFSET=XM_ID_OFFSET; TYPE_ID_LENGTH=XM_ID_SIZE;
  ProcessXM();
 }
 else if ( stringMatches(FilExt(InputFileName),".MID") || stringMatches(FilExt(InputFileName),".mid") ) {
  TYPE_ID_OFFSET=MIDI_ID_OFFSET; TYPE_ID_LENGTH=MIDI_ID_SIZE;
  ProcessMIDI();
 }
 else if ( stringMatches(FilExt(InputFileName),".SWS") || stringMatches(FilExt(InputFileName),".sws") ) {
  TYPE_ID_OFFSET=2; TYPE_ID_LENGTH=4;
  ProcessSWM(1);
 }
 else {
  printf("\n*** Unsupported file-extension... ***\n");
  fclose(InputFile);WaitKeyPress();exit(3);
 }
}


//=================================================================================
void ConvertTune (char* exename) { //convert to target format
 int Data,i;
 printf("Deciding on output-format and filename.\n");

 //read extension and decide if input is SWM or XM
 if (strlen(OutputFileName)==0) { //if no output-file specified, give the same name and opposite/obvious extension
  printf("No output-file given. Using input-filename, appending extension.");
  strcpy(OutputFileName,InputFileName);
  if ( stringMatches(FilExt(InputFileName),".XM") || stringMatches(FilExt(InputFileName),".xm") ) { //no output given
   ChangeExt(OutputFileName,".swm.prg"); //append corresponding extension
   printf("\nConverting XM "); GenerateSWM();
  }
  else if ( stringMatches(FilExt(InputFileName),".MID") || stringMatches(FilExt(InputFileName),".mid") ) { //no output given
   ChangeExt(OutputFileName,".swm.prg"); //append corresponding extension
   printf("\nConverting MIDI "); GenerateSWM();
  }
  else if ( stringMatches(FilExt(InputFileName),".S00") || stringMatches(FilExt(InputFileName),".s00") ) { //if input-format is .S00, converting it to .sid
   ChangeExt(OutputFileName,".sid"); //append with new extension
   printf("\n*** Converting .S00 VICE C64-file to .sid file... ***");
   InputFile = fopen(InputFileName,"rb"); //!!!important "rb" is for binary (not to check text-control characters)
   OutputFile = fopen(OutputFileName,"wb"); //!!!important "wb" is for binary (not to check text-control characters)
   fseek(InputFile,VICE_P00_OFFSET,0); //go right after end of .S00 'C64File' header (omit header from .seq)
   while( (Data=fgetc(InputFile)) != EOF ) { fputc(Data,OutputFile); } //copy source to target
   fclose(InputFile); fclose(OutputFile);
   printf("\n*** Conversion done, file '%s' is generated... ***\n",OutputFileName);
  }
  else if ( stringMatches( exename, "swm2sng" ) || stringMatches( exename, "./swm2sng" ) || stringMatches( exename, "swm2sng.exe" ) ) {
   ChangeExt(OutputFileName,".sng"); //append new extension
   printf("\nConverting SWM "); GenerateSNG();
  }
  else { //no output given
   ChangeExt(OutputFileName,".xm"); //append new extension
   printf("\nConverting SWM "); GenerateXM();
  }
 }
 else if ( stringMatches(FilExt(OutputFileName),".swm") || stringMatches(FilExt(OutputFileName),".prg") ) { //output name & SWM extension given
  if ( stringMatches(FilExt(InputFileName),".xm") )  { printf("\nConverting XM "); GenerateSWM(); }
  else if ( stringMatches(FilExt(InputFileName),".mid") || stringMatches(FilExt(InputFileName),".MID") )  { printf("\nConverting MIDI "); GenerateSWM(); }
  else if ( stringMatches(FilExt(InputFileName),".sws") || stringMatches(FilExt(InputFileName),".SWS") )  { printf("\nConverting SWS "); SWStoSWM(); }
  else if ( stringMatches(FilExt(InputFileName),".SWM") || stringMatches(FilExt(InputFileName),".PRG") ||  stringMatches(FilExt(InputFileName),".swm") || stringMatches(FilExt(InputFileName),".prg") )  //what if input format is the same (SWM)?
  { printf("\n*** Are U crazy Man? SWM to SWM? ***\n"); WaitKeyPress(); exit(111); }
  else if ( stringMatches(FilExt(InputFileName),".P00") || stringMatches(FilExt(InputFileName),".p00") ) { //if input-format is .P00, converting it to .prg  
   printf("\n*** Converting .P00 VICE C64-file to .prg file... ***");
   InputFile = fopen(InputFileName,"rb"); //!!!important "rb" is for binary (not to check text-control characters)
   OutputFile = fopen(OutputFileName,"wb"); //!!!important "wb" is for binary (not to check text-control characters)
   char OutputFileName2[FILENAME_LENGTH_MAX];
   fseek(InputFile,VICE_P00_FILENAME_OFFSET,0); fgets(OutputFileName2,C64_FILENAME_LENGTH_MAX+1,InputFile); //get real C64 filename from P00 file
   for (i=0; i<strlen(OutputFileName2);i++) OutputFileName2[i]=tolower(OutputFileName2[i]); //convert to lowercase for C64
   FILE * OutputFile2 = fopen(OutputFileName2,"wb");
   fseek(InputFile,VICE_P00_OFFSET,0); //go right after end of .P00 'C64File' header (omit header from .prg)
   while( (Data=fgetc(InputFile)) != EOF ) { fputc(Data,OutputFile); fputc(Data,OutputFile2); } //copy source to target
   fclose(InputFile); fclose(OutputFile); fclose(OutputFile2);
   printf("\n*** Conversion done, files '%s' and '%s' are generated... ***\n",OutputFileName,OutputFileName2);
  }
 }
 else if ( stringMatches(FilExt(OutputFileName),".xm") ) { //output name & XM extension given
  if ( stringMatches(FilExt(InputFileName),".xm") || stringMatches(FilExt(InputFileName),".XM") ) { printf("\n*** Are U crazy Man? XM to XM? ***\n"); WaitKeyPress(); exit(111); } //what if input format is the same
  else { printf("\nConverting SWM "); GenerateXM(); }
 }
 else if ( stringMatches(FilExt(OutputFileName),".MID") || stringMatches(FilExt(OutputFileName),".mid") ) { //output name & MIDI extension given
  if ( stringMatches(FilExt(InputFileName),".xm") || stringMatches(FilExt(InputFileName),".XM") ) { printf("\n*** No XM to MIDI conversion support yet. ***\n"); WaitKeyPress(); exit(111); } //what if input format is the same
  else if ( stringMatches(FilExt(InputFileName),".MID") || stringMatches(FilExt(InputFileName),".mid") ) { printf("\n*** Are U crazy Man? MIDI to MIDI? ***\n"); WaitKeyPress(); exit(111); }
  else { printf("\nConverting SWM "); GenerateMIDI(); }
 }
 else if ( stringMatches(FilExt(OutputFileName),".sws") ) { //output name & SWS extension given
  if ( stringMatches(FilExt(InputFileName),".sws") || stringMatches(FilExt(InputFileName),".SWS") ) { printf("\n*** Are U crazy Man? SWS to SWS? ***\n"); WaitKeyPress(); exit(111); } //what if input format is the same
  else if (stringMatches(FilExt(InputFileName),".swm") || stringMatches(FilExt(InputFileName),".prg") || stringMatches(FilExt(InputFileName),".P00") ) 
  { printf("\nConverting SWM "); GenerateSWS(); }
 }
 else if ( stringMatches(FilExt(OutputFileName),".sng") ) { //output name & SNG extension given
  if ( stringMatches(FilExt(InputFileName),".sng") || stringMatches(FilExt(InputFileName),".SNG") ) { printf("\n*** Are U crazy Man? SNG to SNG? ***\n"); WaitKeyPress(); exit(111); } //what if input format is the same
  else if (stringMatches(FilExt(InputFileName),".swm") || stringMatches(FilExt(InputFileName),".prg") || stringMatches(FilExt(InputFileName),".P00") ) 
  { printf("\nConverting SWM "); GenerateSNG(); }
 }
 else if ( stringMatches(FilExt(OutputFileName),".P00") || stringMatches(FilExt(OutputFileName),".p00") ) { //output name & P00 extension given  //.P00 VICE C64file output? convert .prg to .P00
  if ( stringMatches(FilExt(InputFileName),".xm") )  { printf("\nConverting XM "); GenerateSWM(); }
  else if ( stringMatches(FilExt(InputFileName),".P00") || stringMatches(FilExt(InputFileName),".p00") )
   { printf("\n*** Are U crazy Man? P00 to P00? ***\n"); WaitKeyPress(); exit(111); }
  else if ( stringMatches(FilExt(InputFileName),".SWM") || stringMatches(FilExt(InputFileName),".PRG") || stringMatches(FilExt(InputFileName),".swm") || stringMatches(FilExt(InputFileName),".prg") ) {
   printf("\n*** Converting .swm/.prg to .P00 VICE C64-file... ***");
   InputFile = fopen(InputFileName,"rb"); //!!!important "rb" is for binary (not to check text-control characters)
   OutputFile = fopen(OutputFileName,"wb"); //!!!important "wb" is for binary (not to check text-control characters)
   WriteP00header(); while( (Data=fgetc(InputFile)) != EOF ) { fputc(Data,OutputFile); }
   fclose(InputFile); fclose(OutputFile);
   printf("\n*** Conversion done, file '%s.P00' is generated... ***\n",OutputFileName);
  }
 }
 else { //if output extension not found, automatic format (opposite of input-format)
  if ( stringMatches(FilExt(InputFileName),".XM") || stringMatches(FilExt(InputFileName),".xm") ) { //input-file is xm, no output-format/extension was given
   ChangeExt(OutputFileName,".swm.prg"); //append with corresponding extension
   printf("\nConverting XM "); GenerateSWM();
  }
  else if ( stringMatches(FilExt(InputFileName),".MID") || stringMatches(FilExt(InputFileName),".mid") ) { //input-file is MIDI, no output-format/extension was given
   ChangeExt(OutputFileName,".swm.prg"); //append with corresponding extension
   printf("\nConverting MIDI "); GenerateSWM();
  }
  else if ( stringMatches(FilExt(InputFileName),".S00") || stringMatches(FilExt(InputFileName),".s00") ) { //no output format/extension given //if input-format is .S00, converting it to .sid
   ChangeExt(OutputFileName,".sid"); //append with new extension
   printf("\n*** Converting .S00 VICE C64-file to .sid file... ***");
   InputFile = fopen(InputFileName,"rb"); //!!!important "rb" is for binary (not to check text-control characters)
   OutputFile = fopen(OutputFileName,"wb"); //!!!important "wb" is for binary (not to check text-control characters)
   fseek(InputFile,VICE_P00_OFFSET,0); //go right after end of .S00 'C64File' header (omit header from .seq)
   while( (Data=fgetc(InputFile)) != EOF ) { fputc(Data,OutputFile); } //copy source to target
   fclose(InputFile); fclose(OutputFile);
   printf("\n*** Conversion done, file '%s' is generated... ***\n",OutputFileName);
  }
  else { //input-file is swm or P00, no output-format was given
   ChangeExt(OutputFileName,".xm"); //append with new extension
   printf("\nConverting SWM "); GenerateXM(); //preferred output-format is XM for SWM
  }
 }
}


//==========================================================================================
void ProcessSWM(char stereo) { //check SWM type and version---------------------------------
 unsigned int i=0; //counts bytes in loops

 printf("Processing SID-Wizard module.......\n");

  //display filesize and format
 fseek(InputFile, 0, SEEK_END); InputSize=ftell(InputFile); fseek(InputFile, 0, SEEK_SET);
 printf("Input-file size: %d bytes \n",InputSize);

 fread(TUNEDATA,sizeof(TUNEDATA),sizeof(unsigned char),InputFile);

 for(i=0; i<TYPE_ID_LENGTH; InputTypeID[i]=TUNEDATA[i+TYPE_ID_OFFSET],i++);
 printf("Input-filetype ID: \"%s\" (at offset %d) \n",InputTypeID,TYPE_ID_OFFSET);

 if ( stringMatches(InputTypeID, stereo?SWS_ID:SWM_ID) ) {
  ProcessSWMver1(stereo);
 }
 else {
  printf("\n*** The module is of unknown format... ***\n");
  fclose(InputFile);WaitKeyPress();exit(4);
 }
}

//------------------------------------------------------------
void ProcessSWMver1(char stereo) { //import SWM version 1.0 data to arrays
 int i,j,k;
 unsigned int DataIndex,Size;
 unsigned char PatternData,PreceedingData;

 for(i=0;i<256;i++) { for(j=0;j<256;j++) PATTERNS[i][j]=0; } //init patterns (no garbage if checked longer) 

 printf("SW%c module is of version 1.0 .......\n\n", stereo?'S':'M');
 strncpy(AUTHORINFO,(char*)TUNEDATA+TYPE_ID_OFFSET+AUTHORPOS,40);
 printf("Author-info: %s \n",AUTHORINFO);
 SequenceAmount=TUNEDATA[TYPE_ID_OFFSET+SEQAMOPOS];
 SubtuneAmount = (SequenceAmount!=0) ? ((SequenceAmount-1)/3/(stereo+1))+1 : 1 ;
 PatternAmount=TUNEDATA[TYPE_ID_OFFSET+PTAMOUPOS];
 ChordTableSize=TUNEDATA[TYPE_ID_OFFSET+CHRDLEPOS];
 InstAmount=TUNEDATA[TYPE_ID_OFFSET+INSTAMPOS];
 printf("Number of subtunes   : %d (%d sequences) \n",SubtuneAmount,SequenceAmount);
 printf("Number of patterns   : %d \n",PatternAmount);
 printf("Number of instruments: %d \n",InstAmount);

 //fill arrays with SWM data read backwards
 printf("\n**** READING TUNE-DATA (backwards) ****\n---------------------------------------");
 DataIndex=InputSize-1; //point to end of tune-data

 //A.Subtune-funktempos
 printf("\nReading Subtune-tempos: ");
 for (i=SubtuneAmount-1; i>=0; i--) {
  TUNETEMPO[i][1]=TUNEDATA[DataIndex]; printf("%x-",TUNEDATA[DataIndex]); DataIndex--;
  TUNETEMPO[i][0]=TUNEDATA[DataIndex]; printf("%x ",TUNEDATA[DataIndex]); DataIndex--;
 }

 //B.skip Tempotable (no tempotable in XM)
 printf("\nReading Tempo-programs. ");
 DataIndex -= TUNEDATA[TYPE_ID_OFFSET+TMPLENPOS];

 //C.Chord-table
 printf("\nReading Chords: -------------\n ");
 if (ChordTableSize != 0) {
  for (i=ChordTableSize; i>0; i--) {
   CHORDS[i]=TUNEDATA[DataIndex]; printf("%x ",TUNEDATA[DataIndex]);
   DataIndex--;
  }
 }

 //D.Instruments
 printf("\nReading Instruments: --------\n "); 
 if (InstAmount != 0) {
  for (i=InstAmount; i>0; i--) {
   DataIndex-=instnamelength; //point to beginning of instrument-name
   Size=TUNEDATA[DataIndex];  //before name there is the size of instrument
   INSTRUMENTSIZES[i] = Size + SWM_SIZE__INSTRUMENT_TABLE_ENDMARK; //include end-mark, INSTRUMENTSIZES will come handy in SWM-to-SNG conversion
   for (j=0; j<instnamelength; j++) { //display read instrument's name
    INSTNAMES[i][j]=TUNEDATA[DataIndex+1+j]; printf("%c",TUNEDATA[DataIndex+1+j]);
   }
   printf("($%2X) ",Size); //display instrument-size after its name in brackets
   for (j=Size; j>=0; j--) { //copy effective instrument-data
    INSTRUMENTS[i][j]=TUNEDATA[DataIndex]; DataIndex--; //printf("%d,%2x ",j,INSTRUMENTS[i][j]);
   }
   INSTRUMENTS[i][Size] = SWM_VALUE_INSTRUMENT_TABLE_END; //0xff; //correct size-info back to delimiter
  }
 }

 //E.Patterns
 printf("\nReading Patterns: -----------");
 if (PatternAmount != 0) {
  for (i=PatternAmount; i>0; i--) {
   PATTERNLENGTHS[i]=TUNEDATA[DataIndex]; DataIndex--; 
   PATTERNSIZES[i]=Size=TUNEDATA[DataIndex]; DataIndex--;
   PATTERNS[i][Size-1]=0xff;  //set delimiter as pattern-endsignal
   printf("\n Pattern%d ($%2X rows, $%2X bytes): ",i,PATTERNLENGTHS[i],Size); //display pattern-number and size
   for (j=Size-2; j>=0; j--) {
    PatternData=TUNEDATA[DataIndex]; PreceedingData=TUNEDATA[DataIndex-1];
    //check for packed-NOP and handle it
    if ( (PACKEDMIN<=PatternData && PatternData<=PACKEDMAX) && (PreceedingData==0||(PACKEDMIN<=PreceedingData && PreceedingData<=PACKEDMAX)) ) { //packed NOP
     //printf("p"); //sign in displayer that it's treated as packed NOP
     for (k=(PatternData-PACKEDMIN+1); k>0; k--) { PATTERNS[i][j]=0; j--; }
    }
    else {
     PATTERNS[i][j]=PatternData;
    }
    //printf("%02X ",PatternData);
    DataIndex--;
   }
  }
 }

 SWM_SEQ_TOP=DataIndex;

 //F.Sequences
 printf("\nReading Sequences (orderlists): ---------");
 if (SequenceAmount != 0) {
  for (i=SequenceAmount-1; i>=0; i--) {
   Size=SWM_SEQ_SIZE[i]=TUNEDATA[DataIndex];DataIndex--;  printf("\n Seq.%2X ($%2X bytes): ",i,Size);
   for (j=Size-1; j>=0; j--) {
    SEQUENCES[i][j]=TUNEDATA[DataIndex];  printf("%02X ",TUNEDATA[DataIndex]);
    DataIndex--;
 }}}
}


//==========================================================================================
void GenerateSWS() {
 int i,j,k;
 printf("to SWS 2SID/stereo SID-Wizard worktune format. \n");
 printf("Output file-name will be: %s\n",OutputFileName);
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)
 if (OutputFile == NULL) { printf("Couldn't open output-file for writing!\n"); exit(-1); }

 LEwordToFile(0x3333); //-2. load-address of PRG format

 //write SWS header
 for (i=0;i<4;i++) fputc(SWS_ID[i],OutputFile);  //0-3. SWM-ID
 fputc(TUNEDATA[TYPE_ID_OFFSET+FSPEEDPOS],OutputFile);    //4.preferred framespeed
 for(i=0;i<SID_CHANNELCOUNT*2;i++) fputc(0xFF,OutputFile); //5-$A.solo all tracks
 fputc(TUNEDATA[TYPE_ID_OFFSET+SWM_DEFP_POS],OutputFile); //$B. default pattern-length
 fputc(TUNEDATA[TYPE_ID_OFFSET+SEQAMOPOS]*2,OutputFile); //$C. sequence-amount
 fputc(TUNEDATA[TYPE_ID_OFFSET+PTAMOUPOS],OutputFile);   //$D. pattern-amount
 fputc(TUNEDATA[TYPE_ID_OFFSET+INSTAMPOS],OutputFile);   //$E. number of instruments
 fputc(TUNEDATA[TYPE_ID_OFFSET+CHRDLEPOS],OutputFile);   //$F. chordtable-length (calculated in converter)
 fputc(TUNEDATA[TYPE_ID_OFFSET+TMPLENPOS],OutputFile);   //$10. tempo-program table size
 fputc(0,OutputFile);fputc(0,OutputFile); //two obsolete empty bytes
 fputc(TUNEDATA[TYPE_ID_OFFSET+DRIVERTYPE_POS],OutputFile); //$13
 fputc(TUNEDATA[TYPE_ID_OFFSET+TUNINGTYPE_POS],OutputFile); //$14
 fputc(TUNEDATA[TYPE_ID_OFFSET+SWM_HILI_POS],OutputFile); //$15 step-highlighting amount for 2SID version
 fputc(0,OutputFile); //SID2 address ID                   //$16
 fputc(0,OutputFile); //$17 //(kept for possible upcoming expansion)
 for(i=0;i<(tuneheadersize-AUTHORPOS);i++) fputc(TUNEDATA[TYPE_ID_OFFSET+AUTHORPOS+i],OutputFile); //$18..$3f - author-info

 //Sequences
 printf("\nConverting Sequences (orderlist): ---------");
 if (SequenceAmount != 0) {
  for (i=0; i<SubtuneAmount; i++) {
   for (j=0; j<SID_CHANNELCOUNT; j++) {
    for (k=0; k<SWM_SEQ_SIZE[i*3+j]; k++) {
     fputc(SEQUENCES[i*3+j][k],OutputFile);
    }
    fputc(SWM_SEQ_SIZE[i*3+j],OutputFile);
   }
   for (j=0; j<SID_CHANNELCOUNT; j++) {
    fputc(0xFE,OutputFile); //dummy sequence delimiter
    fputc(1,OutputFile); //dummy sequence size
   }
  }
 }

 for (i=SWM_SEQ_TOP+1;i<InputSize;i++) fputc(TUNEDATA[i],OutputFile); //rest of the data matches to SWM

 printf("\n\n**** Conversion is complete. **** \nSWS file '%s' generated... :)"
 "\n-----------------------------------------------------\n",OutputFileName);
 fclose(OutputFile);
}


//==========================================================================================
void SWStoSWM() {
 int i,j,k;
 printf("to SWM 1SID/mono SID-Wizard worktune format (with 2 subtunes). \n");
 printf("Output file-name will be: %s\n",OutputFileName);
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)
 if (OutputFile == NULL) { printf("Couldn't open output-file for writing!\n"); exit(-1); }

 LEwordToFile(0x3333); //-2. load-address of PRG format

 //write SWM header
 for (i=0;i<4;i++) fputc(SWM_ID[i],OutputFile);  //0-3. SWM-ID
 fputc(TUNEDATA[TYPE_ID_OFFSET+FSPEEDPOS],OutputFile);    //4.preferred framespeed
 fputc(TUNEDATA[TYPE_ID_OFFSET+SWS_HILI_POS],OutputFile); //5 step-highlighting amount
 for(i=0;i<2+SID_CHANNELCOUNT;i++) fputc(0xFF,OutputFile); //6..7:unused, 8..$A:solo all tracks
 fputc(TUNEDATA[TYPE_ID_OFFSET+SWM_DEFP_POS],OutputFile); //$B. default pattern-length
 fputc(TUNEDATA[TYPE_ID_OFFSET+SEQAMOPOS],OutputFile); //$C. sequence-amount (twice as SWS, in 2 subtunes)
 fputc(TUNEDATA[TYPE_ID_OFFSET+PTAMOUPOS],OutputFile);   //$D. pattern-amount
 fputc(TUNEDATA[TYPE_ID_OFFSET+INSTAMPOS],OutputFile);   //$E. number of instruments 
 fputc(TUNEDATA[TYPE_ID_OFFSET+CHRDLEPOS],OutputFile);   //$F. chordtable-length (calculated in converter)
 fputc(TUNEDATA[TYPE_ID_OFFSET+TMPLENPOS],OutputFile);   //$10. tempo-program table size
 fputc(0,OutputFile);fputc(0,OutputFile); //two obsolete empty bytes
 fputc(TUNEDATA[TYPE_ID_OFFSET+DRIVERTYPE_POS],OutputFile); //$13
 fputc(TUNEDATA[TYPE_ID_OFFSET+TUNINGTYPE_POS],OutputFile); //$14
 fputc(0,OutputFile); fputc(0,OutputFile); fputc(0,OutputFile); //$15..$17 unused by 1SID (kept for possible upcoming expansion)
 for(i=0;i<(tuneheadersize-AUTHORPOS);i++) fputc(TUNEDATA[TYPE_ID_OFFSET+AUTHORPOS+i],OutputFile); //$18..$3f - author-info

 //Sequences - apart from the header and subtune-funktempos the SWM tune is the same as the SWS :) so a simple copy would do here
 printf("\nConverting Sequences (orderlist): ---------");
 if (SequenceAmount != 0) {
  for (i=0; i<SubtuneAmount; i++) {
   for (j=0; j<SID_CHANNELCOUNT; j++) {
    for (k=0; k<SWM_SEQ_SIZE[i*6+j]; k++) {
     fputc(SEQUENCES[i*6+j][k],OutputFile);
    }
    fputc(SWM_SEQ_SIZE[i*6+j],OutputFile);
   }
   for (j=3; j<SID_CHANNELCOUNT+3; j++) {
    for (k=0; k<SWM_SEQ_SIZE[i*6+j]; k++) {
     fputc(SEQUENCES[i*6+j][k],OutputFile);
    }
    fputc(SWM_SEQ_SIZE[i*6+j],OutputFile);
   }
  }
 }

 for (i=SWM_SEQ_TOP+1;i<InputSize-SubtuneAmount*2;i++) fputc(TUNEDATA[i],OutputFile); //rest of the data matches to SWM
 for(i=0;i<SubtuneAmount;i++) {
  j=TUNEDATA[InputSize-SubtuneAmount*2+i*2]; k=TUNEDATA[InputSize-SubtuneAmount*2+i*2+1];
  fputc(j,OutputFile);fputc(k,OutputFile); fputc(j,OutputFile);fputc(k,OutputFile); //duplicate amount of subtune-funktempos in SWM
 }

 printf("\n\n**** Conversion is complete. **** \nSWM file '%s' generated... :)"
 "\n-----------------------------------------------------\n",OutputFileName);
 fclose(OutputFile);
}


//==========================================================================================
//converting processed SWM musicdata to SNG GoatTracker musicdata  //for converting SNG to SWM use the separate 'sng2swm' tool

enum { SNG_VALUE__INSTRUMENT_FILTERTABLE_INDEX__FILTEROFF = 0x01 };  //placed at 1st table-index $01, for non-filtered instruments only appearing on the same channel as the filtered instrument (when only 1 channel is filtered throughout the tune)


int isSWMnoteColumnFX (unsigned char effect) {  //they'll go into FX column in SNG if possible (so a workaround of moved-down gateon/gateoff can replace them)
 return between( effect, SWM_VALUE__PATTERN_COLUMN1__VIBRATO_AMPLITUDE_MIN, SWM_VALUE__PATTERN_COLUMN1__VIBRATO_AMPLITUDE_MAX )
        || effect == SWM_VALUE__PATTERN_COLUMN1__PORTAMENTO
        || effect == SWM_VALUE__PATTERN_COLUMN1__SYNCON || effect == SWM_VALUE__PATTERN_COLUMN1__SYNCOFF
        || effect == SWM_VALUE__PATTERN_COLUMN1__RINGON || effect == SWM_VALUE__PATTERN_COLUMN1__RINGOFF;
}

int isSNGpitchFX (unsigned char effect) {  //mostly used to determine if the effect remains/repeats in pattern until overridden by a note or other FX
 return  effect == SNG_VALUE__PATTERN_FX__PITCHSLIDE_UP || effect == SNG_VALUE__PATTERN_FX__PITCHSLIDE_DOWN
         || effect == SNG_VALUE__PATTERN_FX__PORTAMENTO || effect == SNG_VALUE__PATTERN_FX__VIBRATO;
}
int isSNGlegatoFX (unsigned char effect, unsigned char parameter) {
 return (effect == SNG_VALUE__PATTERN_FX__PORTAMENTO && parameter == SNG_VALUE__PATTERN_FX_PARAMETER__LEGATO);
}
int isSNGremainingFX (unsigned char effect, unsigned char parameter) {
 return isSNGpitchFX( effect ) && !isSNGlegatoFX( effect, parameter );  //Legato is one-shot
}
int isSNGoverridableFX (unsigned char effect, unsigned char parameter) {  //determine if the effect can be overridden by a note or other FX
 return isSNGpitchFX( effect ) && !isSNGlegatoFX( effect, parameter );  //Legato might be accompanied by a more important FX like tempo-setting in Lenore.swm, but bigFX is read before it, so it has the implicit priority! //Legato is not overrideable? (actually only a bigFX 03FF legato could be overridden on the same row by an instrument/note-column FX, or 3F instrument-column Fx by a note-column FX)
}
int canAddNewFX (unsigned char effect, unsigned char parameter) {
 return (effect == SNG_VALUE__PATTERN_FX__NOP) || isSNGoverridableFX( effect, parameter );
}


int SWM_isFilterControllerInstrument (unsigned char instrument_number/*1-based*/) {
 unsigned char FilterTableIndex = INSTRUMENTS[ instrument_number ][ SWI_FILTERTBPT_POS ];  //printf("ins%.2X: filt:%.2X..%.2X [0]=%.2X\n", instrument_number, FilterTableIndex, INSTRUMENTSIZES[ instrument_number ]-1, INSTRUMENTS[ instrument_number ][ FilterTableIndex ] );
 return ( INSTRUMENTSIZES[ instrument_number ] - FilterTableIndex > SWM_SIZE__INSTRUMENT_TABLE_ENDMARK )  //non empty filtertable?
          ? ( INSTRUMENTS[ instrument_number ][ FilterTableIndex ] != SWM_VALUE__FILTERTABLE_COMMAND_IN_1ST_ROW__PASSIVE ) : 0;  //filtertable sarts with $00 (passive filted instrumen instead of filter-controller, just tuns on filterswitch for the channel in SW, no GT equivalent)?
}


void /*SWMstatistics*/ getSWMstatistics () { //unsigned char instrument_number/*0-based*/) {  //scan through the whole tune as if it was played, and e.g. detect which channel(s) use the filtered instrument exclusively/dominantly (to set filterswitch/mask in its filtertable)
 int i, j, k, l, m;
 unsigned char SeqData, PatternNumber, PatternRowIndex, PatternData;
 unsigned char CurrentNote, CurrentInstrument /*1-based*/, CurrentChord /*1-based*/;
 unsigned char *SeqPointer;
 //static SWMstatistics SWMtuneStatistics;  //better being at global scope so pre-processed instrument/note-position arrays can be reached directly by slide-calculator/etc

 for (i=0; i < SWM_PATTERN_AMOUNT_MAX; ++i) {  //reset statistics before scanning/'playing' tunedata:
  for (j=0; j< SWM_PATTERN_SIZE_MAX; ++j) {
   SWMtuneStatistics.CurrentNotes[i][j] = SWM_NOTE_NUMBER_UNDETECTED;
   SWMtuneStatistics.CurrentInstruments[i][j] = SWM_INSTRUMENT_NUMBER_UNDETECTED; //SWM_INSTRUMENT_NUMBER_DEFAULT;
   SWMtuneStatistics.CurrentChords[i][j] = SWM_CHORD_NUMBER_UNDETECTED;
 }}
 for (i=0; i < SWM_INSTRUMENT_AMOUNT_MAX; ++i) {
  for (j=0; j< SID_CHANNELCOUNT; ++j) { SWMtuneStatistics.Instrument_NoteCounts [i][j] = 0; }
 }
 for (i=0; i < SID_CHANNELCOUNT; ++i) {
  SWMtuneStatistics.FilterControllerInstrumentSelection_Counts[i] = 0;
 }

 CurrentNote = SWM_NOTE_NUMBER_UNDETECTED; CurrentInstrument = SWM_INSTRUMENT_NUMBER_UNDETECTED;  //(should we clear these before every subtune in the next loop intead?  in SW they can come after each other with subtune-jump orderlist-function)
 CurrentChord = SWM_CHORD_NUMBER_UNDETECTED;
 for (j=0; j < SID_CHANNELCOUNT; ++j) {
  for (i=0; i < SubtuneAmount; ++i) {  //scanning through the tune starts here
   for (k = 0; k < SWM_SEQ_SIZE[ i * SID_CHANNELCOUNT + j ] && k < SNG_ORDERLIST_SEQUENCE_SIZE_MAX; ++k) { //'k' is source-pointer here
    SeqPointer = &( SEQUENCES[ i * SID_CHANNELCOUNT + j ][ k ] ); SeqData = *SeqPointer;  //printf("seqpos:%.2X\n",k);
    if (SWM_PATTERN_NUMBER_MIN <= SeqData && SeqData <= SWM_PATTERN_NUMBER_MAX ) {  //Pattern-number
     PatternNumber = SeqData;
     for (l = 0; l < PATTERNSIZES[ PatternNumber ]; ++l) {  //'play' pattern to look for instrument-changes
      PatternRowIndex = l; PatternData = PATTERNS[ PatternNumber ][ l ];  //printf("ptnpos:%.2X\n",l);
      if (PatternData == SWM_VALUE__PATTERN_COLUMN1__PATTERN_END) break;
      else {  //note-column read, not end-mark, get possible additional instrument/FX columns
       if ( between( PatternData & SWM_MASK__PATTERN_COLUMN1, SWM_VALUE__PATTERN_COLUMN1__NOTE_MIN, SWM_VALUE__PATTERN_COLUMN1__NOTE_MAX ) ) {
        CurrentNote = PatternData & SWM_MASK__PATTERN_COLUMN1;
        //if ( SWM_INSTRUMENT_NUMBER_MIN + instrument_number == CurrentInstrument )
        { ++SWMtuneStatistics.Instrument_NoteCounts[ CurrentInstrument ][j]; }
       }
       if (PatternData & SWM_BITVALUE__PATTERN_COLUMN1__NEXTCOLUMN) { //instrument-column exists
        PatternData = PATTERNS[ PatternNumber ][ ++l ];
        if ( between( PatternData & SWM_MASK__PATTERN_COLUMN2, SWM_VALUE__PATTERN_COLUMN2__INSTRUMENT_MIN, SWM_VALUE__PATTERN_COLUMN2__INSTRUMENT_MAX ) ) {
         CurrentInstrument = PatternData & SWM_MASK__PATTERN_COLUMN2;
         CurrentChord = INSTRUMENTS[ CurrentInstrument ][ SWI_DEFCHORD_POS  ];
         if ( SWM_isFilterControllerInstrument( CurrentInstrument ) ) {
          ++SWMtuneStatistics.FilterControllerInstrumentSelection_Counts[j];
         }
        }
        else if ( (PatternData & SWM_MASK__INSTRUMENT_SMALLFX_BASE) == SWM_VALUE__PATTERN_SMALLFX_BASE__CHORD ) {  //register chord-change by instrument-column smallFX
         CurrentChord = PatternData & SWM_MASK__SMALLFX_PARAMETER;
        }
        if (PatternData & SWM_BITVALUE__PATTERN_COLUMN2__NEXTCOLUMN) {  //fx-column exists
         PatternData = PATTERNS[ PatternNumber ][ ++l ];
         if (PatternData < SWM_VALUE__PATTERN_SMALLFX__MIN) {
          ++l;
          if (PatternData == SWM_VALUE__PATTERN_BIGFX__CHORD)  //register chord-change by FX-column bigFX
          { CurrentChord = PATTERNS[ PatternNumber ][ l ]; }
         }
         else if ( (PatternData & SWM_MASK__SMALLFX_BASE) == SWM_VALUE__PATTERN_SMALLFX_BASE__CHORD )  //register chord-change by FX-column smallFX
         { CurrentChord = PatternData & SWM_MASK__SMALLFX_PARAMETER; }
      }}}
      for (m=0; m < (l+1) - PatternRowIndex; ++m) {  //for safety, put the data in all bytes of the row, not just the first
       if ( SWMtuneStatistics.CurrentNotes[ PatternNumber ][ PatternRowIndex + m ] == SWM_NOTE_NUMBER_UNDETECTED)  //if already defined (pattern was used before), prefer the existing value (better to have proper values at beginning than at end of tune in case of discrepancies)
       { SWMtuneStatistics.CurrentNotes[ PatternNumber ][ PatternRowIndex + m ] = CurrentNote; }
       if ( SWMtuneStatistics.CurrentInstruments[ PatternNumber ][ PatternRowIndex + m ] == SWM_INSTRUMENT_NUMBER_UNDETECTED)  //if already defined (pattern was used before), prefer the existing value (better to have proper values at beginning than at end of tune in case of discrepancies)
       { SWMtuneStatistics.CurrentInstruments[ PatternNumber ][ PatternRowIndex + m ] = CurrentInstrument; }
       if ( SWMtuneStatistics.CurrentChords[ PatternNumber ][ PatternRowIndex + m ] == SWM_CHORD_NUMBER_UNDETECTED)  //if already defined (pattern was used before), prefer the existing value (better to have proper values at beginning than at end of tune in case of discrepancies)
       { SWMtuneStatistics.CurrentChords[ PatternNumber ][ PatternRowIndex + m ] = CurrentChord; }
    }}}
    else if (SeqData == SWM_VALUE__SEQUENCE_END || SeqData == SWM_VALUE__SEQUENCE_LOOP) break;
 }}}  printf("Filter usage on Channels: %d %d %d\n",SWMtuneStatistics.FilterControllerInstrumentSelection_Counts[0],SWMtuneStatistics.FilterControllerInstrumentSelection_Counts[1],SWMtuneStatistics.FilterControllerInstrumentSelection_Counts[2]);

 /*for (i=SWM_PATTERN_NUMBER_MIN; i <= PatternAmount; ++i) {  //reset statistics before scanning/'playing' tunedata:
  printf( "\nSWM-Pattern %.2X statistics (rowindex:currentnote-currentinstrument):\n", i );
  for (j=0; j < PATTERNSIZES[i] - 1; ++j) {    //(should we clear statistics before every subtune?  in SW they can come after each other with subtune-jump orderlist-function)
   printf("%.2X:%.2X-%.2X ",j,SWMtuneStatistics.CurrentNotes[i][j], SWMtuneStatistics.CurrentInstruments[i][j] );
  } printf("\n");
 }
 printf( "\nSWM-Instrument usage (note-count) statistics (instrument:channel1,channel2,channel3):\n", i );
 for (i=SWM_INSTRUMENT_NUMBER_MIN; i <= InstAmount; ++i) {
  printf( "$%.2X:%d,%d,%d ", i, SWMtuneStatistics.Instrument_NoteCounts [i][0], SWMtuneStatistics.Instrument_NoteCounts [i][1], SWMtuneStatistics.Instrument_NoteCounts [i][2] );
 }*/

 //return &SWMtuneStatistics;
}


void SNG_addToPattern (SNGpattern* pattern, unsigned char note, unsigned char instrument, unsigned char effect, unsigned char parameter) {
 pattern->Rows[ pattern->Length ].Note = note;  //printf("  -> %.2X:  %.2X  %.2X  %.2X %.2X  \n", pattern->Length, note, instrument, effect, parameter );
 pattern->Rows[ pattern->Length ].Instrument = instrument;
 pattern->Rows[ pattern->Length ].FX = effect;
 pattern->Rows[ pattern->Length++ ].FXparameter = parameter;
}

int SNG_addPatternFXtoPreviousRow (SNGpattern* pattern, unsigned char effect, unsigned char parameter) {
 unsigned char PreviousRowColumn1 = pattern->Rows[ pattern->Length - 1 ].Note;
 unsigned char PreviousRowFX = pattern->Rows[ pattern->Length - 1 ].FX;
 unsigned char PreviousRowParameter = pattern->Rows[ pattern->Length - 1 ].FXparameter;
 if (pattern->Length >= 1) {
  if ( PreviousRowFX == SNG_VALUE__PATTERN_FX__NOP
       || PreviousRowFX == SNG_VALUE__PATTERN_FX__VIBRATO
       || isSNGlegatoFX( PreviousRowFX, PreviousRowParameter )   //It seems legato is a less-important effect than some other FXes (like setting Release for flute solo's end in Flashitback.swm), so even overwrite it if there's a note beside it
       || ( ( PreviousRowFX == SNG_VALUE__PATTERN_FX__PITCHSLIDE_DOWN   //may still overwrite pitch-slide/vibrato FX, but not portamento/legato if there's a note on the previous line (because that would retrigger the note)
              || PreviousRowFX == SNG_VALUE__PATTERN_FX__PITCHSLIDE_UP
              ||PreviousRowFX == SNG_VALUE__PATTERN_FX__PORTAMENTO )
            && !between( PreviousRowColumn1, SNG_VALUE__PATTERN_COLUMN1__NOTE_MIN , SNG_VALUE__PATTERN_COLUMN1__NOTE_MAX ) ) ) {
   pattern->Rows[ pattern->Length - 1 ].FX = effect;
   pattern->Rows[ pattern->Length - 1 ].FXparameter = parameter;
   return 1;  //signal success to caller
  }
 } //else  //caller may still shift the gateon/gateoff downwards as a solution
 return 0;
}


int SNG_addToInstrumentTable (SNGinstrumentTable* table, unsigned char command, unsigned char parameter) {  //printf("  -> %.2X: %.2X %.2X\n", SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN  + table->Length, command, parameter );
 if (table->Length >= SNG_INSTRUMENTTABLE_LENGTH_MAX - 1) { /*printf("\n**** Instrument-table is full!\n");*/ return -1; }
 table->Columns[0][ table->Length ] = command; table->Columns[1][ table->Length++ ] = parameter;
 return 0;
}

unsigned char SNG_addToSpeedTable (unsigned char command, unsigned char parameter) { //if finds match, return its speedtable-index, oherwise make new entry and return its index, but if fails, return 0
 static TableIsFull = 0;
 #if (SNG_OPTIMIZE_SPEEDTABLE)
 int i; unsigned char NewEntryIndex = SNGinstrumentData.SpeedTable.Length;
 for (i=0; i < SNGinstrumentData.SpeedTable.Length; ++i) {
  if ( command == SNGinstrumentData.SpeedTable.Columns[0][i] && parameter == SNGinstrumentData.SpeedTable.Columns[1][i] ) {
   return SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + i;
  }
 }
 #endif
 if ( SNG_addToInstrumentTable( &( SNGinstrumentData.SpeedTable ), command, parameter ) ) { if (!TableIsFull) printf("\n**** SpeedTable is full!\n\n"); TableIsFull=1; return 0x00; }
 return SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + NewEntryIndex;
}


void SNG_addToWFtable (unsigned char command, unsigned char parameter) {
 static TableIsFull = 0;
 if ( SNG_addToInstrumentTable( &( SNGinstrumentData.WaveformArpTable ), command, parameter ) ) { if (!TableIsFull) printf("\n**** WaveForm-Arp Table is full!\n\n"); TableIsFull = 1; }
}

/*void SNG_expandWFtableBy (unsigned char command, unsigned char parameter) {
 if ( !SNG_addToInstrumentTable( &( SNGinstrumentData.WaveformArpTable ), command, parameter ) ) ; //{ ++SNG_WFtablePosition; }
 else printf("\n**** WaveForm-Arp Table is full!\n\n");
}*/

unsigned char convertArpNoteShiftToSNG (unsigned char shift) {
 if ( between( shift, SWM_VALUE_MIN__ARPTABLE__NEGATIVE_SHIFT, SWM_VALUE_MAX__ARPTABLE__NEGATIVE_SHIFT ) ) {
  return shift + (SNG_VALUE_MAX__ARPTABLE__NEGATIVE_SHIFT - SWM_VALUE_MAX__ARPTABLE__NEGATIVE_SHIFT);  //+ SEMITONES_PER_OCTAVE * ; //1..2 octaves of per-instrument Octave-shift may be simulated by Arp-table (rather than notes in patterns)
 }
 else return shift;  //positive shifts are the same as in SW
}

int convertWFtableRowToSNG (unsigned char command, unsigned char parameter, unsigned char parameter2  //detune-column, not supported by GT
                            , int tablerow, unsigned char instrument_number /*0-based*/, unsigned char chordnumber /*1-based*/) {
 enum { SEMITONES_PER_OCTAVE = 12 };
 int i;
 static unsigned char ArpOutput, DefaultChord, ArpChordSpeed, ChordNumber, ChordWFtableIndex;
 /*if ( between( parameter, SWM_VALUE_MIN__ARPTABLE__POSITIVE_SHIFT, SWM_VALUE_MAX__ARPTABLE__POSITIVE_SHIFT ) ) {
  ArpOutput = parameter; //+ SEMITONES_PER_OCTAVE * ; //the same in SW and GT, but 1..2 octaves of per-instrument Octave-shift may be simulated by Arp-table (rather than notes in patterns), though with limited chord-support
 }

 else*/ if ( parameter == SWM_VALUE__ARPTABLE__CALL_CHORD ) {  //delault chord built in, and other chords can be emulated by extra WFARP-table entries called by GT pattern-FX 8 (WFtable jump)
  ArpChordSpeed = INSTRUMENTS[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_ARP_SPEED_POS ] & SWM_MASK__INSTRUMENT_ARPSPEED;
  DefaultChord = INSTRUMENTS[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_DEFCHORD_POS ]; //ChordIns[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ];
  ChordNumber = chordnumber == SWM_CHORD_NUMBER_NONE ? DefaultChord : chordnumber;
  if (ChordNumber == SWM_CHORD_NUMBER_NONE)  //no chord
  { SNG_addToWFtable( command, SNG_VALUE__ARPTABLE__NO_SHIFT ); }
  else {  //expand valid chord into WFarp-table
   SNG_addToWFtable( command, /*SNG_VALUE__ARPTABLE__NO_SHIFT*/ convertArpNoteShiftToSNG( CHORDS[ ChordIndex[ChordNumber] + 0 ] ) );
    //SNG_expandWFtableBy( ArpChordSpeed - 1, convertArpNoteShiftToSNG( CHORDS[ ChordIndex[ChordNumber] + 0 ] ) );
   ChordWFtableIndex = SNGinstrumentData.WaveformArpTable.Length;
   for (i=1; i < ChordLen[ ChordNumber ]; ++i) {  //add chord notes
    SNG_addToWFtable/*expandWFtableBy*/( ArpChordSpeed - (i==0?1:0), convertArpNoteShiftToSNG( CHORDS[ ChordIndex[ChordNumber] + i ] ) );
   }
   if (CHORDS[ ChordIndex[ChordNumber] + i ] == SWM_VALUE_CHORDTABLE__END__LOOP) {  //loop the chord
    SNG_addToWFtable/*expandWFtableBy*/( ArpChordSpeed, convertArpNoteShiftToSNG( CHORDS[ ChordIndex[ChordNumber] + 0 ] ) );
    SNG_addToWFtable/*expandWFtableBy*/( SNG_VALUE_INSTRUMENT_TABLE_JUMP, ChordWFtableIndex + 1 );
    return ChordWFtableIndex + 1;
   }
  }
  return SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE; //ArpOutput = SNG_VALUE__ARPTABLE__NO_SHIFT;
 }

 //else if ( parameter == SWM_VALUE__ARPTABLE__NOP ) ArpOutput = SNG_VALUE__ARPTABLE__NOP;  //the same in SW and GT
 else if ( between( parameter, SWM_VALUE__ARPTABLE__ABSOLUTE_NOTE_MIN, SWM_VALUE__ARPTABLE__ABSOLUTE_NOTE_MAX ) ) {
  ArpOutput = parameter - 1;  //SID-Wizard has it from C-0 (value $81), GT from C#0 (value $81), one semitone upper, so SW $82 C#0 should be GT $81 C#0
 }
 else ArpOutput = convertArpNoteShiftToSNG( parameter ); /*else if ( between( parameter, SWM_VALUE_MIN__ARPTABLE__NEGATIVE_SHIFT, SWM_VALUE_MAX__ARPTABLE__NEGATIVE_SHIFT ) ) {
  ArpOutput = parameter + (SNG_VALUE_MAX__ARPTABLE__NEGATIVE_SHIFT - SWM_VALUE_MAX__ARPTABLE__NEGATIVE_SHIFT);  //+ SEMITONES_PER_OCTAVE * ; //1..2 octaves of per-instrument Octave-shift may be simulated by Arp-table (rather than notes in patterns)
 }
 else ArpOutput = parameter;*/

 if ( between( command, SWM_VALUE__WFTABLE__REPEAT_MIN, SWM_VALUE__WFTABLE__REPEAT_MAX ) ) SNG_addToWFtable( command, ArpOutput ); //matches in SW and GT, SW arp-speed setting doesn't count for these
 else {
  ArpChordSpeed = INSTRUMENTS[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_ARP_SPEED_POS ] & SWM_MASK__INSTRUMENT_ARPSPEED;
  for (i=0; i < ArpChordSpeed + 1; ++i) {
   //if ( between( command, SWM_VALUE__WFTABLE__WAVECONTROL_MIN, SNG_VALUE__WFTABLE__WAVECONTROL_MAX/*SWM_VALUE__WFTABLE__WAVECONTROL_MAX*/ ) ) SNG_addToWFtable( command, ArpOutput ); matches in SW and GT (except $E0..$EF which GT substitutis with waveform $00..$0F but SW doesn't have that waveform)
   /*else*/ if ( between( command, SNG_VALUE__WFTABLE__RUN_FX__MIN, SWM_VALUE__WFTABLE__WAVECONTROL_MAX ) ) {
    SNG_addToWFtable( command - 0x20, ArpOutput /*parameter*/ );  //don't produce FX-call with $F0..$FD waveform-control inputs, but a nearly identical waveform $D0..$DE (noise+pulse+triangle)
   }
   else SNG_addToWFtable( command, ArpOutput );
 }}
 return SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;
}


void SNG_addToPWtable (unsigned char command, unsigned char parameter) {
 static TableIsFull = 0;
 if ( SNG_addToInstrumentTable( &( SNGinstrumentData.PulseWidthTable ), command, parameter ) ) { if (!TableIsFull) printf("\n**** PulseWidth Table is full!\n\n"); TableIsFull = 1; }
}

int convertPWtableRowToSNG (unsigned char command, unsigned char parameter) {  //what about emulating keyboard-track feature in GT by pattern-FX 9?
 if ( between( command, SWM_VALUE_MIN__PWTABLE_COMMAND__SET_PULSEWIDTH, SWM_VALUE_MAX__PWTABLE_COMMAND__SET_PULSEWIDTH ) ) {
  SNG_addToPWtable( command, parameter ); //identical in SW and GT SNG_addToPWtable( command, parameter ); //identical in SW and GT
 }
 else if ( between( command, SWM_VALUE__PWTABLE_COMMAND__SWEEPTIME_MIN, SWM_VALUE__PWTABLE_COMMAND__SWEEPTIME_MAX ) ) {  //identical in SW and GT SNG_addToPWtable( command, parameter ); //seems identical in SW and GT (or maybe shifted by 1?)
  if (command < SWM_VALUE__PWTABLE_COMMAND__SWEEPTIME_MIN) SNG_addToPWtable( SWM_VALUE__PWTABLE_COMMAND__SWEEPTIME_MIN, parameter );  //avoid SNG_VALUE__PWTABLE_COMMAND__STOP
  SNG_addToPWtable( command, parameter );
 }
 else SNG_addToPWtable( command, parameter );  //fallback to keep alignment, normally never happens
 return SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;
}


unsigned char getInstrumentChannelFilterSwitch (unsigned char instrument_number /*0-based*/) {  //detect which channel(s) use the filtered instrument exclusively/dominantly (to set filterswitch/mask in its filtertable)
 printf("Instrument %.2X filterswitch channel determination\n", SWM_INSTRUMENT_NUMBER_MIN + instrument_number );
 int DominantChannel = SNG_CHANNEL__FILTERSWITCH_FALLBACK; //0;
 SWMstatistics *StatisticsPointer = &SWMtuneStatistics; //getSWMstatistics( instrument_number );
 DominantChannel = getArrayMaxValueIndex( StatisticsPointer->Instrument_NoteCounts[ SWM_INSTRUMENT_NUMBER_MIN + instrument_number ], SID_CHANNELCOUNT );  printf( "Note-counts of the filtered instrument on channels: %d %d %d -> dominant: Channel%d.\n",StatisticsPointer->Instrument_NoteCounts[instrument_number+SWM_INSTRUMENT_NUMBER_MIN][0],StatisticsPointer->Instrument_NoteCounts[instrument_number+SWM_INSTRUMENT_NUMBER_MIN][1],StatisticsPointer->Instrument_NoteCounts[instrument_number+SWM_INSTRUMENT_NUMBER_MIN][2],DominantChannel+1);
 return ( 1 << DominantChannel );  //return filter channel-switch bitvalue-nybble
}

void SNG_addToFilterTable (unsigned char command, unsigned char parameter) {
 static TableIsFull = 0;
 if (SNG_addToInstrumentTable( &( SNGinstrumentData.FilterTable ), command, parameter ) ) { if (!TableIsFull) printf("\n**** FilterTable is full!\n\n"); TableIsFull=1; }
}

int convertFilterTableRowToSNG (unsigned char command, unsigned char parameter, unsigned char parameter2
                                , int filtertable_row, unsigned char instrument_number /*0-based*/) {
 enum { FILTER_11to8bit_DIV = 8 };  //if no override in KT keyboard-track column, prefer filter being only on 1st channel, or even better, detect instrument's channel from tunedata
 unsigned char ResonanceNybble, FilterSwitchNybble; int SNGsweepSpeed;
 if (command == SWM_VALUE__FILTERTABLE_COMMAND_IN_1ST_ROW__PASSIVE && filtertable_row==0) { return -1;} //not supported directly in GoatTracker, tell table-maker to avoid table-creation for now
 else if ( between( command, SWM_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MIN, SWM_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MAX ) ) {
  SNGsweepSpeed = ((signed char)parameter);  //if SWM had 8bit filter (light/bare/demo player-type) no conversion needed
  if (SNGsweepSpeed && SWMfilter11bit) SNGsweepSpeed = abs(SNGsweepSpeed) >= FILTER_11to8bit_DIV ?  //usefol for CoZmo's 50shades tune's small intro-sweep
                                          (SNGsweepSpeed / FILTER_11to8bit_DIV) : signed_unity( SNGsweepSpeed );
  if (command < SNG_VALUEDIFF__FILTERTABLE_COMMAND__SWEEPTIME_MIN) command = SNG_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MIN; //+= SNG_VALUEDIFF__FILTERTABLE_COMMAND__SWEEPTIME_MIN;  //$00 is cutoff-setting in GT! if 0, shift it to $01, but keep the rest of values the same (Rain8580 tune)!
   if (command > SNG_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MAX) command = SNG_VALUE__FILTERTABLE_COMMAND__SWEEPTIME_MAX;
  SNG_addToFilterTable( command, SNGsweepSpeed );
 }
 else if ( between( command, SWM_VALUE_MIN__FILTERTABLE_COMMAND__SET_FILTER, SWM_VALUE_MAX__FILTERTABLE_COMMAND__SET_FILTER ) ) {
  ResonanceNybble = ( (command & SWM_VALUEMASK__FILTERTABLE_COMMAND__SET_RESONANCE)
                      << SNG_SHIFTS__FILTERTABLE__SET_RESONANCE )
                    & SNG_VALUEMASK__FILTERTABLE_PARAMETER__SET_RESONANCE;
  FilterSwitchNybble = between( parameter2, SWM_VALUE__FILTERTABLE_COLUMN3__FILTERSWITCH_OVERRIDE_MIN, SWM_VALUE__FILTERTABLE_COLUMN3__FILTERSWITCH_OVERRIDE_MAX )
                       ? (parameter2 & SWM_VALUEMASK__FILTERTABLE__FILTERSWITCH_OVERRIDE)
                         : getInstrumentChannelFilterSwitch( instrument_number ) /*SNG_VALUE__FILTERSWITCH_FALLBACK*/;
  SNG_addToFilterTable( command & SNG_VALUEMASK__FILTERTABLE_COMMAND__SET_BAND
                        , ResonanceNybble | FilterSwitchNybble );
  SNG_addToFilterTable( SNG_VALUE__FILTERTABLE_COMMAND__SET_CUTOFF, parameter );  //separate filter-cutoff setting
 }
 else SNG_addToFilterTable( command, parameter );  //fallback
 return SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;
}


void convertSWMtableToSNGtable (unsigned char instrument_number /*0-based*/, char* tablename
  , int input_table_offset, int input_table_endoffset, unsigned char* output_table_baseindex
  , SNGinstrumentTable* output_table, unsigned char chordnumber, int (convertrow)()
  , unsigned char sng_position_array[][SWM_INSTRUMENT_TABLE_SIZE_MAX]) {  //converts one instrument's table and adds it to the GT table
 int i, j, k, Temp;  unsigned char *SWM_TableBase;
 unsigned char SWM_TableSize, SWM_TableLength, TableIndex, Command, Parameter, Parameter2;
 unsigned char OriginalSNGtableLength, LastSWMtableCommand, LastJumpTarget, SNGnewChunkLength;
 int MaxChunkMatch_Length, MaxChunkMatch_Index;
 static unsigned char SNGinstrumentTablePositions [SWM_INSTRUMENT_TABLE_SIZE_MAX];  //SNG table position equivalents of original SWM table-positions for/inside the instrument

 SWM_TableBase = &INSTRUMENTS[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ][ input_table_offset ];
 SWM_TableSize = input_table_endoffset - input_table_offset;  //printf(" %s table-size (including end-mark):%d\n",tablename,SWM_TableSize);
 if ( !stringMatches( tablename, "Waveform-Chord" ) || chordnumber == SWM_CHORD_NUMBER_NONE ) {  // only set instrument wavetable-position if it's not a new chord entry created by a nondefault-chord caller patternFX
  if (SWM_TableSize > SWM_SIZE__INSTRUMENT_TABLE_ENDMARK)  //only set index if the table has payload
  { *output_table_baseindex = SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + output_table->Length; }  //pre-set table-index points to base for new content
  else { *output_table_baseindex = SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE; return; }  //if SWM-table is empty, set its instrument-index to 0 and return
 }

 OriginalSNGtableLength = output_table->Length;  //SNG_WFtablePosition = 0;
#define SNG_CHECKSET_CHORD_INSTRUMENTTABLE_INDEX(chord_tableindex) \
 if ( output_table == &( SNGinstrumentData.WaveformArpTable ) ) {  /*handle chord positions*/ \
  SNGchordWFtableIndexes[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ] \
   [ chordnumber != SWM_CHORD_NUMBER_NONE ? chordnumber : INSTRUMENTS[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_DEFCHORD_POS ] ] \
    = SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + (chord_tableindex);  /*ChordIns[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ];*/ \
 }
 SNG_CHECKSET_CHORD_INSTRUMENTTABLE_INDEX( OriginalSNGtableLength );
 LastSWMtableCommand = SWM_VALUE_INSTRUMENT_TABLE_END; LastJumpTarget = 0xFF;
 for (i = SWM_TableLength = 0; i < SWM_TableSize && SWM_TableBase[i] != SWM_VALUE_INSTRUMENT_TABLE_END; i += SWM_INSTRUMENT_TABLE_ROWSIZE, ++SWM_TableLength) {  //printf(" %.2X(%.2X): %.2X %.2X %.2X\n", i/SWM_INSTRUMENT_TABLE_ROWSIZE, input_table_offset+i, SWM_TableBase[i+0], SWM_TableBase[i+1], SWM_TableBase[i+2] );
  SNGinstrumentTablePositions[ i ] = SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + output_table->Length;
  //if ( output_table == &( SNGinstrumentData.WaveformArpTable ) ) {  //handle expnded positions (by inserted chords or arp-speed repeats)
   sng_position_array/*SNG_WFtablePositions*/[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ][ /*SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + OriginalSNGtableLength
    +*/ SWM_TableLength /*(i / SWM_INSTRUMENT_TABLE_ROWSIZE)*/ ]
     = SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + output_table->Length; //SNG_WFtablePosition;
  //}
  Command = LastSWMtableCommand = SWM_TableBase[i+0]; Parameter = SWM_TableBase[i+1]; Parameter2 = SWM_TableBase[i+2];
  if (Command == SWM_VALUE_INSTRUMENT_TABLE_JUMP) {  //handle internal jumps/ends (the same scheme for all tables. except WFtable's $FE selfjump above $40 as target, restricting PW/filt-tables to avoid invalid jumps is a good idea too):
   if (Parameter >= SWM_VALUE__WFTABLE_JUMP_PARAMETER__SELFJUMP_MIN || Parameter >= input_table_offset + i )
   { LastJumpTarget = SNG_VALUE_INSTRUMENT_TABLEJUMP_PARAMETER_END; }  //substitute invalid self/forward-jumps too with $FF00 end-mark value
   else LastJumpTarget = /* *output_table_baseindex + */ SNGinstrumentTablePositions[ ( Parameter - input_table_offset ) ];
   SNG_addToInstrumentTable( output_table, SNG_VALUE_INSTRUMENT_TABLE_JUMP, LastJumpTarget );  //add the jump to table
  }
  else {  //convert non-jump content
   if ( 0 > ( Temp = convertrow( Command, Parameter, Parameter2, i, instrument_number, chordnumber ) ) ) {  //converts row and adds it to corresponding table if OK
    *output_table_baseindex = SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE; return;  //if converter signals it, set instrument-tableindex to 0 and abort table-addition (e.g. for a $00 in 1st row of SW-filtertable not supported by GT player)
   }
   else if (Temp != SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE) {
    LastSWMtableCommand = SWM_VALUE_INSTRUMENT_TABLE_JUMP; LastJumpTarget = Temp;
 }}}
 //table-data converted, put an $FF00 SNG 'end' mark if needed (if no jump at the end looping back already):
 if (LastSWMtableCommand != SWM_VALUE_INSTRUMENT_TABLE_JUMP || LastJumpTarget >= output_table->Length + SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN) {
  SNG_addToInstrumentTable( output_table, SNG_VALUE_INSTRUMENT_TABLE_JUMP, SNG_VALUE_INSTRUMENT_TABLEJUMP_PARAMETER_END );
 }

 #if (SNG_OPTIMIZE_WF_PW_FILT_TABLES)  //Now check if the resulting table-portion matches another one already converted, and if yes, reuse it (and set back the table-length and instrument-tableindex)
 SNGnewChunkLength = output_table->Length - OriginalSNGtableLength;  printf("%s Base: %.2X..%.2X  (newchunk-length:%d)\n", tablename, OriginalSNGtableLength+SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN, output_table->Length+SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN, SNGnewChunkLength );
 MaxChunkMatch_Length = 0; MaxChunkMatch_Index = -1;  //for (j=0; j < SNGnewChunkLength; ++j) { printf( "%s: %.2X: %.2X %.2X\n", tablename, OriginalSNGtableLength+j+SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN, output_table->Columns[0][ OriginalSNGtableLength + j ], output_table->Columns[1][ OriginalSNGtableLength + j ] ); }
 for (i=OriginalSNGtableLength-1; SNGnewChunkLength > 0 && i >= 0; --i) {
  for (k=SNGnewChunkLength; k > 1; --k) {
   for (j=0; j < k; ++j) {
    TableIndex = OriginalSNGtableLength + (SNGnewChunkLength - k) + j;
    Command = output_table->Columns[0][ TableIndex ];
    if ( Command != output_table->Columns[0][ i + j ] ) break;  //difference found
    else {  //if command matches, check if parameter (or backward jump's relative distance) matches too
     Parameter = output_table->Columns[1][ TableIndex ];  //printf("SNGnewChunkLength:%.2X,k:%.2X,j:%.2X: %.2X:%.2X %.2X <-> Check: %.2X %.2X\n",SNGnewChunkLength,k,j,TableIndex+SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN,Command,Parameter,output_table->Columns[0][i+j],output_table->Columns[1][i+j]);
     if (Command == SNG_VALUE_INSTRUMENT_TABLE_JUMP) {
      if (Parameter == SNG_VALUE_INSTRUMENT_TABLEJUMP_PARAMETER_END)  //for $FF00 it's the absolute value $00 should be checked
      { if ( output_table->Columns[1][i+j] != Parameter ) break; }
      else if (Parameter < SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + TableIndex - j ) break;  //if jump points before checked area, avoid copying it below (would jump to a different part than the matched)
      else if ( TableIndex - Parameter != (i+j) - output_table->Columns[1][i+j] ) break;  //if jump-target relative-position doesn't match, it's not a full match
     }
     else if ( Parameter != output_table->Columns[1][ i + j ] ) break;  //if normal non-jump parameter differs
    }
   }
   if (j >= k) {  //for (j=0; j < k; ++j) { printf("Match with row $%.2X: %d rows  ->  %s: %.2X: %.2X %.2X\n",i+SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN,j, tablename, OriginalSNGtableLength + (SNGnewChunkLength-k)+j+SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN, output_table->Columns[0][ OriginalSNGtableLength + (SNGnewChunkLength-k)+j  ], output_table->Columns[1][ OriginalSNGtableLength + (SNGnewChunkLength-k)+j  ] ); } printf("\n");
    if (k > MaxChunkMatch_Length) { MaxChunkMatch_Length = k; MaxChunkMatch_Index = i; }
    break;
   }
  }
 }

 if (MaxChunkMatch_Length) {  printf( "Best (longest+closest) match in %s table: %d rows at $%.2X (matches chunk at $%.2X)\n", tablename, MaxChunkMatch_Length, OriginalSNGtableLength + SNGnewChunkLength - MaxChunkMatch_Length + SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN, MaxChunkMatch_Index + SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN );
  if (MaxChunkMatch_Length >= SNGnewChunkLength) {  printf("*** Full match -> only adding table/chord-index ***\n");  //full match, replacing instrument-tableindex, cancelling new chunk
   if ( !stringMatches( tablename, "Waveform-Chord" ) || chordnumber == SWM_CHORD_NUMBER_NONE ) {  // only set instrument wavetable-position if it's not a new chord entry created by a nondefault-chord caller patternFX
    *output_table_baseindex = MaxChunkMatch_Index + SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN;
   }
   SNG_CHECKSET_CHORD_INSTRUMENTTABLE_INDEX( MaxChunkMatch_Index /*+ SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN*/ );
   for (i = OriginalSNGtableLength; i < output_table->Length; ++i) {
    output_table->Columns[0][i] = SNG_VALUE_INSTRUMENTTABLE_EMPTY; output_table->Columns[1][i] = 0x00;
   }
   output_table->Length = OriginalSNGtableLength;
   //if ( output_table == &( SNGinstrumentData.WaveformArpTable ) ) {  //handle expnded positions (by inserted chords or arp-speed repeats)
    for (i = 0; i < SWM_TableLength; ++i) {
     sng_position_array/*SNG_WFtablePositions*/[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ]
      [ /*SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN +*/ i ] -= (OriginalSNGtableLength - MaxChunkMatch_Index); // = SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;
    }
   //}
  }
  else {  printf("*** Partial match -> partial addition and a jump to the matched existing end-chunk. ***\n");  //partial match at the end, keeping instrument-index, but replacing 1st entry of match with a jump to the match, and deleting the rest
   output_table->Length = OriginalSNGtableLength + SNGnewChunkLength - MaxChunkMatch_Length;  //shorten to only non-matching part
   for (i = output_table->Length; i < OriginalSNGtableLength + SNGnewChunkLength; ++i) {  //remove the rest of data (matched 2nd half)
    output_table->Columns[0][i] = SNG_VALUE_INSTRUMENTTABLE_EMPTY; output_table->Columns[1][i] = 0x00;
   }
   //if ( output_table == &( SNGinstrumentData.WaveformArpTable ) ) {  //handle expnded positions (by inserted chords or arp-speed repeats)
    for (i = 0; i < SNGnewChunkLength; ++i) {  //find SWM-table position for SNG-position-modifications
     if ( sng_position_array/*SNG_WFtablePositions*/[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ][i]
          == SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + output_table->Length ) break;
    }
    for (; i < SWM_TableLength; ++i) {  //change cancelled output's positions to matching jumped-to chunk's positions
     sng_position_array/*SNG_WFtablePositions*/[ instrument_number + SWM_INSTRUMENT_NUMBER_MIN ]
      [ /*SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN +*/ i ]
       -= (OriginalSNGtableLength + SNGnewChunkLength - MaxChunkMatch_Length) - MaxChunkMatch_Index; //= SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;
    }
   //}
   if ( output_table->Columns[0][ MaxChunkMatch_Index ] == SNG_VALUE_INSTRUMENT_TABLE_JUMP
        && output_table->Columns[1][ MaxChunkMatch_Index ] == SNG_VALUE_INSTRUMENT_TABLEJUMP_PARAMETER_END )  //if a jump, stop (GT skips a jump pointed by another jump)
   { SNG_addToInstrumentTable( output_table, SNG_VALUE_INSTRUMENT_TABLE_JUMP, SNG_VALUE_INSTRUMENT_TABLEJUMP_PARAMETER_END ); }
   else SNG_addToInstrumentTable( output_table, SNG_VALUE_INSTRUMENT_TABLE_JUMP, SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + MaxChunkMatch_Index );
  }
 }
 #endif  //end of SNG_OPTIMIZE_WF_PW_FILT_TABLES preprocessor-condition

}


//slide (FREQMODL,FREQMODH) calculation in SID-Wizard 1.2 (if non-zero):
// ExpTabIndex=(Input/2)+DPITCH; //(pitch-dependent if calculated slide/vibrato is supported by player-type)
// if (ExpPabIndex<ExpTreshold) { FREQMODL=SWExpTabH[ExpTabIndex]; FREQMODH=0}
// else { FREQMODL=SWExpTabL[ExpTabIndex-ExpTreshold]; FREQMODH=SWExpTabH[ExpTabIndex-ExpTreshold]:
void convertSWMslideToSNGslide (unsigned char slidein, int note_number, int is_portamento,  //note-numbering is assumed as 1-based SWM-format on entry
                                unsigned char* slideout_hi, unsigned char* slideout_lo) {
 enum { //SWM_FREQ_EXPTABLE_BASED_SLIDES_THRESHOLD = 0x60 /*congabeat.swm orderlistpos. $26*/, //0x70 /*oakyardmemo.swm bass octave-slides*/ //0x80, //0x98,  //GT calculated slides has a plateau, so above this input-value we're using non-calculated slide/portamento instead of the calculated which has a speed/slidesize limit
         SWM_FREQ_EXPTABLE_BASED_SLIDE_INPUTDIV = 2,  //in the SW playercode the division is 2 (and by 4 for vibratos btw), reproducing that here too
        SWM_TO_SNG_EXPLOOKUP_BASEINDEX_COMPENSATION = (SWM_FREQTBH_POS / 2),  //it seems about a half-octave shift is needed for some reason, without it the slides are slower
        SNG_CALCULATED_SLIDE_DIVSHIFTS = 3, SNG_CALCULATED_SLIDE_DIV = (1 << SNG_CALCULATED_SLIDE_DIVSHIFTS),
        SNG_NONCALCULATED_SLIDE_DIVSHIFTS = 3, SNG_NONCALCULATED_SLIDE_DIV = (1 << SNG_CALCULATED_SLIDE_DIVSHIFTS) /*, NONCALC_COMPENSATION=80*/ };
 int SlideTableIndex, PitchModValue;

 if (SWMcalculatedVibSlideON /*&& slidein < SNG_NONCALCULATED_SLIDES_THRESHOLD*/ /*&& !is_portamento*/) {  //GT has calculated slide if high-byte is bigger than $7F (bit7 set)?! - should be converted differently (omitting 'Dpitch' from the equation)
  if (slidein < SWM_FREQ_EXPTABLE_BASED_SLIDES_THRESHOLD /*|| is_portamento*/) {  //if possible, use calculated slide of GT (but that doesn't produce high-enough range for some slides)
   *slideout_hi = SNG_BITVALUE__SPEEDTABLE_PERIOD__CALCULATED_VIBRATO;  //turn on GT calculated slide/portamento bit (the other bits don't count here)
   SlideTableIndex = ( slidein >= SNG_CALCULATED_SLIDE_DIV || slidein==0 ) ?
                        (slidein >> SNG_CALCULATED_SLIDE_DIVSHIFTS) : 1; //the minimum nonzero slide is at 2nd position in the table
   if (is_portamento) {  //portamento sounds more consistent converted like this in GT
    if ( SlideTableIndex > sizeof(SNGcalculatedPortamentoSpeedDivisors) - 1 ) SlideTableIndex = sizeof(SNGcalculatedPortamentoSpeedDivisors) - 1;
    *slideout_lo = SNGcalculatedPortamentoSpeedDivisors[ SlideTableIndex ];  //the calculated slide divisor
   }
   else {
    if ( SlideTableIndex > sizeof(SNGcalculatedSlideSpeedDivisors) - 1 ) SlideTableIndex = sizeof(SNGcalculatedSlideSpeedDivisors) - 1;
    *slideout_lo = SNGcalculatedSlideSpeedDivisors[ SlideTableIndex ];  //the calculated slide divisor
   }
  }

  else {  //for higher slide-values calculate the SWM slide to GT slide with the original SW freqtable-exptable content
   if (slidein == 0x00) { *slideout_hi = *slideout_lo = 0x00; return; }  //so $00 input produces $00 output, no matter the tabledata and lookup baseindex. table doesn't need to have 00-s at the first places corresponding to divided slidein's 0 value
   SlideTableIndex = SWM_TO_SNG_EXPLOOKUP_BASEINDEX_COMPENSATION
                     + (slidein / SWM_FREQ_EXPTABLE_BASED_SLIDE_INPUTDIV)  //the indexing needs to be kept multiples of 12 no be in sync with the bigger slides)
                     + (note_number - SWM_NOTE_NUMBER_MIN);  //and as SWMcalculatedVibSlideON is true here, add notepitch too for calculated note-dependent slide-value
   if (SlideTableIndex < SWM_EXPTHRESHOLD) {  //looked-up resulting GT-slide value fits in low-byte
    PitchModValue = GTexpTabH/*SWexpTabH*/[ SlideTableIndex ];  // *slideout_hi = 0; *slideout_lo = SWexpTabH[ SlideTableIndex ];
   }
   else {  //bigger (2-byte) GT slide-value:
    SlideTableIndex -= SWM_EXPTHRESHOLD;
    if (SlideTableIndex > sizeof(SWexpTabL) ) SlideTableIndex = sizeof(SWexpTabL) - 1;  //limiting/saturation of freq-tableindex range (slide + note can make it bigger than exptable-size)
    PitchModValue = ( GTexpTabH/*SWexpTabH*/[ SWM_FREQTBH_POS + SlideTableIndex ] << 8 ) | GTexpTabL/*SWexpTabL*/[ SlideTableIndex ];
   }  //PitchModValue /= SNG_VALUE__SPEEDTABLE__NONCALCULATED_SLIDE_DIV;
   if (is_portamento) {
    if (PitchModValue > SNG_VALUE__SPEEDTABLE__NONCALCULATED_PORTAMENTO_MAX) PitchModValue = SNG_VALUE__SPEEDTABLE__NONCALCULATED_PORTAMENTO_MAX;  //avoid strange portamento-overload (that produces a slide in GT instead of portamento by missing the target note-frequency)
   }
   else {
    if (PitchModValue > SNG_VALUE__SPEEDTABLE__NONCALCULATED_SLIDE_MAX) PitchModValue = SNG_VALUE__SPEEDTABLE__NONCALCULATED_SLIDE_MAX;  //avoid strange portamento-overload (that produces a slide in GT instead of portamento by missing the target note-frequency)
   }
   *slideout_hi = PitchModValue >> 8; *slideout_lo = PitchModValue & 0xFF;
  }
 }

 else {  //if no calculated slide in SWM light/bare/demo-player, direct conversion into GT
  SlideTableIndex = ( slidein >= SNG_NONCALCULATED_SLIDE_DIV || slidein==0 ) ?
                       (slidein >> SNG_NONCALCULATED_SLIDE_DIVSHIFTS) : 1; //the minimum nonzero slide is at 2nd position in the table
  if (is_portamento) {
   PitchModValue = SNGportamentoSpeeds[ SlideTableIndex ];
   if ( SlideTableIndex > sizeof(SNGportamentoSpeeds) - 1 ) SlideTableIndex = sizeof(SNGportamentoSpeeds) - 1;
  }
  else {
   PitchModValue = SNGslideSpeeds[ SlideTableIndex ];
   if ( SlideTableIndex > sizeof(SNGslideSpeeds) - 1 ) SlideTableIndex = sizeof(SNGslideSpeeds) - 1;
  }
  *slideout_hi = PitchModValue >> 8; *slideout_lo = PitchModValue & 0xFF;
 }
}


//In SWM player, about the same calculation as for slide, but the input-value is half of slide's
void convertSWMvibratoToSNGvibrato (unsigned char input, unsigned char* output_period, unsigned char* output_amplitude) {
 unsigned char ExpTableIndex, InputAmplitude = ( *(SWMvibratoAmpAndSpeed*) &input ).Amplitude;
 *output_period = GTvibRates[ ( *(SWMvibratoAmpAndSpeed*) &input ).Period ];
 if (SWMcalculatedVibSlideON) {  //GT has calculated vibrato if period is bigger than $7F (bit7 set)?! - should be converted differently (omitting 'Dpitch'/note from the equation)
  *output_period |= SNG_BITVALUE__SPEEDTABLE_PERIOD__CALCULATED_VIBRATO;  //turn on GT calculated vibrato bit
  *output_amplitude = SNGcalculatedVibratoAmplitudeDivisors[ InputAmplitude ];
 }
 else { //if no calculated vibrato in SWM light/bare/demo-player, direct conversion into GT
  ExpTableIndex = SWM_FREQTBH_POS-1 + InputAmplitude * 8;
  if (ExpTableIndex >= sizeof(SWexpTabH) ) ExpTableIndex = sizeof(SWexpTabH) - 1;
  *output_amplitude = SWexpTabH[ ExpTableIndex ];  //else *output_amplitude = ( ( SWexpTabH[ ExpTableIndex - FREQTB_SIZE ] << 8 ) | SWexpTabL[ ExpTableIndex - FREQTB_SIZE ] ) >> 8;
 }
}


unsigned char convertSWMslideToSNGspeedTable (unsigned char slidein, unsigned char note) {  //1-based SWM note-numbering
 //unsigned char SpeedTableIndex = SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + SNGinstrumentData.SpeedTable.Length;
 unsigned char SNGslideSpeedHi, SNGslideSpeedLo;
 convertSWMslideToSNGslide( slidein, note, 0, &SNGslideSpeedHi, &SNGslideSpeedLo );
 return SNG_addToSpeedTable( SNGslideSpeedHi, SNGslideSpeedLo );
}

unsigned char convertSWMportamentoToSNGspeedTable (unsigned char slidein, unsigned char note) {  //1-based SWM note-numbering  //0-based SNG note-numbering
 //unsigned char SpeedTableIndex = SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + SNGinstrumentData.SpeedTable.Length;
 unsigned char SNGslideSpeedHi, SNGslideSpeedLo;
 convertSWMslideToSNGslide( slidein, note, 1, &SNGslideSpeedHi, &SNGslideSpeedLo );
 return SNG_addToSpeedTable( SNGslideSpeedHi, SNGslideSpeedLo );
}


//This is the same scanthrough-code as getSWMstatistics, so it's obsoleted, would be a redundant code to use at this point
/*unsigned char getInstrumentAtRow (unsigned char pattern, unsigned char rowindex) {  //scans through the orderlist until the given pattern and patternrow-index is found, and tells the current instrument (with 0-based numbering)
 int i,j,k,l;  //printf("%.2X,%.2X\n",SWM_PATTERN_NUMBER_MIN+pattern,rowindex);
 unsigned char SWMpattern = SWM_PATTERN_NUMBER_MIN + pattern, SeqData, PatternRowIndex, PatternData;
 unsigned char *SeqPointer;
 unsigned char CurrentInstrument [SID_CHANNELCOUNT]; //1-based internally
 unsigned char CurrentNote [SID_CHANNELCOUNT];  //aids frequency-dependent slide-calculations based on note-number
 for (i=0; i < SubtuneAmount; ++i) {
  for (j=0; j < SID_CHANNELCOUNT; ++j) {
   CurrentInstrument[ j ] = SWM_INSTRUMENT_NUMBER_DEFAULT;  //printf("channel:%.2X\n",j);
   for (k = 0; k < SWM_SEQ_SIZE[ i * SID_CHANNELCOUNT + j ] && k < SNG_ORDERLIST_SEQUENCE_SIZE_MAX; ++k) { //'k' is source-pointer here
    SeqPointer = &( SEQUENCES[ i * SID_CHANNELCOUNT + j ][ k ] ); SeqData = *SeqPointer;  //printf("seqpos:%.2X\n",k);
    if (SWM_PATTERN_NUMBER_MIN <= SeqData && SeqData <= SWM_PATTERN_NUMBER_MAX ) {  //Pattern-number
     for (l = 0; l < PATTERNSIZES[ SeqData ]; ++l) {  //play pattern to look for instrument-changes
      PatternRowIndex = l; PatternData = PATTERNS[ SeqData ][ l ];  //printf("ptnpos:%.2X\n",l);
      if (PatternData == SWM_VALUE__PATTERN_COLUMN1__PATTERN_END) break;
      else {  //note-column read, not end-mark, get possible additional instrument/FX columns
       if (PatternData & SWM_BITVALUE__PATTERN_COLUMN1__NEXTCOLUMN) { //instrument-column exists
        PatternData = PATTERNS[ SeqData ][ ++l ];
        if ( between( PatternData & SWM_MASK__PATTERN_COLUMN2, SWM_VALUE__PATTERN_COLUMN2__INSTRUMENT_MIN, SWM_VALUE__PATTERN_COLUMN2__INSTRUMENT_MAX ) ) {
         CurrentInstrument[ j ] = PatternData & SWM_MASK__PATTERN_COLUMN2;  //printf("Selected instrument on channel%d: seqindex%.2X, pattern:$%.2X index:$%.2X -> instrument:%.2X\n",j,k,SeqData,PatternRowIndex,CurrentInstrument[ j ]);
        }
        if (PatternData & SWM_BITVALUE__PATTERN_COLUMN2__NEXTCOLUMN) { //fx-column exists
         PatternData = PATTERNS[ SeqData ][ ++l ];
         if (PatternData < SWM_VALUE__PATTERN_SMALLFX__MIN) ++l;
        }
       }
       if (SeqData == SWMpattern && PatternRowIndex == rowindex) {  //printf("Found instrument:on channel%d: seqindex%.2X, pattern:$%.2X index:$%.2X -> instrument:%.2X\n",j,k,SeqData,PatternRowIndex,CurrentInstrument[ j ]);
        return CurrentInstrument[j] - SWM_INSTRUMENT_NUMBER_MIN;  //returns instrument-number (0-based numbering)
       }
    }}}
    else if (SeqData == SWM_VALUE__SEQUENCE_END || SeqData == SWM_VALUE__SEQUENCE_LOOP) break;
 }}}  printf("Couldn't find pattern%.2X rowindex%.2X when scanning through subtunes for instrument-changes. Probably pattern not used in orderlist.\n",SWM_PATTERN_NUMBER_MIN+pattern,index);
 return SWM_INSTRUMENT_NUMBER_DEFAULT - SWM_INSTRUMENT_NUMBER_MIN;  //fallback
};*/

unsigned char getInstrumentSWMindexAtRow (unsigned char pattern, unsigned char rowindex) {  //1-based instrument-numbering (first valid instrument is at 1), input: 0-based pattern-number
 unsigned char Instrument;
 Instrument = SWMtuneStatistics.CurrentInstruments[ pattern + SWM_PATTERN_NUMBER_MIN ][ rowindex ];
 if (Instrument == SWM_INSTRUMENT_NUMBER_UNDETECTED) return SWM_INSTRUMENT_NUMBER_DEFAULT;  //if instrument is not selected in tune (e.g. vibrato.swm), use default
 return Instrument;
}
unsigned char getInstrumentSNGindexAtRow (unsigned char pattern, unsigned char rowindex) {  //0-based instrument-numbering (first valid instrument is 0, as in SNG file), input: 0-based pattern-number
 unsigned char Instrument;
 Instrument = SWMtuneStatistics.CurrentInstruments[ pattern + SWM_PATTERN_NUMBER_MIN ][ rowindex ];
 if (Instrument == SWM_INSTRUMENT_NUMBER_UNDETECTED) return SNG_INSTRUMENT_NUMBER_DEFAULT;  //if instrument is not selected in tune (e.g. vibrato.swm), use default
 return Instrument - SWM_INSTRUMENT_NUMBER_MIN;
}

unsigned char getChordSWMindexAtRow (unsigned char pattern, unsigned char rowindex) {  //1-based chord-numbering (first valid chord is at 1), input: 0-based pattern-number
 return SWMtuneStatistics.CurrentChords[ pattern + SWM_PATTERN_NUMBER_MIN ][ rowindex ];
}


unsigned char getNoteSWMindexAtRow (unsigned char pattern, unsigned char rowindex) {  //1-based note numbering, input: 0-based pattern-number, doesn't take per-instrument octave into account
 return SWMtuneStatistics.CurrentNotes[ pattern + SWM_PATTERN_NUMBER_MIN ][ rowindex ];
}
unsigned char getNoteSWMindexAtRowForSlide (unsigned char pattern, unsigned char rowindex) {  //1-based note numbering, //this one adds SWM instrument's signed 'octave' setting
 unsigned char Note = getNoteSWMindexAtRow( pattern, rowindex );
 unsigned char CurrentInstrument = getInstrumentSWMindexAtRow( pattern, rowindex );
 if (Note == SWM_NOTE_NUMBER_NONE) return SWM_NOTE_NUMBER_DEFAULT; //middle note default is better than nothing for the pattern before any new note gets detected for slide-calculation, but it's better to scan the whole sourcetune for notes in previous patterns, as pattern might not even have a note, just slides (e.g. congabeat.swm sequence-position 26 pattern 20..22)
 return Note + (signed char) INSTRUMENTS[ CurrentInstrument ][ SWI_OCTAVE_POS ];  //* SEMITONES_PER_OCTAVE;
}



void GenerateSNG() {
 enum { SEMITONES_PER_OCTAVE = 12 };
 int i,j,k,l,Temp, SourceIndex,TargetIndex,AuthorNameLength,TitleLength, InstrumentTableDataSize;
 int SWMseqTempoSetting = -1, SWMseqVolumeSetting = -1;
 unsigned char PlayerType, SourceData, /*SWMnoteNumber*/ SWMcurrentNote;
 unsigned char SWMinstrumentNumber, SNGinstrumentNumber, SWMchordNumber;
 unsigned char *CharPointer, *SourcePointer, *TargetPointer; SNGinstrumentHeader *InstrumentPointer;
 unsigned char TargetOrderlistPositions [SNG_ORDERLIST_SEQUENCE_SIZE_MAX];
 unsigned char SWMinstrumentVibratoValue, SNGvibratoPeriod, SNGvibratoAmplitude, DummyByte;
 unsigned char SWMinstrumentTableIndex, SNGinstrumentTableIndex, SNGchordWFtableIndex;
 unsigned char SWMinstrumentGateOffWFtableIndex, SWMinstrumentGateOffPWtableIndex, SWMinstrumentGateOffFilterTableIndex;
 unsigned char SWMpatternRow, SWMpatternRowIndex, SNGpatternRowIndex;
 unsigned char SWMcolumn1data, SWMcolumn2data, SWMcolumn3data, SWMcolumn4data;  //SIDwizard pattern-row column1..4
 unsigned char  SNGnote, SNGinstrument, SNGfx, SNGparameter;  //GoatTracker pattern-row column1..4
 int SNGpitchFXstart=0, NoteColumnPortamentoON=0, MoveDownNoteColumnFX = -1;
 int SWMsustainByPatternFX = -1, SWMreleaseByPatternFX = -1;
 int SWMattackByPatternFX = -1, SWMdecayByPatternFX = -1;
 SWMinstrumentControlFlags SWMinstrumentControlByte;
 SWMvibratoAmpAndSpeed SWMvibratoParameters;
 static time_t CurrentTime; static char* TimeString;
 //static SWMstatistics *StatisticsPointer;


 printf("to SNG GoatTracker worktune format. \n");
 printf("Output file-name will be: %s\n",OutputFileName);
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)
 if (OutputFile == NULL) { printf("Couldn't open output-file for writing!\n"); exit(-1); }

 //Perform statistics on the SWM-tune by scanning through orderlist and patterns in the playback-order
 getSWMstatistics();  //init statistics and collect them: scan through the whole tune as if it was played, and e.g. detect which channel(s) use the filtered instrument exclusively/dominantly (to set filterswitch/mask in its filtertable)
 SetChordInstr(); //register Chord-Instruments and Chords

 //Fill out data in SNG header:
 printf( "AuthorInfo in SWM: \"%s\"\n", AUTHORINFO );
 if ( ( CharPointer = strchr( AUTHORINFO, ':' ) ) != NULL ) {
  removeTrailingSpaces( AUTHORINFO );
  AuthorNameLength = (char*)CharPointer - AUTHORINFO;  //if ( AuthorNameLength > sizeof(SNGheader.AuthorName) ) AuthorNameLength = sizeof(SNGheader.AuthorName);
   strncpy( SNGheader.AuthorName, AUTHORINFO, sizeof(SNGheader.AuthorName) - 1 /*AuthorNameLength*/ );  SNGheader.AuthorName[ AuthorNameLength ] = '\0';
    removeTrailingSpaces( SNGheader.AuthorName );
  TitleLength = strlen(AUTHORINFO) - (AuthorNameLength+1);  //if ( TitleLength > sizeof(SNGheader.TuneTitle) ) TitleLength = sizeof(SNGheader.TuneTitle);
   for (i=0; i < TitleLength; ++i) { if (AUTHORINFO[ AuthorNameLength+1 + i ] != ' ') break; }  //skip leading spaces by 'i'
   strncpy( SNGheader.TuneTitle, AUTHORINFO + AuthorNameLength+1 + i, sizeof(SNGheader.TuneTitle) - 1 /*TitleLength*/ );  //SNGheader.TuneTitle[ TitleLength ] = '\0';
    removeTrailingSpaces( SNGheader.TuneTitle );
 }
 else { strncpy( SNGheader.TuneTitle, AUTHORINFO, sizeof(SNGheader.TuneTitle) ); }
 CurrentTime = time(NULL);
 strftime( SNGheader.ReleaseInfo, sizeof(SNGheader.ReleaseInfo), "%d %b %Y %H:%M ", localtime(&CurrentTime) ); strcat( SNGheader.ReleaseInfo, "(SWMconvert)" );
 SNGheader.SubtuneCount = SubtuneAmount;  printf(" Author: \"%s\"\n Title: \"%s\"\n ReleaseInfo: \"%s\"\n Subtunes:%d\n",SNGheader.AuthorName,SNGheader.TuneTitle,SNGheader.ReleaseInfo,SNGheader.SubtuneCount);

 PlayerType = TUNEDATA[ TYPE_ID_OFFSET + DRIVERTYPE_POS ];
 SWMcalculatedVibSlideON = PlayerType == SWM_VALUE__PLAYERTYPE_NORMAL || PlayerType == SWM_VALUE__PLAYERTYPE_EXTRA || PlayerType == SWM_VALUE__PLAYERTYPE_MEDIUM;
 SWMfilter11bit = PlayerType != SWM_VALUE__PLAYERTYPE_LIGHT && PlayerType != SWM_VALUE__PLAYERTYPE_BARE;  printf("Player-type:%d, Calculated vibrato/slide:%d, 11bit-filter:%d\n",PlayerType,SWMcalculatedVibSlideON,SWMfilter11bit);


 //Convert instruments (headers and common tables) (Note: SID-Wizard instruments start from '1' while SNG instruments start from '0')
 SNGinstrumentData.InstrumentCount = InstAmount;  printf("\nInstruments: %d\n",SNGinstrumentData.InstrumentCount);

 for (i=0; i < SNG_INSTRUMENTTABLE_LENGTH_MAX; ++i) {
  SNGinstrumentData.WaveformArpTable.Length = 0;
  SNGinstrumentData.WaveformArpTable.Columns[0][i] = SNGinstrumentData.WaveformArpTable.Columns[1][i] = 0x00;
  SNGinstrumentData.PulseWidthTable.Length = 0;
  SNGinstrumentData.PulseWidthTable.Columns[0][i] = SNGinstrumentData.PulseWidthTable.Columns[1][i] = 0x00;
  SNGinstrumentData.FilterTable.Length = 0;
  SNGinstrumentData.FilterTable.Columns[0][i] = SNGinstrumentData.FilterTable.Columns[1][i] = 0x00;
  SNGinstrumentData.SpeedTable.Length = 0;
  SNGinstrumentData.SpeedTable.Columns[0][i] = SNGinstrumentData.SpeedTable.Columns[1][i] = 0x00;
 }

 //Init chord-indexes to None. //Add chords to the beginning of WFtable? though less important than normal WF+Arp instrument entries in case the table gets full, but it's easier to do like this, filling WFarp-tables with chord-jumps/data on-the-fly, than correcting/adding afterwards)
 SetChordInstr(); //scan through SWM CHORDS[] ,into ChordIndex[] and ChordLen[] arrays, setting ChordAmount
 for (i=0; i <= SWM_INSTRUMENT_AMOUNT_MAX /*ChordAmount*/; ++i) {
  for (j=0; j <= SWM_CHORD_AMOUNT_MAX; ++j) { //for (j=0; j < ChordLen[i+SWM_CHORD_NUMBER_MIN]; ++j) {
   SNGchordWFtableIndexes[i][j] = SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;  //SNGinstrumentData.WaveformArpTable.Length;
   //SNG_addToWFtable( 0, CHORDS[ ChordIndex[i+SWM_CHORD_NUMBER_MIN] + j ] );
  }
  //SNG_addToWFtable( SNG_VALUE_INSTRUMENT_TABLE_JUMP, SNG_VALUE_INSTRUMENT_TABLEJUMP_PARAMETER_END );
 }
 for (i=0; i <= SWM_INSTRUMENT_AMOUNT_MAX; ++i) {
  for (j=0; j < SWM_INSTRUMENT_TABLE_SIZE_MAX; ++j) {
   SNG_FilterTablePositions [SWM_INSTRUMENT_AMOUNT_MAX+1] [SWM_INSTRUMENT_TABLE_SIZE_MAX+1]
    = SNG_PWtablePositions [SWM_INSTRUMENT_AMOUNT_MAX+1] [SWM_INSTRUMENT_TABLE_SIZE_MAX+1]
     = SNG_WFtablePositions[i][j] = SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;
 }}

 for (i=0; i < InstAmount; ++i) {
  InstrumentPointer = &( SNGinstrumentData.InstrumentHeaders[i] );
  strncpy( InstrumentPointer->Name, INSTNAMES[ i + SWM_INSTRUMENT_NUMBER_MIN ], instnamelength ); for (j=instnamelength; j < SNG_INSTRUMENT_NAME_SIZE_MAX; ++j) InstrumentPointer->Name[j] = '\0';  printf("\nInstrument %.2X name:%s\n",i+SWM_INSTRUMENT_NUMBER_MIN,InstrumentPointer->Name);
  SWMinstrumentControlByte = *(SWMinstrumentControlFlags*) &( INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_CONTROL_POS ] );
  SWMinstrumentVibratoValue = INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_INSVIBRATO_POS ];
  SWMvibratoParameters = *(SWMvibratoAmpAndSpeed*) &( SWMinstrumentVibratoValue );
  InstrumentPointer->AttackDecay = INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_AD_POS ];  //and Hard-restart ADSR is a global (not per-instrument) setting in GoatTracker
  InstrumentPointer->SustainRelease = INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_SR_POS ];

  //Legato and hard-restart / frame1 waveform settings are totally confused in GT and its docs/references, tried to mimic SW settings by reverse-engineering and experience...
  InstrumentPointer->HardRestartTime =  //$00/$80 not allowed in GT (causes GT-bugs, sound-delays)
   ( SWMinstrumentControlByte.HRtimer ? SWMinstrumentControlByte.HRtimer : SNG_VALUE__HARDRESTART_TIMER_MIN )  //0x02; //INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_CONTROL_POS ];
   | (SWMinstrumentControlByte.HRtimer == 0) * SNG_BITVALUE__INSTRUMENT_HRTIMER_NOHR  //noHR bit $80 doesn't seem to be very effective in GT btw.
   /*| (SWMinstrumentControlByte.Staccatto == 0) * SNG_BITVALUE__INSTRUMENT_HRTIMER_NO_HRGATEOFF*/;  //this clashes with $06 FX in GT (causes dead notes)
  InstrumentPointer->Frame1WaveForm = INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_FRAME1WAVEFORM_POS ];
   if (InstrumentPointer->Frame1WaveForm == 0x00 && SWMinstrumentControlByte.WFrame1) {
    InstrumentPointer->Frame1WaveForm = SWM_VALUE__FRAME1WAVEFORM__FALLBACK;  //workaround for pre-SW-v1.2 SWM tunes (used by SID-Wizard depacker too)
   }
   if (!InstrumentPointer->Frame1WaveForm) InstrumentPointer->Frame1WaveForm = SNG_VALUE__INSTRUMENT_FRAME1_WAVECONTROL_SKIP; //$00, actually $00 can't be set in SID-Wizard as 1st frame waveform, just the control-byte's corresponding flag set
   if (!SWMinstrumentControlByte.WFrame1 && !SWMinstrumentControlByte.Staccatto) {  //1stframe waveform setting turned off in SW?
    InstrumentPointer->Frame1WaveForm = SNG_VALUE__INSTRUMENT_FRAME1_WAVEFORM_SKIP_GATE_ON;  //gate is set but waveform not disturbed, gateoff variant $FE or $00 would disable note
    InstrumentPointer->HardRestartTime = SNG_VALUE__HARDRESTART_TIMER_MIN | (InstrumentPointer->HardRestartTime & SNG_MASK__HARDRESTART_FLAGS) /*| SNG_BITVALUE__INSTRUMENT_HRTIMER_NOHR*/;  //get the closest to SW's frame1waveform-OFF as possible with the trick of setting minimal HR-time  (with longer timer the 1st frame waveform would be that many audible frames with no-yet-set pitch)
   }

  InstrumentPointer->VibratoDelay = INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_VIBDELAY_POS ];
   if ( InstrumentPointer->VibratoDelay == SNG_VALUE__INSTRUMENT_VIBRATO_DELAY__DISABLE_VIBRATO  //avoid disabling vibrato by 0-delay
        && (SWMvibratoParameters.Amplitude != 0 || SWMvibratoParameters.Period != 0) )
              { InstrumentPointer->VibratoDelay = SNG_VALUE__INSTRUMENT_VIBRATO_DELAY_VALID_MIN; }

  InstrumentPointer->Vibrato_SpeedTableIndex = SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;

  if (SWMvibratoParameters.Amplitude != 0 || SWMvibratoParameters.Period != 0) {
   convertSWMvibratoToSNGvibrato( SWMinstrumentVibratoValue, &SNGvibratoPeriod, &SNGvibratoAmplitude );
   InstrumentPointer->Vibrato_SpeedTableIndex = SNG_addToSpeedTable( SNGvibratoPeriod, SNGvibratoAmplitude );
  }
  else InstrumentPointer->Vibrato_SpeedTableIndex = SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE;

  //add instrument's tables to SNG instrument-tables (or maybe link to an existing matching table-portion)

  convertSWMtableToSNGtable( i, "Waveform-Arpeggio", WFTABLEPOS  //table-position is fixed in SWM at instrument-index $10
   , INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_PULSETBPT_POS ]
   , &( InstrumentPointer->WaveformArpTableIndex ), &SNGinstrumentData.WaveformArpTable
   , SWM_CHORD_NUMBER_NONE, convertWFtableRowToSNG, SNG_WFtablePositions );

  convertSWMtableToSNGtable( i, "PulseWidth", INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_PULSETBPT_POS ]
   , INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_FILTERTBPT_POS ]
   , &( InstrumentPointer->PulseWidthTableIndex ), &SNGinstrumentData.PulseWidthTable
   , SWM_CHORD_NUMBER_NONE, convertPWtableRowToSNG, SNG_PWtablePositions );

  SNG_addToFilterTable( SNG_VALUE_MIN__FILTERTABLE_COMMAND__SET_BAND_RESO_SWITCH, 0x00 );  //placed at 1st table-index $01, for non-filtered instruments only appearing on the same channel as the filtered instrument (when only 1 channel is filtered throughout the tune)
   SNG_addToFilterTable( SNG_VALUE_INSTRUMENT_TABLE_JUMP, SNG_VALUE_INSTRUMENT_TABLEJUMP_PARAMETER_END );
  convertSWMtableToSNGtable( i, "Filter", INSTRUMENTS[ i + SWM_INSTRUMENT_NUMBER_MIN ][ SWI_FILTERTBPT_POS ]
   , INSTRUMENTSIZES[ i + SWM_INSTRUMENT_NUMBER_MIN ]
   , &( InstrumentPointer->FilterTableIndex ), &SNGinstrumentData.FilterTable
   , SWM_CHORD_NUMBER_NONE, convertFilterTableRowToSNG, SNG_FilterTablePositions );
  if (InstrumentPointer->FilterTableIndex == SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE) {
   //StatisticsPointer = &SWMtuneStatistics; //getSWMstatistics( i );  //check if instrument is only used on the only filtered channel
   SourceIndex = getArraySingleNonzeroIndex( SWMtuneStatistics/*Pointer->*/.FilterControllerInstrumentSelection_Counts, SID_CHANNELCOUNT );
   TargetIndex = getArraySingleNonzeroIndex( SWMtuneStatistics/*Pointer->*/.Instrument_NoteCounts[i+SWM_INSTRUMENT_NUMBER_MIN], SID_CHANNELCOUNT );  printf("Instrument $%.2X usage on channels: %d %d %d\n",i+SWM_INSTRUMENT_NUMBER_MIN, SWMtuneStatistics.Instrument_NoteCounts[i+SWM_INSTRUMENT_NUMBER_MIN][0],SWMtuneStatistics.Instrument_NoteCounts[i+SWM_INSTRUMENT_NUMBER_MIN][1],SWMtuneStatistics.Instrument_NoteCounts[i+SWM_INSTRUMENT_NUMBER_MIN][2]);
   if (SourceIndex >= 0 && TargetIndex >= 0 && SourceIndex == TargetIndex) {  //allow nonfilt-instrument turn off filter on the sole channel using any filtered instrument (because GT doesn't do it automatically as SW)
    InstrumentPointer->FilterTableIndex = SNG_VALUE__INSTRUMENT_FILTERTABLE_INDEX__FILTEROFF;  printf("Set instrument $%.2X to turn filter off, as it's only used on the single filtered channel%d.\n",i+SWM_INSTRUMENT_NUMBER_MIN,SourceIndex+1);
  }}
 }  //for (i=0; i < SNG_INSTRUMENTTABLE_LENGTH_MAX; ++i) printf("%.2X:%.2X ",i,SNG_WFtablePositions[i]);

 //for (i=0; i < SWM_INSTRUMENT_AMOUNT_MAX /*ChordAmount*/; ++i) {
 // for (j=0; j < SWM_CHORD_AMOUNT_MAX; ++j) {
 //  if (SNGchordWFtableIndexes[i][j] != SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE) printf("%.2X/%.2X:%.2X ", i,j, SNGchordWFtableIndexes[i][j] );
 //}} printf("\n");


 //Convert patterns (Note: first usable SID-Wizard pattern number is '1', while SNG patern-numbering is 0-based)
 SNGpatternData.PatternCount = PatternAmount;  printf("SWM Patterns: %d\n",PatternAmount);
 for (i=0; i < SNGpatternData.PatternCount; ++i) {  printf("SWMpattern:$%.2X, Length:$%.2X, Size:$%.2X -> SNGpattern:$%.2X\n",SWM_PATTERN_NUMBER_MIN+i,PATTERNLENGTHS[SWM_PATTERN_NUMBER_MIN+i],PATTERNSIZES[SWM_PATTERN_NUMBER_MIN+i], i );
  //SWMcurrentNote = SWM_NOTE_NUMBER_DEFAULT;  //middle note default is better than nothing for the pattern before any new note gets detected for slide-calculation, but it's better to scan the whole sourcetune for notes in previous patterns, as pattern might not even have a note, just slides (e.g. congabeat.swm sequence-position 26 pattern 20..22
  SNGpatternData.Patterns[i].Length = 0;
  SNGnote = SNG_VALUE__PATTERN_COLUMN1__NOP; SNGinstrument = SNG_VALUE__PATTERN_COLUMN2__NOP;
   SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00;
  SWMsustainByPatternFX = -1, SWMreleaseByPatternFX = -1;  //pattern with no ADSR FX yet should prioritize on instruent's ADSR for Sustain/Release settings (instead of previous Sustain/Release set by PatternFX which cannot be determined here, would need to account for sequencing in which previous pattern can be different many times)
   SWMattackByPatternFX = -1, SWMdecayByPatternFX = -1;  //the same goes for Attack/Decay
    MoveDownNoteColumnFX = -1;

  for (j=SWMpatternRow=NoteColumnPortamentoON=0; j < PATTERNSIZES[ SWM_PATTERN_NUMBER_MIN + i ]; ++j) {
   SNGnote = SNG_VALUE__PATTERN_COLUMN1__NOP; SNGinstrument = SNG_VALUE__PATTERN_COLUMN2__NOP;  //init these before every processed row
   if ( !isSNGremainingFX( SNGfx, SNGparameter ) ) { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; }  //in GT the pitch-slide/vibrato effects should be permanent (in SW they're so even accross pattern but it's not mimiced yet)
   SNGpitchFXstart = 0;
   SWMpatternRowIndex = j; SNGpatternRowIndex = SNGpatternData.Patterns[i].Length;
    //SWMcurrentNote = getNoteSWMindexAtRow( i, SWMpatternRowIndex );
   SWMcolumn1data = PATTERNS[ i + SWM_PATTERN_NUMBER_MIN ][j]; //SWM_VALUE__PATTERN_COLUMN1__NOP;
    SWMcolumn2data = SWM_VALUE__PATTERN_COLUMN2__NOP; SWMcolumn3data = SNG_VALUE__PATTERN_FX__NOP; SWMcolumn4data = 0x00;

   if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__PATTERN_END) {
    SNGnote = SNG_VALUE__PATTERN_COLUMN1__PATTERN_END;
   }
   else {  //note-column read, not end-mark, get possible additional instrument/FX columns
    if ( ( Temp = between( SWMcolumn1data & SWM_MASK__PATTERN_COLUMN1, SWM_VALUE__PATTERN_COLUMN1__NOTE_MIN, SWM_VALUE__PATTERN_COLUMN1__NOTE_MAX ) )
         || (SWMcolumn1data & SWM_MASK__PATTERN_COLUMN1) == SWM_VALUE__PATTERN_COLUMN1__GATEON ) {
     //if (Temp) SWMcurrentNote = SWMcolumn1data & SWM_MASK__PATTERN_COLUMN1;  //set for octave-dependent slide calculation (which is used in SID-Wizard normal/extra/medium players
     SWMsustainByPatternFX = -1, SWMreleaseByPatternFX = -1;  //new/restarted note should also prioritize on instruent's ADSR for Sustain/Release settings instead of previous Sustain/Release set by PatternFX
      SWMattackByPatternFX = -1, SWMdecayByPatternFX = -1; MoveDownNoteColumnFX = -1;  //should cancel delayed gateon/gateoff if no space for it
    }
    else if ( (SWMcolumn1data & SWM_MASK__PATTERN_COLUMN1) != SWM_VALUE__PATTERN_COLUMN1__NOP
              && !isSWMnoteColumnFX( SWMcolumn1data & SWM_MASK__PATTERN_COLUMN1 ) ) MoveDownNoteColumnFX = -1;  //should cancel delayed gateon/gateoff if no space for it (note-column effects don't count as they'll go to FX column)

    if (SWMcolumn1data & SWM_BITVALUE__PATTERN_COLUMN1__NEXTCOLUMN) {
     SWMcolumn1data &= SWM_MASK__PATTERN_COLUMN1;  //this is really important to avoid maskings of 'nextbyte-flag' bit7 below
     SWMcolumn2data = PATTERNS[ i + SWM_PATTERN_NUMBER_MIN ][++j];

     if (SWMcolumn2data & SWM_BITVALUE__PATTERN_COLUMN2__NEXTCOLUMN) {
      SWMcolumn2data &= SWM_MASK__PATTERN_COLUMN2;  //this is really important to avoid maskings of 'nextbyte-flag' bit7 below
      SWMcolumn3data = PATTERNS[ i + SWM_PATTERN_NUMBER_MIN ][++j];

     //convert FX-column (column2):
      if (SWMcolumn3data < SWM_VALUE__PATTERN_SMALLFX__MIN) {  //convert SWM Big-FX
       SWMcolumn4data = PATTERNS[ i + SWM_PATTERN_NUMBER_MIN ][++j];
       switch (SWMcolumn3data) {
        case SWM_VALUE__PATTERN_BIGFX__PITCHSLIDE_UP: {
         if (SWMcolumn4data == 0x00) { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; break; }  //$00 parameter-value should stop current multirow slide/portamento/vibrato effects
         SNGfx = SNG_VALUE__PATTERN_FX__PITCHSLIDE_UP; SNGpitchFXstart = 1;
         SWMcurrentNote = getNoteSWMindexAtRowForSlide( i, SWMpatternRowIndex );
         SNGparameter = convertSWMslideToSNGspeedTable( SWMcolumn4data, SWMcurrentNote );
        } break;
        case SWM_VALUE__PATTERN_BIGFX__PITCHSLIDE_DOWN: {
         if (SWMcolumn4data == 0x00) { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; break; }  //$00 parameter-value should stop current multirow slide/portamento/vibrato effects
         SNGfx = SNG_VALUE__PATTERN_FX__PITCHSLIDE_DOWN; SNGpitchFXstart = 1;
         SWMcurrentNote = getNoteSWMindexAtRowForSlide( i, SWMpatternRowIndex );
         SNGparameter = convertSWMslideToSNGspeedTable( SWMcolumn4data, SWMcurrentNote );
        } break;
        case SWM_VALUE__PATTERN_BIGFX__PORTAMENTO: {
         if (SWMcolumn4data == 0x00) { //portamento with speed 00 in SW essentially cancels the note on the same row (never arriving to it, so deletable, e.g. in Spider's QuickScore ballad tune
          SWMcolumn1data = SWM_VALUE__PATTERN_COLUMN1__NOP;  //$00 parameter-value should stop current multirow slide/portamento/vibrato effects
          SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; break;
         }
         SNGfx = SNG_VALUE__PATTERN_FX__PORTAMENTO; SNGpitchFXstart = 1;
         if (SWMcolumn4data == SWM_VALUE__PATTERN_BIGFX_PARAMETER__LEGATO)
         { SNGparameter = SNG_VALUE__PATTERN_FX_PARAMETER__LEGATO; }
         else {
          SWMcurrentNote = getNoteSWMindexAtRowForSlide( i, SWMpatternRowIndex );
          SNGparameter = convertSWMportamentoToSNGspeedTable( SWMcolumn4data, SWMcurrentNote );
         }
        } break;
        case SWM_VALUE__PATTERN_BIGFX__WAVECONTROL: {
         SNGfx = SNG_VALUE__PATTERN_FX__WAVECONTROL; SNGparameter = SWMcolumn4data;
        } break;
        case SWM_VALUE__PATTERN_BIGFX__ATTACK_DECAY: {
         SNGfx = SNG_VALUE__PATTERN_FX__ATTACK_DECAY; SNGparameter = SWMcolumn4data;
         SWMattackByPatternFX = SNGparameter >> 4; SWMdecayByPatternFX = SNGparameter & 0xF;
         if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__GATEON) {  //handle the above-mentioned shifting/copying up to accomodate for GT's gateoff happening BEFORE setting ADSR
          if ( SNG_addPatternFXtoPreviousRow( &( SNGpatternData.Patterns[i] ), SNGfx, SNGparameter ) )
          { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; } //this line ensures 'move' instead of 'copy';
          else /*if (SWMpatternRowIndex == 0)*/ MoveDownNoteColumnFX = SNG_VALUE__PATTERN_COLUMN1__GATEON;  //if failed due to 'no previous row available' still try to move the gate-on effect down as a solution
         }
        } break;
        case SWM_VALUE__PATTERN_BIGFX__SUSTAIN_RELEASE: {  //if used with gateoff on pattern-row, in SW the ADSR setting happens first (as expeted), but in GT the gate-off happens first, a workaround is to move/copy the ADSR setting up in the pattern if possible (unoccupied FX-place and we're not on first pattern-row here)
         SNGfx = SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE; SNGparameter = SWMcolumn4data;
         SWMsustainByPatternFX = SNGparameter >> 4; SWMreleaseByPatternFX = SNGparameter & 0xF;
         if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__GATEOFF) {  //handle the above-mentioned shifting/copying up to accomodate for GT's gateoff happening BEFORE setting ADSR
          if ( SNG_addPatternFXtoPreviousRow( &( SNGpatternData.Patterns[i] ), SNGfx, SNGparameter ) )
          { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; } //this line ensures 'move' instead of 'copy';
          else /*if (SWMpatternRowIndex == 0)*/ MoveDownNoteColumnFX = SNG_VALUE__PATTERN_COLUMN1__GATEOFF;  //if failed due to 'no previous row available' still try to move the gate-off effect down as a solution
         }
        } break;
        case SWM_VALUE__PATTERN_BIGFX__CHORD: {  //not directly supported in GT but can be mimiced with WFtable-jumps
#define SNG_PATTERNFX_CORE__CHORD(swm_parameter) \
         SWMinstrumentNumber = getInstrumentSWMindexAtRow( i, SWMpatternRowIndex ); \
         SNGinstrumentNumber = SWMinstrumentNumber - SWM_INSTRUMENT_NUMBER_MIN; \
         if ( !between( swm_parameter, SWM_CHORD_NUMBER_MIN, ChordAmount ) ) break;  /*don't process FX with invalid chord-number*/ \
         SNGfx = SNG_VALUE__PATTERN_FX__WFTABLE_JUMP; \
         if ( SNGchordWFtableIndexes[ SWMinstrumentNumber ][ swm_parameter ] == SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE ) {  /*chord doesn't exist yet, create new chord in WF-table*/ \
          /*SNGchordWFtableIndexes[ SWMinstrumentNumber ][ swm_parameter ] = SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN + SNGinstrumentData.WaveformArpTable.Length;*/ /*done in table-creator routine*/ \
          convertSWMtableToSNGtable( SNGinstrumentNumber, "Waveform-Chord", WFTABLEPOS  /*table-position is fixed in SWM at instrument-index $10*/ \
           , INSTRUMENTS[ SWMinstrumentNumber ][ SWI_PULSETBPT_POS ] \
           , &( InstrumentPointer->WaveformArpTableIndex ), &SNGinstrumentData.WaveformArpTable \
           , swm_parameter, convertWFtableRowToSNG, SNG_WFtablePositions ); \
         } \
         SNGparameter = SNGchordWFtableIndexes[ SWMinstrumentNumber ][ swm_parameter ];
         SNG_PATTERNFX_CORE__CHORD( SWMcolumn4data );
        } break;
        case SWM_VALUE__PATTERN_BIGFX__VIBRATO: {
         SWMvibratoParameters = *(SWMvibratoAmpAndSpeed*) &( SWMcolumn4data );
         if (SWMvibratoParameters.Amplitude == 0x00) { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; break; }  //$00 amplitude in parameter should stop current multirow slide/portamento/vibrato effects
         SNGfx = SNG_VALUE__PATTERN_FX__VIBRATO; SNGpitchFXstart = 1;
         convertSWMvibratoToSNGvibrato( SWMcolumn4data, &SNGvibratoPeriod, &SNGvibratoAmplitude );
         SNGparameter = SNG_addToSpeedTable( SNGvibratoPeriod, SNGvibratoAmplitude );
        } break;
        case SWM_VALUE__PATTERN_BIGFX__WFTABLE_JUMP: {  //jump-target should point current instrument's table-entry
         SWMinstrumentNumber = getInstrumentSWMindexAtRow( i, SWMpatternRowIndex ); //+ SWM_INSTRUMENT_NUMBER_MIN;
         SWMchordNumber = getChordSWMindexAtRow( i, SWMpatternRowIndex );
         InstrumentTableDataSize = INSTRUMENTS[ SWMinstrumentNumber ][ SWI_PULSETBPT_POS ] - WFTABLEPOS;
         if (SWMcolumn4data >= InstrumentTableDataSize - SWM_SIZE__INSTRUMENT_TABLE_ENDMARK) { printf("WFtable-jump FX points beyond Instrument$%.2X table-data. So omitting this FX from the output.\n",SWMinstrumentNumber); break; }  //if jump-target points erroneously outside of the instrument-table data in the SWM (e.g. SW-pattern$29 in oakyardmemo.swm)
         SNGfx = SNG_VALUE__PATTERN_FX__WFTABLE_JUMP;  //target is calculatable by scanning through instrument-changes in the tune, assuming that the same instrument is used each time the pattern is played.
         InstrumentPointer = &( SNGinstrumentData.InstrumentHeaders[ SWMinstrumentNumber - SWM_INSTRUMENT_NUMBER_MIN ] );
         SNGchordWFtableIndex = SNGchordWFtableIndexes[ SWMinstrumentNumber ][ SWMchordNumber ];
         SNGinstrumentTableIndex = ( SWMchordNumber == INSTRUMENTS[ SWMinstrumentNumber ][ SWI_DEFCHORD_POS ]
          || SWMchordNumber == SWM_CHORD_NUMBER_NONE || SNGchordWFtableIndex == SNG_VALUE_INSTRUMENTTABLE_INDEX_NONE )
           ? InstrumentPointer->WaveformArpTableIndex : SNGchordWFtableIndex;
         SNGparameter = SNG_WFtablePositions[ SWMinstrumentNumber ][ /*SNGinstrumentTableIndex +*/ SWMcolumn4data ];
        } break;
        case SWM_VALUE__PATTERN_BIGFX__PWTABLE_JUMP: {  //jump-target should point current instrument's table-entry
         SWMinstrumentNumber = getInstrumentSWMindexAtRow( i, SWMpatternRowIndex ); //+ SWM_INSTRUMENT_NUMBER_MIN;
         InstrumentTableDataSize = INSTRUMENTS[ SWMinstrumentNumber ][ SWI_FILTERTBPT_POS ] - INSTRUMENTS[ SWMinstrumentNumber ][ SWI_PULSETBPT_POS ];
         if (SWMcolumn4data >= InstrumentTableDataSize - SWM_SIZE__INSTRUMENT_TABLE_ENDMARK) { printf("PWtable-jump FX points beyond Instrument$%.2X table-data. So omitting this FX from the output.\n",SWMinstrumentNumber); break; }  //if jump-target points erroneously outside of the instrument-table data in the SWM (e.g. SW-pattern$29 in oakyardmemo.swm)
         SNGfx = SNG_VALUE__PATTERN_FX__PWTABLE_JUMP;
         InstrumentPointer = &( SNGinstrumentData.InstrumentHeaders[ SWMinstrumentNumber - SWM_INSTRUMENT_NUMBER_MIN ] );
         SNGparameter = SNG_PWtablePositions[ SWMinstrumentNumber ][ /*InstrumentPointer->PulseWidthTableIndex +*/ SWMcolumn4data ];
        } break;
        case SWM_VALUE__PATTERN_BIGFX__FILTERTABLE_JUMP: {  //jump-target should point current instrument's table-entry
         SWMinstrumentNumber = getInstrumentSWMindexAtRow( i, SWMpatternRowIndex ); //+ SWM_INSTRUMENT_NUMBER_MIN;
         InstrumentTableDataSize = INSTRUMENTSIZES[ SWMinstrumentNumber ] - INSTRUMENTS[ SWMinstrumentNumber ][ SWI_FILTERTBPT_POS ];
         if (SWMcolumn4data >= InstrumentTableDataSize - SWM_SIZE__INSTRUMENT_TABLE_ENDMARK) { printf("FilterTable-jump FX points beyond Instrument$%.2X table-data. So omitting this FX from the output.\n",SWMinstrumentNumber); break; }  //if jump-target points erroneously outside of the instrument-table data in the SWM (e.g. SW-pattern$29 in oakyardmemo.swm)
         SNGfx = SNG_VALUE__PATTERN_FX__FILTERTABLE_JUMP;
         InstrumentPointer = &( SNGinstrumentData.InstrumentHeaders[ SWMinstrumentNumber - SWM_INSTRUMENT_NUMBER_MIN ] );
         SNGparameter = SNG_FilterTablePositions[ SWMinstrumentNumber ][ /*InstrumentPointer->FilterTableIndex +*/ SWMcolumn4data ];
         /*for (k=0; k < SWMcolumn4data; ++k) {  //count added SNG filter-cutoff setter commands who increased the needed jump-position
          SourceData = SNGinstrumentData.FilterTable.Columns[0][ InstrumentPointer->FilterTableIndex + k ];
          if (SourceData == SNG_VALUE__FILTERTABLE_COMMAND__SET_CUTOFF) ++SNGparameter;
         }*/
        } break;
        //case SWM_VALUE__PATTERN_BIGFX__CHORDSPEED: {  //not supported in GT
        //} break;
        case SWM_VALUE__PATTERN_BIGFX__FILTER_CUTOFF_FREQ: {
         SNGfx = SNG_VALUE__PATTERN_FX__FILTER_CUTOFF_FREQ; SNGparameter = SWMcolumn4data;
        } break;
        case SWM_VALUE__PATTERN_BIGFX__TEMPO: {
         SNGfx = SNG_VALUE__PATTERN_FX__SET_TEMPO;
         SNGparameter = SWMcolumn4data & SNG_MASK__PATTERN_FX__TEMPO;
         if (SNGparameter < SNG_VALUE__PATTERN_FX__TEMPO__MIN) SNGparameter = SNG_VALUE__PATTERN_FX__TEMPO__MIN;
        } break;
        case SWM_VALUE__PATTERN_BIGFX__FUNKTEMPO: {  //no direct per-track funktempo in GT? (or maybe a following tracktempo($0F) with $80..$82)
         SNGfx = SNG_VALUE__PATTERN_FX__FUNKTEMPO;
         SNGparameter = SNG_addToSpeedTable( SWMcolumn4data >> 4, SWMcolumn4data & 0xF );
        } break;
        case SWM_VALUE__PATTERN_BIGFX__TRACK_TEMPO: {
         SNGfx = SNG_VALUE__PATTERN_FX__SET_TEMPO;
         if (SWMcolumn4data < SNG_VALUE__PATTERN_FX__TEMPO__MIN) SWMcolumn4data = SNG_VALUE__PATTERN_FX__TEMPO__MIN;
         SNGparameter = (SWMcolumn4data & SNG_MASK__PATTERN_FX__TEMPO) | SNG_BITVALUE__PATTERN_FX__TRACK_TEMPO;
        } break;
        case SWM_VALUE__PATTERN_BIGFX__TRACK_FUNKTEMPO: {  //no direct per-track funktempo in GT? (or maybe a following tracktempo($0F) with $80..$82), or simply averaging the left-right speeds into a single track-tempo
         SNGfx = SNG_VALUE__PATTERN_FX__SET_TEMPO;
         SNGparameter = ( ( ( SWMcolumn4data>>4) + (SWMcolumn4data&0xF) ) / 2 ) | SNG_BITVALUE__PATTERN_FX__TRACK_TEMPO;
        } break;
        case SWM_VALUE__PATTERN_BIGFX__FILTER_CONTROL: {
         SNGfx = SNG_VALUE__PATTERN_FX__FILTER_CONTROL; SNGparameter = SWMcolumn4data;
         /*if (SNGparameter == SNG_VALUE__PATTERN_FX__FILTER_CONTROL__STOP) {
          SNGparameter == SNG_VALUE__PATTERN_FX__FILTER_CONTROL__MIN;  //avoid stopping filter-execution? (SW doesn't do it?)
         }*/
        } break;
        //default: { SNGfx = SWMcolumn3data; SNGparameter = SWMcolumn4data; } break;
       }
      }

      else {  //convert SWM FX-column Small-FX
       switch (SWMcolumn3data & SWM_MASK__SMALLFX_BASE) {
        case SWM_VALUE__PATTERN_SMALLFX_BASE__ATTACK: {  //if used with gateon on pattern-row, in SW the ADSR setting happens first (as expeted), but in GT the gate-on happens first, a workaround is to move/copy the ADSR setting up in the pattern if possible (unoccupied FX-place and we're not on first pattern-row here)
         SNGfx = SNG_VALUE__PATTERN_FX__ATTACK_DECAY;
         SWMattackByPatternFX = (SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER);
         if (SWMdecayByPatternFX < 0x00) {
          SNGinstrumentNumber = getInstrumentSNGindexAtRow( i, SWMpatternRowIndex );
          SWMdecayByPatternFX = SNGinstrumentData.InstrumentHeaders[ SNGinstrumentNumber ].AttackDecay & 0x0F;
         }
         SNGparameter = (SWMattackByPatternFX << 4) | (SWMdecayByPatternFX & 0xF);
         if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__GATEON) {  /*handle the above-mentioned shifting/copying up to accomodate for GT's gateon happening BEFORE setting ADSR*/
          if ( SNG_addPatternFXtoPreviousRow( &( SNGpatternData.Patterns[i] ), SNGfx, SNGparameter ) )
          { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; } /*this line ensures 'move' instead of 'copy'*/
          else /*if (SWMpatternRowIndex == 0)*/ MoveDownNoteColumnFX = SNG_VALUE__PATTERN_COLUMN1__GATEON;  //if failed due to 'no previous row available' still try to move the gate-on effect down as a solution
         }
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__DECAY: {
         SNGfx = SNG_VALUE__PATTERN_FX__ATTACK_DECAY;
         SWMdecayByPatternFX = (SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER);
         if (SWMattackByPatternFX < 0x00) {
          SNGinstrumentNumber = getInstrumentSNGindexAtRow( i, SWMpatternRowIndex );
          SWMattackByPatternFX = SNGinstrumentData.InstrumentHeaders[ SNGinstrumentNumber ].AttackDecay >> 4;
         }
         SNGparameter = (SWMattackByPatternFX << 4) | (SWMdecayByPatternFX & 0xF);
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__WAVEFORM: {  //no support in GT (Gate/Sync/Ring/Test not known here)
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__SUSTAIN: {
         SNGfx = SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE;
         SWMsustainByPatternFX = (SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER);
#define SNG_PATTERN_SMALLFX_CORE__SUSTAIN() \
         if (SWMreleaseByPatternFX < 0x00) { \
          SNGinstrumentNumber = getInstrumentSNGindexAtRow( i,SWMpatternRowIndex ); \
          SWMreleaseByPatternFX = SNGinstrumentData.InstrumentHeaders[ SNGinstrumentNumber ].SustainRelease & 0x0F; \
         } \
         SNGparameter = (SWMsustainByPatternFX << 4) | (SWMreleaseByPatternFX & 0xF);
         SNG_PATTERN_SMALLFX_CORE__SUSTAIN();
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__RELEASE: {  //if used with gateoff on pattern-row, in SW the ADSR setting happens first (as expeted), but in GT the gate-off happens first, a workaround is to move/copy the ADSR setting up in the pattern if possible (unoccupied FX-place and we're not on first pattern-row here)
         //SNGfx = SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE;
         SWMreleaseByPatternFX = (SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER);
#define SNG_PATTERN_SMALLFX_CORE__RELEASE(fxset_condition) \
         if (SWMsustainByPatternFX < 0x00) { \
          SNGinstrumentNumber = getInstrumentSNGindexAtRow( i, SWMpatternRowIndex ); \
          SWMsustainByPatternFX = SNGinstrumentData.InstrumentHeaders[ SNGinstrumentNumber ].SustainRelease >> 4; \
         } \
         if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__GATEOFF) {  /*handle the above-mentioned shifting/copying up to accomodate for GT's gateoff happening BEFORE setting ADSR*/ \
          if ( SNG_addPatternFXtoPreviousRow( &( SNGpatternData.Patterns[i] ), SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE, (SWMsustainByPatternFX << 4) | (SWMreleaseByPatternFX & 0xF) ) ) ; /*{ if (fxset_condition) { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; } }*/  /*this line ensures 'move' instead of 'copy', but SNGfx is not set to this FX yet here, and if a slide and a release is together with a gateoff in SW, the slide shouldn't be deleted*/ \
          else /*if (SWMpatternRowIndex == 0)*/ { /*couldn't move the FX upwards, so telling to move the gateoff downwards and set the FX in its own place as normally*/ \
           MoveDownNoteColumnFX = SNG_VALUE__PATTERN_COLUMN1__GATEOFF; /*if failed due to 'no previous row available' still try to move the gate-off effect down as a solution*/ \
           if ( fxset_condition /*canAddNewFX( SNGfx, SNGparameter )*/ ) {  /*may still overwrite pitch-slide/vibrato FX?*/ \
            SNGfx = SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE; \
            SNGparameter = (SWMsustainByPatternFX << 4) | (SWMreleaseByPatternFX & 0xF); \
         }}} \
         else { /* normally (when not gateoff in note-column) we simply write it out */ \
          if ( fxset_condition /*canAddNewFX( SNGfx, SNGparameter )*/ ) {  /*may still overwrite pitch-slide/vibrato FX?*/ \
           SNGfx = SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE; \
           SNGparameter = (SWMsustainByPatternFX << 4) | (SWMreleaseByPatternFX & 0xF); \
         }}
         SNG_PATTERN_SMALLFX_CORE__RELEASE( 1 );  //condition '1' here, because FX-column effect always sets FX and overrides previous running FX
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__CHORD: {  //not directly supported in GT but can be mimiced with WFtable-jumps
         SNG_PATTERNFX_CORE__CHORD( SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER );
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__VIBRATO_AMPLITUDE: {
         if (SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER == 0x0) { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; break; }  //$00 amplitude in parameter should stop current multirow slide/portamento/vibrato effects
         SNGfx = SNG_VALUE__PATTERN_FX__VIBRATO; SNGpitchFXstart = 1;
         SNGinstrumentNumber = getInstrumentSNGindexAtRow( i, SWMpatternRowIndex );
         SourceIndex = SNGinstrumentData.InstrumentHeaders[ SNGinstrumentNumber ]
           .Vibrato_SpeedTableIndex - SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN;
          SNGvibratoPeriod = SNGinstrumentData.SpeedTable.Columns[0][ SourceIndex ];
         convertSWMvibratoToSNGvibrato( (SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER) << 4, &DummyByte, &SNGvibratoAmplitude );
         SNGparameter = SNG_addToSpeedTable( SNGvibratoPeriod, SNGvibratoAmplitude );
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__VIBRATO_PERIOD: {
         SNGfx = SNG_VALUE__PATTERN_FX__VIBRATO; SNGpitchFXstart = 1;
         SNGinstrumentNumber = getInstrumentSNGindexAtRow( i, SWMpatternRowIndex );
         SourceIndex = SNGinstrumentData.InstrumentHeaders[ SNGinstrumentNumber ]
           .Vibrato_SpeedTableIndex - SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN;
          SNGvibratoAmplitude = SNGinstrumentData.SpeedTable.Columns[1][ SourceIndex ];
         convertSWMvibratoToSNGvibrato( SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER, &SNGvibratoPeriod, &DummyByte );
         SNGparameter = SNG_addToSpeedTable( SNGvibratoPeriod, SNGvibratoAmplitude );
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__MAIN_VOLUME: {
         SNGfx = SNG_VALUE__PATTERN_FX__MAINVOLUME_OR_TIMINGMARK;
         SNGparameter = SWMcolumn3data & SWM_MASK__SMALLFX_PARAMETER;
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__FILTERBAND: {  //current resonance is not known here but assuming maximm ($F) (or get from filtertable?)
        } break;
        //case SWM_VALUE__PATTERN_SMALLFX_BASE__CHORDSPEED: {  //not supported in GT
        //} break;
        //case SWM_VALUE__PATTERN_SMALLFX_BASE__DETUNE: {  //not supported in GT
        //} break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__WAVECONTROL: {  //no support in GT (running WaveForm not known here)
        } break;
        case SWM_VALUE__PATTERN_SMALLFX_BASE__RESONANCE: {  //current filterband is not known here but assuming channel1 ($1) (or get from filtertable?)
        } break;
        //default: { SNGfx = SWMcolumn3data; SNGparameter = SWMcolumn4data; } break;
    }}}}
    SWMcolumn1data &= SWM_MASK__PATTERN_COLUMN1; SWMcolumn2data &= SWM_MASK__PATTERN_COLUMN2;  //this is really important to avoid maskings of 'nextbyte-flag' bit7 below

    //convert Instrument-column (column2):
    if (SWMcolumn2data == SWM_VALUE__PATTERN_COLUMN2__NOP) SNGinstrument = SNG_VALUE__PATTERN_COLUMN2__NOP;  //actually SW and GT values are the same
    else if ( between( SWMcolumn2data, SWM_VALUE__PATTERN_COLUMN2__INSTRUMENT_MIN, SWM_VALUE__PATTERN_COLUMN2__INSTRUMENT_MAX ) ) {
     SNGinstrument = SWMcolumn2data;
    }
    else if (SWMcolumn2data == SWM_VALUE__PATTERN_COLUMN2__LEGATO) {  //maybe legato can be made by a dedicated 'legato' instrument without hard-restart? that would save space for another patternFX on the same line. But proper legato instrument isn't possible in GT2.76 it seems, hrtimer at $81/$41, fram1waveform at $FF, ADSR at FFFF and wavetable with a single $00 $00 entry followed by FF00 gets the closest, but still modifies ADSR.
     if ( canAddNewFX( SNGfx, SNGparameter ) ) {  //can overwrite pitch-slide/vibrato FX
      SNGfx = SNG_VALUE__PATTERN_FX__PORTAMENTO; SNGpitchFXstart = 1;
      SNGparameter = SNG_VALUE__PATTERN_FX_PARAMETER__LEGATO;
     }
     //else ;  //FX-column already occupied, but can still create this FX in WFtable and a new legato instrument calling it
    }
    else {  //small effects in SWM instrument-column that can be added as SNG FX if it's not yet occupied in pattern
     switch (SWMcolumn2data & SWM_MASK__INSTRUMENT_SMALLFX_BASE) {
      case SWM_VALUE__PATTERN_SMALLFX_BASE__WAVEFORM: {  //no support in GT (Gate/Sync/Ring/Test not known here)
      } break;
      case SWM_VALUE__PATTERN_SMALLFX_BASE__SUSTAIN: {
       if ( canAddNewFX( SNGfx, SNGparameter ) ) {  //may still overwrite pitch-slide/vibrato FX?
        SNGfx = SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE;
        SWMsustainByPatternFX = (SWMcolumn2data & SWM_MASK__SMALLFX_PARAMETER);
        SNG_PATTERN_SMALLFX_CORE__SUSTAIN();
       }
       //else ;  //FX-column already occupied, but can still create this FX in WFtable and a new legato instrument calling it
      } break;
      case SWM_VALUE__PATTERN_SMALLFX_BASE__RELEASE: {  //if used with gateoff on pattern-row, in SW the ADSR setting happens first (as expeted), but in GT the gate-off happens first, a workaround is to move/copy the ADSR setting up in the pattern if possible (unoccupied FX-place and we're not on first pattern-row here)
       //if ( canAddNewFX( SNGfx, SNGparameter ) ) {  //may still overwrite running pitch-slide/vibrato FX?
        //SNGfx = SNG_VALUE__PATTERN_FX__SUSTAIN_RELEASE;
        SWMreleaseByPatternFX = (SWMcolumn2data & SWM_MASK__SMALLFX_PARAMETER);
        SNG_PATTERN_SMALLFX_CORE__RELEASE( canAddNewFX( SNGfx, SNGparameter ) );
       //}
       //else ;  //FX-column already occupied, but can still create this FX in WFtable and a new legato instrument calling it
      } break;
      case SWM_VALUE__PATTERN_SMALLFX_BASE__CHORD: {  //not directly supported in GT but can be mimiced with WFtable-jumps
       //if ( canAddNewFX( SNGfx, SNGparameter ) ) {  //chord is considered more important than other effects here
        SNG_PATTERNFX_CORE__CHORD( SWMcolumn2data & SWM_MASK__SMALLFX_PARAMETER );  //but might be called beside other FX if the chord was turned to a whole instrument and called, instead of a WFtable-jump that occupies FX-space
       //}                                                  //(though that would change the current instrument which would cause even more differences compared to the original tune)
      } break;
      //default: //SNGinstrument = SWMcolumn2data; break;
     }
    }

    //convert Note-column (column1):
    if (NoteColumnPortamentoON) {  //note-column portamento precedes note, so delayed here by 1 row
     NoteColumnPortamentoON = 0;
     if ( canAddNewFX( SNGfx, SNGparameter ) ) {  //may still overwrite running pitch-slide/vibrato FX?
      SNGfx = SNG_VALUE__PATTERN_FX__PORTAMENTO; SNGpitchFXstart = 1;
      SWMcurrentNote = getNoteSWMindexAtRowForSlide( i, SWMpatternRowIndex );
      SNGparameter = convertSWMportamentoToSNGspeedTable( SNG_VALUE__PATTERN_FX_PARAMETER__NOTECOLUMN_PORTAMENTO, SWMcurrentNote );
     }
     //else ;  //FX-column already occupied, but can still create this FX in WFtable and a new legato instrument calling it
    }

    if ( SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__NOP || isSWMnoteColumnFX( SWMcolumn1data ) ) {  //all note-column FX-es that will go into SNG FX-column should be in this branch, as they don't ruin the move-gateoff-down workaround for GT player's gateoff+release order problem
     if (MoveDownNoteColumnFX < 0) SNGnote = SNG_VALUE__PATTERN_COLUMN1__NOP;
     else {  //if the solution was to move gateon/gateoff down, this does it
      SNGnote = MoveDownNoteColumnFX; MoveDownNoteColumnFX = -1;  //do the 'copy' of 'delayed'/'moved' gateon/gateoff to current place
      SNGpatternData.Patterns[i].Rows[ SNGpatternData.Patterns[i].Length - 1 ].Note = SNG_VALUE__PATTERN_COLUMN1__NOP;  //deleting the previos place ensures 'move' of gateon/gateoff
     }
     if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__PORTAMENTO) {
      NoteColumnPortamentoON = 1;  //note-column portamento precedes note, so delayed for next row
     }
     else if ( between( SWMcolumn1data, SWM_VALUE__PATTERN_COLUMN1__VIBRATO_AMPLITUDE_MIN, SWM_VALUE__PATTERN_COLUMN1__VIBRATO_AMPLITUDE_MAX ) ) {
      if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__VIBRATO_AMPLITUDE_MIN)
      { SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00; }  //$00 amplitude in parameter should stop current multirow slide/portamento/vibrato effects
      else {  //start/modify vibrato amplitude
       if ( canAddNewFX( SNGfx, SNGparameter ) ) {  //may still overwrite running pitch-slide/vibrato FX?
        SNGfx = SNG_VALUE__PATTERN_FX__VIBRATO; SNGpitchFXstart = 1;
        SNGinstrumentNumber = getInstrumentSNGindexAtRow( i, SWMpatternRowIndex );
        SourceIndex = SNGinstrumentData.InstrumentHeaders[ SNGinstrumentNumber ]
          .Vibrato_SpeedTableIndex - SNG_VALUE_INSTRUMENTTABLE_INDEX_MIN;
         SNGvibratoPeriod = SNGinstrumentData.SpeedTable.Columns[0][ SourceIndex ];
        convertSWMvibratoToSNGvibrato( (SWMcolumn1data
         - SWM_VALUE__PATTERN_COLUMN1__VIBRATO_AMPLITUDE_MIN) << 4, &DummyByte, &SNGvibratoAmplitude );
        SNGparameter = SNG_addToSpeedTable( SNGvibratoPeriod, SNGvibratoAmplitude );
       }
       //else ;  //FX-column already occupied, but can still create this FX in WFtable and a new legato instrument calling it
      }
     }
     else if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__SYNCON) ;  //no support in GT (WaveForm not known here)
     else if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__SYNCOFF) ;  //no support in GT (WaveForm not known here)
     else if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__RINGON) ;  //no support in GT (WaveForm not known here)
     else if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__RINGOFF) ;  //no support in GT (WaveForm not known here)
    }
    else if ( between( SWMcolumn1data, SWM_VALUE__PATTERN_COLUMN1__NOTE_MIN, SWM_VALUE__PATTERN_COLUMN1__NOTE_MAX ) ) {
     MoveDownNoteColumnFX = -1;  //to be on the safe side, resetting/forgetting delayed FX when a new note is converted (also done above at pattern-column reader for notes and other non-NOPs that are not note-FXes)
     SNGnote = SWMcolumn1data + (SNG_VALUE__PATTERN_COLUMN1__NOTE_MIN - SWM_VALUE__PATTERN_COLUMN1__NOTE_MIN);
     SWMinstrumentNumber = getInstrumentSWMindexAtRow( i, SWMpatternRowIndex ); //+ SWM_INSTRUMENT_NUMBER_MIN;
     SNGnote += ( (signed char) INSTRUMENTS[ SWMinstrumentNumber ][ SWI_OCTAVE_POS ] ); //* SEMITONES_PER_OCTAVE;  //might work well most of the time if the same pattern-notes are always played by the same insttruments (embedding octave in arp would be very limited)
     if (SNGnote < SNG_VALUE__PATTERN_COLUMN1__NOTE_MIN) SNGnote = SNG_VALUE__PATTERN_COLUMN1__NOTE_MIN;
     if (SNGnote > SNG_VALUE__PATTERN_COLUMN1__NOTE_MAX) SNGnote = SNG_VALUE__PATTERN_COLUMN1__NOTE_MAX;  //GT might not have tha highest notes of SW
     if (!SNGpitchFXstart && isSNGpitchFX( SNGfx ) ) {  //new note should stop current multirow slide/portamento/vibrato effects
      SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00;
    }}
    else if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__GATEON) {
     SNGnote = SNG_VALUE__PATTERN_COLUMN1__GATEON;
     if (!SNGpitchFXstart && isSNGpitchFX( SNGfx ) ) {  //restarted note too should stop current multirow slide/portamento/vibrato effects
      SNGfx = SNG_VALUE__PATTERN_FX__NOP; SNGparameter = 0x00;
    }}
    else if (SWMcolumn1data == SWM_VALUE__PATTERN_COLUMN1__GATEOFF) {
     SNGnote = SNG_VALUE__PATTERN_COLUMN1__GATEOFF;
     if ( canAddNewFX( SNGfx, SNGparameter ) ) {  //if there's gateoff-pointer set for SW-instrument, we'll try to add a tablejump-FX (if FX-column is still empty on this row)
      SWMinstrumentNumber = getInstrumentSWMindexAtRow( i, SWMpatternRowIndex );
      InstrumentPointer = &( SNGinstrumentData.InstrumentHeaders[ SWMinstrumentNumber - SWM_INSTRUMENT_NUMBER_MIN ] );
      SWMinstrumentGateOffWFtableIndex = INSTRUMENTS[ SWMinstrumentNumber ][ SWI_GATEOFFINDEX__WFTABLE ];
      SWMinstrumentGateOffPWtableIndex = INSTRUMENTS[ SWMinstrumentNumber ][ SWI_GATEOFFINDEX__PWTABLE ];
      SWMinstrumentGateOffFilterTableIndex = INSTRUMENTS[ SWMinstrumentNumber ][ SWI_GATEOFFINDEX__FILTERTABLE ];
      if (SWMinstrumentGateOffPWtableIndex > 0) {  //probably PW-table is the most-used, priorizing it if gateoffzindex is set for other tables too
       SNGfx = SNG_VALUE__PATTERN_FX__PWTABLE_JUMP;
       SWMinstrumentTableIndex = INSTRUMENTS[ SWMinstrumentNumber ][ SWI_PULSETBPT_POS ];
       SNGparameter = SNG_PWtablePositions[ SWMinstrumentNumber ][ /*InstrumentPointer->PulseWidthTableIndex
        +*/ (SWMinstrumentGateOffPWtableIndex - SWMinstrumentTableIndex) / SWM_INSTRUMENT_TABLE_ROWSIZE ];
      }
      else if (SWMinstrumentGateOffFilterTableIndex > 0) {
       SNGfx = SNG_VALUE__PATTERN_FX__FILTERTABLE_JUMP;
       SWMinstrumentTableIndex = INSTRUMENTS[ SWMinstrumentNumber ][ SWI_FILTERTBPT_POS ];
       Temp = (SWMinstrumentGateOffFilterTableIndex - SWMinstrumentTableIndex) / SWM_INSTRUMENT_TABLE_ROWSIZE;
       SNGparameter = SNG_FilterTablePositions[ SWMinstrumentNumber ][ /*InstrumentPointer->FilterTableIndex +*/ Temp ];
       /*for (k=0; k < Temp; ++k) {  //count added SNG filter-cutoff setter commands who increased the needed jump-position
        SourceData = SNGinstrumentData.FilterTable.Columns[0][ InstrumentPointer->FilterTableIndex + k ];
        if (SourceData == SNG_VALUE__FILTERTABLE_COMMAND__SET_CUTOFF) ++SNGparameter;
       }*/
      }
      else if (SWMinstrumentGateOffWFtableIndex > 0) {
       SNGfx = SNG_VALUE__PATTERN_FX__WFTABLE_JUMP;
       SNGparameter = SNG_WFtablePositions[ SWMinstrumentNumber ][ /*InstrumentPointer->WaveformArpTableIndex
                      +*/ (SWMinstrumentGateOffWFtableIndex - WFTABLEPOS) / SWM_INSTRUMENT_TABLE_ROWSIZE ];
      }
     }
    }  //there can be other effects in SWM note-column that can be added as SNG FX if it's not yet occupied in pattern (or as instrument-table FX if instrument is not occupied, or jump from that FX to instrument in WFtable)
    //else SNGnote = SWMcolumn1data;

   }  //end of non-patternend if-section  //printf(" $%.2X($%.2X): $%.2X  $%.2X  $%.2X $%.2X  ->  $%.2X: $%.2X  $%.2X  $%.2X $%.2X\n", SWMpatternRow, SWMpatternRowIndex, SWMcolumn1data, SWMcolumn2data, SWMcolumn3data, SWMcolumn4data,  SNGpatternRowIndex, SNGnote, SNGinstrument, SNGfx, SNGparameter );

   SNG_addToPattern( &( SNGpatternData.Patterns[i] ), SNGnote, SNGinstrument, SNGfx, SNGparameter );
   ++SWMpatternRow;
  }  //SNG_addToPattern( &( SNGpatternData.Patterns[i] ), SNG_VALUE__PATTERN_COLUMN1__PATTERN_END, SNG_VALUE__PATTERN_COLUMN2__NOP,  SNG_VALUE__PATTERN_FX__NOP, 0x00 );
  if ( SNGpatternData.Patterns[i].Length >= SNG_PATTERN_LENGTH_MAX ) printf("SNG pattern-$%.2X length maximum %d exceeded. Converted anyway, length:%d.\n", SWM_PATTERN_NUMBER_MIN+i, SNG_PATTERN_LENGTH_MAX, SNGpatternData.Patterns[i].Length );
 }


 //Convert subtune-orderlists ( 'SubtuneAmount' times channel 1,2,3 sequences )  //was at the beginning before, but to insert tempo/volume orderlistFX-es into patterns as a workarond, the patterns should already exist here
 SNGsubtuneOrderLists = (SNGsubtuneOrderList*) malloc( sizeof(SNGsubtuneOrderList) * SubtuneAmount );
 for (i=0; i < SubtuneAmount; ++i) {
  for (j=0; j < SID_CHANNELCOUNT; ++j) {  //convert pattern-numbers, transpose/volume/etc. effects
   SNGsubtuneOrderLists[i].Sequences[j].Size = SWM_SEQ_SIZE[ i * SID_CHANNELCOUNT + j ];  //might get shortened by jump/loop or lengthened by prepended tempo-setter patterns
   SNGsubtuneOrderLists[i].Sequences[j].RestartPosition = 0x00;  //default, might get overwritten by jump/loop-target
   SWMseqVolumeSetting = SWMseqTempoSetting = -1;  //orderlist-sequence FX will be injected into SNG pattern if possible

   for (k = TargetIndex = 0; k < SNG_ORDERLIST_SEQUENCE_SIZE_MAX; ++k) {  //'k' is source-index here
    SourcePointer = &( SEQUENCES[ i * SID_CHANNELCOUNT + j ][ k ] ); SourceData = *SourcePointer;
    TargetPointer = &( SNGsubtuneOrderLists[i].Sequences[j].Data[ TargetIndex ] );

    if (k == 0) {  //add extra tempo-setting pattern at the beginning of sequence (Add only if needed? (if tempo not set in 1st row of 1st pattern or if not default tempo value 6))
     if (j == 0) {  //(will also be used as last pattern to implement '$FE' SID-Wizard 'end' feature)
      SNGpatternData.Patterns[ SNGpatternData.PatternCount ].Length = 0; //PATTERNLENGTHS[ ++PatternAmount ] = 1;  PATTERNSIZES[ PatternAmount ] = sizeof(SWMtempoPatternTemplate);
      if (TUNETEMPO[i][0] & SWM_BITVALUE__TEMPOTABLE_END) {  //normal single tempo
       SNGfx = SNG_VALUE__PATTERN_FX__SET_TEMPO; //PATTERNS[ PatternAmount ][2] = SWM_VALUE__PATTERN_BIGFX__TEMPO;
       SNGparameter = (TUNETEMPO[i][0] & SWM_BITVALUE__TEMPOTABLE_MASK) & SNG_MASK__PATTERN_FX__TEMPO;  //{ PATTERNS[ PatternAmount ][3] = TUNETEMPO[i][0] & SWM_BITVALUE__TEMPOTABLE_MASK; }
       if (SNGparameter < SNG_VALUE__PATTERN_FX__TEMPO__MIN) SNGparameter = SNG_VALUE__PATTERN_FX__TEMPO__MIN;
      }
      else {  //if no bit7, it's a funktempo, treated a bit differently (will get a speedtable-entry during pattern-conversion)
       SNGfx = SNG_VALUE__PATTERN_FX__FUNKTEMPO; //PATTERNS[ PatternAmount ][2] = SWM_VALUE__PATTERN_BIGFX__FUNKTEMPO;
       SNGparameter = SNG_addToSpeedTable(  //PATTERNS[ PatternAmount ][3] = ( ( TUNETEMPO[i][0] & 0xF ) << 4 ) | ( TUNETEMPO[i][1] & 0xF );
        (TUNETEMPO[i][0] & SWM_BITVALUE__TEMPOTABLE_MASK) & SNG_MASK__PATTERN_FX__TEMPO,
        (TUNETEMPO[i][1] & SWM_BITVALUE__TEMPOTABLE_MASK) & SNG_MASK__PATTERN_FX__TEMPO );
      }
      SNG_addToPattern( &( SNGpatternData.Patterns[ SNGpatternData.PatternCount ] )  //for (l = 0; l < sizeof(SWMtempoPatternTemplate); ++l) PATTERNS[ PatternAmount ][l] = SWMtempoPatternTemplate[l];
       , SNG_VALUE__PATTERN_COLUMN1__NOP, SNG_VALUE__PATTERN_COLUMN2__NOP, SNGfx, SNGparameter );
      SNG_addToPattern( &( SNGpatternData.Patterns[ SNGpatternData.PatternCount++] )
       , SNG_VALUE__PATTERN_COLUMN1__PATTERN_END, SNG_VALUE__PATTERN_COLUMN2__NOP,  SNG_VALUE__PATTERN_FX__NOP, 0x00 );
     }
     *(TargetPointer++) = SNGpatternData.PatternCount/*PatternAmount*/ - 1; ++TargetIndex;  ++SNGsubtuneOrderLists[i].Sequences[j].RestartPosition;
    }
    TargetOrderlistPositions[k] = TargetIndex;  //Registers if removal were made to aid corrected restartposition-calculation below.

    if (SWM_PATTERN_NUMBER_MIN <= SourceData && SourceData <= SWM_PATTERN_NUMBER_MAX ) {  //Pattern-number
     *TargetPointer = SourceData - SWM_PATTERN_NUMBER_MIN;  //Pattern-number numbering is 1-based in SW, 0-based in GT
     if (SWMseqVolumeSetting >= 0) {
      if ( SNGpatternData.Patterns[ *TargetPointer ].Rows[0].FX == SNG_VALUE__PATTERN_FX__NOP ) {  //1st FX-row is empty, fine
       SNGpatternData.Patterns[ *TargetPointer ].Rows[0].FX = SNG_VALUE__PATTERN_FX__MAINVOLUME_OR_TIMINGMARK;
       SNGpatternData.Patterns[ *TargetPointer ].Rows[0].FXparameter = SWMseqVolumeSetting;
      }
      SWMseqVolumeSetting = -1;
     }
     if (SWMseqTempoSetting >= 0) {  //might need to check if pattern is used more times in the sequence (because it mighth be preceded by other tempo-setters setting a different tempo than this one, and in that case this tempo should be baked into a new unique copy of this pattern)
      if ( SNGpatternData.Patterns[ *TargetPointer ].Rows[0].FX == SNG_VALUE__PATTERN_FX__NOP ) {  //1st FX-row is empty, fine
       SNGpatternData.Patterns[ *TargetPointer ].Rows[0].FX = SNG_VALUE__PATTERN_FX__SET_TEMPO;
       SNGpatternData.Patterns[ *TargetPointer ].Rows[0].FXparameter = SWMseqTempoSetting | SNG_BITVALUE__PATTERN_FX__TRACK_TEMPO;
      }
      SWMseqTempoSetting = -1;
     }
     ++TargetIndex;
    }
    else if (SWM_VALUE__SEQUENCE_TRANSPOSE_MIN <= SourceData &&  SourceData <= SWM_VALUE__SEQUENCE_TRANSPOSE_MAX ) {
     *TargetPointer = SNG_VALUE__SEQUENCE_TRANSPOSE_MID + (SourceData - SWM_VALUE__SEQUENCE_TRANSPOSE_MID); ++TargetIndex;
     if (*TargetPointer >= SNG_VALUE__SEQUENCE_TRANSPOSE_MAX) {  printf( "GoatTracker transpose maximum exceeded -> lowered by an octave.\n" );
      *TargetPointer -= SEMITONES_PER_OCTAVE;  //$9F would result in $FF 'end'-mark in GT, transpose it down by 1 octave to $93 as a bearable fallback
     }
    }
    else if (SWM_VALUE__SEQUENCE_VOLUME_MIN <= SourceData && SourceData <= SWM_VALUE__SEQUENCE_VOLUME_MAX) {  //not directly supported by GT, maybe inject into a pattern (duplicated if needed{
     SWMseqVolumeSetting = (SourceData - SWM_VALUE__SEQUENCE_VOLUME_MIN);  //continue;
    }
    else if (SWM_VALUE__SEQUENCE_TEMPO_MIN <= SourceData && SourceData <= SWM_VALUE__SEQUENCE_TEMPO_MAX) {  //not directly supported by GT, maybe inject into a pattern (duplicated if needed{
     SWMseqTempoSetting = (SourceData - SWM_VALUE__SEQUENCE_TEMPO_MIN);  //continue;
    }
    else if (SWM_VALUE__SEQUENCE_NOP_MIN <= SourceData && SourceData <= SWM_VALUE__SEQUENCE_NOP_MAX) {  //eye-candy numbered NOPs not supported by GT, skipping them
     continue;
    }
    else if (SourceData == SWM_VALUE__SEQUENCE_END) {
     SNGsubtuneOrderLists[i].Sequences[j].RestartPosition = TargetIndex;  //emulate 'end' by jump to added patterns (self-jump would be invalid, GT complains at saving)
     *TargetPointer = SNGpatternData.PatternCount/*PatternAmount*/ - 1; ++TargetIndex;  //extra 'end' pattern to loop to
     *(TargetPointer+1) = SNG_VALUE__SEQUENCE_LOOP; ++TargetIndex;
     SNGsubtuneOrderLists[i].Sequences[j].Size = TargetIndex; //accounting for end-mark and appeended stopper patterns
     break;
    }
    else if (SourceData == SWM_VALUE__SEQUENCE_LOOP) {
     *TargetPointer = SNG_VALUE__SEQUENCE_LOOP; ++TargetIndex;  SourceData = *(SourcePointer+1);
     if (SourceData < SWM_VALUE__SEQUENCE_SUBTUNEJUMP_MIN /*|| SWM_VALUE__SEQUENCE_SUBTUNEJUMP_MAX < SourceData*/) {  //not subtune-jump: loop to position (if subtune-jump, otherwise simple loop to $00 stays (GoatTracker doesn't have subtune-jump)
      //if NOPs and OrderlistFX-es were removed, RestartPosition should be corrected!: done by TargetOrderlistPositions
      SNGsubtuneOrderLists[i].Sequences[j].RestartPosition = TargetOrderlistPositions[ SourceData ];
     }
     SNGsubtuneOrderLists[i].Sequences[j].Size = TargetIndex;
     break;
    }
    else *TargetPointer = SourceData;  //unlikely fallback if not found/categorized effect

   }

  }
 }


 //Write out resulting SNG-musicdata to the output-file:
 writeArrayToFile( (char*)&SNGheader, sizeof(SNGheader) );  //write music-header
 for (i=0; i < SubtuneAmount; ++i) { //write out the converted orderlists
  for (j=0; j < SID_CHANNELCOUNT; ++j) {
   byteToFile( SNGsubtuneOrderLists[i].Sequences[j].Size );
   writeArrayToFile( SNGsubtuneOrderLists[i].Sequences[j].Data, SNGsubtuneOrderLists[i].Sequences[j].Size );
   byteToFile( SNGsubtuneOrderLists[i].Sequences[j].RestartPosition );
 }}
 byteToFile( SNGinstrumentData.InstrumentCount );  //write out the converted instruments
  writeArrayToFile( SNGinstrumentData.InstrumentHeaders, SNGinstrumentData.InstrumentCount * sizeof(SNGinstrumentHeader) );
  byteToFile( SNGinstrumentData.WaveformArpTable.Length );
   writeArrayToFile( SNGinstrumentData.WaveformArpTable.Columns[0], SNGinstrumentData.WaveformArpTable.Length );
   writeArrayToFile( SNGinstrumentData.WaveformArpTable.Columns[1], SNGinstrumentData.WaveformArpTable.Length );
  byteToFile( SNGinstrumentData.PulseWidthTable.Length );
   writeArrayToFile( SNGinstrumentData.PulseWidthTable.Columns[0], SNGinstrumentData.PulseWidthTable.Length );
   writeArrayToFile( SNGinstrumentData.PulseWidthTable.Columns[1], SNGinstrumentData.PulseWidthTable.Length );
  byteToFile( SNGinstrumentData.FilterTable.Length );
   writeArrayToFile( SNGinstrumentData.FilterTable.Columns[0], SNGinstrumentData.FilterTable.Length );
   writeArrayToFile( SNGinstrumentData.FilterTable.Columns[1], SNGinstrumentData.FilterTable.Length );
  byteToFile( SNGinstrumentData.SpeedTable.Length );
   writeArrayToFile( SNGinstrumentData.SpeedTable.Columns[0], SNGinstrumentData.SpeedTable.Length );
   writeArrayToFile( SNGinstrumentData.SpeedTable.Columns[1], SNGinstrumentData.SpeedTable.Length );
 byteToFile( SNGpatternData.PatternCount );  //write out the converted patterns
 for (i=0; i < SNGpatternData.PatternCount; ++i) {
  byteToFile( SNGpatternData.Patterns[i].Length );
  writeArrayToFile( SNGpatternData.Patterns[i].Rows, SNGpatternData.Patterns[i].Length * sizeof(SNGpatternRow) );
 }

 printf("\n\n**** Conversion is complete. **** \nSNG file '%s' generated... :)"
 "\n-----------------------------------------------------\n",OutputFileName);
 fclose(OutputFile);
}


//==========================================================================================
void GenerateMIDI() //----- Export MIDI data from the imported SWM data ------------------
{
 unsigned int i,j,k,l,SWMpattPointer,MIDItrackPointer,DeltaCount,PPQN=0x60,PALpulses=PPQN/(4*6),RowDelta;
 unsigned char PrevChannel,PrevNote,PrevIns,PrevChord,WasGateOff,ActNote,ActInsFx,ActIns,ActFx,ActFxVal,ActChord;
 unsigned char TrackTemp[65536]; //pattern is collected here before writing to file, because size must be detected beforehands
 signed int InsOctave=0;
 signed char Transpose[SID_CHANNELCOUNT];

 printf("to MIDI format 1 (contains 'MTrk' tracks for each SWM track). \n");
 printf("Output file-name will be: %s\n",OutputFileName);
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)

 //assemble MIDI header chunk
 fputs(MIDI_ID,OutputFile);       //put MIDI-ID to the output-file
 BEwordToFile(0);BEwordToFile(6); //MIDI header-size
 BEwordToFile(1);                 //MIDI version 1 (each track separated)
 BEwordToFile(SID_CHANNELCOUNT);     //number of separate MIDI tracks
 BEwordToFile(PPQN);             //PPQN - MIDI pulses per quarter-note

 SetChordInstr();  //check for chord-instruments, set chords

 //assemble MIDI track chunks
 for(i=0;i<SID_CHANNELCOUNT;i++)
 {
  RowDelta = (TUNETEMPO[0][0]&0x80)? (TUNETEMPO[0][0]&0x7F)*PALpulses : (TUNETEMPO[0][0]+(TUNETEMPO[0][1]&0x7F))/2*PALpulses; //init tune-tempo (no funktempo yet)
  //printf ("Delta-time between pattern-rows (default tempo): %d\n",RowDelta);
  printf("\nAssembling Track (channel) %d  \n",i);
  fputs(MIDI_TRACK_ID,OutputFile); //put MIDI-Track ID 'MTrk' into output file
  MIDItrackPointer=DeltaCount=ActIns=PrevIns=0; PrevNote=WasGateOff=GATEOFFX; PrevChord=PrevChannel=0; //init
  for(j=0;j<seqlength;j++) //j counts current sequence's positions
  {
   if (RowDelta==0) RowDelta=6*PALpulses; //handle accidental zero-tempo, set 6 as default tempo in this case
   if (SEQUENCES[i][j]<0x80)
   {

    //transfer SWM pattern to MIDI
    ActSWMptn=SEQUENCES[i][j]; printf(" Converting Pattern $%x, ",ActSWMptn);
    SWMpattPointer=0; //init non-linear SWM data pointer for SWM source
    for (k=0;k<maxptnlen;k++) //safety loop to prevent hangs, k counts pattern-rows
    {
     //A. Scan a SWM row and set note/ins/fx variables
     ActInsFx=ActFx=0; //no continuous FX is needed yet
     ActNote=PATTERNS[ActSWMptn][SWMpattPointer]; //printf(" n%2X",ActNote[k]);
     if (ActNote==0xFF) break;
     SWMpattPointer++;
     if ( (ActNote&0x7F) && ((ActNote&0x7F)<0x60) ) ActChord=ChordIns[ActIns]; //new note re-selects default chord
     if (ActNote&0x80) //check if next (instrument) column exists
     {
      ActInsFx=PATTERNS[ActSWMptn][SWMpattPointer]; //printf(" i%2X",ActInsFx[k]);
      if ( (XM_Ins=ActInsFx&0x7F) && XM_Ins < INSTR_MAX ) //select instrument
       { ActIns=XM_Ins; InsOctave=(signed char)INSTRUMENTS[ActIns][SWI_OCTAVE_POS]/12; ActChord=ChordIns[ActIns]; }
      if ((ActInsFx&0x70)==0x70) ActChord=ActInsFx&0xF;
      SWMpattPointer++; 
      if (ActInsFx&0x80) //check if next (FX) column exists
      { 
       ActFx=PATTERNS[ActSWMptn][SWMpattPointer];  //printf(" f%2X",ActFx[k]);
       SWMpattPointer++;
       if ((ActFx&0xF0)==0x70) ActChord=ActFx&0xF;
       if (ActFx < 0x20) //check if next (BigFX-Value) column exists
       {
        ActFxVal=PATTERNS[ActSWMptn][SWMpattPointer];  //printf(" v%2X",ActFxVal[k]);
        SWMpattPointer++; 
        if (ActFx==0x07) ActChord=ActFxVal&0x3F;
       }
      }
     }

     //B. Convert ActNote,ActIns,etc. to MIDI events, fill TrackTemp[] array
     if (ActNote&0x7F) //if nonzero and valid note
     {
      if ( (ActNote&0x7F)==GATEOFFX || !(WasGateOff) )
      {
       //write VARIABLE LENTGH (!) MIDI delta-timing value
       if (DeltaCount>=0x80*0x80) { TrackTemp[MIDItrackPointer]=((DeltaCount&0x1FC000)/0x4000)|0x80; MIDItrackPointer++; }  
        if (DeltaCount>=0x80) { TrackTemp[MIDItrackPointer]=((DeltaCount&0x3F80)/0x80)|0x80; MIDItrackPointer++; } 
         TrackTemp[MIDItrackPointer]=DeltaCount&0x7F; MIDItrackPointer++; DeltaCount=0; 
       TrackTemp[MIDItrackPointer]=0x80+PrevChannel; MIDItrackPointer++; //write Note-Off MIDI message
       TrackTemp[MIDItrackPointer]=PrevNote; MIDItrackPointer++; //write MIDI note
       TrackTemp[MIDItrackPointer]=0; MIDItrackPointer++;  //write MIDI-note velocity (should be based on SWM Sustain-FX)
       WasGateOff=GATEOFFX;
       if (ChordIns[PrevIns]) // mute chord-sounds too if Instrument uses chords
       {
        for(l=0;l<ChordLen[PrevChord];l++)
        {
         if ( CHORDS[ ChordIndex[PrevChord] + l ] ) //base sound wasn't repeated
         {
          TrackTemp[MIDItrackPointer]=0; MIDItrackPointer++;
          TrackTemp[MIDItrackPointer]=PrevNote + (signed char) CHORDS[ ChordIndex[PrevChord] + l ] ; MIDItrackPointer++;
          TrackTemp[MIDItrackPointer]=0; MIDItrackPointer++;
         }
        }
       }
      }
      if ( ( ActNote&0x7F)!=GATEOFFX && (ActNote&0x7F)<0x60 )
      {
       //write VARIABLE LENTGH (!) MIDI delta-timing value
       if (DeltaCount>=0x80*0x80) { TrackTemp[MIDItrackPointer]=((DeltaCount&0x1FC000)/0x4000)|0x80; MIDItrackPointer++; }  
        if (DeltaCount>=0x80) { TrackTemp[MIDItrackPointer]=((DeltaCount&0x3F80)/0x80)|0x80; MIDItrackPointer++; } 
         TrackTemp[MIDItrackPointer]=DeltaCount&0x7F; MIDItrackPointer++; DeltaCount=0; 
       PrevChannel=ActIns&0x7; TrackTemp[MIDItrackPointer]=0x90+PrevChannel; MIDItrackPointer++; //write Note-On MIDI message
       TrackTemp[MIDItrackPointer]=PrevNote=(ActNote&0x7F)+Transpose[i]+InsOctave*12+12-1; MIDItrackPointer++; //write MIDI note
       TrackTemp[MIDItrackPointer]=0x7F; MIDItrackPointer++;  //write MIDI-note velocity (should be based on SWM Sustain-FX)
       WasGateOff=0; PrevIns=ActIns; PrevChord=ActChord; 
       if (ChordIns[ActIns]) //expand chord-sounds if Instrument uses chords
       {
        for(l=0;l<ChordLen[ActChord];l++)
        {
         if ( CHORDS[ ChordIndex[ActChord] + l ] ) //if 0, don't repeat base sound
         {
          TrackTemp[MIDItrackPointer]=0; MIDItrackPointer++; //timing is 0 between chords
          TrackTemp[MIDItrackPointer]=PrevNote + (signed char) CHORDS[ ChordIndex[ActChord] + l ] ; MIDItrackPointer++;
          TrackTemp[MIDItrackPointer]=0x5F; MIDItrackPointer++; //chord-sounds should be a bit silenter
         }
        }
       }
      }
     }
     
     //C. Handle possible pattern-effects
     if (ActFx==0x13) RowDelta=(ActFxVal&0x3F)*PALpulses; //track-tempo change
     
     DeltaCount+=RowDelta; //time being spent during NOPs
    }

   }
   //handle orderlist/sequence effects
   else if (SEQUENCES[i][j]==0xFE || SEQUENCES[i][j]==0xFF) break; //exit j-loop if sequence ends
   else if (SEQUENCES[i][j]<0xA0) { Transpose[i]=(signed char)(SEQUENCES[i][j]-0x90); printf(" Transpose %d, ",Transpose[i]); }
   else if (SEQUENCES[i][j]>=0xB0) RowDelta=(SEQUENCES[i][j]-0xB0)*PALpulses;  //changed tempo from sequence
   else ; //other orderlist FX? e.g. tempo-setting / volume-setting
  }
  BEwordToFile(0);BEwordToFile(MIDItrackPointer+4); //put size of track to MIDI file
  for(j=0;j<MIDItrackPointer;j++) fputc(TrackTemp[j],OutputFile); //write entire track to MIDI file
  fputc(RowDelta&0x7F,OutputFile);fputc(0xFF,OutputFile);fputc(0x2F,OutputFile); fputc(0,OutputFile); //put an end to the track
 }
 
 printf("\n\n**** Conversion is complete. **** \nMIDI file '%s' generated... :)"
 "\n-----------------------------------------------------\n",OutputFileName);
 fclose(OutputFile);
}


//==========================================================================================
void GenerateXM()  //----- Export XM data from the imported SWM data ------------------
{
 unsigned int i,j,k,l,MustClearChords[SID_CHANNELCOUNT],RowCount;
 char strend=0,PtnMatch,NotePortaFx[3]={0,0,0}; //'strend' is used to detect end of string
 unsigned char InsWaveForm[32], InsArpTable[32], InsPWpos, ChordPos, InsPulseWid1, InsSpeed, ActPitch[32],GateOFFprg,Transpose[3],VibDelay;
 float SampleYscale, InsDutyCycle1;
 int PrevSample, ActSample, SampleDiff, WaveProgLen, InsOctave, ActWaveForm, PrevWaveForm,PrevPitch,PitchPeriod[32],PrevPitchPeriod;
 //float f; //frequency
 //float t; //counts time spent in sample
  
 printf("to XM (Fasttracker v2.04) format. \n");
 printf("Output file-name will be: %s\n",OutputFileName);
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)

 //detect orderlist-length and loop-position, shrink orderlist effects, resilt is in XMseqTemp array. 
 //No support for different track-patternlengths and track-tempos, sequence-effects and subtunes in XM.
 for (j=0;j<SID_CHANNELCOUNT;j++)
 {
  Transpose[j]=0; //init
  for (i=k=0; i<seqlength; i++)
  {
   XMseqPos[i]=k;
   if (SEQUENCES[j][i] < 0x80 ) {XMseqTemp[j][k]=SEQUENCES[j][i]; XMseqTranspose[j][k]=Transpose[j]; k++; }
   else if (SEQUENCES[j][i] == 0xFE) {XMseqTemp[j][k]=0xFF; XMrestartPos=0;break;} //check hardcoded end-signals
   else if (SEQUENCES[j][i] == 0xFF) {XMseqTemp[j][k]=0xFF; XMrestartPos=XMseqPos[SEQUENCES[j][i+1]]; break;}
   else if (SEQUENCES[j][i] < 0xA0) Transpose[j]=(signed char)(SEQUENCES[j][i]-0x90); //transpose-FX? 
   else ;//other orderlist-FX - not supported in XM
  }
 } 
 XMsongLength=k; //effective length of XM orderlist (without seq-fx, length based on 3rd channel only)
 
 //calculate orderlist itself and XM pattern-references ----------------
 printf("Converting 3-sequence orderlist to 1-sequence XM orderlist...\n");
 strend=0;XMorderlist[0]=0; //reset pattern-count and 1st pattern for XM
 for (j=0;j<SID_CHANNELCOUNT;j++) { XMpattRef[j][0]=XMseqTemp[j][0]; XMpattTranspose[j][0]=XMseqTranspose[j][0]; } //zero pattern always the 1st
 XMpatternCount=1;
  
 for (i=1;i<=XMsongLength;i++) //roll through XM-orderlist
 { 
  for (k=0; k<XMpatternCount;k++) //seek for match of all 3 patterns //and check if end of orderlist arrived
  {
   PtnMatch=0; //init match-detector (will be set to 1 if the bunch of 3 patterns has occured already)
   for (j=0; j<SID_CHANNELCOUNT; j++) //check 3 channels in SWM, if new pattern-set or occured already
   {
    if (XMseqTemp[j][i] == 0xFF) strend=1; //check hardcoded end-signal (created in previous loop)
    else if ( (XMseqTemp[j][i]==XMpattRef[j][k]) && (XMseqTranspose[j][i]==XMpattTranspose[j][k]) ) PtnMatch++; 
   } //if counts up to 3, all channels have there's match with existing pattern-layout
   if (PtnMatch==SID_CHANNELCOUNT) break;
  } 
  //here we got the info in 'PtnMatch' if new pattern is needed or reusing existing (PtnMatch==3)
  if (strend==1) break; //if we reached the end, don't register endsignal in pattern-list
  if (PtnMatch==SID_CHANNELCOUNT) //if matches, just store existing pattern-number, 
  {
   XMorderlist[i]=k; //reuse existing pattern-layout
  }
  else
  {  //if no match, create new 3-pattern bunch  //register new pattern and give reference to original SWM patterns 
   for (j=0; j<SID_CHANNELCOUNT; j++) { XMpattRef[j][XMpatternCount]=XMseqTemp[j][i]; XMpattTranspose[j][XMpatternCount]=XMseqTranspose[j][i]; } 
   XMorderlist[i]=XMpatternCount++;
  }
 }
 printf("\nOrderlist-length: %d (restarts at position %d) , %d XM pattern(s) needed.\n",XMsongLength,XMrestartPos,XMpatternCount);
 
 //write XM header------------------------------------------------------
 fputs(XM_ID,OutputFile); //write XM TYPE ID to output file
 for (i=strend=0;i<XM_TITLE_SIZE;i++) //write XM title to output file (filename for now)
 {
  //if (OutputFileName[i] == 0) strend=1; //end of string reached? sign in 'strend' 
  fputc( (strend==0) ? AUTHORINFO[i] : ' ' , OutputFile); //or OutputFileName[i]?
 }
 fputc(0x1A,OutputFile); //write mysterious $1A
 
 for (i=strend=0;i<XM_TRACKERNAME_SIZE ;i++) //write tracker-name to output file (filename for now)
 {
  if (XM_TRACKERNAME[i] == 0) strend=1; //end of string reached? sign in 'strend' 
  fputc( (strend==0) ? XM_TRACKERNAME[i] : ' ' , OutputFile);
 }
 
 LEwordToFile(XM_VERSION); //XM-version
 for (i=0;i<4;i++) fputc(XM_HEADERSIZE[i],OutputFile); //XM-Header size
 
 LEwordToFile(XMsongLength); LEwordToFile(XMrestartPos); LEwordToFile(XM_CHANNELCOUNT); 
 LEwordToFile(XMpatternCount); LEwordToFile(InstAmount); LEwordToFile(XM_FREQTYPE); 
 LEwordToFile((TUNETEMPO[0][0]&0x80)? TUNETEMPO[0][0]&0x7F : (TUNETEMPO[0][0]+(TUNETEMPO[0][1]&0x7F))/2  ); LEwordToFile(XM_DEFAUBPM); //Tempo values (no funktempo in XM)
 
 //write orderlist------------------------------------------------------
 for (i=0;i<XM_ORDERLIST_SIZE;i++) 
 {
  if (i<XMsongLength) {fputc(XMorderlist[i],OutputFile);printf("$%x[%2X] ",i,XMorderlist[i]);} //write conversion result to file
  else fputc(0,OutputFile); //fill rest with zeroes
 }
 
 //write pattern-data---------------------------------------------------
 printf("\n\nConverting SWM pattern-data to XM pattern-data format...");
 
 SetChordInstr(); //register Chord-Instruments and Chords
 //-------------
 for (i=0;i<XMpatternCount;i++) //counts patterns 
 {
  for (j=0;j<4;j++) fputc(XM_PATTERN_HEADER_SIZE[j],OutputFile); //XM-Header size
  fputc(XM_PACKTYPE,OutputFile); //packtype is always the same
  printf("\nConverting SWM-patterns ($%X,$%X,$%X) to XM-pattern $%X:  ",XMpattRef[0][i],XMpattRef[1][i],XMpattRef[2][i],i);
  
  for (j=0;j<SID_CHANNELCOUNT;j++) SWMpattPointer[j]=0; //init non-linear SWM data pointer for SWM source
  j=RowCount=0; strend=0;    // 'j' indexes XM pattern-data (SWMpattPointer[] indexes SWM pattern datas)
  while (strend==0) //pattern-array filling loop with 'j' index variable
  {

   //A. Scan a SWM row and set note/ins/fx variables
   for (k=0;k<SID_CHANNELCOUNT;k++)
   {
    MustClearChords[k]=0; //init
    ActSWMptn=XMpattRef[k][i]; //printf("%2X,",ActSWMptn); 
    ActNote[k]=PATTERNS[ActSWMptn][ SWMpattPointer[k] ]; //printf(" n%2X",ActNote[k]);
    if ( (ActNote[k]&0x7F)==PORTAMFX ) NotePortaFx[k]=0xFF; //register note-culumn initiated portamento
    if ( (ActNote[k]&0x7F) && ((ActNote[k]&0x7f) < 0x60 ) )  //new note exists? valid note?
    { 
     ActChord[k]=ChordIns[ActIns[k]]; //new note re-selects default chord
     if (ResetFx[k]!=0) ActInsFx[k]=ActFx[k]=0; //ActFX=0 if no continuous FX is needed
    }
    SWMpattPointer[k]++; 
    if (ActNote[k]&0x80) //check if next (instrument) column exists
    {
     ActInsFx[k]=PATTERNS[XMpattRef[k][i]][ SWMpattPointer[k] ]; //printf(" i%2X",ActInsFx[k]);
     if ( (XM_Ins=ActInsFx[k]&0x7F) && XM_Ins < INSTR_MAX ) 
     {
      if ( (ChordIns[ActIns[k]]) && (ChordIns[XM_Ins]==0) ) MustClearChords[k]=1; //if changing from chord-instrument to non-chord-instrument
      ActIns[k]=XM_Ins; ActChord[k]=ChordIns[ActIns[k]]; 
     }
     if ((ActInsFx[k]&0x70)==0x70) ActChord[k]=ActInsFx[k]&0xF;
     SWMpattPointer[k]++; 
     if (ActInsFx[k]&0x80) //check if next (FX) column exists
     { 
      ActFx[k]=PATTERNS[XMpattRef[k][i]][ SWMpattPointer[k] ];  //printf(" f%2X",ActFx[k]);
      if ((ActFx[k]&0xF0)==0x70) ActChord[k]=ActFx[k]&0xF;
      SWMpattPointer[k]++;
      if (ActFx[k] < 0x20) //check if next (BigFX-Value) column exists
      {
       ActFxVal[k]=PATTERNS[XMpattRef[k][i]][ SWMpattPointer[k] ];  //printf(" v%2X",ActFxVal[k]);
       if (ActFx[k]==0x07) ActChord[k]=ActFxVal[k]&0x3F;
       SWMpattPointer[k]++; 
      }
     }
    }
   }

   //B. Create XM patterndata-output based on ActNote/ActIns/ActFx/ActFxVal SWM data
   for (k=0;k<SID_CHANNELCOUNT;k++)
   {  
    if (ActNote[k] == 0xFF) strend=1; //end of pattern should stop the pattern's conversion	 
   }
   if (strend==0) RowCount++;

   for (k=0;k<XM_CHANNELCOUNT;k++) //counts tracks/channels in SWM-patterns    //handle 4th or more column for XM-pattern!!!
   {
    if (k<SID_CHANNELCOUNT) //branch where SWM channels exist
    {
     //CASE 0 - nothing in the column - init
     ColumnCount=XMpattData[j]=XM_NOP_PACKSIGN; PackSignPos=j; j++; //init data to 'NOP' signal //remember where to put XM pack-sign 
     //CASE 1 testing - is there something to be put into XM note-column 
     if ((XM_Note=ActNote[k]&0x7F)) //check if note is nonzero
     {    
      if (XM_Note==GATEOFFX) { XMpattData[j]=XM_GATEOFF_VALUE; ColumnCount|=XM_NOTE_PACKSIGN; j++;}  //convert gate-off signal to be XM-compatible
      else if (XM_Note<0x60) 
      { 
       XMpattData[j]=XM_Note+XMpattTranspose[k][i]; ColumnCount|=XM_NOTE_PACKSIGN; j++; 
       //CASE 2 put current instrument-number into XM instrument-column (only after note)
       XMpattData[j]=ActIns[k]; ColumnCount|=XM_INST_PACKSIGN; j++;
       if (NotePortaFx[k]) { NotePortaFx[k]=0; ActFx[k]=3; ActFxVal[k]=DEFAULTPORTA; } //if portamento note-FX was registered previously
      }
     } 
     //CASE 3 testing - is there something to be put into XM volume-column? (should it be continuous?)
     if ( (ActInsFx[k]&0x70) == SWM_VOLUME_SMALLFX )
     { XMpattData[j]= (ActInsFx[k]&0xF) * (ChordIns[ActIns[k]]?(4/CHORD_SILENCER_RATIO):4) + 0x10; ColumnCount|=XM_VOL_PACKSIGN; j++; ActInsFx[k]=0; }
     if ( (ActFx[k]&0x70) == SWM_VOLUME_SMALLFX )
     { XMpattData[j]= (ActFx[k]&0xF) * (ChordIns[ActIns[k]]?(4/CHORD_SILENCER_RATIO):4) + 0x10; ColumnCount|=XM_VOL_PACKSIGN; j++; ActFx[k]=0; }
     
     //CASE4 testing - is there something to be put into FX and FX-Value column, convert it
     XM_Fx=0; XM_FxVal=0; ResetFx[k]=0; //init
     if (ActFx[k]) //check if effect exists
     {
      if ( (ActFx[k]) < 0x20 ) 
      { 
       XM_Fx=BigFxConvTable[ActFx[k]]; XM_FxVal=ActFxVal[k]; ResetFx[k]=SWM_bigFX_NoteResets[ActFx[k]];
       if ( (XM_Fx<=3) && XM_FxVal ) { XM_FxVal=SWMtoXMslide(ActFxVal[k]); ActFxVal[k]=0; } //if slide/portamento, convert slide-amount from SWM to XM 
       if ( (XM_Fx==XM_VIBRATO_FX) && XM_FxVal ) //handle vibrato differently
        { XM_FxVal=SWMtoXMvibAmp[ActFxVal[k]/16] | SWMtoXMvibFreq[ActFxVal[k]&0xF]*16; ActFxVal[k]=0; } //swap amp&speed and invert speed
       if (ActFx[k]==SWM_NOTEDELAY_FX) XM_FxVal=0xD0|(XM_FxVal&0xF); //complete note-delay value for $ED XM effect
       if (ActFx[k]==SWM_DETUNE_FX) XM_FxVal=0x50|(XM_FxVal&0xF); //complete note-delay value for $E5 XM effect
       if ( SWM_bigFX_Oneshot[ActFx[k]] ) ActFx[k]=0; //if one-shot bigFx, delete bigFX value
      }
      else 
      { 
       XM_Fx=SmallFxConvTable[ActFx[k]/16]; XM_FxVal=(ActFx[k]&0xF)*16; 
       if (XM_Fx==XM_NOTE_VOL_FX || XM_Fx==XM_MAIN_VOL_FX) XM_FxVal=(ActFx[k]&0xF)*4+0x10;
       if ( ActFx[k]/16 == SWM_VIBRATO_BIGFX )  //vibrato-smallFX is continuous - not resetting ActFx[k]
        { XM_FxVal=SWMtoXMvibAmp[ActFx[k]&0xF] | 16*SWMtoXMvibFreq[INSTRUMENTS[ActIns[k]][SWI_INSVIBRATO_POS]&0xF]; ActFx[k]=SWM_VIBRATO_BIGFX; ActFxVal[k]=0; ResetFx[k]=1; }
       else ActFx[k]=0; //all other SWM small-effects are one-shot
      }
     }
     if ( (ActInsFx[k]&0x7F)==SWM_LEGATO_INSFX ) { XM_Fx=XM_PORTAMENTO_FX; XM_FxVal=XM_LEGATO_FXVALUE; ActInsFx[k]=0; }
     if ( (XM_Note&0x70)==VIBRATOFX ) //note-culumn initiated vibrato?
     {XM_Fx=XM_VIBRATO_FX; XM_FxVal=SWMtoXMvibAmp[XM_Note&0xF] | 16*SWMtoXMvibFreq[INSTRUMENTS[ActIns[k]][SWI_INSVIBRATO_POS]&0xF]; ActFx[k]=SWM_VIBRATO_BIGFX; ActFxVal[k]=0; }
     //XM_Fx and its value calculated, write it out to the pattern-array
     if (XM_Fx) //check if effect exists
     { 
      XMpattData[j]=XM_Fx; ColumnCount|=XM_FX_PACKSIGN;j++; 
      if (XM_FxVal) { XMpattData[j]=XM_FxVal; ColumnCount|=XM_FXVAL_PACKSIGN;j++;   }
     }
     XMpattData[PackSignPos]=ColumnCount; //put $80..$88 XM packed-columns signal based on number of columns
     //printf(":%x|",ColumnCount);
    }
    else //zero out rest of XM columns (where no SWM data lies) - or put chord-notes in
    {
     //CASE5 testing -is there chord to be put into XM tracks 4..8  //handle chords on rest of the tracks?
     l = k % SID_CHANNELCOUNT; //SWM channel to test for chord in current column
     ChordPos=(k/SID_CHANNELCOUNT)-1; //index inside chord
     if ( (ActChord[l]) && (XM_Note=ActNote[l]&0x7F) && (ChordPos<ChordLen[l]) )
     {
      if ( XM_Note < 0x60 ) //only chord-instruments and valid notes should trigger chords
      { 
       XMpattData[j]=XM_NOTE_PACKSIGN|XM_INST_PACKSIGN|XM_VOL_PACKSIGN;j++;
       XMpattData[j]=XM_Note + (signed char) CHORDS[ ChordIndex[ActChord[l]] + ChordPos ] + XMpattTranspose[l][i]; j++;
       XMpattData[j]=ActIns[l]&0x7F; j++; //instrument must be next to note to hear sound in XM
       XMpattData[j]=(0x40/CHORD_SILENCER_RATIO)+0x10; j++;  //volume of chord-notes must be low to prevent distortions
      }
      else if ( XM_Note == GATEOFFX )
      { 
       XMpattData[j] = XM_NOTE_PACKSIGN; j++;
       XMpattData[j] = XM_GATEOFF_VALUE; j++; 
      }
      else { XMpattData[j]=XM_NOP_PACKSIGN; j++; }
     }
     else if (MustClearChords[l]) //if there was no Gate-OFF for chords but a non-chord instrument follows
     {
      XMpattData[j] = XM_NOTE_PACKSIGN; j++;
      XMpattData[j] = XM_GATEOFF_VALUE; j++;
     }
     else { XMpattData[j]=XM_NOP_PACKSIGN; j++; } //printf(" --|"); //signs empty column in XM
    }
    if (strend==1) break;
   }
  }
  
  LEwordToFile(RowCount); //write pattern-length (number of rows)
  LEwordToFile(j-1); //write packed pattern-size
  printf("\nXM-Packed Size is %d",j-1);
  for (k=0;k<j-1;k++) fputc(XMpattData[k],OutputFile); 
 }

 //Write Instruments---------------------------------------------------
 printf("\n\nConverting SWM instruments to XI instruments...\n"); 
 for (i=1;i<=InstAmount;i++)
 {
  PrevWaveForm=0x21; PrevPitch=SWM_C5_NOTE_VALUE; PrevPitchPeriod=XM_C4_NOTE_PERIOD; GateOFFprg=0; //init
  InsPWpos=INSTRUMENTS[i][SWI_PULSETBPT_POS]; InsPulseWid1=(INSTRUMENTS[i][InsPWpos]&0xF)*16 + (INSTRUMENTS[i][InsPWpos+1]/16) ;
  if (InsPulseWid1<0x10) InsPulseWid1=0x10; if (InsPulseWid1>0xF0) InsPulseWid1=0xF0; //make thin sounds audible 
  InsDutyCycle1=InsPulseWid1/256.00; //printf("-%d-%f ",InsPulseWid1,InsDutyCycle1); 
  InsSpeed=INSTRUMENTS[i][SWI_ARP_SPEED_POS] & SWM_MASK__INSTRUMENT_ARPSPEED; InsOctave=(signed char)INSTRUMENTS[i][SWI_OCTAVE_POS]/12;
  FrameSampleLength=( (XM_1X_PERIOD*(InsSpeed+1))/TUNEDATA[TYPE_ID_OFFSET+FSPEEDPOS]) / OctavePeriod[InsOctave+8]; //calculate how many sample-periods are needed
  for(j=WaveProgLen=0;j<32;j++) //examine WF/Arp table
  {
   InsWaveForm[j]=INSTRUMENTS[i][WFTABLEPOS+j*3]; InsArpTable[j]=INSTRUMENTS[i][WFTABLEPOS+j*3+1]; //get wavetable
   if ( (InsArpTable[j]>0x80) && (InsArpTable[j]<0xE0) ) ActPitch[j]=((unsigned int)InsArpTable[j]&0x7F)-InsOctave*12; //absolute-pitch handling
   else if (InsArpTable[j]==0x7F) ActPitch[j]=SWM_C5_NOTE_VALUE;   //chord handling...done in patterndata-conversion
   else if (InsArpTable[j]==0x80) ActPitch[j]=PrevPitch;   //NOP handling...
   else ActPitch[j]=SWM_C5_NOTE_VALUE+(signed int)InsArpTable[j]; //relative-pitch handling
   PitchPeriod[j]=XM_C4_NOTE_PERIOD/pow(2,((ActPitch[j]-SWM_C5_NOTE_VALUE)/12.00)) ;  //note-frequency on piano keyboard, relative to C-5 piano key (49 in tracker data)
   PrevPitch=ActPitch[j];  // printf("%2x ",)
   LoopLength = (PrevWaveForm&0x80) ? FrameSampleLength : PrevPitchPeriod; //period is one frame in case of noise-waveform
   if (PrevWaveForm>=0x10) { if ((PrevWaveForm&1)==0) GateOFFprg=1; else GateOFFprg=0; } //sign for ADSR creator that no decay-time (skipped by Gate-off at end of arp-table)
   if ( InsWaveForm[j] == 0xFF ) { SampleLoopPos=FrameSampleLength*j-LoopLength; break; } //end of wavetable?
   if ( InsWaveForm[j] == 0xFE ) //looping end of wavetable?   
   {
    if ( (InsArpTable[j]-WFTABLEPOS) < j*3 ) SampleLoopPos=FrameSampleLength*((InsArpTable[j]-WFTABLEPOS)/3); //jumpaddress valid and points backwards? 
    else if ( (InsArpTable[j]-WFTABLEPOS) >= 0x40*3 ) SampleLoopPos=FrameSampleLength*j-LoopLength; //waveform-table jump-to-itself endsignal? equals to $FF endsignal
    else SampleLoopPos=FrameSampleLength*j-LoopLength;  //or jumpaddress not pointing inside...
    break;
   }
   WaveProgLen=j+1;PrevWaveForm=InsWaveForm[j];PrevPitchPeriod=PitchPeriod[j];
  }
  SampleLength=FrameSampleLength*WaveProgLen;
  printf("$%x:",i);
  for (j=0;j<4;j++) fputc(XM_INST_HEADER_SIZE[j],OutputFile); //+0 instrument-header-size
  for (j=0;j<XM_INSNAME_SIZE;j++)
  {
   if ((j<instnamelength) && (i!=0) ) //+4 instrument-name (nothing for instrument 0)
   { 
    fputc(INSTNAMES[i][j],OutputFile); printf("%c",INSTNAMES[i][j]); 
   }  
   else fputc(0,OutputFile); //fill rest of instrument-name with zeroes (or inst. 0)
  }
  fputc(XM_INSTYPE,OutputFile);    //+26 instrument-type is 0 or 'random' according to XM documentation
  LEwordToFile(1);                 //+27 number of samples in instrument
  //generate instrument sound (settings and sample)
  LEwordToFile(XM_SAMPLE_HEADER_SIZE); LEwordToFile(0); //+29 (dword) sample-header size
  for(j=0;j<96;j++) fputc(0,OutputFile);  //+33 96bytes Sample number assignments for all notes
  //ADSR, envelope points
  LEwordToFile(ADSRpoint=0); LEwordToFile(0); //starting point of ADSR
  if (GateOFFprg) { ; } //no Attack-time if Waveform-table gated off
  else { ADSRpoint=ATTACKtoXMtime( (INSTRUMENTS[i][SWI_AD_POS]&0xF0)/16 ); } //calculate Attack-time of ADSR
   LEwordToFile(ADSRpoint); //set Attack-time of ADSR
   LEwordToFile(0x3F);     //level after attack of ADSR
  if (GateOFFprg) { ADSRpoint++;  } //no Decay-time if Waveform-table gated off
  else { ADSRpoint+=1+DECAYtoXMtime( (INSTRUMENTS[i][SWI_AD_POS]&0x0F), INSTRUMENTS[i][SWI_SR_POS]/16 ); } //calculate Decay-time of ADSR
   LEwordToFile(ADSRpoint); //added Decay-time of ADSR
   LEwordToFile((INSTRUMENTS[i][SWI_SR_POS]&0xF0)/4); //Sustain-level of ADSR
  ADSRpoint+=1+RELEASEtoXMtime( (INSTRUMENTS[i][SWI_SR_POS]&0x0F), INSTRUMENTS[i][SWI_SR_POS]/16 );  //calculate Release-time of ADSR
   LEwordToFile(ADSRpoint);  //added Release-time of ADSR 
   LEwordToFile(0);    //level of end of Release phase
  for(j=0;j<(48-4*4);j++) { fputc(0,OutputFile); } //   +129     48   (byte) Points for volume envelope
  //....the rest...
  for(j=0;j<24;j++) { fputc(0,OutputFile); fputc(0,OutputFile); } //   +177     48   (byte) Points for panning envelope
  fputc(4,OutputFile);  //   +225      1   (byte) Number of volume points
  fputc(0,OutputFile);  //   +226      1   (byte) Number of panning points
  fputc(2,OutputFile);  //   +227      1   (byte) Volume sustain point
  fputc(0,OutputFile);  //   +228      1   (byte) Volume loop start point
  fputc(0,OutputFile);  //   +229      1   (byte) Volume loop end point
  fputc(0,OutputFile);  //   +230      1   (byte) Panning sustain point
  fputc(0,OutputFile);  //   +231      1   (byte) Panning loop start point
  fputc(0,OutputFile);  //   +232      1   (byte) Panning loop end point
  fputc(1+((GateOFFprg)?0:2),OutputFile);  //   +233      1   (byte) Volume type: bit 0: On; 1: Sustain; 2: Loop
  fputc(0,OutputFile);  //   +234      1   (byte) Panning type: bit 0: On; 1: Sustain; 2: Loop
  fputc(0,OutputFile);  //   +235      1   (byte) Vibrato type
  VibDelay= (INSTRUMENTS[i][0]&0x30) ? INSTRUMENTS[i][SWI_VIBDELAY_POS] : 0x80-(INSTRUMENTS[i][SWI_VIBDELAY_POS]&0x7F) ; //incremental vibrato?
  fputc(VibDelay,OutputFile);       //   +236      1   (byte) Vibrato sweep
  //XMinsVibAmp[i] = (INSTRUMENTS[i][0]&0x30) ? SWMtoXMvibAmp[INSTRUMENTS[i][SWI_INSVIBRATO_POS]/16] : 0xF ; //incremental vibrato is treated differently
  fputc(SWMtoXMvibAmp[INSTRUMENTS[i][SWI_INSVIBRATO_POS]/16],OutputFile);  //   +237      1   (byte) Vibrato depth
  if (INSTRUMENTS[i][SWI_INSVIBRATO_POS]&0xF) XMinsVibFreq[i]=4 * SWMtoXMvibFreq[INSTRUMENTS[i][SWI_INSVIBRATO_POS]&0xF];
  else XMinsVibFreq[i]=0;
  fputc(XMinsVibFreq[i],OutputFile);  //   +238      1   (byte) Vibrato rate
  LEwordToFile(0);  //   +239      2   (word) Volume fadeout
  for(j=0;j<11;j++) LEwordToFile(0);  //   +241      11 words Reserved
  //sample-calculations and settings
  LEwordToFile(SampleLength%0x10000); LEwordToFile(SampleLength/0x10000); //?  4  (dword) Sample length
  LEwordToFile(SampleLoopPos%0x10000); LEwordToFile(SampleLoopPos/0x10000);    //     +4      4  (dword) Sample loop start
  LEwordToFile((SampleLength-SampleLoopPos)%0x10000); LEwordToFile((SampleLength-SampleLoopPos)/0x10000); //     +8      4  (dword) Sample loop length
  fputc( (ChordIns[i])?(0x40/CHORD_SILENCER_RATIO):0x40 ,OutputFile); //    +12      1   (byte) Volume - more silent for chord-instruments
  fputc(0,OutputFile); //    +13      1   (byte) Finetune (signed byte -16..+15)
  fputc(1,OutputFile); //    +14      1   (byte) Type: Bit 0-1: 0 = No loop, 1 = Forward loop, 2 = Ping-pong loop;  4: 16-bit sampledata
  fputc(0x80,OutputFile); //    +15      1   (byte) Panning (0-255)
  fputc(INSTRUMENTS[i][SWI_OCTAVE_POS]+12,OutputFile); //    +16      1   (byte) Relative note number (signed byte)
  fputc(0,OutputFile); //    +17      1   (byte) Reserved
  for(j=0;j<22;j++)    //    +18     22   (char) Sample name
  { 
   if (j>=WaveProgLen) fputc('.',OutputFile); 
   else fputc(WFchar[InsWaveForm[j]/16],OutputFile);
  }
  //WaveForm Samples 
  PrevSample=-128; PrevWaveForm=0x21; //init
  for (j=0;j<WaveProgLen;j++)
  {
   if ( (ActWaveForm=InsWaveForm[j]) <= 0x0F ) ActWaveForm=PrevWaveForm; //repeat previous waveform if 0..F
   if (PitchPeriod[j]==0) PitchPeriod[j]=1;  //safety:prevent floating point exception //printf(" %d ",PitchPeriod[j]);
   for (k=0;k<FrameSampleLength;k++)
   {
    l= k % PitchPeriod[j]; SampleYscale=(256.00/PitchPeriod[j]);
    if (ActWaveForm&0x10) ActSample= (l<=PitchPeriod[j]/2) ? l*SampleYscale+0x40 : (0x100-l*SampleYscale)+0x40; //triangle-waveform
    if (ActWaveForm&0x20) ActSample= l*SampleYscale/2+0x40; //sawtooth-waveform
    if (ActWaveForm&0x40) ActSample= (l>=(PitchPeriod[j]*InsDutyCycle1)) ? 0x40 : 0xC0; //pulse-waveform (with adjustable pulsewidth)
    if (ActWaveForm&0x80) ActSample=(rand()%0x80)+0x40; //noise-waveform  
    SampleDiff=ActSample-PrevSample; fputc(SampleDiff,OutputFile); PrevSample=ActSample; PrevWaveForm=ActWaveForm;
   }
  }
    
  printf("  ");
 }
 
 printf("\n\n**** Conversion is complete. **** \nXM file '%s' generated... :)"
 "\n-----------------------------------------------------\n",OutputFileName);
 fclose(OutputFile);
}



//==========================================================================================
void ProcessXM()  //----- Import XM data to arrays ------------------------------------
{
 unsigned int InputXMver;
 
 printf("Processing Fasttracker XM module...\n");
 
 //display filesize and format
 fseek(InputFile, TYPE_ID_OFFSET, 0); fgets(InputTypeID, TYPE_ID_LENGTH, InputFile);
 fseek(InputFile, XM_VERSION_OFFSET, 0); InputXMver=LEwordFromFile();
 fseek(InputFile, 0, SEEK_END); InputSize=ftell(InputFile); fseek(InputFile, 0, SEEK_SET);
 printf("Input-file size: %d bytes \n",InputSize);
 printf("Input-filetype ID: \"%s\" (at offset %d) , version: \"%X\" \n",InputTypeID,TYPE_ID_OFFSET,InputXMver);
 
 if (stringMatches(InputTypeID,XM_ID))
 { //check module-version, if doesn't match, notify but try to do...
  if (InputXMver!=XM_VERSION) printf("\n*** The module version doesn't match with \"%X\", but trying... ***\n",XM_VERSION);
  ProcessXM204(); 
 }
 else
 {
  printf("\n*** The module is of unknown format... ***\n");
  fclose(InputFile);WaitKeyPress();exit(4);
 }
}

//------------------------------------------------------------------
void ProcessXM204() //parse & import XM version 2.04 data to arrays
{
 unsigned int i,j,k,l,db,PackSign, XMheaderSize=0x0104, SampleAmount=0, titlend=0;  
 for(i=0;i<40;AUTHORINFO[i]=0x20,i++); //init 
 unsigned int PtnMatch, InsHeaderSize=0; //PtnMatch is used to detect if new pattern matches an existing one
 uint64_t/*unsigned long long*/ SampleIndex,SampleSize; //,InstOffset; 
 unsigned int XMinsEnvVol[12],XMinsEnvTime[12];
 unsigned char XMinsEnv; 

 //process XM header----------------------------------------------
 printf("\nProcessing XM-header...\n");
 fseek(InputFile, XM_TITLE_OFFSET, 0); fgets(AUTHORINFO,XM_TITLE_SIZE,InputFile); printf("\nXM title: \"%s\"",AUTHORINFO);
 for (i=0;i<40;i++)
 {
  if (AUTHORINFO[i]==0) titlend++;
  if (titlend>0) AUTHORINFO[i]=0x20; //SPACE-character
 }
 fseek(InputFile, XM_HEADERSIZE_OFFSET, 0); XMheaderSize=LEwordFromFile(); printf("\nXM Header-size: $%X",XMheaderSize);
 fseek(InputFile, XM_SONGLENGTH_OFFSET, 0); XMsongLength=LEwordFromFile(); printf("\nXM Orderlist length: $%X",XMsongLength);
 fseek(InputFile, XM_RESTARTPOS_OFFSET, 0); XMrestartPos=LEwordFromFile(); printf(" (looping at $%2X)",XMrestartPos);
 fseek(InputFile, XM_CHANNELCOUNT_OFFSET, 0); XM_CHANNELCOUNT=LEwordFromFile(); printf("\nNumber of channels: %d",XM_CHANNELCOUNT);
 fseek(InputFile, XM_PTNAMOUNT_OFFSET, 0); XMpatternCount=LEwordFromFile(); printf(", Number of XM patterns: $%X",XMpatternCount);
 fseek(InputFile, XM_INSAMOUNT_OFFSET, 0); XMinstAmount=LEwordFromFile(); printf("\nXM Instruments: $%X",XMinstAmount);
 if (InstAmount>=INSTR_MAX) { InstAmount=INSTR_MAX-1; printf(" --> reduced to $%X",InstAmount); } //to be on the safe side
 fseek(InputFile, XM_DEFTEMPO_OFFSET, 0); TUNETEMPO[0][0]=LEwordFromFile()|0x80; printf("\nDefault tune-tempo: $%X \n",TUNETEMPO[0][0]&0x7F);
 
 //process XM orderlist----------------------------------------
 printf("\nProcessing XM pattern-orderlist... \n");
 fseek(InputFile, XM_ORDERLIST_OFFSET, 0); //position to beginning of Orderlist
 for(i=0;i<XMsongLength;i++) { XMorderlist[i]=fgetc(InputFile); printf("$%X ",XMorderlist[i]); }

 //process XM patterns-----------------------------------------------
 printf("\nProcessing XM patterndata... \n");
 XMpatternOffset[0]=XM_HEADERSIZE_OFFSET+XMheaderSize; SWMpatternCount=0; //init
 for(i=0;i<XMpatternCount;i++) //i counts patterns
 {
  fseek(InputFile, XMpatternOffset[i], 0); XM_PATTERN_HEADER_SIZE[0]=LEwordFromFile(); //how long is pattern-header?
  fseek(InputFile, XMpatternOffset[i]+XM_PTNROWS_OFFSET, 0); XMpatternLen[i]=LEwordFromFile();
  fseek(InputFile, XMpatternOffset[i]+XM_PTNSIZE_OFFSET, 0); XMpatternSize[i]=LEwordFromFile();
  XMpatternOffset[i+1]=XMpatternOffset[i] + XMpatternSize[i] + XM_PATTERN_HEADER_SIZE[0]; //prepare place of upcoming pattern
  fseek(InputFile, XMpatternOffset[i]+XM_PATTERN_HEADER_SIZE[0], 0); //go to beginning of effective pattern-data
  printf("Pattern $%x (length:$%2x offset:$%4x, size:$%x):  ",i,XMpatternLen[i],(unsigned int)XMpatternOffset[i],XMpatternSize[i]);
  
  //unpack one XM pattern, convert selected 3 tracks to 3 vertical-oriented unpacked XM patterns
  for(j=0;j<XMpatternLen[i];j++) // j counts pattern-rows
  {
   XMpattPos=j*XM_COL_PER_TRK;
   for (k=0;k<XM_CHANNELCOUNT;k++) //k counts pattern-channels
   {
    db=fgetc(InputFile); //read pack-signal
    if (!(db&0x80)) 
    {
     ActNote[k]=db; ActIns[k]=fgetc(InputFile); ActVol[k]=fgetc(InputFile); //no XM packing, all columns filled
     ActFx[k]=fgetc(InputFile);ActFxVal[k]=fgetc(InputFile);
    }
    else
    { //depack XM data of 1 channel of 1 row
     PackSign=db;
     ActNote[k]=ActIns[k]=ActVol[k]=ActFx[k]=ActFxVal[k]=0; //init to 0
     if ( PackSign & (XM_NOTE_PACKSIGN&0x7F) ) ActNote[k]=fgetc(InputFile); //
     if ( PackSign & (XM_INST_PACKSIGN&0x7F) ) ActIns[k]=fgetc(InputFile); //
     if ( PackSign & (XM_VOL_PACKSIGN&0x7F) ) ActVol[k]=fgetc(InputFile); //
     if ( PackSign & (XM_FX_PACKSIGN&0x7F) ) ActFx[k]=fgetc(InputFile); //
     if ( PackSign & (XM_FXVAL_PACKSIGN&0x7F) ) ActFxVal[k]=fgetc(InputFile);
    }
    //convert horizontal XM row to vertical XM format, write result-patterns to temporary array
    if (k<SID_CHANNELCOUNT) //should be selectable which tracks of the XM to convert.
    { 
     XMpattTemp[k][XMpattPos+0]=ActNote[k]; //printf("%2x-",ActNote[k]);
     XMpattTemp[k][XMpattPos+1]=ActIns[k]; //printf("%2x-",ActIns[k]);
     XMpattTemp[k][XMpattPos+2]=ActVol[k]; //printf("%2x-",ActVol[k]);
     XMpattTemp[k][XMpattPos+3]=ActFx[k]; //printf("%2x-",ActFx[k]);
     XMpattTemp[k][XMpattPos+4]=ActFxVal[k]; //printf("%2x  ",ActFxVal[k]);
    }
   }
  }
  
  //compare the resulted 3 vertical XM patterns to existing ones, register if not found, reuse existing if found
  //(might be good idea to look for transposed patterns here or later, were relative note-intervals and effects match)
  for (k=0;k<SID_CHANNELCOUNT;k++)
  {
   //compare the 3 vertical XM patterns to all already registered patterns, if the same, drop it
   PtnMatch=0; //init
   for (l=0;l<SWMpatternCount;l++) //check registered XM patterns from beginning
   {
    if (PATTERNLENGTHS[l]!=XMpatternLen[i]) continue; //if mismatch in pattern-sizes, don't even try to compare their contents
    for (j=0;j<(XMpatternLen[i]*XM_COL_PER_TRK);j++) //compare bytes one-by-one to pattern 'l'
    {
     //printf("%d,%d ",XMpattTemp[k][j],XMPATTERNS[l][j]);
     if (XMpattTemp[k][j]!=XMPATTERNS[l][j]) {PtnMatch=0;break;} //mismatch found
     else PtnMatch++; //count matching bytes
    }
    if (PtnMatch) break;
   }
   //printf("%d.%d ",l,PtnMatch);
   //register or reuse patterns based on PtnMatch
   if (PtnMatch) //if match is found, reusing pattern 'l'
   {
    printf("$%2x  ",l);
    XMpattRef[k][i]=l;
   }
   else //no match found, registering new pattern
   {
    XMpattRef[k][i]=++SWMpatternCount;
    PATTERNLENGTHS[SWMpatternCount]=(XMpatternLen[i]<=maxptnlen)? XMpatternLen[i] : maxptnlen ; //register SWM-patternlength in time
    for (j=0;j<XMpatternLen[i]*XM_COL_PER_TRK;j++) //copy the registered temporary vertical XM pattern to pattern-registry 
    { 
     XMPATTERNS[SWMpatternCount][j]=XMpattTemp[k][j]; //printf("%2X,",XMPATTERNS[SWMpatternCount][j]);
    }
    printf("$%2x  ",XMpattRef[k][i]);
   }
  }
  printf("\n");
 }

 fseek(InputFile, XMpatternOffset[XMpatternCount-1]+XM_PATTERN_HEADER_SIZE[0]+XMpatternSize[XMpatternCount-1], 0); //surely go to beginning of instruments even if file-index doesn't yet point right after last pattern's data

 //process XM instruments
 InstAmount=0; //init - will count instruments that are not empty
 for(i=0;i<XMinstAmount;i++)
 {
  printf("\nInstr. $%X: ",i); //display number of instrument
  InsHeaderSize=LEwordFromFile(); LEwordFromFile();  //offset 0 - how long is instrument-header?
  for (j=0;j<XM_INSNAME_SIZE;j++) { XMinstName[i][j]=fgetc(InputFile); printf("%c",XMinstName[i][j]); } //offset 4 - ins.name
  fgetc(InputFile);  //offset $1A (26) - inst.type (dummy: 0)
  SampleAmount = LEwordFromFile();  //offset $1B (27)
  
  if (InsHeaderSize==0x21) { for (j=0;j<(InsHeaderSize-0x1D);j++) fgetc(InputFile); } //till end of header - needed for some modules
  
  if (SampleAmount!=0) // - from offset $1D (29) sample is coming (if exists)
  {
   InstAmount=i+1; //register last used (non-empty) instrument's number
   LEwordFromFile(); LEwordFromFile();  //how long is sample-header?  ($28) //
   for (k=0;k<96;k++) fgetc(InputFile); //sample assignments
   for (k=0;k<12;k++) { XMinsEnvTime[k]=LEwordFromFile(); XMinsEnvVol[k]=LEwordFromFile(); }//volume envelope points
   for (k=0;k<24;k++) LEwordFromFile(); //panning envelope points
   XMinsRelease[i]=XMinsEnvTime[fgetc(InputFile)-1];  //   +225      1   (byte) Number of volume points
   fgetc(InputFile);  //   +226      1   (byte) Number of panning points
   XMinsSustain[i]=fgetc(InputFile);  //   +227      1   (byte) Volume sustain point
   XMinsRelease[i]-=XMinsEnvTime[XMinsSustain[i]]; //search 'release'
   fgetc(InputFile);  //   +228      1   (byte) Volume loop start point
   fgetc(InputFile);  //   +229      1   (byte) Volume loop end point
   fgetc(InputFile);  //   +230      1   (byte) Panning sustain point
   fgetc(InputFile);  //   +231      1   (byte) Panning loop start point
   fgetc(InputFile);  //   +232      1   (byte) Panning loop end point
   XMinsEnv=fgetc(InputFile);  //   +233      1   (byte) Volume type: bit 0: On; 1: Sustain; 2: Loop
   if ((XMinsEnv&0x1)==0) //if envelope is used at all...
   { //no envelope, make standard 00F0 ADSR
    XMinsAttack[i]=0; XMinsDecay[i]=0; XMinsSustain[i]=0x3F; XMinsRelease[i]=0;
   }
   else if (XMinsEnv&0x2) //if sustain is on
   {
    //maybe: if ( XMinsEnvVol[XMinsSustain[i]] > XMinsEnvVol[0] ) 
    if ( XMinsEnvTime[1]) { XMinsAttack[i]=XMinsEnvTime[XMinsSustain[i]]; XMinsDecay[i]=0; }
    else { XMinsDecay[i]=XMinsEnvTime[XMinsSustain[i]]; XMinsAttack[i]=0; }
    XMinsSustain[i]=XMinsEnvVol[XMinsSustain[i]]; //turn sustain-index to sustain-value
    if (XMinsSustain[i]>=0x40) XMinsSustain[i]=0x3F; //compensate overload
   }
   else //if no sustain point given, create decaying instrument
   { 
    XMinsAttack[i]=0; XMinsDecay[i]=XMinsRelease[i]; XMinsSustain[i]=0; XMinsRelease[i]=0; 
   } 
   //printf("  (A:$%x, D:$%x,S:$%x , R:$%x)  ",XMinsAttack[i],XMinsDecay[i],XMinsSustain[i],XMinsRelease[i]);
   fgetc(InputFile);  //   +234      1   (byte) Panning type: bit 0: On; 1: Sustain; 2: Loop
   fgetc(InputFile);  //   +235      1   (byte) Vibrato type
   XMinsVibDelay[i]=fgetc(InputFile);  //   +236      1   (byte) Vibrato sweep
   XMinsVibAmp[i]=fgetc(InputFile);  //   +237      1   (byte) Vibrato depth
   XMinsVibFreq[i]=fgetc(InputFile);  //   +238      1   (byte) Vibrato rate
   //if (XMinsVibFreq[i]>=0x80) XMinsVibFreq[i]=0xFF-XMinsVibFreq[i]; //compensate XM vibrato-charactiristics
   LEwordFromFile();  //   +239      2   (word) Volume fadeout
   for(j=0;j<11;j++) LEwordFromFile();  //   +241      11 words Reserved
   //rolling through sample(s)
   SampleSize=0; //init. will be accumulated by each sample
   for (k=0;k<SampleAmount;k++)
   {
    SampleSize += ( LEwordFromFile(SampleLength) + LEwordFromFile(0)*0x10000 ); //?  4  (dword) Sample length
    printf("      (Sample $%X - %llu bytes)",k,SampleSize);
    LEwordFromFile(); LEwordFromFile();    //     +4      4  (dword) Sample loop start
    LEwordFromFile(); LEwordFromFile(); //     +8      4  (dword) Sample loop length
    fgetc(InputFile); //    +12      1   (byte) Volume - more silent for chord-instruments
    fgetc(InputFile); //    +13      1   (byte) Finetune (signed byte -16..+15)
    fgetc(InputFile); //    +14      1   (byte) Type: Bit 0-1: 0 = No loop, 1 = Forward loop, 2 = Ping-pong loop;  4: 16-bit sampledata
    fgetc(InputFile); //    +15      1   (byte) Panning (0-255)
    XMinsTransp[i]=fgetc(InputFile); //    +16      1   (byte) Relative note number (signed byte)
    fgetc(InputFile); //    +17      1   (byte) Reserved
    for(j=0;j<22;j++) fgetc(InputFile);   //    +18     22   (char) Sample name
   }
   for(SampleIndex=0;SampleIndex<SampleSize;SampleIndex++) { fgetc(InputFile); } //roll through each sample's audio-data
  }
 }
}


//==========================================================================================
void ProcessMIDI() //check MIDI type and version--------------------------------
{
 unsigned int PrevVelo,OKtrkAmount,MinimumDelta[SID_CHANNELCOUNT],MaximumDelta[SID_CHANNELCOUNT],i,j,k,l,BarPulses,NoteResolution; //counts bytes in loops
 unsigned int RowPulses, SourceIndex; 
 unsigned char CurrentNote, CurrentVelo, FirstRow, PtnMatch, PattTemp[256*XM_COL_PER_TRK]; 
 int CurrentDelta; //,DeltaDiff;
 float BarQuarters;

 printf("Processing MIDI file...\n");
 
 for (i=0;i<40;i++) AUTHORINFO[i]=0x20; //init for SWM
 
 //display filesize and format
 fseek(InputFile, TYPE_ID_OFFSET, 0); fgets(InputTypeID, TYPE_ID_LENGTH+1, InputFile);
 fseek(InputFile, MIDI_VERSION_OFFSET, 0); InputMIDIver=BEwordFromFile();
 fseek(InputFile, 0, SEEK_END); InputSize=ftell(InputFile); fseek(InputFile, 0, SEEK_SET);
 printf("Input-file size: %d bytes \n",InputSize);
 printf("Input-filetype ID: \"%s\" (at offset %d) , version: \"%X\" \n",InputTypeID,TYPE_ID_OFFSET,InputMIDIver);
 
 for(i=0;i<MIDI_CHANNELS_MAX;i++) MIDItrkIndex[i]=0; //init MIDI track/channel pointers before conversion
 
 if (stringMatches(InputTypeID,MIDI_ID))
 { //check module-version, if doesn't match, notify but try to do...  
  fseek(InputFile, MIDI_DIVISION_OFFSET, 0); MIDI_DIVISION=BEwordFromFile(); printf("Delta-timing resolution (units per quarter-note - PPQN): $%x ",MIDI_DIVISION);
  if (InputMIDIver==2) {printf("\nProcessing multi-song MIDI's 1st track...");InputMIDIver=0;} //only 1st tune processed of version 2
  if (InputMIDIver==0)
  { //single-track MIDI
   printf("\nProcessing single-track MIDI...");
   fseek(InputFile, (MIDI_TRACKS_OFFSET+4), 0); MIDI_TRACK_SIZE[0]=256*BEwordFromFile()+BEwordFromFile(); printf("\nSize of the track (containing all channels): $%2X bytes\n",(unsigned int)MIDI_TRACK_SIZE[0]);
   ProcessMIDItrack(); //fill MIDI delta/note/velocity arrays of channels/tracks
  }
  else if (InputMIDIver==1)
  { //multi-track MIDI
   printf("\nProcessing multi-track MIDI...");
   fseek(InputFile, MIDI_TRACK_AMOUNT_OFFSET, 0); MIDI_TRACK_AMOUNT=BEwordFromFile(); printf("\nAmount of MIDI-tracks (channels): $%2X \n",MIDI_TRACK_AMOUNT);
   MIDI_TRACK_OFFSET[0]=MIDI_TRACKS_OFFSET; //init
   for(i=0;i<MIDI_TRACK_AMOUNT;i++) //positioning to beginning of tracks
   {
    fseek(InputFile, (MIDI_TRACK_OFFSET[i]+4), 0); MIDI_TRACK_SIZE[i]=256*BEwordFromFile()+BEwordFromFile(); 
    printf("\nProcessing Track $%x: offset $%x , size $%x",i+1,(unsigned int)MIDI_TRACK_OFFSET[i],(unsigned int)MIDI_TRACK_SIZE[i]);
    CurrentTrack=i; ProcessMIDItrack(); //fill MIDI delta/note/velocity arrays of channels/tracks
    MIDI_TRACK_OFFSET[i+1]=MIDI_TRACK_OFFSET[i]+4+MIDI_TRACK_SIZE[i]+4;
   }
  }
 }
 else
 {
  printf("\n*** The MIDI is of unknown format... ***\n");
  fclose(InputFile);WaitKeyPress();exit(4);
 }

 //tune processed, select only tracks which have note events
 printf("\nTune processing finished...");
 for(i=OKtrkAmount=0;i<MIDI_CHANNELS_MAX;i++)
 { 
  if(MIDItrkIndex[i])
  {
   printf("\nChannel %d: %d note events",i+1,MIDItrkIndex[i]); //for(j=0;j<MIDItrkIndex[i];j++) printf("%x*%x ",MIDIdeltas[i][j],MIDInotes[i][j]);
   OKtrk[OKtrkAmount]=i;OKtrkAmount++; //register non-empty tracks
  }
 }
 //note ON/OFF and velocity are in channels at array MIDIdeltas[], MIDInotes[], MIDIvelos[]

 //calculate minimal and maximal delta values - might be used to automatically determine needed note-resolution
 for(i=0;(i<SID_CHANNELCOUNT)&&(i<OKtrkAmount);i++) //only care about 1st 3 channels wchich contain notes
 {
  MinimumDelta[i]=MaximumDelta[i]=MIDI_DIVISION; PrevVelo=0; //init
  for(j=0;j<MIDItrkIndex[OKtrk[i]];j++) 
  { //examine smallest delta (except 0 of course) to guess needed resolution
   if ( (MIDIdeltas[OKtrk[i]][j]) && (MIDIdeltas[OKtrk[i]][j]<MinimumDelta[i]) && (PrevVelo)) MinimumDelta[i]=MIDIdeltas[OKtrk[i]][j];
   if (MIDIdeltas[OKtrk[i]][j]>MaximumDelta[i]) MaximumDelta[i]=MIDIdeltas[OKtrk[i]][j];
   PrevVelo=MIDIvelos[OKtrk[i]][j];
  }
  printf("\nTrack $%x Min.delta: $%x (1/%d quarternote), Max.delta: $%x (%.1f quarternotes)",OKtrk[i]+1,MinimumDelta[i],MIDI_DIVISION/MinimumDelta[i],MaximumDelta[i],(float)MaximumDelta[i]/(float)MIDI_DIVISION);
 }

 //determine pattern-lengths for SWM
 BarQuarters=(float) (4.00/pow(2,(float)TimeSignature[1]))*TimeSignature[0];  //calculate how many quarter notes and deltas are in a bar (can be smaller than 1 if e.g. unusual 2/8 = 1/4 time signature)
 BarPulses = (unsigned int)(BarQuarters*MIDI_DIVISION); 
 NoteResolution=16; //16th notes resolution
 printf("\nThere are %.2f quarter notes (%.2f*$%x = $%x pulses) in a Bar.",BarQuarters,BarQuarters,MIDI_DIVISION,BarPulses);
 for(i=0;i<PATT_MAX;i++) { PATTERNLENGTHS[i]=(unsigned int)(BarQuarters*NoteResolution); } 
 RowPulses=MIDI_DIVISION*(4.00/NoteResolution);
 printf("\nOne pattern-row takes $%x pulses",RowPulses);
 printf("\nPattern-lengths will be $%x rows with %dth-note resolution = $%x pulses",PATTERNLENGTHS[0],NoteResolution,PATTERNLENGTHS[0]*RowPulses); 
 //convert BPM to C64 tempo info - BPM is quarter-notes/minute in 4/4, but e.g. eighth-notes/minute for 6/8 or 8/8, etc. 
 //unsigned int BeatTime=(unsigned int)( MIDItempo*(float)(4.00/pow(2,(float)TimeSignature[1])) ); //microseconds for a beat
 TuneBPM=(60000000/MIDItempo)*(float)(pow(2,(float)TimeSignature[1])/4.00); printf("\nBeats Per Minute: %d",TuneBPM); //real BPM based on denominator
 //unsigned int BarTime=BeatTime*BarQuarters; //time of a bar in microseconds (from quarternote-microseconds MIDItempo)
 unsigned int SWMrowTime=MIDItempo*(4.00/NoteResolution); //time of SWM pattern-row
 unsigned int PALframeTime=20000; //microseconds a PAL (50Hz) frame takes
 unsigned int PALframesPerRow=(SWMrowTime/PALframeTime); //how much PAL frames build up a pattern-row (C64 tune speed)
 //if (PALframesPerRow<3) PALframesPerRow=3; //limit to usable speed
 TUNETEMPO[0][0]=PALframesPerRow+0x80; printf("\nC64 tempo (based on BPM %d and Time-signature denominator): $%x\n",TuneBPM,PALframesPerRow);

//----create SWM patterns and orderlists--------------------------------------------------------------
 printf("\n\n*** Generating patterns from the MIDI-datastream. ***");
 SWMpatternCount=XMsongLength=XMrestartPos=0; //init
 for(i=0; i<SID_CHANNELCOUNT; i++) //only care about 1st 3 channels wchich contain notes
 {
  SourceIndex=k=0; 
  if (i<OKtrkAmount)
  {
   printf("\n\nGenerating patterns from track $%x: \n",OKtrk[i]+1);
   FirstRow=1; CurrentDelta=MIDIdeltas[OKtrk[i]][SourceIndex]; printf("\nDelta-time of 1st Note: $%x pulses\n",CurrentDelta); //init
   while ( ( SourceIndex < MIDItrkIndex[OKtrk[i]] ) && ( (SWMpatternCount+1)<PATT_MAX ) && (k<seqlength) )
   {
    printf("pattern $%x, ",SWMpatternCount+1);  //  printf("\n");
    for (j=0;j<PATTERNLENGTHS[1]*XM_COL_PER_TRK;j++) PattTemp[j]=0; //init pattern
    for(j=0;j<PATTERNLENGTHS[1];j++) //j counts pattern-rows
    {
     if (FirstRow) { PattTemp[j*XM_COL_PER_TRK+1]=i+1; FirstRow=0; printf("*Insturment%d* ",i+1); } //select instrument 1..3 at the beginning to get sound 
     if ( SourceIndex >= MIDItrkIndex[OKtrk[i]] ) break; //don't continue pattern-creation if MIDI data is over
     for(l=0;l<RowPulses;l++) //counts pattern-row pulses one-by-one, quantizes in 50% range back&forth
     {
      if (CurrentDelta==0) //delay expired, put event into this or next row, depending on 50% treshold
      { //if current note timed out, register it and go to next in MIDI-track
       do  //wait in place if zero-delay(s) happened - hopefully won't cause endless loops
       {
        CurrentNote=MIDInotes[OKtrk[i]][SourceIndex]; CurrentVelo=MIDIvelos[OKtrk[i]][SourceIndex];
        if ( CurrentNote<12 || CurrentNote>=106) CurrentNote=(12-1); //limit note-range to SWM
        if ( l < (RowPulses/2) ) //check if less than 50%
        { //if smaller than 50%, put in this row...
         if ( (PattTemp[j*XM_COL_PER_TRK]==0) || (PattTemp[j*XM_COL_PER_TRK]==XM_GATEOFF_VALUE) ) //touch data only if there's no note data in the place already (one-shot writing)
         { PattTemp[j*XM_COL_PER_TRK]=(CurrentVelo) ? CurrentNote-(12-1) : XM_GATEOFF_VALUE ;} 
        }
        else 
        {  //if bigger than 50%, put to next row instead
         if ( (PattTemp[(j+1)*XM_COL_PER_TRK]==0) || (PattTemp[(j+1)*XM_COL_PER_TRK]==XM_GATEOFF_VALUE) ) //touch data only if there's no note in the place already (one-shot writing)
         { PattTemp[(j+1)*XM_COL_PER_TRK]=(CurrentVelo) ? CurrentNote-(12-1) : XM_GATEOFF_VALUE ; }
        }
        SourceIndex++; CurrentDelta=MIDIdeltas[OKtrk[i]][SourceIndex];
       } 
       while (CurrentDelta==0); //wait in place if zero-delay(s) happened 
       //printf("%x*",CurrentDelta);
      }
      CurrentDelta--; //roll through 0 delays, maximum Rowpulses/2 zero-delays allowed
     }
     //printf("$%2X ",PattTemp[j*XM_COL_PER_TRK]);
    }
   
    //compare the resulted vertical XM pattern to existing ones, register if not found, reuse existing if found
    //(might be good idea to look for transposed patterns here or later, were relative note-intervals and effects match)
    PtnMatch=0; //init
    for (l=1;l<=SWMpatternCount;l++) //check registered XM patterns from beginning
    {
     for (j=0;j<(PATTERNLENGTHS[1]*XM_COL_PER_TRK);j++) //compare bytes one-by-one to pattern 'l'
     {
      if (PattTemp[j]!=XMPATTERNS[l][j]) {PtnMatch=0;break;} //mismatch found
      else PtnMatch++; //count matching bytes
     }
     if (PtnMatch) break;
    }
    //register or reuse patterns based on PtnMatch
    if (PtnMatch) //if match is found, reusing pattern 'l'
    {
     printf("$%2x  ",l);
     XMpattRef[i][k]=l;
    }
    else //no match found, registering new pattern
    {
     XMpattRef[i][k]=++SWMpatternCount;
     for (j=0;j<PATTERNLENGTHS[1]*XM_COL_PER_TRK;j++) //copy the registered temporary vertical XM pattern to pattern-registry 
     { 
      XMPATTERNS[SWMpatternCount][j]=PattTemp[j]; //printf("%2X,",XMPATTERNS[SWMpatternCount][j]);
     }
     printf("$%2x  ",XMpattRef[i][k]);
    }
    k++;
    if (i==0) { XMorderlist[k]=k; XMsongLength=k; }
   }
  }
  else
  { //handle empty sequences possibly left
   SWMpatternCount++;
   printf("\n\nGenerating empty sequence with empty pattern $%x... \n",SWMpatternCount);
   for (k=0;k<XMsongLength;k++) 
   {
    XMpattRef[i][k]=SWMpatternCount; //last unused pattern empty pattern
   }
  }
 }
 
 //Generating some basic instruments to get some sound at least...
 for(i=0;(i<SID_CHANNELCOUNT) && (i<OKtrkAmount); i++)
 {
  for(j=0;j<instnamelength;j++) XMinstName[i][j]='.';
  XMinsAttack[i]=0; XMinsDecay[i]=0; XMinsSustain[i]=0x3F; XMinsRelease[i]=0;
 }
 InstAmount=i;
}

//------------------------------------------------------------------------
void ProcessMIDItrack()
{
 unsigned char EventTemp,Event,DestinationTrack,MetaEvent=0,MIDIdata1,MIDIdata2,MIDInote,MIDIvelo;
 unsigned int i,j; //set default BPM
 unsigned long MIDIdelta,MetaDataLength; //set default tempo (120bpm in 4/4 mode)

 for(i=0;i<MIDI_CHANNELS_MAX;i++) ChannelDelta[i]=0; //init

 //file pointer points to beginning of track here - read and put MIDI notes to corresponding arrays of channels
 while (MetaEvent!=0x2F) //for(i=0;i<MIDI_TRACK_SIZE[0];)
 {
  MIDIdata1=MIDIdata2=0; //init
  MIDIdelta=ReadVarLenNum(); //i++; //printf ("$%x ",MIDIdelta);
  EventTemp=fgetc(InputFile); //printf("Event %x,  ",EventTemp); //examine Event
  if (EventTemp&0x80) Event=EventTemp; //if legal event 'status' byte
  else fseek(InputFile, -1, SEEK_CUR); //otherwise if no status-byte given, we use the previously read 'Event'
  if (InputMIDIver!=1) //if version 0 (single-track) or 2, distribute based on channel-number
  {  
   DestinationTrack=Event&0xF; 
   for(i=0;i<MIDI_CHANNELS_MAX;i++) ChannelDelta[i]+=MIDIdelta;   //accumulate timing for not touched tracks
  }
  else 
  {
   DestinationTrack=CurrentTrack; //if version 1, distribute channel notes based on track-number
   ChannelDelta[CurrentTrack]+=MIDIdelta; 
  }

  if(Event==0xFF) //check if Meta-event
  {
   MetaEvent=fgetc(InputFile); MetaDataLength=ReadVarLenNum();
   switch (MetaEvent)
   {
    case 0x01 : //text event
    {
	 printf("\nText: ");
	 for(j=0;j<MetaDataLength;j++) printf("%c",fgetc(InputFile));
	 break;
    }
    case 0x02 : //copyright note
    {
	 printf("\nCopyright text: ");
	 for(j=0;j<MetaDataLength;j++) 
     {
      MIDIdata1=fgetc(InputFile);
      if (j<40 && MIDIdata1!=0)  AUTHORINFO[j]=MIDIdata1;
      printf("%c",MIDIdata1);
     }
	 break;
    }
    case 0x03 : //sequence/track name
    {
	 printf("\nTrack-name: ");
	 for(j=0;j<MetaDataLength;j++) printf("%c",fgetc(InputFile));
	 break;
    }
    case 0x2F : //end of track
    {
	 break;
    }
    case 0x51 : //tempo setting
    {
	 printf("\nTune-tempo (microseconds per quarter-note): ");
	 MIDItempo=0; //reset value, 24bit value (3 bytes)
	 for(j=0;j<MetaDataLength;j++) MIDItempo+=pow(256.00,(float)(2-j))*fgetc(InputFile); //printf("%x,",fgetc(InputFile));
     printf("%d  -> %d quarter-notes/minute \n",(unsigned int)MIDItempo,60000000/MIDItempo); 
	 break;
    }
    case 0x58 : //Time signature
    {
	 printf("\nTime-signature: ");
	 for(j=0;j<MetaDataLength;j++) TimeSignature[j]=fgetc(InputFile); //printf("%x ",fgetc(InputFile));
	 printf("Numerator: %d ; Denominator(2:quarter-note,3:eigth,etc.): %d",TimeSignature[0],TimeSignature[1]);
     printf("\n                MIDI-clocks per metronome-tick: %d \n                Notated 32nd notes in a quarter-note: %d \n",TimeSignature[2],TimeSignature[3]);
	 break;
    }
    default : 
    {
	 for(j=0;j<MetaDataLength;j++) fgetc(InputFile); //printf("%x ",fgetc(InputFile));
	 break;
	}
   }
  }
  else if (Event==0xF0) //check if System Exclusive Event
  {
   while (fgetc(InputFile)!=0xF7) ; //simply roll through sysex till its end
  }
  else if (Event==0xF7) 
  {
   while (fgetc(InputFile)!=0xF7) ; //simply roll through sysex till its end
  }
  else //normal MIDI events
  {
   switch (Event&0xF0)
   {
	case 0x90 : //Note-On MIDI event
	{ //put different channels' notes in separate array-tables
	 MIDIdeltas[DestinationTrack][MIDItrkIndex[DestinationTrack]]=ChannelDelta[DestinationTrack]; 
     //printf("*%x* ",ChannelDelta[DestinationTrack]);
     ChannelDelta[DestinationTrack]=0;
	 MIDIdata1=MIDInote=fgetc(InputFile); //printf("$%x*%x-%x-",(unsigned int)MIDIdelta,Event,MIDInote);
	 MIDIdata2=MIDIvelo=fgetc(InputFile); //printf("%x ",MIDIvelo);
	 MIDIvelos[DestinationTrack][MIDItrkIndex[DestinationTrack]]=MIDIvelo; 
	 MIDInotes[DestinationTrack][MIDItrkIndex[DestinationTrack]]=MIDInote; //(MIDIvelo==0) ? GATEOFFX : MIDInote;
	 MIDItrkIndex[DestinationTrack]++;
	 break;
    }
    case 0x80 : //Note-OFF MIDI event
    {
	 MIDIdeltas[DestinationTrack][MIDItrkIndex[DestinationTrack]]=ChannelDelta[DestinationTrack]; ChannelDelta[DestinationTrack]=0;
     MIDIdata1=MIDInote=fgetc(InputFile); //printf("%x-%x-%x-",Event&0xF,(unsigned int)MIDIdelta,MIDInote);
	 MIDInotes[DestinationTrack][MIDItrkIndex[DestinationTrack]]=MIDIdata1; //GATEOFFX;
	 MIDIdata2=MIDIvelo=fgetc(InputFile); //printf("%x ",MIDIvelo);
	 MIDIvelos[DestinationTrack][MIDItrkIndex[DestinationTrack]]=0; //MIDIvelo;
	 MIDItrkIndex[DestinationTrack]++;
	 break;
    }
    case 0xC0 : {MIDIdata1=fgetc(InputFile);break;}
    case 0xD0 : {MIDIdata1=fgetc(InputFile);break;}
	default :
	{
	 MIDIdata1=fgetc(InputFile); MIDIdata2=fgetc(InputFile);
    }
   }
   //printf("*%x-%x-%x ",Event,MIDIdata1,MIDIdata2);
  }
 }
}


//==========================================================================================
void GenerateSWM() //---- Export SWM data from the imported XM/MIDI/SNG/etc. data -----------
{
 /* these global variables/arrays should be loaded with imported music-data:
 //  SWMpatternCount - number of imported patterns
 //  (ChordTableSize - number of possibly imported chords)
 //  (SequenceAmount - number of sequences - now hardwired to SID_CHANNELCOUNT=3 below)
 //  InstAmount - amount of imported instruments
 //  AUTHORINFO[] - assembled author-text, first $28 characters will be used for SWM output...
 //  XMsongLength - orderlist-length (in case of Goattracker SNG it should be a 3-element array instead)
 //  XMpattRef[] - 
 //  XMorderlist[] - converts to SEQUENCES[i][j] and SWMseqLength[i] then TUNEDATA[];
 //  XMPATTERNS[i][j+0..4] - pattern-arrays with these columns of j: note,instrument,volume,fx,fxvalue
 //  etc...                - converted to PATTERNS[i][SWMpattIndex], PATTERNLENGTHS[],PATTERNSIZES[], then TUNEDATA[]
 //  conclusion: NOT SUITABLE TO PRECISELY CONVERT FROM GOATTRACKER .SNG FORMAT. USING CONRAD'S CODE INSTEAD. - now sng2swm tool
 */

 unsigned int i,j,DataIndex=0,SWMpattIndex=0;
 unsigned char SWM_Note=0,SWM_Ins=0,SWM_Fx=0,SWM_FxVal=0,PrevInst=0,PrevFx=0,PrevTempo=0,PrevVib=0,NOPcount=0,PattData=0,PrevData=0,PrevSlide=0;
 
 printf("to SWM (SID-WIZARD v1.0) format. \n");
 printf("Output file-name will be: %s\n",OutputFileName); 
 
 //---------- assembling TUNEDATA array before writing to file ---------
 OutputSize=tuneheadersize; for(i=0;i<TUNEDATA_SIZE_MAX;TUNEDATA[i]=0,i++); //init
 SequenceAmount=SID_CHANNELCOUNT; //only 3, because only 1 subtune gets generated
 PatternAmount=(SWMpatternCount<PATT_MAX) ? SWMpatternCount : PATT_MAX;
 ChordTableSize=0; //might be used later

 //write SWM header
 for (i=0;SWM_ID[i];i++) TUNEDATA[i]=SWM_ID[i];  //0-3. SWM-ID
 TUNEDATA[FSPEEDPOS]=1;    //4.preferred framespeed
 TUNEDATA[SWM_HILI_POS]=4; //5.preferred step-highlighting amount - should depend on patternlength
 TUNEDATA[SWM_AUTO_POS]=1; //6.preferred auto-advance amount
 TUNEDATA[SWM_CBIT_POS]=1; //7.track-binding ON  (, other 'confbit' newsettings are 'OFF')
 for(i=0;i<SID_CHANNELCOUNT;i++) TUNEDATA[SWM_MUTE_POS+i]=0xFF; //8-$A.solo all tracks
 TUNEDATA[SWM_DEFP_POS]=DEFAULTPTNLEN; //$B. default pattern-length - should depend on XM patternlengths
 TUNEDATA[SEQAMOPOS]=SequenceAmount; //$C. sequence-amount
 TUNEDATA[PTAMOUPOS]=PatternAmount; //$D. pattern-amount calculated in pattern-converter
 TUNEDATA[INSTAMPOS]=InstAmount;    //$E. number of instruments 
 TUNEDATA[CHRDLEPOS]=ChordTableSize;   //$F. chordtable-length (calculated in converter)
 TUNEDATA[TMPLENPOS]=0; //tempo-programs are not supported in XM/MIDI, leaving it out
 TUNEDATA[COLORTHEMEPOS]=0; //default colour-theme
 TUNEDATA[KEYBOARDTYPE_POS]=0; //default (GOATTRACKER-like) 2-row keyboard mode
 TUNEDATA[DRIVERTYPE_POS]=0; //normal music-player/driver routine version
  //....some bytes not used yet in SWM version 1. (kept for possible upcoming expansion)
 for(i=0;i<(tuneheadersize-AUTHORPOS);i++) TUNEDATA[AUTHORPOS+i]=toupper(AUTHORINFO[i]); //$18..$3f - author-info
 
 DataIndex=tuneheadersize; //init pack-target-address
 
 //write SWM sequences XMorderlist[] contains XM-orderlist, XMsongLength is its length
 //convert XM-orderlist to SWM-orderlist based on pattern-numbers in XMpattRef[]
 printf("\nConverting processed XM Orderlist to 3 SWM sequences"); 
 for(i=0;i<SequenceAmount;i++) 
 {
  printf("\nSWM Sequence $%x: ",i);
  for (j=0;j<XMsongLength;j++)
  {
   if ( (XMpattRef[i][XMorderlist[j]]) && (XMpattRef[i][XMorderlist[j]]<=PATT_MAX) ) SEQUENCES[i][j]=XMpattRef[i][XMorderlist[j]]; //don't allow illegal patterns
   else SEQUENCES[i][j]=1; //however SEQ-FX-es must be allowed later 
   printf("%2x ",SEQUENCES[i][j]);
  }
  SEQUENCES[i][j]=0xFF;j++; SEQUENCES[i][j]=XMrestartPos; j++; //put 'jump' delimiter and corresponding loop-address (will be harder with transpose)
  SWMseqLength[i]=j;  //if transpose gets supported, the SWM sequence-lengths will differ
 }
 for(i=0;i<SequenceAmount;i++)  //write orderlist to TUNEDATA-array
 {
  for (j=0;j<SWMseqLength[i];j++)
  {
   TUNEDATA[DataIndex]=SEQUENCES[i][j]; //copy SWM sequence to TUNEDATA[]
   DataIndex++;
  }
  TUNEDATA[DataIndex]=SWMseqLength[i]; DataIndex++; //write size of sequence into TUNEDATA[]
 }
 
 //write SWM patterns - XMPATTERNS[][] filled with needed XM pattern-data ---------------------
 //XMpattref[][] contains which pattern of XMPATTERNS array corresponds which SWM pattern 
 //XMpatternCount holds how much patterns are needed
 //Now, convert XM patterns to SWM equivalents (make packed NOPs too)     // !!! SWM patterns start from 1
 printf("\nConverting processed XM patterndata to SWM format:  "); 
 for (i=1;i<=PatternAmount;i++) // i counts patterns
 {
  SWMpattIndex=PrevInst=PrevVib=0; //init non-linear SWM data pointer for SWM target and previous value buffers
  for(j=0; j<(PATTERNLENGTHS[i]*XM_COL_PER_TRK); j+=XM_COL_PER_TRK) // j counts pattern-rows
  {
   if (SWMpattIndex>=maxptnlen) { PATTERNLENGTHS[i]=(j/XM_COL_PER_TRK); break; } //don't allow bigger than allowed length of patterns
   XM_Note = XMPATTERNS[i][j+0];
   XM_Ins  = XMPATTERNS[i][j+1]; 
   XM_Vol  = XMPATTERNS[i][j+2];
   XM_Fx   = XMPATTERNS[i][j+3]; // XM-FX 0 (arpeggio) should be converted to something else for SWM chord-support
   XM_FxVal= XMPATTERNS[i][j+4];
   //convert XM Ins., Vol/Fx to SWM ins., small-/big-FX --- mark every NOP as 0! -----------
   //CASE1 - if there's something to put into note-column (note / note-fx / maybe packed NOP)
   SWM_Note=(XM_Note!=XM_GATEOFF_VALUE) ? (XM_Note>=0x60)?0:XM_Note : GATEOFFX ;  
   if(SWM_Note!=0 && SWM_Note<0x60) 
   { //if valid note starts, init Vibrato-signal and slide
    PrevVib=0; 
    if (PrevFx==1 || PrevFx==2) PrevSlide=0;
   }
   //CASE2 - if there's something to put into instrument-column (instrument/legato/inst-FX) 
   if ( XM_Ins < INSTR_MAX ) //check if legal instrument-selection happens in XM-pattern
   {
    if ((SWM_Ins=XM_Ins)) //check if SWM_ins and XM_ins is nonzero
    {
     if (SWM_Ins!=PrevInst) { SWM_Ins=XM_Ins; PrevInst=SWM_Ins; } //store current instrument to check later if repeated
     else SWM_Ins=0; //no need to refresh the same instrument in SWM. can be used for FX/legato instead
    }
   }
   if (SWM_Ins==0) //if no instrument-selection, maybe effects can come in instrument-column 
   {
    if ( XM_Fx==XM_PORTAMENTO_FX) 
    { 
     if (XM_FxVal>=XM_LEGATO_MIN_VAL) {SWM_Ins=SWM_LEGATO_INSFX; XM_Fx=0; PrevSlide=XM_LEGATO_FXVALUE;} //legato-FX in case
     else if (SWM_Note!=0 && XM_FxVal==0 && PrevSlide>=XM_LEGATO_MIN_VAL) { SWM_Ins=SWM_LEGATO_INSFX; XM_Fx=0;}
    }  
    else if (XM_Fx==XM_NOTE_VOL_FX) { SWM_Ins=(XM_FxVal==0x40)?SWM_VOLUME_SMALLFX+0xF : SWM_VOLUME_SMALLFX+((XM_FxVal/4)&0xF); XM_Fx=0; } //volume-setting
    else if ((XM_Vol>0xF) && (XM_Vol<0x50) ) { SWM_Ins=SWM_VOLUME_SMALLFX+(((XM_FxVal-0x10)/4)&0xF); XM_Fx=0; } //volume-setting
   }
   //CASE 3 & 4 - if there's something to be put into FX-column and FX-value (Small/Big FX)
   SWM_Fx=0; //init, will stay 0 if no transferrable XM_FX is coming
   if ((XM_Vol&0xF0)==0xA0) SWM_Fx = 0x90 + XMtoSWMvibFreq(XM_Vol&0xF); //vibrato frequency Small-Fx
   else if ((XM_Vol&0xF0)==0xB0) SWM_Fx = 0x80 + XMtoSWMvibAmp(XM_Vol&0xF); //vibrato amplitude Small-FX
   else if ((XM_Vol&0xF0)==0xF0) { SWM_Fx=3; SWM_FxVal=(XM_Vol&0xF); } //portamento Big-FX
   else if (XM_Fx<=XM_PORTAMENTO_FX) 
   { //slide/portamento Big-FX
    if (XM_FxVal) 
    { 
     SWM_Fx=XM_Fx; SWM_FxVal=XMtoSWMslide(XM_FxVal);
     PrevSlide=SWM_FxVal; //memorize slide/portamento speed for the case when a 100/200/300 (XM_FxVal=0) command comes
    }
    else 
    {
     if (SWM_Note!=0 && SWM_Ins==0) {SWM_Fx=XM_Fx; SWM_FxVal=PrevSlide;}
    }
   } 
   else if ( (XM_Fx==XM_VIBRATO_FX) && (XM_FxVal) && (XM_FxVal!=PrevVib) )      //vibrato Big-FX
    { PrevVib=XM_FxVal; SWM_Fx = SWM_VIBRATO_BIGFX; SWM_FxVal = XMtoSWMvibFreq(XM_FxVal/16) + XMtoSWMvibAmp(XM_FxVal&0xF)*16; } 
   else if (XM_Fx == XM_NOTE_VOL_FX) { SWM_Fx = (XM_FxVal==0x40)?SWM_VOLUME_SMALLFX+0xF : SWM_VOLUME_SMALLFX+((XM_FxVal/4)&0xF);  }  //note-volume (sustain level) Small-FX
   else if (XM_Fx == 0xE) { //extra XM-effects 
    if ((XM_FxVal&0xF0)==0x50) SWM_Fx=0xD0+(XM_FxVal&0xF); //SWM_DETUNE_FX would be Big-Fx; this is Small-FX
    if ((XM_FxVal&0xF0)==0xD0) { if ((TUNETEMPO[0][0]&0x7F)>3) {SWM_Fx=SWM_NOTEDELAY_FX;SWM_FxVal=(XM_FxVal&0xF);} } //delay Big-Fx
   }
   else if ( (XM_Fx==XM_MAIN_TEMPO_FX) && (XM_FxVal<0x20) && !(PrevFx==XM_MAIN_TEMPO_FX && PrevTempo==XM_FxVal) ) { SWM_Fx = 0x10; SWM_FxVal=PrevTempo=XM_FxVal; } //tempo Big-Fx
   else if (XM_Fx == XM_MAIN_VOL_FX) { SWM_Fx = (XM_FxVal==0x40)?SWM_MAIN_VOLUME_SMALLFX+0xF : SWM_MAIN_VOLUME_SMALLFX+((XM_FxVal/4)&0xF); } //main-volume Small-Fx 
   if (SWM_Fx==0 && (PrevFx==0x1 || PrevFx==0x2) && SWM_Note==0 && PrevSlide!=0) {SWM_Fx=PrevFx; SWM_FxVal=0; PrevFx=0;} //suppress previous slide
   if (XM_Fx) PrevFx=XM_Fx; //used to prevent XM funktempo from taking whole pattern
   //set bit7 in columns if necessary (following columns exist as nonzero) ----------
   if (SWM_Fx) SWM_Ins|=0x80; //switch bit7 on
   if (SWM_Ins) SWM_Note|=0x80; //switch bit7 on
   //write out note/inst. columns
   PATTERNS[i][SWMpattIndex]=SWM_Note; SWMpattIndex++; //note column always exists
   if (SWM_Ins) { PATTERNS[i][SWMpattIndex]=SWM_Ins; SWMpattIndex++; }
   if (SWM_Fx)  
   { 
    PATTERNS[i][SWMpattIndex]=SWM_Fx; SWMpattIndex++; 
    if (SWM_Fx<0x20) { PATTERNS[i][SWMpattIndex]=SWM_FxVal; SWMpattIndex++; }
   }
  }
  PATTERNS[i][SWMpattIndex]=0xFF; SWMpattIndex++; //put $FF pattern-delimiter - will be overwritten by size-indicator
  PATTERNSIZES[i]=SWMpattIndex;
 } 
 //maybe we should look for transposed patterns (where note-differences and effects are the same)
 //(Adjust 'PatternAmount' if less pattern was used by further optimizations)
 for (i=1;i<=PatternAmount;i++) //pack patterns into TUNEDATA[] array
 {
  for (j=PrevData=0;j<PATTERNSIZES[i];j++) 
  {
   PattData=PATTERNS[i][j];
   //pack NOPs if enough found------------------------- (mimic the packdepack.inc assembly pattern-packer algorithm)
   if ( (PattData!=0) || (j==0) ) goto wrcmptr;
   else if (PATTERNS[i][j-1]) goto wrcmptr;
   NOPcount=PACKEDMIN-1; //init
   packloop: if (PATTERNS[i][j+1]!=0) goto putprst; 
   NOPcount++; j++; 
   if ( NOPcount < PACKEDMAX ) goto packloop; 
   putprst: if ((PattData=NOPcount)==(PACKEDMIN-1)) PattData=0;  //simple NOP in case of only 2 consequent zeroes
   //write Patterndata to target-----------------------
   wrcmptr: TUNEDATA[DataIndex]=PrevData=PattData; DataIndex++; 
  }
  //printf("\n");
  printf("  pattern$%X(length:$%2X,size:$%2X),  ",i,PATTERNLENGTHS[i],PATTERNSIZES[i]);
  DataIndex--; TUNEDATA[DataIndex]=PATTERNSIZES[i]; DataIndex++; //write out size of pattern to TUNEDATA - in place of 0xFF delimiter
  TUNEDATA[DataIndex]=PATTERNLENGTHS[i]; DataIndex++; //write length of pattern to TUNEDATA after size
 } 

 //write default SWM instruments to get some sound at least, some parameters get transferred
 for (i=0;i<InstAmount;i++)
 {
  TUNEDATA[DataIndex]=(XMinsVibAmp[i]<4)?0x0a:0x1a; DataIndex++; //0 CONTROL REGISTER (HR TIMER,HRTYPE ,VIBRATO-TYPE,TIED PW/CTF)
  TUNEDATA[DataIndex]=0x0F;DataIndex++; TUNEDATA[DataIndex]=((XMinsSustain[i]&0x3F)/4)*16+0;DataIndex++; //1..2 HR-ADSR
  TUNEDATA[DataIndex]=ATTACKtoSWMtime(XMinsAttack[i])*16+DECAYtoSWMtime(XMinsDecay[i],XMinsSustain[i]); DataIndex++; //3 Attack/Decay ON NOTESTART (GATE-ON)
  TUNEDATA[DataIndex]=((XMinsSustain[i]&0x3F)/4)*16+RELEASEtoSWMtime(XMinsRelease[i],XMinsSustain[i]);DataIndex++; //3 Sustain/Release ON NOTESTART (GATE-ON)
  TUNEDATA[DataIndex]=XMinsVibAmp[i]*16+(XMinsVibFreq[i]/8); DataIndex++; //5 VIBRATO (CALCULATED) FREQ+AMP (amplitude:high nibble)
  TUNEDATA[DataIndex]=(XMinsVibAmp[i]<4) ? (0x100-XMinsVibDelay[i])&0x0F : XMinsVibDelay[i] ; DataIndex++; //6 VIBRATO DELAY OR AMPLITUDE-INCREMENT SPEED
  TUNEDATA[DataIndex]=0; DataIndex++; //7 ARPEGGIO AND CHORD-SPEED (0=1X)
  TUNEDATA[DataIndex]=1; DataIndex++; //8 DEFAULT CHORD FOR INSTRUMENT
  TUNEDATA[DataIndex]=0; DataIndex++; //round(XMinsTransp[i]/12)*12-12; DataIndex++; //9 OCTAVE SHIFT (2's complement) - it's transpose essentially but in 12 steps by editor
  TUNEDATA[DataIndex]=0x14; DataIndex++; //A PW-TABLE POINTER RELATIVE TO INSTRUMENT-ADDRESS
  TUNEDATA[DataIndex]=0x15; DataIndex++; //B FILTER-TABLE POINTER RELATIVE TO INSTRUMENT-ADDRESS
  TUNEDATA[DataIndex]=0; DataIndex++; //C GATEOFF POINTER FOR WFARP-TABLE RELATIVE TO WF-TABLE POSITION
  TUNEDATA[DataIndex]=0; DataIndex++; //D GATEOFF POINTER FOR PW-TABLE RELATIVE TO PW-TABLE POSITION
  TUNEDATA[DataIndex]=0; DataIndex++; //E GATEOFF POINTER FOR FILTER-TABLE RELATIVE TO FILTER-TABLE POS.
  TUNEDATA[DataIndex]=9; DataIndex++; //F - used as 1st-frame waveform if its switch enabled in control-byte - in SW1.x tracker 00 value gets converted to $09 default
  TUNEDATA[DataIndex]=0x21; DataIndex++;TUNEDATA[DataIndex]=0; DataIndex++;TUNEDATA[DataIndex]=0; DataIndex++;TUNEDATA[DataIndex]=0xFF; DataIndex++; //WFtable
  TUNEDATA[DataIndex]=0xFF; DataIndex++; //PulseWidth-table
  TUNEDATA[DataIndex]=0x15; DataIndex++; // !!!! would be Filter-table 0xFF delimiter, but turns into size-info for SWM module decompression
  printf("\nSWM instrument $%X, name: ",i+1);
  for(j=0;j<instnamelength;j++) { TUNEDATA[DataIndex]=(XMinstName[i][j]) ? toupper(XMinstName[i][j]) : 0x20; printf("%c",TUNEDATA[DataIndex]); DataIndex++; }
 }
 
 //write SWM chordtable - in case there's chord-support in the converter ;]
 //not writing tempo-table yet.
 
 //writing SWM subtune-funktempo (only one subtune)
 TUNEDATA[DataIndex]=TUNETEMPO[0][0]|0x80; DataIndex++; //funktempo-program not supported in XM this simple way
 TUNEDATA[DataIndex]=0x84; DataIndex++; //won't be used anyway
 
 OutputSize=DataIndex;
 //write out array TUNEDATA to disk
 //if ( stringMatches(FilExt(OutputFileName),".swm") ) strcat(OutputFileName,".prg"); //if only .swm given, append .prg
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)
 LEwordToFile(0x3333); //-2. load-address of PRG format
 fwrite(TUNEDATA,OutputSize,sizeof(unsigned char),OutputFile); 
 printf("\n\n**** Conversion is complete. **** \nSWM file '%s' generated... :)",OutputFileName);
 fclose(OutputFile);
 
 //write VICE P00 too, easily loadable from VICE
 /* ChangeExt(OutputFileName,".P00"); 
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)
 WriteP00header(); //write 'C64File' header to file
 LEwordToFile(0x3333); //-2. load-address of PRG format
 fwrite(TUNEDATA,OutputSize,sizeof(unsigned char),OutputFile); 
 fclose(OutputFile); //printf("\n(And '%s.P00' as well for easy work.)",OutputFileName); */

 printf("\n-----------------------------------------------------\n");
}


//==========================================================================================
//------------------------------------------------------------------------------
void SetChordInstr() { //check for chord-instruments (will be expanded to tracks 4..8 in XM, generate polyphonic track in MIDI) 
 unsigned int i,j;
 for (i=1;i<=InstAmount;i++) {
  for(j=0; j<32; j++) { //examine WF/Arp table
   if (INSTRUMENTS[i][WFTABLEPOS+j*3]==0xFF) break;
   if (INSTRUMENTS[i][WFTABLEPOS+j*3+1]==0x7F) ChordIns[i]=INSTRUMENTS[i][SWI_DEFCHORD_POS]; //check if arp-column data is chord-jumping
   else ChordIns[i]=0;
  }
 }
 ChordIndex[0]=0; ChordIndex[1]=1; j=SWM_CHORD_NUMBER_MIN; //1; //j counts chords
 for(i=1; i <= ChordTableSize; i++) { //examine chord-lengths
  if ( (CHORDS[i]==0x7E) || (CHORDS[i]==0x7F) ) { //check chord-endsignal
   ChordLen[j]=i-ChordIndex[j]; j++; ChordIndex[j]=i+1;
  }
  //printf("\n%d,%.2X %d-%d ",i,CHORDS[i],ChordIndex[j-1],ChordLen[j-1]);
 }  //printf("\n%d\n",j);
 ChordAmount = j - SWM_CHORD_NUMBER_MIN;
}

unsigned char SWMtoXMslide(unsigned char SWMslide) {
 double XMslide=exp2(SWMslide/32);
 return (unsigned char) XMslide;
}

unsigned char XMtoSWMslide(unsigned char XMslide) {
 double SWMslide;
 if (XMslide==1) SWMslide=1.0; //workaround for log2()
 else SWMslide=log2(XMslide)*32;
 return (unsigned char) SWMslide;
}

unsigned int ATTACKtoXMtime(unsigned int AttackNibble) {
 return ATTACK_times[AttackNibble]/ADSR_time_ScaleRate;
}

unsigned char ATTACKtoSWMtime(unsigned int XMattack) {
 unsigned char i;
 for (i=0;i<0xF;i++)
 {
  if ( (ATTACK_times[i]/ADSR_time_ScaleRate) >= XMattack ) break;
 }
 return ((i)?i-1:i);
}

unsigned int DECAYtoXMtime(unsigned int DecayNibble, float SustainNibble) {
 return (DECAY_RELEASE_MaxTimes[DecayNibble]/ADSR_time_ScaleRate) * ((0xF-SustainNibble)/16);
}

unsigned char DECAYtoSWMtime(unsigned int XMdecay, unsigned char XMsustain) {
 unsigned char i;
 float SustainNibble=XMsustain/4; //convert range 0..3F to 0..F
 for (i=0;i<0xF;i++) {
  if ( ((DECAY_RELEASE_MaxTimes[i]/ADSR_time_ScaleRate) * ((0xF-SustainNibble)/16)) >= XMdecay ) break;
 }
 return ((i)?i-1:i);
}

unsigned int RELEASEtoXMtime(unsigned int ReleaseNibble, float SustainNibble) {
 return (DECAY_RELEASE_MaxTimes[ReleaseNibble]/ADSR_time_ScaleRate) * (SustainNibble/16);
}

unsigned char RELEASEtoSWMtime(unsigned int XMrelease, unsigned char XMsustain) {
 unsigned char i;
 float SustainNibble=XMsustain/4; //convert range 0..3F to 0..F
 for (i=0;i<0xF;i++) {
  if ( ((DECAY_RELEASE_MaxTimes[i]/ADSR_time_ScaleRate) * (SustainNibble/16)) >= XMrelease ) break;
 }
 return ((i)?i-1:i);
}


//=================== Common Functions ==============================================


char* FilExt(char *filename) { //if no '.' found, point to end of the string //get pointer of file-extension from filename string
 char* LastDotPos = strrchr(filename,'.');
 if (LastDotPos == NULL) return (filename+strlen(filename)); //make strcmp not to find match, otherwise it would be segmentation fault
 return LastDotPos;
}

void ChangeExt(char *filename,char *newExt) { //change the extension of the file 
 CutExt(filename); //omit original extension with 0 string-delimiter
 strcat(filename,newExt); //expand with new extension
}

void CutExt(char *filename) { //cut the extension of the filename by putting 0 at position of '.'
 *FilExt(filename)=0; //omit original extension with 0 string-delimiter
}

void WriteP00header() { //write 'C64File' header to file
 unsigned int i;
 char* P00ID="C64File";
 char* SWMextension=".SWM";
 fputs(P00ID,OutputFile);fputc(0,OutputFile);
 CutExt(OutputFileName); //remove prg/xm extension , but if .SWM extension is present, it will stay
 char * onlyname=OutputFileName; //init - will contain filename without the folders
 for(i=strlen(OutputFileName);i>0;i--) { if(OutputFileName[i]=='/' || OutputFileName[i]=='\\') {i++;onlyname=&OutputFileName[i];break;} } //skip file-path
 //printf("\n%s - %s",OutputFileName,onlyname);
 for(i=0;(i<C64_FILENAME_LENGTH_MAX)&&(onlyname[i]);i++) fputc(toupper(onlyname[i]),OutputFile);
 if ( strcmp(FilExt(OutputFileName),".swm") || i==C64_FILENAME_LENGTH_MAX )  //if too long filename, ensure .SWM extension in .P00 file
  {fseek(OutputFile,strlen(P00ID)+1+12,0); fputs(SWMextension,OutputFile); }
 fseek(OutputFile,VICE_P00_OFFSET,0); //go right after end of .P00 'C64File' header
}


//----------------------------------------------------------------------

void removeTrailingSpaces (char* string) {
 int i;
 for (i = strlen(string) - 1; i >= 0; --i) { if ( string[i] != ' ' ) {string[i+1]='\0'; break;} }
}

int getArrayMaxValueIndex (int* array, int count) {  //0 is the fallback index-value if every value is the same
 int i,Max=0,MaxIndex=0; for (i=0; i< count; ++i) { if (array[i] > Max) { Max = array[i]; MaxIndex = i; } }
 return MaxIndex;
}

int getArraySingleNonzeroIndex (int* array, int count) {
 int i, NonZeroCount = 0, NonZeroIndex=-1; //if no nonzero-value or more than one nonzeros, we'll return -1
 for (i=0; i < count; ++i) { if (array[i]) { ++NonZeroCount; NonZeroIndex=i; } }
 if (NonZeroCount != 1) return -1;
 return NonZeroIndex;
}


void WaitKeyPress() {
 //printf("\nPress ENTER key...\n\n");
 //getchar();
}

int byteToFile (unsigned char data) {
 return fputc( data, OutputFile );
}

char LowByte(int num) { //returns low-byte of integer
 return num-(num/256)*256; //integer division - no need for 'floor' function
}

char HiByte(int num) { //returns hi-byte of integer
 return (num/256); //integer division - no need for 'floor' function
}

int LEwordToFile(unsigned int DataToWrite) { //put word in little-endian to output-file
 fputc(LowByte(DataToWrite),OutputFile);
 return fputc(HiByte(DataToWrite),OutputFile);
}

int BEwordToFile(unsigned int DataToWrite) { //put word in big-endian to output-file
 fputc(HiByte(DataToWrite),OutputFile);
 return fputc(LowByte(DataToWrite),OutputFile);
}

unsigned int LEwordFromFile() { //read a little-endian word from input-file
 return ( fgetc(InputFile) + (fgetc(InputFile)*256) );
}

unsigned int BEwordFromFile() { //read a big-endian word from input-file
 return ( (fgetc(InputFile)*256) + fgetc(InputFile) );
}

int writeArrayToFile (char* buffer, int size) {
 return fwrite( buffer, size, sizeof(unsigned char), OutputFile );
}


unsigned long ReadVarLenNum() { //read variable-length encoded number from the file
 int i=0;
 unsigned char SevenBits, NumberOfDigits; //hopefully no more than 8 digits exist
 unsigned long VarLenNum=0;
 for(i=0; i<MIDI_VLV_LEN_MAX; i++) {
  SevenBits=fgetc(InputFile);
  Digits[i]=SevenBits&0x7F;
  if ((SevenBits&0x80)==0) break;
 }
 NumberOfDigits=i;
 for(i=0; i<=NumberOfDigits; i++) {
  VarLenNum+=((int)pow(128.00,(float)(NumberOfDigits-i)) * Digits[i]);
  //printf(" *%x-%x-%x",i,Digits[i],(int)pow(128.00,(float)(NumberOfDigits-i)));
 }
 //printf(" %x*",VarLenNum);
 return VarLenNum;
}

//==============================================================================
