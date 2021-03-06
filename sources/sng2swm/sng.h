// $Id: sng.h 360 2014-02-15 14:50:28Z soci $
#define CASTINT static_cast<int>
#define GT2_TAG_SIZE 4
#define GT2_NAME_SIZE 32
#define GT2_AUTHOR_SIZE 32
#define GT2_COPYRIGHT_SIZE 32


#define GT2_ORDERLIST_REPEAT    0xd0 //$d0..%df
#define GT2_ORDERLIST_TRANS_MIN 0xe0 //minimal value of $ef..$e0 down-transpose region
#define GT2_ORDERLIST_TRANS     0xf0 //base for transpose, means 0 transpose
#define GT2_ORDERLIST_TRANS_MAX 0xff //maximal value of %f0..%ff up-tanspose region
#define GT2_ORDERLIST_LOOPSONG  0xff //end of sequence, always followed by the loop-position

//GT2 patterns ('sng_pattern') start with their length (number of rows) and each line is a 'sng_pattern_row' structure described below...
//the last line of the patterns is the 0xFF (followed by 3 zeroes in the file)
#define GT2_PATTERN_ENDPATT          0x00
#define GT2_PATTERN_INS              0x00
#define GT2_INSTRUMENT_MAX           0x3F
#define GT2_PATTERN_FX               0x40
#define GT2_PATTERN_FXONLY           0x50

#define GT2_PATTERN_NOTE             0x60
#define GT2_PATTERN_FIRSTPACKEDREST  0xc0
#define GT2_PATTERN_REST             0xBD
#define GT2_PATTERN_KEYOFF           0xBE
#define GT2_PATTERN_KEYON            0xBF
#define GT2_PATTERN_END              0xFF
#define GT2_PATTERN_PACKEDREST       0x00

#define GT2_COMMAND_MAX 0xF //maximal fx-number
#define GT2_COMMAND_NOP          0x00
#define GT2_COMMAND_PORTUP       0x01
#define GT2_COMMAND_PORTDOWN     0x02
#define GT2_COMMAND_TONEPORT     0x03
#define GT2_VALUE_LEGATO         0x00 //used with tone-portamento
#define GT2_COMMAND_VIBRATO      0x04
#define GT2_COMMAND_AD           0x05
#define GT2_COMMAND_SR           0x06
#define GT2_COMMAND_WAVEREG      0x07
#define GT2_COMMAND_WAVETBL      0x08
#define GT2_COMMAND_PULSETBL     0x09
#define GT2_COMMAND_FILTERTBL    0x0a
#define GT2_COMMAND_FILTERCTRL   0x0b
#define GT2_COMMAND_FILTERCUT    0x0c
#define GT2_COMMAND_VOLUME       0x0d
#define GT2_COMMAND_FUNK         0x0e
#define GT2_COMMAND_TEMPO        0x0f


//instrument table data
#define GT2_ARP_REL_MAX      0x5F //$00..$5f - positive relative pitch-changes
#define GT2_ARP_REL_MIN      0x60 //$60..$7F - negative relative pitch-changes
#define GT2_ARP_NOP          0x80 
#define GT2_ARP_ABS_MIN      0x81 //from C#0 note onwards
#define GT2_ARP_ABS_MAX      0xDF //last absolute note
#define GT2_WFTABLE_FX       0xF0 //FX from $F0..$FE - only in GT
#define GT2_TABLE_JUMP       0xFF

#define GT2_TABLE_SETPULSE   0x80
#define GT2_TABLE_SETFILTER  0x80
#define GT2_TABLE_SETCUTOFF  0x00
#define GT2_INSTRUMENT_NAMESIZE 16

const char GT2_TAG[] = "GTS5";

struct sng_header
{
    char tag[GT2_TAG_SIZE];
    char name[GT2_NAME_SIZE];
    char author[GT2_AUTHOR_SIZE]; 
    char copyright[GT2_COPYRIGHT_SIZE];
    unsigned char subtunes;
};

struct sng_orderlist
{
    unsigned char length, restart;
    unsigned char * data;
};

struct sng_instrument
{
    unsigned char ad, sr, wv, pl, fl, vibspd, vibtim, gtoff, hr;
    char name[GT2_INSTRUMENT_NAMESIZE]; 
};

struct sng_table
{
    unsigned char length;
    unsigned char * left, * right;
};

struct sng_pattern_row
{
    unsigned char note;
    unsigned char instrument;
    unsigned char command;
    unsigned char parameter;
};

struct sng_pattern
{
    unsigned char rows;
    sng_pattern_row * data;
};
