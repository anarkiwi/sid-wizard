//==============================================================================
// SID-Wizard's SWM v1 module converter application (2013 Hermit Software)
//==============================================================================
// $Id: SWMconvert.c 382 2014-06-23 17:53:01Z hermitsoft $
//-----------------source-library inclusions------------------------------------
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>
//----------------------literate constant definitions---------------------------
#include "SWM-spec.src"
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
#define SID_CHNAMOUNT 3   //SID/SWM maximal amount of channels - hardwired
unsigned int XM_CHNAMOUNT=18; //number channels to represent converted SWM channels in XM
#define XM_COL_PER_TRK 5 //number of (byte) columns in one XM track (note,inst.,vol.,fx,fx-value
#define XM_SAMPLING_FREQUENCY 48000.00
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define CHORD_SILENCER_RATIO 2 //can be 1..4 - how much the chord-note loudness is smaller
#define VICE_P00_OFFSET 0x1A //vice program-offset (not containing PRG load-address following after)
#define VICE_P00_FILENAME_OFFSET 0x8
 char* SWM_ID="SWM1"; 
 char* SWS_ID="SWMS";
#define SWS_HILI_POS 0x15

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
#define XM_CHNAMOUNT_OFFSET XM_HEADERSIZE_OFFSET+8 //(l.e.word) Number of channels (2,4,6,8,10,...,32)
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

unsigned int InstAmount,PatternAmount,SequenceAmount,SubtuneAmount,ChordAmount;
unsigned int XMinstAmount;
unsigned long int SampleLength,FrameSampleLength,SampleLoopPos,LoopLength;
char AUTHORINFO[256]; //array to hold tune author-info
unsigned char SEQUENCES[32*SID_CHNAMOUNT*2][256]; //array to hold orderlist (3/6 sequences)
unsigned char SWM_SEQ_SIZE[256]; //array of SWM sequence-sizes
unsigned int SWM_SEQ_TOP;
unsigned char TUNETEMPO[32][2]; //subtune-tempos
unsigned char PATTERNS[256][256]; //array to hold SWM pattern-data
unsigned char PATTERNSIZES[256]; //array of effective SWM pattern-sizes
//unsigned char PACKEDSIZES[256]; //compressed pattern-sizes
unsigned char PATTERNLENGTHS[256]; //array of musical lengths of patterns
 unsigned char XMorderlist[256]; //result pattern-numbers of 3-to-1 orderlist conversion / imported XM orderlist
 unsigned long XMpatternOffset[XM_PATTERNS_MAX]; //offsets of patterns in XM-file
 unsigned int XMpatternLen[XM_PATTERNS_MAX];  //array of musical lengths (number of rows) of XM patterns
 unsigned int XMpatternSize[XM_PATTERNS_MAX]; //array of effective XM pattern-sizes
 unsigned char XMseqTemp[SID_CHNAMOUNT][256]; //groups of 3 SWM patterns put into XM patterns 
 unsigned char XMseqTranspose[SID_CHNAMOUNT][256]; //transpose-value of sequence-positions
 unsigned char XMseqPos[256]; //skew register for XM-orderlist, where seq-effects are left out
 unsigned char XMpattRef[SID_CHNAMOUNT][256]; //XM pattern-bunch references to the original SWM pattern-numbers
 unsigned char XMpattTranspose[SID_CHNAMOUNT][256]; //to detect if pattern-notes need to be transposed
 unsigned char XMpattData[256*32]; //temporary place for one pattern, before writing it to file
 unsigned char XMpattTemp[SID_CHNAMOUNT][256*XM_COL_PER_TRK]; //holds three temporarily read vertical unpacked XM patterns
 unsigned char XMPATTERNS[XM_PATTERNS_MAX][256*XM_COL_PER_TRK]; //holds registered (non-duplicated) vertical unpacked XM patterns
 unsigned char XMinstName[XM_INSTRUMENTS_MAX][XM_INSNAME_SIZE]; //XM instrument-names
 unsigned char XMinsVibDelay[XM_INSTRUMENTS_MAX],XMinsVibAmp[XM_INSTRUMENTS_MAX],XMinsVibFreq[XM_INSTRUMENTS_MAX];
 unsigned char XMinsSustain[XM_INSTRUMENTS_MAX]; 
 unsigned int XMinsAttack[XM_INSTRUMENTS_MAX], XMinsDecay[XM_INSTRUMENTS_MAX], XMinsRelease[XM_INSTRUMENTS_MAX];
 signed char XMinsTransp[XM_INSTRUMENTS_MAX];
 unsigned char SWMpattPointer[SID_CHNAMOUNT]={0,0,0}; //init non-linear SWM data pointer for SWM source
 unsigned char ResetFx[SID_CHNAMOUNT]={0,0,0}; //signs if an effect should be reset at note-start
unsigned char INSTRUMENTS[64][256]; //array to hold instruments
unsigned char ChordIns[64]; //0 if instrument is not chord, otherwise the default chord-number
unsigned char INSTNAMES[64][32]; //array of instrument-names
unsigned char CHORDS[256]; //array of chords
unsigned char ChordLen[64]; //length of chords
unsigned char ChordIndex[64]; //base-indexes/positions of chords inside chordtable
unsigned int ActSWMptn, ColumnCount, PackSignPos, ADSRpoint,SWMpatternCount,XMpattPos,SWMseqLength[SID_CHNAMOUNT];
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
unsigned char XMtoSWMvibAmp(unsigned char XMamp)
{
 int i;
 for (i=0; i<=15; i++) if(SWMtoXMvibAmp[i]>=XMamp) break; //find the matching SW-amplitude in the array
 return i;
}
unsigned char XMtoSWMvibFreq(unsigned char XMvib)
{
 int i;
 for (i=0; i<=15; i++) if(SWMtoXMvibFreq[i]<=XMvib) break; //find the matching SW-amplitude in the array
 if (i==16) i=4; //default vibrato for a zero-case 
 return i;
}

//-----------------function-prototype declarations------------------------------
void ProcessInput(); char* FilExt(char *filename); void ChangeExt(char *filename,char *newExt);
void ProcessSWM(char stereo); void ProcessXM(); void ProcessMIDI(); void ConvertTune(); 
void GenerateXM(); void GenerateMIDI(); void GenerateSWM(); void GerenateSWS(); void SWStoSWM();
void ProcessSWMver1(char stereo); void ProcessXM204(); void WaitKeyPress(); void DisplayInputInfo();
char LowByte(int num); char HiByte(int num); void CutExt(char *filename);
int LEwordToFile(unsigned int DataToWrite); int BEwordToFile(unsigned int DataToWrite);
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


//==============================================================================

int main(int argc, char *argv[])
{
 printf ("\n====================================\n"
         "SID-Wizard SWM1 converter (app v1.8)\n"
         "====================================\n");

 if ( (argc==2 && !strcmp(argv[1],"-help")) || argc==1 )
 {
  printf ("Converts to/from XM or MIDI formats,\n"
          "between .prg & .P00, and S00 to SID.\n"
          "And .swm to/from .sws (2SID/stereo) \n"
          "Usage syntax is pretty simple:      \n"
          "SWMconvert <inputfile> [outputfile] \n"
          "------------------------------------\n"
          "(Formats will depend on extensions.)\n"
          "If you don't specify an output-file,\n"
          "name of the input file will be used \n"
          "as output-filename. Existing files  \n"
          "will be overwritten automatically.  \n"
          "------------------------------------\n"
          " 2018 Hermit Soft. (Mihaly Horvath) \n"
          "====================================\n");
  printf("\nPress ENTER key...\n\n");
  getchar();
  exit(1);
 }
 else
 {
  InputFile = fopen(argv[1],"rb"); //!!!important "rb" is for binary (not to check text-control characters)
  if ( InputFile == 0 )
  {
   printf( "Could not open input-file...\n" );
   WaitKeyPress();exit(2);
  }
  else 
  {
   strcpy(InputFileName,argv[1]);
   if (argc==3) strcpy(OutputFileName,argv[2]);
   ProcessInput(); printf("\n\n**** Module processed. Going to Convert ****.\n---------------------------------------------\n");
   fclose(InputFile);
   ConvertTune();
  }
 }
  WaitKeyPress();
  return 0;
}

//==========================FUNCTIONS=====================================================


void ProcessInput()
{
 printf("Input-file name: %s \n",InputFileName);
 
 printf("Input-file extension: %s \n",FilExt(InputFileName));
 
 //read extension and decide if input is SWM or XM
 if ( !strcmp(FilExt(InputFileName),".SWM") || !strcmp(FilExt(InputFileName),".PRG") || !strcmp(FilExt(InputFileName),".swm") || !strcmp(FilExt(InputFileName),".prg") )
 {
  TYPE_ID_OFFSET=2; TYPE_ID_LENGTH=4;
  ProcessSWM(0);
 }
 else if ( !strcmp(FilExt(InputFileName),".P00") || !strcmp(FilExt(InputFileName),".p00") )
 {
  TYPE_ID_OFFSET=VICE_P00_OFFSET+2; TYPE_ID_LENGTH=4;
  if ( strlen(OutputFileName)==0 || strcmp(FilExt(OutputFileName),".prg") ) ProcessSWM(0); //only call ProcessSWM if the given output-file is not .prg
 }
 else if ( !strcmp(FilExt(InputFileName),".S00") ||  !strcmp(FilExt(InputFileName),".s00") )
 {
  printf("Converting .S00 format to .SID format.");
 }
 else if ( !strcmp(FilExt(InputFileName),".XM") || !strcmp(FilExt(InputFileName),".xm"))
 {
  TYPE_ID_OFFSET=XM_ID_OFFSET; TYPE_ID_LENGTH=XM_ID_SIZE;
  ProcessXM();
 }
 else if ( !strcmp(FilExt(InputFileName),".MID") || !strcmp(FilExt(InputFileName),".mid") )
 {
  TYPE_ID_OFFSET=MIDI_ID_OFFSET; TYPE_ID_LENGTH=MIDI_ID_SIZE;
  ProcessMIDI();
 } 
 else if ( !strcmp(FilExt(InputFileName),".SWS") || !strcmp(FilExt(InputFileName),".sws") )
 {
  TYPE_ID_OFFSET=2; TYPE_ID_LENGTH=4;
  ProcessSWM(1);
 }
 else
 { 
  printf("\n*** Unsupported file-extension... ***\n");
  fclose(InputFile);WaitKeyPress();exit(3);
 }
}

//==========================================================================================
void GenerateSWS()
{
 int i,j,k;
 printf("to SWS 2SID/stereo SID-Wizard worktune format. \n");
 printf("Output file-name will be: %s\n",OutputFileName);
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)

 LEwordToFile(0x3333); //-2. load-address of PRG format
 
 //write SWS header
 for (i=0;i<4;i++) fputc(SWS_ID[i],OutputFile);  //0-3. SWM-ID
 fputc(TUNEDATA[TYPE_ID_OFFSET+FSPEEDPOS],OutputFile);    //4.preferred framespeed
 for(i=0;i<SID_CHNAMOUNT*2;i++) fputc(0xFF,OutputFile); //5-$A.solo all tracks
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
 if (SequenceAmount != 0)
 {
  for (i=0; i<SubtuneAmount; i++)
  {
   for (j=0; j<SID_CHNAMOUNT; j++)     
   {
    for (k=0; k<SWM_SEQ_SIZE[i*3+j]; k++)
    {
     fputc(SEQUENCES[i*3+j][k],OutputFile);
    }
    fputc(SWM_SEQ_SIZE[i*3+j],OutputFile);
   }
   for (j=0; j<SID_CHNAMOUNT; j++)
   {
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
void SWStoSWM() //check MIDI type and version--------------------------------
{
 int i,j,k;
 printf("to SWM 1SID/mono SID-Wizard worktune format (with 2 subtunes). \n");
 printf("Output file-name will be: %s\n",OutputFileName);
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)

 LEwordToFile(0x3333); //-2. load-address of PRG format
 
 //write SWM header
 for (i=0;i<4;i++) fputc(SWM_ID[i],OutputFile);  //0-3. SWM-ID
 fputc(TUNEDATA[TYPE_ID_OFFSET+FSPEEDPOS],OutputFile);    //4.preferred framespeed
 fputc(TUNEDATA[TYPE_ID_OFFSET+SWS_HILI_POS],OutputFile); //5 step-highlighting amount
 for(i=0;i<2+SID_CHNAMOUNT;i++) fputc(0xFF,OutputFile); //6..7:unused, 8..$A:solo all tracks
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
 if (SequenceAmount != 0)
 {
  for (i=0; i<SubtuneAmount; i++)
  {
   for (j=0; j<SID_CHNAMOUNT; j++)     
   {
    for (k=0; k<SWM_SEQ_SIZE[i*6+j]; k++)
    {
     fputc(SEQUENCES[i*6+j][k],OutputFile);
    }
    fputc(SWM_SEQ_SIZE[i*6+j],OutputFile);
   }
   for (j=3; j<SID_CHNAMOUNT+3; j++)
   {
    for (k=0; k<SWM_SEQ_SIZE[i*6+j]; k++)
    {
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



//=================================================================================
void ConvertTune()  //convert to target format
{
 int Data,i;
 printf("Deciding on output-format and filename.\n");
 
 //read extension and decide if input is SWM or XM
 if (strlen(OutputFileName)==0) //if no output-file specified, give the same name and opposite/obvious extension
 { //no output-filename given
  printf("No output-file given. Using input-filename, appending extension.");
  strcpy(OutputFileName,InputFileName); 
  if ( !strcmp(FilExt(InputFileName),".XM") || !strcmp(FilExt(InputFileName),".xm") )
  { //no output given
   ChangeExt(OutputFileName,".swm.prg"); //append corresponding extension
   printf("\nConverting XM "); GenerateSWM();
  }
  else if ( !strcmp(FilExt(InputFileName),".MID") || !strcmp(FilExt(InputFileName),".mid") )
  { //no output given
   ChangeExt(OutputFileName,".swm.prg"); //append corresponding extension
   printf("\nConverting MIDI "); GenerateSWM();
  }  
  else if ( !strcmp(FilExt(InputFileName),".S00") || !strcmp(FilExt(InputFileName),".s00") ) //if input-format is .S00, converting it to .sid
  { //no output given
   ChangeExt(OutputFileName,".sid"); //append with new extension
   printf("\n*** Converting .S00 VICE C64-file to .sid file... ***");    
   InputFile = fopen(InputFileName,"rb"); //!!!important "rb" is for binary (not to check text-control characters)
   OutputFile = fopen(OutputFileName,"wb"); //!!!important "wb" is for binary (not to check text-control characters)
   fseek(InputFile,VICE_P00_OFFSET,0); //go right after end of .S00 'C64File' header (omit header from .seq)
   while( (Data=fgetc(InputFile)) != EOF ) { fputc(Data,OutputFile); } //copy source to target
   fclose(InputFile); fclose(OutputFile);
   printf("\n*** Conversion done, file '%s' is generated... ***\n",OutputFileName);
  } 
  else
  { //no output given
   ChangeExt(OutputFileName,".xm"); //append new extension
   printf("\nConverting SWM "); GenerateXM();
  }
 }
 else if ( !strcmp(FilExt(OutputFileName),".swm") || !strcmp(FilExt(OutputFileName),".prg") )
 { //output name & SWM extension given
  if ( !strcmp(FilExt(InputFileName),".xm") )  { printf("\nConverting XM "); GenerateSWM(); }
  else if ( !strcmp(FilExt(InputFileName),".mid") || !strcmp(FilExt(InputFileName),".MID") )  { printf("\nConverting MIDI "); GenerateSWM(); }
  else if ( !strcmp(FilExt(InputFileName),".sws") || !strcmp(FilExt(InputFileName),".SWS") )  { printf("\nConverting SWS "); SWStoSWM(); }
  else if ( !strcmp(FilExt(InputFileName),".SWM") || !strcmp(FilExt(InputFileName),".PRG") ||  !strcmp(FilExt(InputFileName),".swm") || !strcmp(FilExt(InputFileName),".prg") )  //what if input format is the same (SWM)?
  { printf("\n*** Are U crazy Man? SWM to SWM? ***\n"); WaitKeyPress(); exit(111); }
  else if ( !strcmp(FilExt(InputFileName),".P00") || !strcmp(FilExt(InputFileName),".p00") ) //if input-format is .P00, converting it to .prg
  {  
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
 else if ( !strcmp(FilExt(OutputFileName),".xm") )
 { //output name & XM extension given
  if ( !strcmp(FilExt(InputFileName),".xm") || !strcmp(FilExt(InputFileName),".XM") ) { printf("\n*** Are U crazy Man? XM to XM? ***\n"); WaitKeyPress(); exit(111); } //what if input format is the same
  else { printf("\nConverting SWM "); GenerateXM(); }
 }
 else if ( !strcmp(FilExt(OutputFileName),".MID") || !strcmp(FilExt(OutputFileName),".mid") ) 
 { //output name & MIDI extension given
  if ( !strcmp(FilExt(InputFileName),".xm") || !strcmp(FilExt(InputFileName),".XM") ) { printf("\n*** No XM to MIDI conversion support yet. ***\n"); WaitKeyPress(); exit(111); } //what if input format is the same
  else if ( !strcmp(FilExt(InputFileName),".MID") || !strcmp(FilExt(InputFileName),".mid") ) { printf("\n*** Are U crazy Man? MIDI to MIDI? ***\n"); WaitKeyPress(); exit(111); }
  else { printf("\nConverting SWM "); GenerateMIDI(); }
 } 
 else if ( !strcmp(FilExt(OutputFileName),".sws") )
 { //output name & SWS extension given
  if ( !strcmp(FilExt(InputFileName),".sws") || !strcmp(FilExt(InputFileName),".SWS") ) { printf("\n*** Are U crazy Man? SWS to SWS? ***\n"); WaitKeyPress(); exit(111); } //what if input format is the same
  else if (!strcmp(FilExt(InputFileName),".swm") || !strcmp(FilExt(InputFileName),".prg") || !strcmp(FilExt(InputFileName),".P00") ) 
  { printf("\nConverting SWM "); GenerateSWS(); }
 }
 else if ( !strcmp(FilExt(OutputFileName),".P00") || !strcmp(FilExt(OutputFileName),".p00") ) //.P00 VICE C64file output? convert .prg to .P00
 {//output name & P00 extension given
  if ( !strcmp(FilExt(InputFileName),".xm") )  { printf("\nConverting XM "); GenerateSWM(); }
  else if ( !strcmp(FilExt(InputFileName),".P00") || !strcmp(FilExt(InputFileName),".p00") ) 
   { printf("\n*** Are U crazy Man? P00 to P00? ***\n"); WaitKeyPress(); exit(111); }
  else if ( !strcmp(FilExt(InputFileName),".SWM") || !strcmp(FilExt(InputFileName),".PRG") || !strcmp(FilExt(InputFileName),".swm") || !strcmp(FilExt(InputFileName),".prg") )
  {
   printf("\n*** Converting .swm/.prg to .P00 VICE C64-file... ***");
   InputFile = fopen(InputFileName,"rb"); //!!!important "rb" is for binary (not to check text-control characters)
   OutputFile = fopen(OutputFileName,"wb"); //!!!important "wb" is for binary (not to check text-control characters)
   WriteP00header(); while( (Data=fgetc(InputFile)) != EOF ) { fputc(Data,OutputFile); }
   fclose(InputFile); fclose(OutputFile);
   printf("\n*** Conversion done, file '%s.P00' is generated... ***\n",OutputFileName);
  }
 }
 else //if output extension not found, automatic format (opposite of input-format)
 {
  if ( !strcmp(FilExt(InputFileName),".XM") || !strcmp(FilExt(InputFileName),".xm") )
  { //input-file is xm, no output-format/extension was given
   ChangeExt(OutputFileName,".swm.prg"); //append with corresponding extension
   printf("\nConverting XM "); GenerateSWM();
  }
  else if ( !strcmp(FilExt(InputFileName),".MID") || !strcmp(FilExt(InputFileName),".mid") )
  { //input-file is MIDI, no output-format/extension was given
   ChangeExt(OutputFileName,".swm.prg"); //append with corresponding extension
   printf("\nConverting MIDI "); GenerateSWM();
  }
  else if ( !strcmp(FilExt(InputFileName),".S00") || !strcmp(FilExt(InputFileName),".s00") ) //if input-format is .S00, converting it to .sid
  { //no output format/extension given
   ChangeExt(OutputFileName,".sid"); //append with new extension
   printf("\n*** Converting .S00 VICE C64-file to .sid file... ***");    
   InputFile = fopen(InputFileName,"rb"); //!!!important "rb" is for binary (not to check text-control characters)
   OutputFile = fopen(OutputFileName,"wb"); //!!!important "wb" is for binary (not to check text-control characters)
   fseek(InputFile,VICE_P00_OFFSET,0); //go right after end of .S00 'C64File' header (omit header from .seq)
   while( (Data=fgetc(InputFile)) != EOF ) { fputc(Data,OutputFile); } //copy source to target
   fclose(InputFile); fclose(OutputFile);
   printf("\n*** Conversion done, file '%s' is generated... ***\n",OutputFileName);
  } 
  else
  { //input-file is swm or P00, no output-format was given
   ChangeExt(OutputFileName,".xm"); //append with new extension
   printf("\nConverting SWM "); GenerateXM(); //preferred output-format is XM for SWM
  }
 }
}


//==========================================================================================
void ProcessSWM(char stereo)  //check SWM type and version---------------------------------
{
 unsigned int i=0; //counts bytes in loops
 
 printf("Processing SID-Wizard module.......\n");
 
  //display filesize and format
 fseek(InputFile, 0, SEEK_END); InputSize=ftell(InputFile); fseek(InputFile, 0, SEEK_SET);
 printf("Input-file size: %d bytes \n",InputSize);
 
 fread(TUNEDATA,sizeof(TUNEDATA),sizeof(unsigned char),InputFile);
 
 for(i=0; i<TYPE_ID_LENGTH; InputTypeID[i]=TUNEDATA[i+TYPE_ID_OFFSET],i++);
 printf("Input-filetype ID: \"%s\" (at offset %d) \n",InputTypeID,TYPE_ID_OFFSET);
 
 if ( !strcmp(InputTypeID, stereo?SWS_ID:SWM_ID) )
 {
  ProcessSWMver1(stereo);
 }
 else
 {
  printf("\n*** The module is of unknown format... ***\n");
  fclose(InputFile);WaitKeyPress();exit(4);
 }
}

//------------------------------------------------------------
void ProcessSWMver1(char stereo) //import SWM version 1.0 data to arrays
{
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
 ChordAmount=TUNEDATA[TYPE_ID_OFFSET+CHRDLEPOS];
 InstAmount=TUNEDATA[TYPE_ID_OFFSET+INSTAMPOS];
 printf("Number of subtunes   : %d (%d sequences) \n",SubtuneAmount,SequenceAmount);
 printf("Number of patterns   : %d \n",PatternAmount);
 printf("Number of instruments: %d \n",InstAmount);
 
 //fill arrays with SWM data read backwards
 printf("\n**** READING TUNE-DATA (backwards) ****\n---------------------------------------");
 DataIndex=InputSize-1; //point to end of tune-data
  
 //A.Subtune-funktempos
 printf("\nReading Subtune-tempos: ");
 for (i=SubtuneAmount-1; i>=0; i--)
 { 
  TUNETEMPO[i][1]=TUNEDATA[DataIndex]; printf("%x-",TUNEDATA[DataIndex]); DataIndex--;
  TUNETEMPO[i][0]=TUNEDATA[DataIndex]; printf("%x ",TUNEDATA[DataIndex]); DataIndex--;
 }
 
 //B.skip Tempotable (no tempotable in XM)
 printf("\nReading Tempo-programs. ");
 DataIndex -= TUNEDATA[TYPE_ID_OFFSET+TMPLENPOS];
 
 //C.Chord-table
 printf("\nReading Chords: -------------\n ");
 if (ChordAmount != 0)
 {
  for (i=ChordAmount; i>0; i--)
  {
   CHORDS[i]=TUNEDATA[DataIndex]; printf("%x ",TUNEDATA[DataIndex]);
   DataIndex--;
  }
 }
 
 //D.Instruments
 printf("\nReading Instruments: --------\n "); 
 if (InstAmount != 0)
 {
  for (i=InstAmount; i>0; i--)
  {
   DataIndex-=instnamelength; //point to beginning of instrument-name
   Size=TUNEDATA[DataIndex];  //before name there is the size of instrument
   INSTRUMENTS[i][Size]=0xff; //correct size-info back to delimiter
   for (j=0; j<instnamelength; j++) //display read instrument's name
   {
    INSTNAMES[i][j]=TUNEDATA[DataIndex+j+1]; printf("%c",TUNEDATA[DataIndex+j+1]);
   }
   printf("($%2X) ",Size); //display instrument-size after its name in brackets
   for (j=Size; j>=0; j--) //copy effective instrument-data
   {
    INSTRUMENTS[i][j]=TUNEDATA[DataIndex]; DataIndex--; //printf("%d,%2x ",j,INSTRUMENTS[i][j]);
   }
  }
 }

 //E.Patterns
 printf("\nReading Patterns: -----------");
 if (PatternAmount != 0)
 {
  for (i=PatternAmount; i>0; i--)
  {
   PATTERNLENGTHS[i]=TUNEDATA[DataIndex]; DataIndex--; 
   PATTERNSIZES[i]=Size=TUNEDATA[DataIndex]; DataIndex--;
   PATTERNS[i][Size-1]=0xff;  //set delimiter as pattern-endsignal
   printf("\n Pattern%d ($%2X rows, $%2X bytes): ",i,PATTERNLENGTHS[i],Size); //display pattern-number and size
   for (j=Size-2; j>=0; j--)
   {
    PatternData=TUNEDATA[DataIndex]; PreceedingData=TUNEDATA[DataIndex-1];
    //check for packed-NOP and handle it
    if ( (PACKEDMIN<=PatternData && PatternData<=PACKEDMAX) && (PreceedingData==0||(PACKEDMIN<=PreceedingData && PreceedingData<=PACKEDMAX)) )
    { //packed NOP
     //printf("p"); //sign in displayer that it's treated as packed NOP
     for (k=(PatternData-PACKEDMIN+1); k>0; k--) { PATTERNS[i][j]=0; j--; }
    }
    else
    {
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
 if (SequenceAmount != 0)
 {
  for (i=SequenceAmount-1; i>=0; i--)
  {
   Size=SWM_SEQ_SIZE[i]=TUNEDATA[DataIndex];DataIndex--;
   printf("\n Seq.%2X ($%2X bytes): ",i,Size);
   for (j=Size-1; j>=0; j--)
   {
    SEQUENCES[i][j]=TUNEDATA[DataIndex];
    printf("%02X ",TUNEDATA[DataIndex]); 
    DataIndex--;
   }
  }
 }
}



//==========================================================================================
void GenerateMIDI() //----- Export MIDI data from the imported SWM data ------------------
{
 unsigned int i,j,k,l,SWMpattPointer,MIDItrackPointer,DeltaCount,PPQN=0x60,PALpulses=PPQN/(4*6),RowDelta;
 unsigned char PrevChannel,PrevNote,PrevIns,PrevChord,WasGateOff,ActNote,ActInsFx,ActIns,ActFx,ActFxVal,ActChord;
 unsigned char TrackTemp[65536]; //pattern is collected here before writing to file, because size must be detected beforehands
 signed int InsOctave=0;
 signed char Transpose[SID_CHNAMOUNT];

 printf("to MIDI format 1 (contains 'MTrk' tracks for each SWM track). \n");
 printf("Output file-name will be: %s\n",OutputFileName);
 OutputFile = fopen(OutputFileName,"wb"); // !!!important "wb" is for binary (not to check text-control characters)
 
 //assemble MIDI header chunk
 fputs(MIDI_ID,OutputFile);       //put MIDI-ID to the output-file
 BEwordToFile(0);BEwordToFile(6); //MIDI header-size
 BEwordToFile(1);                 //MIDI version 1 (each track separated)
 BEwordToFile(SID_CHNAMOUNT);     //number of separate MIDI tracks
 BEwordToFile(PPQN);             //PPQN - MIDI pulses per quarter-note

 SetChordInstr();  //check for chord-instruments, set chords

 //assemble MIDI track chunks
 for(i=0;i<SID_CHNAMOUNT;i++)
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
 unsigned int i,j,k,l,MustClearChords[SID_CHNAMOUNT],RowCount;
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
 for (j=0;j<SID_CHNAMOUNT;j++)
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
 for (j=0;j<SID_CHNAMOUNT;j++) { XMpattRef[j][0]=XMseqTemp[j][0]; XMpattTranspose[j][0]=XMseqTranspose[j][0]; } //zero pattern always the 1st
 XMpatternCount=1;
  
 for (i=1;i<=XMsongLength;i++) //roll through XM-orderlist
 { 
  for (k=0; k<XMpatternCount;k++) //seek for match of all 3 patterns //and check if end of orderlist arrived
  {
   PtnMatch=0; //init match-detector (will be set to 1 if the bunch of 3 patterns has occured already)
   for (j=0; j<SID_CHNAMOUNT; j++) //check 3 channels in SWM, if new pattern-set or occured already
   {
    if (XMseqTemp[j][i] == 0xFF) strend=1; //check hardcoded end-signal (created in previous loop)
    else if ( (XMseqTemp[j][i]==XMpattRef[j][k]) && (XMseqTranspose[j][i]==XMpattTranspose[j][k]) ) PtnMatch++; 
   } //if counts up to 3, all channels have there's match with existing pattern-layout
   if (PtnMatch==SID_CHNAMOUNT) break;
  } 
  //here we got the info in 'PtnMatch' if new pattern is needed or reusing existing (PtnMatch==3)
  if (strend==1) break; //if we reached the end, don't register endsignal in pattern-list
  if (PtnMatch==SID_CHNAMOUNT) //if matches, just store existing pattern-number, 
  {
   XMorderlist[i]=k; //reuse existing pattern-layout
  }
  else
  {  //if no match, create new 3-pattern bunch  //register new pattern and give reference to original SWM patterns 
   for (j=0; j<SID_CHNAMOUNT; j++) { XMpattRef[j][XMpatternCount]=XMseqTemp[j][i]; XMpattTranspose[j][XMpatternCount]=XMseqTranspose[j][i]; } 
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
 
 LEwordToFile(XMsongLength); LEwordToFile(XMrestartPos); LEwordToFile(XM_CHNAMOUNT); 
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
  
  for (j=0;j<SID_CHNAMOUNT;j++) SWMpattPointer[j]=0; //init non-linear SWM data pointer for SWM source
  j=RowCount=0; strend=0;    // 'j' indexes XM pattern-data (SWMpattPointer[] indexes SWM pattern datas)
  while (strend==0) //pattern-array filling loop with 'j' index variable
  {

   //A. Scan a SWM row and set note/ins/fx variables
   for (k=0;k<SID_CHNAMOUNT;k++)
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
   for (k=0;k<SID_CHNAMOUNT;k++)
   {  
    if (ActNote[k] == 0xFF) strend=1; //end of pattern should stop the pattern's conversion	 
   }
   if (strend==0) RowCount++;

   for (k=0;k<XM_CHNAMOUNT;k++) //counts tracks/channels in SWM-patterns    //handle 4th or more column for XM-pattern!!!
   {
    if (k<SID_CHNAMOUNT) //branch where SWM channels exist
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
     l = k % SID_CHNAMOUNT; //SWM channel to test for chord in current column
     ChordPos=(k/SID_CHNAMOUNT)-1; //index inside chord
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
  InsSpeed=INSTRUMENTS[i][SWI_ARP_SPEED_POS]&0x3F; InsOctave=(signed char)INSTRUMENTS[i][SWI_OCTAVE_POS]/12;
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
 printf("Input-filetype ID: \"%s\" (at offset %d) , version: \"\%X\" \n",InputTypeID,TYPE_ID_OFFSET,InputXMver);
 
 if (!strcmp(InputTypeID,XM_ID))
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
 unsigned int i,j,k,l,db,PackSign, XMheaderSize=0x0104, SampleAmount, titlend=0;  
 for(i=0;i<40;AUTHORINFO[i]=0x20,i++); //init 
 unsigned int PtnMatch, InsHeaderSize=0; //PtnMatch is used to detect if new pattern matches an existing one
 unsigned long SampleIndex,SampleSize; //,InstOffset; 
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
 fseek(InputFile, XM_CHNAMOUNT_OFFSET, 0); XM_CHNAMOUNT=LEwordFromFile(); printf("\nNumber of channels: %d",XM_CHNAMOUNT);
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
   for (k=0;k<XM_CHNAMOUNT;k++) //k counts pattern-channels
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
    if (k<SID_CHNAMOUNT) //should be selectable which tracks of the XM to convert.
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
  for (k=0;k<SID_CHNAMOUNT;k++)
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
 
 //process XM instruments
 InstAmount=0; //init - will count instruments that are not empty
 for(i=0;i<XMinstAmount;i++)
 {
  printf("\nInstr. $%X: ",i); //display number of instrument
  InsHeaderSize=LEwordFromFile(); LEwordFromFile();  //offset 0 - how long is instrument-header?
  for (j=0;j<XM_INSNAME_SIZE;j++) { XMinstName[i][j]=fgetc(InputFile); printf("%c",XMinstName[i][j]); } //offset 4 - ins.name
  fgetc(InputFile); //offset $1A (26) - inst.type (dummy: 0)
  SampleAmount=LEwordFromFile();  //offset $1B (27) 
  
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
    printf("      (Sample $%X - $%x bytes)",k,(unsigned int)SampleSize);
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
 unsigned int PrevVelo,OKtrkAmount,MinimumDelta[SID_CHNAMOUNT],MaximumDelta[SID_CHNAMOUNT],i,j,k,l,BarPulses,NoteResolution; //counts bytes in loops
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
 printf("Input-filetype ID: \"%s\" (at offset %d) , version: \"\%X\" \n",InputTypeID,TYPE_ID_OFFSET,InputMIDIver);
 
 for(i=0;i<MIDI_CHANNELS_MAX;i++) MIDItrkIndex[i]=0; //init MIDI track/channel pointers before conversion
 
 if (!strcmp(InputTypeID,MIDI_ID))
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
 for(i=0;(i<SID_CHNAMOUNT)&&(i<OKtrkAmount);i++) //only care about 1st 3 channels wchich contain notes
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
 for(i=0; i<SID_CHNAMOUNT; i++) //only care about 1st 3 channels wchich contain notes
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
 for(i=0;(i<SID_CHNAMOUNT) && (i<OKtrkAmount); i++)
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
 //  (ChordAmount - number of possibly imported chords)
 //  (SequenceAmount - number of sequences - now hardwired to SID_CHNAMOUNT=3 below)
 //  InstAmount - amount of imported instruments
 //  AUTHORINFO[] - assembled author-text, first $28 characters will be used for SWM output...
 //  XMsongLength - orderlist-length (in case of Goattracker SNG it should be a 3-element array instead)
 //  XMpattRef[] - 
 //  XMorderlist[] - converts to SEQUENCES[i][j] and SWMseqLength[i] then TUNEDATA[];
 //  XMPATTERNS[i][j+0..4] - pattern-arrays with these columns of j: note,instrument,volume,fx,fxvalue
 //  etc...                - converted to PATTERNS[i][SWMpattIndex], PATTERNLENGTHS[],PATTERNSIZES[], then TUNEDATA[]
 //  conclusion: NOT SUITABLE TO PRECISELY CONVERT FROM GOATTRACKER .SNG FORMAT. USING CONRAD'S CODE INSTEAD.
 */

 unsigned int i,j,DataIndex=0,SWMpattIndex=0;
 unsigned char SWM_Note=0,SWM_Ins=0,SWM_Fx=0,SWM_FxVal=0,PrevInst=0,PrevFx=0,PrevTempo=0,PrevVib=0,NOPcount=0,PattData=0,PrevData=0,PrevSlide=0;
 
 printf("to SWM (SID-WIZARD v1.0) format. \n");
 printf("Output file-name will be: %s\n",OutputFileName); 
 
 //---------- assembling TUNEDATA array before writing to file ---------
 OutputSize=tuneheadersize; for(i=0;i<TUNEDATA_SIZE_MAX;TUNEDATA[i]=0,i++); //init
 SequenceAmount=SID_CHNAMOUNT; //only 3, because only 1 subtune gets generated
 PatternAmount=(SWMpatternCount<PATT_MAX) ? SWMpatternCount : PATT_MAX;
 ChordAmount=0; //might be used later

 //write SWM header
 for (i=0;SWM_ID[i];i++) TUNEDATA[i]=SWM_ID[i];  //0-3. SWM-ID
 TUNEDATA[FSPEEDPOS]=1;    //4.preferred framespeed
 TUNEDATA[SWM_HILI_POS]=4; //5.preferred step-highlighting amount - should depend on patternlength
 TUNEDATA[SWM_AUTO_POS]=1; //6.preferred auto-advance amount
 TUNEDATA[SWM_CBIT_POS]=1; //7.track-binding ON  (, other 'confbit' newsettings are 'OFF')
 for(i=0;i<SID_CHNAMOUNT;i++) TUNEDATA[SWM_MUTE_POS+i]=0xFF; //8-$A.solo all tracks
 TUNEDATA[SWM_DEFP_POS]=DEFAULTPTNLEN; //$B. default pattern-length - should depend on XM patternlengths
 TUNEDATA[SEQAMOPOS]=SequenceAmount; //$C. sequence-amount
 TUNEDATA[PTAMOUPOS]=PatternAmount; //$D. pattern-amount calculated in pattern-converter
 TUNEDATA[INSTAMPOS]=InstAmount;    //$E. number of instruments 
 TUNEDATA[CHRDLEPOS]=ChordAmount;   //$F. chordtable-length (calculated in converter)
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
 //if ( !strcmp(FilExt(OutputFileName),".swm") ) strcat(OutputFileName,".prg"); //if only .swm given, append .prg
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
void SetChordInstr()  //check for chord-instruments (will be expanded to tracks 4..8 in XM, generate polyphonic track in MIDI) 
{
 unsigned int i,j;
 for (i=1;i<=InstAmount;i++)
 {
  for(j=0;j<32;j++) //examine WF/Arp table
  {
   if (INSTRUMENTS[i][WFTABLEPOS+j*3]==0xFF) break;
   if (INSTRUMENTS[i][WFTABLEPOS+j*3+1]==0x7F) ChordIns[i]=INSTRUMENTS[i][SWI_DEFCHORD_POS]; //check if arp-column data is chord-jumping
   else ChordIns[i]=0;
  }
 }
 ChordIndex[0]=0; ChordIndex[1]=1; j=1; //j counts chords
 for(i=1;i<=ChordAmount;i++) //examine chord-lengths
 {
  if ( (CHORDS[i]==0x7E) || (CHORDS[i]==0x7F) ) //check chord-endsignal
  {
   ChordLen[j]=i-ChordIndex[j]; j++; ChordIndex[j]=i+1;
  }
  //printf("\n%d,%d %d-%d ",i,CHORDS[i],ChordIndex[j-1],ChordLen[j-1]);
 }
}

unsigned char SWMtoXMslide(unsigned char SWMslide)
{
 double XMslide=exp2(SWMslide/32);
 return (unsigned char) XMslide;
}

unsigned char XMtoSWMslide(unsigned char XMslide)
{
 double SWMslide;
 if (XMslide==1) SWMslide=1.0; //workaround for log2()
 else SWMslide=log2(XMslide)*32;
 return (unsigned char) SWMslide;
}

unsigned int ATTACKtoXMtime(unsigned int AttackNibble)
{
 return ATTACK_times[AttackNibble]/ADSR_time_ScaleRate;
}

unsigned char ATTACKtoSWMtime(unsigned int XMattack)
{
 unsigned char i;
 for (i=0;i<0xF;i++)
 {
  if ( (ATTACK_times[i]/ADSR_time_ScaleRate) >= XMattack ) break;
 }
 return ((i)?i-1:i);
}

unsigned int DECAYtoXMtime(unsigned int DecayNibble, float SustainNibble)
{
 return (DECAY_RELEASE_MaxTimes[DecayNibble]/ADSR_time_ScaleRate) * ((0xF-SustainNibble)/16);
}

unsigned char DECAYtoSWMtime(unsigned int XMdecay, unsigned char XMsustain)
{
 unsigned char i;
 float SustainNibble=XMsustain/4; //convert range 0..3F to 0..F
 for (i=0;i<0xF;i++)
 {
  if ( ((DECAY_RELEASE_MaxTimes[i]/ADSR_time_ScaleRate) * ((0xF-SustainNibble)/16)) >= XMdecay ) break;
 }
 return ((i)?i-1:i);
}

unsigned int RELEASEtoXMtime(unsigned int ReleaseNibble, float SustainNibble)
{
 return (DECAY_RELEASE_MaxTimes[ReleaseNibble]/ADSR_time_ScaleRate) * (SustainNibble/16);
}

unsigned char RELEASEtoSWMtime(unsigned int XMrelease, unsigned char XMsustain)
{
 unsigned char i;
 float SustainNibble=XMsustain/4; //convert range 0..3F to 0..F
 for (i=0;i<0xF;i++)
 {
  if ( ((DECAY_RELEASE_MaxTimes[i]/ADSR_time_ScaleRate) * (SustainNibble/16)) >= XMrelease ) break;
 }
 return ((i)?i-1:i);
}

//=================== Common Functions ==============================================

char* FilExt(char *filename) //get pointer of file-extension from filename string
{  //if no '.' found, point to end of the string
 char* LastDotPos = strrchr(filename,'.');
 if (LastDotPos == NULL) return (filename+strlen(filename)); //make strcmp not to find match, otherwise it would be segmentation fault
 return LastDotPos;
}

void ChangeExt(char *filename,char *newExt) //change the extension of the file 
{
 CutExt(filename); //omit original extension with 0 string-delimiter
 strcat(filename,newExt); //expand with new extension
}

void CutExt(char *filename) //cut the extension of the filename by putting 0 at position of '.'
{
 *FilExt(filename)=0; //omit original extension with 0 string-delimiter
}

void WriteP00header() //write 'C64File' header to file
{
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
void WaitKeyPress()
{
 //printf("\nPress ENTER key...\n\n");
 //getchar();
}

char LowByte(int num) //returns low-byte of integer
{
 return num-(num/256)*256; //integer division - no need for 'floor' function
}

char HiByte(int num) //returns hi-byte of integer
{
 return (num/256); //integer division - no need for 'floor' function
}

int LEwordToFile(unsigned int DataToWrite) //put word in little-endian to output-file
{
 fputc(LowByte(DataToWrite),OutputFile); 
 return fputc(HiByte(DataToWrite),OutputFile); 
}

int BEwordToFile(unsigned int DataToWrite) //put word in big-endian to output-file
{
 fputc(HiByte(DataToWrite),OutputFile);  
 return fputc(LowByte(DataToWrite),OutputFile);
}

unsigned int LEwordFromFile() //read a little-endian word from input-file
{
 return ( fgetc(InputFile) + (fgetc(InputFile)*256) );
}

unsigned int BEwordFromFile() //read a big-endian word from input-file
{
 return ( (fgetc(InputFile)*256) + fgetc(InputFile) );
}

unsigned long ReadVarLenNum() //read variable-length encoded number from the file
{
 int i=0;
 unsigned char SevenBits, NumberOfDigits; //hopefully no more than 8 digits exist
 unsigned long VarLenNum=0;
 for(i=0; i<MIDI_VLV_LEN_MAX; i++) 
 {
  SevenBits=fgetc(InputFile);
  Digits[i]=SevenBits&0x7F;
  if ((SevenBits&0x80)==0) break;
 }
 NumberOfDigits=i;
 for(i=0; i<=NumberOfDigits; i++)
 {
  VarLenNum+=((int)pow(128.00,(float)(NumberOfDigits-i)) * Digits[i]);
  //printf(" *%x-%x-%x",i,Digits[i],(int)pow(128.00,(float)(NumberOfDigits-i)));
 }
 //printf(" %x*",VarLenNum);
 return VarLenNum;
}

//==============================================================================
