//A simple RLE, dictionary-based pattern-matching, and arithmetic-coding based compressor by Hermit
//Includable in other programs (with compression.h) to compress data.


#include "compression.h"

#if (COMPRESSION_PROGRESS)
 #include <sys/time.h>  //for gettimeofday() to print elapsed seconds
#endif


extern const unsigned char _binary_selfextract_prg_start [], _binary_selfextract_prg_end [];
//extern const unsigned char _binary_selfextract_rle_prg_start [], _binary_selfextract_rle_prg_end [];

extern const unsigned char _binary_selfextract_nomove_prg_start [], _binary_selfextract_nomove_prg_end [];
//extern const unsigned char _binary_selfextract_rle_nomove_prg_start [], _binary_selfextract_rle_nomove_prg_end [];


#if (COMPRESSION_DEBUG_VERIFICATION)  //usable to calculate the in-place extraction offset after compressoin (overlaps can be detected)
 int Compression_CompressedDataIndexes [COMPRESSION_BUFFERSIZE_MAX];  //indexes in compressed data corresponding to indexes in output-data
#endif



static inline int Compression_getSetCompressMode (int mode) {  //using scoped variable for mode instead of littering the existing API with extra arguments
 static int CompressionMode = COMPRESSION_MODE__NORMAL;
 if (mode != COMPRESSION_MODE__GET) CompressionMode = mode;
 return CompressionMode;
}

static inline void Compression_setCompressMode (int mode) { Compression_getSetCompressMode( mode ); }

static inline int Compression_getCompressMode () { return Compression_getSetCompressMode( COMPRESSION_MODE__GET ); }

static inline int Compression_compressModeInPlace () { return Compression_getCompressMode() == COMPRESSION_MODE__IN_PLACE; }

static inline int Compression_compressModeSFX () { return Compression_getCompressMode() == COMPRESSION_MODE__SELF_EXTRACT; }
static inline int Compression_compressModeRLESFX () { return Compression_getCompressMode() == COMPRESSION_MODE__RLE_SELF_EXTRACT; }



static inline void Compression_writeWord (unsigned char* output, int index, register unsigned int value) { //write little-endian word
 output[ index + 0 ] = value & COMPRESSION_LOWBYTE_MASK; output[ index + 1 ] = value >> COMPRESSION_HIGHBYTE_SHIFTS;
}


static inline int Compression_writeVariableLengthNumber (register unsigned int value, unsigned char* output) {
 register int ByteCount = 0;  //little-endian variable-length number encoding (used in RLE and pattern-match prefixes)
 do {
  if (output != NULL) output[ ByteCount ] = (value << COMPRESSION_VARIABLE_LENGTH_NUMBER_BYTE_VALUESHIFTS) & COMPRESSION_VARIABLE_LENGTH_NUMBER_BYTE_VALUEMASK;
  value >>= COMPRESSION_VARIABLE_LENGTH_NUMBER_BYTE_DIVSHIFTS;  //shifting out written bits
  if (value != 0x0000 && output != NULL) output[ ByteCount ] |= COMPRESSION_VARIABLE_LENGTH_NUMBER_NEXTBYTE_SIGN_BITVALUE;  //still have '1' bits?
  ++ByteCount;
 }
 while (value != 0x0000);
 return ByteCount;
}


static inline int Compression_writeVariableLengthNumberWithSFXcheck (register unsigned int value, unsigned char* output) {
 int ByteCount = 0;
 if ( Compression_compressModeSFX() /*|| Compression_compressModeRLESFX()*/ ) {  //C64-based SFX/MEM has special max. 2byte variable-length number more effective for max value of 128*256 = 32768
  if (output != NULL) {
   output[ ByteCount ] = (value << COMPRESSION_VARIABLE_LENGTH_NUMBER_BYTE_VALUESHIFTS) & COMPRESSION_VARIABLE_LENGTH_NUMBER_BYTE_VALUEMASK;
  }
  value >>= COMPRESSION_VARIABLE_LENGTH_NUMBER_BYTE_DIVSHIFTS;
  if (value != 0x0000) {
   if (value > COMPRESSION_VALUEMAX__BYTE) return COMPRESSION_SFX_VARIABLE_LENGTH_NUMBER_SIZE_UNSUPPORTED;
   if (output != NULL) output[ ByteCount ] |= COMPRESSION_VARIABLE_LENGTH_NUMBER_NEXTBYTE_SIGN_BITVALUE;
   ++ByteCount;
   if (output != NULL) output[ ByteCount ] = value & COMPRESSION_VALUEMASK__BYTE;
  }
  ++ByteCount;
 }
 else if ( Compression_compressModeRLESFX() ) {  //RLE-SFX self-extraction code has space for handling only 1byte (0...255) repeat-count (COMPRESSION_SFX_RLE_SEQUENCE_LENGTH_MAX)
  output[0] = value; return 1;
 }
 else ByteCount = Compression_writeVariableLengthNumber( value, output );
 return ByteCount; //( !Compression_compressModeSFX() || ByteCount <= COMPRESSION_PATTERNMATCH_SFX_VARIABLE_LENGTH_NUMBER_SIZE_MAX)
                       //? ByteCount : COMPRESSION_PATTERNMATCH_VARIABLE_LENGTH_NUMBER_SIZE_UNSUPPORTED;
}



int Compression_compressValueRepeatsWithSizeStats (unsigned char* inputdata, int inputsize, unsigned char* outputdata
                                                   , int* sizediff_min, int* sizediff_max, int* outdata_indexes) {
 int i, j, Temp, CheckBase, RepeatCount, OutputSize;  //RunLength-encoding (RLE) compression
 int InOutSizeDiff=0, InOutSizeDiffMin = COMPRESSION_SIGNED_VALUE_MAX, InOutSizeDiffMax = 0;

 for (i = OutputSize = InOutSizeDiff = CheckBase = RepeatCount = 0; i < inputsize + 1; ++i) {  //'+1' is for End-of-Data-detection (as a 'change' of value)
  if ( inputdata[i] == inputdata[ CheckBase ] && CheckBase < i && i < inputsize /*&& RepeatCount < COMPRESSION_RLE_REPEATCOUNT_MAX*/  //matching (and not end of data), beginning escape-sequence would be shorter if (i > CheckBase) was ruled out here. this fix breaks format backward-compatibility
       && !( Compression_compressModeRLESFX() && RepeatCount >= COMPRESSION_SFX_RLE_SEQUENCE_LENGTH_MAX /*- 1*/ ) ) {  //checking max makes it a bit faster on C64 (not checking very long RLE-sequences unnecessarily)
   if (outdata_indexes != NULL) outdata_indexes[i] = OutputSize;  //get some internal statistics
   outputdata[ OutputSize++ ] = inputdata[i]; ++RepeatCount;
  }
  else {  //not matching (or end of data)
   if (RepeatCount < COMPRESSION_RLE_REPEATCOUNT_COMPRESSION_THRESHOLD) {
    CheckBase = i;
    if (i < inputsize) {
     if (outdata_indexes != NULL) outdata_indexes[i] = OutputSize;  //get some internal statistics
     outputdata[ OutputSize++ ] = inputdata[i];
    }
   }
   else {  //Itt a lenyeg:  :)
    OutputSize -= (RepeatCount - COMPRESSION_RLE_REPEATCOUNT_COMPRESSION_THRESHOLD);
     Temp = CheckBase;  CheckBase = i;  i -= 1;
    OutputSize += Compression_writeVariableLengthNumberWithSFXcheck( RepeatCount - COMPRESSION_RLE_REPEATCOUNT_COMPRESSION_THRESHOLD, &outputdata[OutputSize] );
     if (outdata_indexes != NULL) { for (j = Temp; j < CheckBase; ++j) { outdata_indexes[j] = OutputSize-1; } }  //set all statistics (output-indexes) which were substituted by repeat-counter to last byte of stored repeatcount
   }
   RepeatCount = 0;
  }
  if (i < inputsize) InOutSizeDiff = (i + 1) - OutputSize;  //positive if current output smaller than input (we got compression)
  if (InOutSizeDiff < InOutSizeDiffMin) InOutSizeDiffMin = InOutSizeDiff;
  if (InOutSizeDiff > InOutSizeDiffMax) InOutSizeDiffMax = InOutSizeDiff;  //if (InOutSizeDiff < 0) { printf("RLE: in:%d - out:%d = %d\n",i+1,OutputSize,InOutSizeDiff); for(j=0;j<=i;++j) printf("%.2X ",inputdata[j]); printf(" -> "); for(j=0;j<OutputSize;++j) printf("%.2X ",outputdata[j]); printf("\n"); }
 }  if (COMPRESSION_VERBOSE) printf("RLE:          in:%d - out:%d = diff:%d  (diff-min:%d, diff-max:%d)\n",inputsize,OutputSize,inputsize-OutputSize,InOutSizeDiffMin,InOutSizeDiffMax);

 *sizediff_min = InOutSizeDiffMin; *sizediff_max = InOutSizeDiffMax;
 return OutputSize;
}


int Compression_compressValueRepeats (unsigned char* inputdata, int inputsize, unsigned char* outputdata) { //RunLength-encoding (RLE) compression
 static int DummyInt;
 return Compression_compressValueRepeatsWithSizeStats( inputdata, inputsize, outputdata, &DummyInt, &DummyInt, NULL );
}



static inline unsigned char Compression_convertPrefixType (register unsigned int value) {
 return (value & 0xC0) ?  ((value & 0x3F) << 2) | ((value & 0x80) >> 7) | ((value & 0x40) >> 5)
          : ( (value & 0x20) ?  ((value & 0x1F) << 3) | ((value & 0x20) >> 3)
            : ((value & 0xF) << 4) | ((value & 0x10) >> 1) );
}


static inline int Compression_writePrefixType (register unsigned int value, unsigned char** output) {
 if (*output == NULL) return 0;
 **output = ( Compression_compressModeSFX() ) ? Compression_convertPrefixType( value ) : value;  //SFX-mode has different prefix-type bit-positions (type-bits are LSBs for smaller C64 decision tree with 'lsr-bcs' pairs, and values without the need to AND-masking)
 ++(*output);
 return 1;  //byte-count of 1st prefix-byte is always 1
}



static inline int Compression_writeSequencePrefix (register unsigned int offset, register unsigned int length, unsigned char* output) {
 register int ByteCount = 1, Temp, Distance = offset - length;  //int i; unsigned char* OutputBase = output;  //Distance points to the end of match, not the beginning (better)

 if (offset == COMPRESSION_PATTERNMATCH_OFFSET_VALUE__LITERAL_SEQUENCE) {  //write literal sequence prefix
  if (length > COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_LITERAL_LENGTH_MAX) {  //write big-length prefix  (if output is NULL we don't write any output, just calculate prefix-length)
   Compression_writePrefixType( COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__LITERAL_NEXTBYTE, &output );
   if ( Compression_compressModeSFX() ) { ++ByteCount; if (output != NULL) { *output = length; /*++output;*/ } }
   else {
    Temp = Compression_writeVariableLengthNumber( length - COMPRESSION_PATTERNMATCH_PREFIX_BYTE2_LITERAL_LENGTH_MIN, output );
    ByteCount += Temp; //if (output != NULL) output += Temp;
  }}
  else Compression_writePrefixType( length - COMPRESSION_PATTERNMATCH_LITERAL_BYTE1_LENGTH_VALUEDIFF, &output );  //write small-length prefix
 }
 else if (offset == COMPRESSION_PATTERNMATCH_OFFSET_VALUE__RLE_SEQUENCE) {  //same as COMPRESSION_PATTERNMATCH_OFFSET_VALUE__MATCH_REPEAT
  Compression_writePrefixType( COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__RLE, &output );
  if ( Compression_compressModeSFX() ) { ++ByteCount; if (output != NULL) { *output = length; /*++output;*/ } }
  else {
   Temp = Compression_writeVariableLengthNumber( length - COMPRESSION_PATTERNMATCH_PREFIX_RLE_LENGTH_MIN, output );
   ByteCount += Temp; //if (output != NULL) output += Temp;
 }}
 else { //write pattern-match prefix
  if (length > COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_BIGGER_MATCHLENGTH_MAX) {  //write prefix with big-length and Distance
   Compression_writePrefixType( Distance > 0 ? COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__BIG_MATCHLENGTH_NEXTBYTE : COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__BIG_MATCHLENGTH_NEXTBYTE_DISTANCE0, &output );
   if ( Compression_compressModeSFX() ) {
    if (output != NULL) {  //subtracting min-value just like with matchlen5..19 makes C64 SFX-code a bit shorter and enhances length-range a bit:
     *output = length;  if (Distance > 0) *output -= COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_BIG_MATCHLENGTH_VALUEDIFF;
     ++output;
    } ++ByteCount;
   }
   else { //normal (non-SFX) length:
    Temp = Compression_writeVariableLengthNumber( length - COMPRESSION_PATTERNMATCH_PREFIX_BYTE2_BIGGEST_MATCHLENGTH_MIN, output );
    ByteCount += Temp; if (output != NULL) output += Temp;
   }
   if (Distance > 0) {  //filt out special case (with special prefix set above) when Distance is 0
    Temp = Compression_writeVariableLengthNumberWithSFXcheck( Distance, output );
    ByteCount += Temp; //if (output != NULL) output += Temp;
   }
  }
  else if ( length >= COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_BIG_MATCHLENGTH_MIN  //write smaller-length prefix and Distance
            /*|| (length==4 && Distance > COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_MATCHLENGTH4_DISTANCE_MAX)*/ ) {
   Compression_writePrefixType( (length - COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_BIG_MATCHLENGTH_VALUEDIFF) | COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__BIG_MATCHLENGTH_BASE, &output );
   Temp = Compression_writeVariableLengthNumberWithSFXcheck( Distance, output );
   ByteCount += Temp; //if (output != NULL) output += Temp;
  }
  else {  //write prefix for really small-matchlengths (3 with any distance, 2 with small distance, 4 with big distance)
   switch (length) {  //write really small-length 1-byte prefix or if Distance doesn't fit, big-distance prefix
    case 0: case 1: return COMPRESSION_PATTERNMATCH_INVALID_PREFIX_SIZE_VALUE /*length+1*/;  //if came here by e.g. checking for prefix-size, this should return a value that's invalidating unnecessary compression of 0 or 1 byte
    case 2: {  //matchlength2 prefix is only available in 1-byte form (others aren't worth substituting/compressing)
     if (Distance >= COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_MATCHLENGTH2_DISTANCE_MAX ) {  //this normally never happens if configs/settings are right
      if (output != NULL && COMPRESSION_DEBUG) printf( "Matchlength2 has no long-distance prefix variant (not worth compressing): offset:%d (Distance:%d)\n", offset, Distance ); //only print error in compression, not in prefix-length detection
      return COMPRESSION_PATTERNMATCH_INVALID_PREFIX_SIZE_VALUE /*length+1*/;
     }
     Distance += COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_MATCHLENGTH2_DISTANCE_BASE;  //likening length2 to length3 for shorter code in SFX asm-code causes shift (and shortening) in the value-range by 1, no much difference in compression-ratio
     Compression_writePrefixType( Distance | COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__MATCHLENGTH2__BASE, &output );
    } break;
    case 3: {
     if (Distance > COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_MATCHLENGTH3_DISTANCE_MAX) {  //small-matchlength 3 but Distance bigger than 62
      if ( !Compression_compressModeSFX() ) Distance -= COMPRESSION_PATTERNMATCH_PREFIX_MATCHLENGTH3_BIG_DISTANCE_MIN;  //further optimization of this case, range starts from 63, a bit less chance of the need for another high-byte (above distance 64*128=8192)
      Compression_writePrefixType( (Distance & COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_MATCHLENGTH3_DISTANCE_MASK) | COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__MATCHLENGTH3_WITH_NEXTBYTE__BASE, &output );
      /*if ( Compression_compressModeSFX() ) {
       Temp = Compression_writePatternMatchVariableLengthNumber( Distance >> COMPRESSION_PATTERNMATCH_PREFIX_BYTE2_MATCHLENGTH3_DISTANCE_DIVSHIFTS, output );
       ByteCount += Temp; //if (output != NULL) output += Temp;
      }*/
      //else {  //optimization for length3 to double the distance-range to 16384 using bit7 (var.len wouldn't be worth compressing, using bit7)
       if (Distance >> COMPRESSION_PATTERNMATCH_PREFIX_BYTE2_MATCHLENGTH3_DISTANCE_DIVSHIFTS >= COMPRESSION_VALUERANGE__BYTE) return COMPRESSION_PATTERNMATCH_INVALID_PREFIX_SIZE_VALUE;
       if (output != NULL) { *output = Distance >> COMPRESSION_PATTERNMATCH_PREFIX_BYTE2_MATCHLENGTH3_DISTANCE_DIVSHIFTS; ++output; }
       ByteCount += 1; //if (output != NULL) output += Temp;
      //}
     }
     else { //small-matchlength 3 and Distance smaller than 63
      Distance += COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_MATCHLENGTH3_DISTANCE_BASE;  //introduced END_OF_DATA prefix shifts the value-range by 1
      Compression_writePrefixType( Distance | COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__MATCHLENGTH3__BASE, &output );
     }
    } break;
    case 4: {
     //if (Distance > COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_MATCHLENGTH4_DISTANCE_MAX) { //small-matchlength 4 but bigger than 31 Distance
      Compression_writePrefixType( (Distance & COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_MATCHLENGTH4_DISTANCE_MASK) | COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__MATCHLENGTH4_WITH_NEXTBYTE__BASE, &output );
      //if ( Compression_compressModeSFX() ) {  //disabling emitting a variable-length number for length4 for simpler SFX-extractor code, and using bit7 enhances range to a doubled 0..8192 too as a bonus
       if (Distance >> COMPRESSION_PATTERNMATCH_PREFIX_BYTE2_MATCHLENGTH4_DISTANCE_DIVSHIFTS >= COMPRESSION_VALUERANGE__BYTE) return COMPRESSION_PATTERNMATCH_INVALID_PREFIX_SIZE_VALUE;
       if (output != NULL) { *output = Distance >> COMPRESSION_PATTERNMATCH_PREFIX_BYTE2_MATCHLENGTH4_DISTANCE_DIVSHIFTS; ++output; }
       ByteCount += 1; //if (output != NULL) output += Temp;
      /*}
      else {
       Temp = Compression_writePatternMatchVariableLengthNumber( Distance >> COMPRESSION_PATTERNMATCH_PREFIX_BYTE2_MATCHLENGTH4_DISTANCE_DIVSHIFTS, output );
       ByteCount += Temp; //if (output != NULL) output += Temp;
      }*/
     //} else { ++ByteCount; if (output != NULL) { *output = Distance | COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__MATCHLENGTH4__BASE; ++output; } }
     } break;
    default: {
     if (COMPRESSION_DEBUG) printf( "Invalid prefix-case: matchlength:%d, offset:%d  -> Distance:%d   (output:%p)\n", length, offset, Distance, output );
    } return COMPRESSION_PATTERNMATCH_INVALID_PREFIX_SIZE_VALUE /*length+1*/;
   }
  }
 }  //if (output!=NULL) { int i; printf("Prefix: "); for (i=0; i < ByteCount; ++i) printf("%.2X ", output[i]);  printf("\n"); }

 return ByteCount; //(ByteCount < COMPRESSION_PATTERNMATCH_PREFIX_SIZE_MAX) ? ByteCount : COMPRESSION_PATTERNMATCH_INVALID_PREFIX_SIZE_VALUE;
}



static inline int Compression_compressPatternMatchesWithSizeStats (
 unsigned char* inputdata, register int inputsize, unsigned char* outputdata
 , int* sizediff_min, int* sizediff_max, int* outdata_indexes) {
 register int i, j, Temp=0, CheckBase, BestMatchLength, MatchOffset;  //pattern-matching method, find longest repeated patterns and create/replace by pattern-dictionaries
 int LiteralSequenceLength, BestMatchInputPosition, LiteralPrefixSize, PatternPrefixSize, RLEprefixSize, RepeatPrefixSize, OutputSize;  //BastMatch is the longest (and nearest) match
 int RLElength, RLEnetSaving, PatternMatchNetSaving;
 int PreviousMatchIndex = -1, PreviousMatchPrefixSize = 0, MatchPrefixRepeatCount = 0, PreviousInputIndex = -1;
  int RepeatCheckMatchIndex = -1, RepeatCheckMatchLength = 0, RepeatCheckInputIndex = -1;
 unsigned char WriteLiteral, CompressRLE, CompressPattern;  //int PrefixCount=0, MinDistance=0xFFFF,MaxDistance=0,DistanceSum=0, SmallDistanceCount=0, SmallLengthCount=0, SmallSumCount=0, SmallPrefixCount=0; //statistics for optimal prefix-format determination
 int InOutSizeDiff=0, InOutSizeDiffMin = COMPRESSION_SIGNED_VALUE_MAX, InOutSizeDiffMax = 0;
 #if (COMPRESSION_PROGRESS)
  struct timeval StartTime, CurrentTime, PreviousTime;
 #endif

 CheckBase = LiteralSequenceLength = MatchPrefixRepeatCount = OutputSize = 0;
 #if (COMPRESSION_PROGRESS)
  if (COMPRESSION_PROGRESS) { gettimeofday( &StartTime, NULL ); PreviousTime = StartTime; printf("\n"); }
 #endif
 while (CheckBase < inputsize + 1) {  //'+1' adds plus 1 cycle which is used for detection of end-of-data
  BestMatchLength = BestMatchInputPosition = 0;
  #if (COMPRESSION_PROGRESS)
   if (COMPRESSION_VERBOSE) printf( "PatternMatching phase progress:\n" );
   if (COMPRESSION_PROGRESS) { gettimeofday( &CurrentTime, NULL ); if(CurrentTime.tv_sec != PreviousTime.tv_sec || CheckBase>=inputsize) { PreviousTime = CurrentTime; printf( "\e[1A%d%% done (%d bytes compressed to %d bytes (%d%% of original, no header) in %d seconds)\n", CheckBase*100/inputsize, CheckBase, OutputSize, OutputSize*100/inputsize, (int)(CurrentTime.tv_sec - StartTime.tv_sec) ); } }
  #endif

  if ( !Compression_compressModeSFX() && CheckBase < inputsize ) {
   for (i = CheckBase, RLElength = 0, Temp = inputdata[ CheckBase ]; i < inputsize; ++i) {  //find RLE-sequence (forward-looking)
    if ( inputdata[i] == Temp ) ++RLElength; else break;
  }}
  else RLElength = 0;

  LiteralPrefixSize = LiteralSequenceLength ?
   Compression_writeSequencePrefix( COMPRESSION_PATTERNMATCH_OFFSET_VALUE__LITERAL_SEQUENCE, LiteralSequenceLength, NULL )
   : COMPRESSION_PATTERNMATCH_PREFIX_SIZE_UNSUPPORTED;  //precalculate prefix-size

  if ( !LiteralSequenceLength || (LiteralSequenceLength >= LiteralPrefixSize && LiteralSequenceLength >= COMPRESSION_PATTERNMATCH_LITERAL_SEQUENCE_MINIMUM_SIZE) ) {  //if not worth prefixing short literal sequence, don't even search for pattern-matches yet
   for (i = CheckBase-COMPRESSION_PATTERNMATCH_LENGTH_COMPRESSION_THRESHOLD; i >= 0; --i) { //for (i = 0; i < CheckBase; ++i) { //check at all previous input-positions for match (brute-force approach, with this and GCC optimizations it takes ~5 seconds on RaspberryPI0 to compress an 50kByte .prg)
    if ( inputdata[i] == inputdata[ CheckBase ] ) {  //acceleration: if 1st character doesn't match, don't do any slower loop-based comparison
     if ( Compression_compressModeSFX() ) {
      for (j = 1; i + j < CheckBase && CheckBase + j < inputsize + 1; ++j) { //'j' will give current match-length at exiting this loop
       if ( CheckBase + j >= inputsize || inputdata[ CheckBase + j ] != inputdata[ i + j ]  //short-circuit in if-condition is used to prevent reading above inputsize, don't modify the order!
            || j >= COMPRESSION_PATTERNMATCH_SFX_SEQUENCE_LENGTH_MAX ) break;  //checking max makes it a bit faster (not checking very long matching sequences unnecessarily)
     }}
     else {
      for (j = 1; i + j < CheckBase && CheckBase + j < inputsize + 1; ++j) { //'j' will give current match-length at exiting this loop
       if ( CheckBase + j >= inputsize || inputdata[ CheckBase + j ] != inputdata[ i + j ] ) break; //short-circuit in if-condition is used to prevent reading above inputsize, don't modify the order!
     }}
     if (j >/*=*/ BestMatchLength) {  //if ( Compression_writeSequencePrefix( CheckBase - i, j, NULL ) + j <= Compression_writeSequencePrefix( CheckBase - BestMatchInputPosition, BestMatchLength, NULL ) + BestMatchLength ) {  //predicting complete size makes compression very long and doesn't seem to improve compression-ration
      BestMatchLength = j; //( Compression_compressModeSFX() && j >= COMPRESSION_PATTERNMATCH_SFX_SEQUENCE_LENGTH_MAX ) ? COMPRESSION_PATTERNMATCH_SFX_SEQUENCE_LENGTH_MAX : j; //slow solution
      BestMatchInputPosition = i;
      if ( Compression_compressModeSFX() && BestMatchLength >= COMPRESSION_PATTERNMATCH_SFX_SEQUENCE_LENGTH_MAX ) {
       BestMatchLength = COMPRESSION_PATTERNMATCH_SFX_SEQUENCE_LENGTH_MAX; break; //fast check and erly exit in 'true' case, to surely restrict match-length for SFX-format for smaller C64 extractor code
   }}}}
  }  //if (BestMatchLength >= 2) { ++PrefixCount; if(CheckBase-BestMatchInputPosition<9) ++SmallDistanceCount; if(BestMatchLength<9) ++SmallLengthCount; if(CheckBase-BestMatchInputPosition<9 && BestMatchLength<9) ++SmallPrefixCount; if(CheckBase-BestMatchInputPosition+BestMatchLength<17) ++SmallSumCount; }
  //if ( Compression_compressModeSFX() && BestMatchLength >= COMPRESSION_PATTERNMATCH_SFX_SEQUENCE_LENGTH_MAX ) BestMatchLength = COMPRESSION_PATTERNMATCH_SFX_SEQUENCE_LENGTH_MAX;  //slow solution

  MatchOffset = CheckBase - BestMatchInputPosition;
  PatternPrefixSize = (BestMatchLength >= COMPRESSION_PATTERNMATCH_LENGTH_COMPRESSION_THRESHOLD) ?
   Compression_writeSequencePrefix( MatchOffset, BestMatchLength, NULL ) : COMPRESSION_PATTERNMATCH_PREFIX_SIZE_UNSUPPORTED;  //precalculate prefix-size

  RLEprefixSize = (RLElength >= COMPRESSION_PATTERNMATCH_LENGTH_RLE_COMPRESSION_THRESHOLD) ?
   Compression_writeSequencePrefix( COMPRESSION_PATTERNMATCH_OFFSET_VALUE__RLE_SEQUENCE, RLElength, NULL )
   : COMPRESSION_PATTERNMATCH_PREFIX_SIZE_UNSUPPORTED;  //precalculate prefix-size

  CompressPattern = BestMatchLength >= PatternPrefixSize + COMPRESSION_PATTERNMATCH_IGNORE_SAMESIZE_PREFIX
                    && BestMatchLength >= COMPRESSION_PATTERNMATCH_LENGTH_COMPRESSION_THRESHOLD
                    && PatternPrefixSize < COMPRESSION_PATTERNMATCH_PREFIX_SIZE_UNSUPPORTED;
  PatternMatchNetSaving = (CompressPattern) ? BestMatchLength - PatternPrefixSize : 0;

  CompressRLE = RLElength >= (RLEprefixSize + 1) + COMPRESSION_PATTERNMATCH_IGNORE_SAMESIZE_PREFIX
                && RLEprefixSize < COMPRESSION_PATTERNMATCH_PREFIX_SIZE_UNSUPPORTED;
  RLEnetSaving = (CompressRLE) ? RLElength - (RLEprefixSize + 1) : 0;

  if (RLEnetSaving < PatternMatchNetSaving) CompressRLE = 0; else CompressPattern = 0;  //if equal saving, prefer patternmatch

  WriteLiteral = LiteralSequenceLength
   && ( CompressPattern || CompressRLE || CheckBase >= inputsize /*|| LiteralPrefixSize >= COMPRESSION_PATTERNMATCH_PREFIX_SIZE_UNSUPPORTED*/
        || ( Compression_compressModeSFX() && LiteralSequenceLength >= COMPRESSION_PATTERNMATCH_SFX_SEQUENCE_LENGTH_MAX ) );

  if (WriteLiteral) {  //append previous literal sequence's prefix
   Temp = OutputSize - LiteralSequenceLength;  //beginning of literal sequence
   for (i = OutputSize - 1; i >= Temp; --i) outputdata[ i + LiteralPrefixSize ] = outputdata[i];  //move data forward
   if (outdata_indexes != NULL) { for (i=0; i < LiteralSequenceLength; ++i) outdata_indexes[ CheckBase - LiteralSequenceLength + i ] += LiteralPrefixSize; }  //update outdata-indexes too together with moving output-data:
   Compression_writeSequencePrefix( COMPRESSION_PATTERNMATCH_OFFSET_VALUE__LITERAL_SEQUENCE, LiteralSequenceLength, &outputdata[ Temp ] );
   #if (COMPRESSION_SHOW)
    if (COMPRESSION_SHOW_STARTINDEX < 0 || ( COMPRESSION_SHOW_STARTINDEX <= CheckBase && CheckBase < COMPRESSION_SHOW_STARTINDEX + COMPRESSION_SHOW_RANGE ) ) {
     printf("Prefixed literal sequence at output[$%X] (input[$%X]), length: $%X -> prefix: ", Temp, CheckBase-LiteralSequenceLength, LiteralSequenceLength);
     for(i=0;i<LiteralPrefixSize;++i) printf("$%.2X ", outputdata[Temp+i]);  printf(", ");
     for(i=0;i<LiteralSequenceLength;++i) printf("$%.2X ", outputdata[Temp+LiteralPrefixSize+i]);  printf("\n");  //{ ++PrefixCount; if(LiteralSequenceLength<9) ++SmallLengthCount; }
    }
   #endif
   LiteralSequenceLength = 0;  OutputSize += LiteralPrefixSize;
  }

  if (CompressPattern) {  //Itt a lenyeg:  :)
   MatchOffset = CheckBase - BestMatchInputPosition;
   PatternPrefixSize = Compression_writeSequencePrefix( MatchOffset, BestMatchLength, &outputdata[ OutputSize ] );  //write prefix, distance and length info to output
   #if (COMPRESSION_SHOW)
    if (COMPRESSION_SHOW_STARTINDEX < 0 || ( COMPRESSION_SHOW_STARTINDEX <= CheckBase && CheckBase < COMPRESSION_SHOW_STARTINDEX + COMPRESSION_SHOW_RANGE ) ) {
     printf("Longest match between checkbase input[$%X] and input[$%X] (distance:$%X): $%X:  ", CheckBase, BestMatchInputPosition, MatchOffset-BestMatchLength, BestMatchLength );  //{ ++PrefixCount; int Distance=CheckBase-BestMatchInputPosition-BestMatchLength; if(0<=Distance && Distance<1024) ++SmallDistanceCount; if(BestMatchLength==4 && PatternPrefixSize >= 3) { ++SmallLengthCount; if(Distance>MaxDistance) MaxDistance=Distance; if(Distance<MinDistance) MinDistance=Distance; DistanceSum+=Distance; }; if(Distance<64 && BestMatchLength<5) ++SmallPrefixCount; if(CheckBase-BestMatchInputPosition+BestMatchLength<17) ++SmallSumCount;}
     for (i=0; i<BestMatchLength; ++i) printf("$%.2X ",inputdata[CheckBase+i]);  printf("-> [$%.4X]: ",OutputSize);
     for (i=0; i<PatternPrefixSize; ++i) printf("$%.2X ",outputdata[OutputSize+i]); printf("\n");
    }
   #endif
   if (outdata_indexes != NULL) { for (i=0; i<BestMatchLength; ++i) outdata_indexes[ CheckBase + i ] = OutputSize + PatternPrefixSize - 1; }  //store output-indexes to written match-prefix's last-byte's index for each pattern-byte (prefix is read before decompressing the pattern so decompressed pattern's end can overwrite it)

   if ( Compression_compressModeSFX() ) { //RLE for SFX-mode: check and handle repeated pattern-match prefixes
    RepeatPrefixSize = Compression_writeSequencePrefix( COMPRESSION_PATTERNMATCH_OFFSET_VALUE__MATCH_REPEAT, 0/*MatchPrefixRepeatCount*/, NULL );
    if ( PreviousMatchIndex >= 0 && PreviousMatchPrefixSize == PatternPrefixSize && OutputSize == PreviousMatchIndex + PatternPrefixSize  //(only do the matching for tightly adjacent patterns, as literals could come inbetween)
         && Compression_compareData( outputdata + OutputSize, PatternPrefixSize, outputdata + PreviousMatchIndex ) < 0 ) {  //repeated matchprefix found
     if (MatchPrefixRepeatCount <= 0) {  //remember 1st instance of repeated match-prefixes
      RepeatCheckMatchIndex = PreviousMatchIndex; RepeatCheckMatchLength = BestMatchLength; RepeatCheckInputIndex = PreviousInputIndex;
     }
     ++MatchPrefixRepeatCount;
     if (outdata_indexes != NULL) {  //correct output-indexes to the shrinked new output (increased with repeat-prefix)
      for (i=0; i < BestMatchLength; ++i) outdata_indexes[ CheckBase + i ] = RepeatCheckMatchIndex + RepeatPrefixSize;
    }}
    else {  //patternmatch-prefix doesn't mach the latest one, if there were repeats of previous register them, otherwise go on normally
     if (MatchPrefixRepeatCount > 0) {  //insert repeat-prefix (with count value) before patternmatch-prefix and append current different matchprefix
      if (RepeatPrefixSize + PreviousMatchPrefixSize < PreviousMatchPrefixSize * (MatchPrefixRepeatCount + 1)
          && PreviousMatchPrefixSize == COMPRESSION_PATTERNMATCH_RLE_REPEAT_SUPPORTED_PREFIXSIZE) {  //only add repetition if it's smaller in size (would really compress), and only support matchprefix-size 2 now by asm
       for (i = PreviousMatchPrefixSize - 1; i >= 0; --i) outputdata[ RepeatCheckMatchIndex + RepeatPrefixSize + i ] = outputdata[ RepeatCheckMatchIndex + i ];  //move to-be-repeated match-prefix forward
       if (outdata_indexes != NULL) {  //correct output-indexes to the shrinked new output (add repeat-prefix-size)
        for (i=0; i < RepeatCheckMatchLength; ++i) outdata_indexes[ RepeatCheckInputIndex + i ] += RepeatPrefixSize;
       }
       RepeatPrefixSize = Compression_writeSequencePrefix( COMPRESSION_PATTERNMATCH_OFFSET_VALUE__MATCH_REPEAT, MatchPrefixRepeatCount, &outputdata[ RepeatCheckMatchIndex ] );  //and insert repeat-prefix before it
       OutputSize = RepeatCheckMatchIndex + RepeatPrefixSize + PreviousMatchPrefixSize;  //reposition output-index right after the repeated matchprefix (decreasing size of output)
       PatternPrefixSize = Compression_writeSequencePrefix( MatchOffset, BestMatchLength, &outputdata[ OutputSize ] );  //rewrite current match-prefix to the lowered index
       if (outdata_indexes != NULL) {  //correct latest matched pattern's output-indexes to the shrinked new output
        for (i=0; i < BestMatchLength; ++i) outdata_indexes[ CheckBase + i ] = OutputSize + PatternPrefixSize - 1;  //store output-indexes to written match-prefix's last-byte's index for each pattern-byte (prefix is read before decompressing the pattern so decompressed pattern's end can overwrite it)
       }
       #if (COMPRESSION_SHOW)
        if (COMPRESSION_SHOW_STARTINDEX < 0 || ( COMPRESSION_SHOW_STARTINDEX <= CheckBase && CheckBase < COMPRESSION_SHOW_STARTINDEX + COMPRESSION_SHOW_RANGE ) ) {
         printf( "Repeated Pattern-match prefix %d times\n", MatchPrefixRepeatCount );
        }
       #endif
      }
      MatchPrefixRepeatCount = 0;
     }
    }
   }

   PreviousMatchIndex = OutputSize; PreviousMatchPrefixSize = PatternPrefixSize; PreviousInputIndex = CheckBase;
   CheckBase += BestMatchLength; OutputSize += PatternPrefixSize;  //add length of prefix to output-size
  }

  else if (CompressRLE) {
   Temp = Compression_writeSequencePrefix( COMPRESSION_PATTERNMATCH_OFFSET_VALUE__RLE_SEQUENCE, RLElength, &outputdata[ OutputSize ] );
   if (outdata_indexes != NULL) { for (i=0; i<RLElength; ++i) outdata_indexes[ CheckBase + i ] = OutputSize + Temp; }
   OutputSize += Temp ;  //add length of prefix to output-size
   outputdata[ OutputSize++ ] = inputdata[ CheckBase ]; //add repeated byte-value and its size to outputsize
   CheckBase += RLElength;
  }

  else { //not enough matching bytes found yet at input check base-position, collect uncompressed literal sequence output-bytes until long enough match is encountered
   if (outdata_indexes != NULL) outdata_indexes[ CheckBase ] = OutputSize;
   if (CheckBase < inputsize) outputdata[ OutputSize++ ] = inputdata[ CheckBase ];
   ++CheckBase; ++LiteralSequenceLength;
  }

  /*if (CheckBase < inputsize)*/ InOutSizeDiff = CheckBase - OutputSize;  //these statistics are only for input-output-base at the same address (their ends made to be at the same address comes after compression using outdata_indexes)
  if (InOutSizeDiff < InOutSizeDiffMin) InOutSizeDiffMin = InOutSizeDiff;
  if (InOutSizeDiff > InOutSizeDiffMax) InOutSizeDiffMax = InOutSizeDiff;  //if (InOutSizeDiff < 0) { printf("PatternMatch: in:%d - out:%d = %d   ",CheckBase,OutputSize,InOutSizeDiff); for(j=0;j<=10;++j) printf("%.2X ",inputdata[j]); printf("..."); for(j=CheckBase-10;CheckBase >= 10 && j<=CheckBase;++j) printf("%.2X ",inputdata[j]); printf(" -> "); for(j=0;j<10;++j) printf("%.2X ",outputdata[j]); printf("..."); for(j=OutputSize-10;OutputSize >= 10 && j<OutputSize;++j) printf("%.2X ",outputdata[j]);  printf("\n"); }
 }
 if ( Compression_compressModeSFX() ) {  //for the moment only SFX uses END_OF_DATA signaling (to reduce C64 self-extractor's code-size for end/length-checking)
  outdata_indexes[ CheckBase ] = OutputSize;  outputdata[ OutputSize++ ] = Compression_convertPrefixType( COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__END_OF_DATA );
 }
 *sizediff_min = InOutSizeDiffMin; *sizediff_max = InOutSizeDiffMax;  if (COMPRESSION_VERBOSE) printf("PatternMatch    : in:%d($%.4X) - out:%d($%.4X) = diff:%d  (diff-min:%d, diff-max:%d)\n",inputsize,inputsize,OutputSize,OutputSize,inputsize-OutputSize,InOutSizeDiffMin,InOutSizeDiffMax);  //printf("PrefixCount:%d,  SmallDistanceCount:%d, SmallLengthCount:%d,  SmallSumCount:%d, SmallPrefixCount:%d, MinDistance:%d,AverageDistance:%d,MaxDistance:%d\n", PrefixCount, SmallDistanceCount, SmallLengthCount, SmallSumCount, SmallPrefixCount,  MinDistance, DistanceSum/SmallLengthCount, MaxDistance );
 return OutputSize;
}



int Compression_compressPatternMatches (unsigned char* inputdata, register int inputsize, unsigned char* outputdata) {
 static int DummyInt;
 return Compression_compressPatternMatchesWithSizeStats( inputdata, inputsize, outputdata, &DummyInt, &DummyInt, NULL );
}



static inline int Compression_writeOctet (int value, unsigned char* outputdata, unsigned int* output_size) {
 outputdata[ (*output_size)++ ]  = value & COMPRESSION_VALUEMASK__BYTE;  return 1;
}

static inline void Compression_writeValueBand (int value, unsigned char* outputdata, unsigned int* index) {
 Compression_writeOctet( value, outputdata, index );  //outputdata[ index++ ] = value & COMPRESSION_LOWBYTE_MASK;
 if (COMPRESSION_ARITHMETIC_PROBABILITY_BYTECOUNT >= 2) Compression_writeOctet( value >> COMPRESSION_HIGHBYTE_SHIFTS, outputdata, index );  //outputdata[ index++ ] = value >> COMPRESSION_HIGHBYTE_SHIFTS ;
}

static int Compression_writeBit (signed char bitvalue, int* octet, int* octet_bitcounter, unsigned char* outputdata, unsigned int* output_size) {
 int OutputSize = 0;  static unsigned int OutputValue = 0; //OutputValue is just for display purposes and for comparison with decoder dataflow
 if (bitvalue) *octet |= (0x80 >> *octet_bitcounter);
 if ( ++(*octet_bitcounter) >= COMPRESSION_BITS_PER_BYTE ) {
  *octet_bitcounter = 0;  OutputSize = Compression_writeOctet( *octet, outputdata, output_size );  if(COMPRESSION_SHOW)printf(" $%.2X", *octet );  //*output = *octet;
  *octet = 0x00;  /*OutputSize = 1*/ /*return 1;*/;
 }  if(COMPRESSION_SHOW) printf(" %d:%.4X", bitvalue, OutputValue = ( (OutputValue << 1) | bitvalue ) & COMPRESSION_ARITHMETIC_PROBABILITY_MASK );
 return OutputSize;
}

static int Compression_writePendingBits (signed char bitvalue, int* pendingbit_count, int* octet, int* octetcnt, unsigned char* outputdata, unsigned int* output_size) {
 int OutputSize = Compression_writeBit( bitvalue, octet, octetcnt, outputdata, output_size );  //write output-bit plus pending bits
 while (*pendingbit_count > 0) { //pending bits will have the inverted value of MSB (High went down to %0111... or Low went up to %1000..)
  OutputSize += Compression_writeBit( !bitvalue, octet, octetcnt, outputdata/*&output[ OutputSize ]*/, output_size );  --(*pendingbit_count);
 }
 return OutputSize;
}


int Compression_compressArithmetic (unsigned char* inputdata, register int inputsize, unsigned char* outputdata) {  //Arithmetic-coding method of compression. Implementation loosely based on Mark Nelson's 2014 article 'Data Compression With Arithmetic Coding'
 static enum {  //Arithmetic-coding method of entropy compression / range-encoder (better than binary-oriented Huffman method)
  PREFERRED_INPUTSIZE_BITCOUNT = COMPRESSION_ARITHMETIC_PROBABILITY_BITCOUNT /*16*/,
   PREFERRED_MAXIMUM_INPUTSIZE = (1 << PREFERRED_INPUTSIZE_BITCOUNT) /*0x10000*/,  //above this the value-counts get scaled down (yielding more inaccurate probability-values/bands and so worse compression ratios as filesize increases)
   INPUTSIZE_MULSHIFTS = (COMPRESSION_VALUE_BITCOUNT/*32*/ - COMPRESSION_BUFFERSIZE_BITCOUNT/*20*/) /*12*/,
    PREFERRED_INPUTSIZE_MULSHIFTS = (PREFERRED_INPUTSIZE_BITCOUNT/*16*/ - INPUTSIZE_MULSHIFTS/*12*/) /*4*/,
  VALUECOUNT_MIN = 1, VALUECOUNT_MAX = PREFERRED_MAXIMUM_INPUTSIZE,
  VALUEBAND_CALCSHIFTS = (COMPRESSION_VALUE_BITCOUNT/*32*/ - COMPRESSION_ARITHMETIC_PROBABILITY_BITCOUNT/*16*/) /*16*/,
  END_OF_DATA_BYTECOUNT = 1,
  DISPSHIFTS = (COMPRESSION_ARITHMETIC_CALC_BITCOUNT/*32*/ - COMPRESSION_ARITHMETIC_PROBABILITY_BITCOUNT/*16*/) /*16*/,
  CHARTSHIFTS = (COMPRESSION_ARITHMETIC_CALC_BITCOUNT/*32*/ - COMPRESSION_CHART_WIDTH_SHIFTS/*6*/) /*26*/,
  CHARTWIDTH = (1 << COMPRESSION_CHART_WIDTH_SHIFTS) /*64*/, CHARTPARTCOUNT = 4,
  GRIDMASK = ( ( CHARTWIDTH / CHARTPARTCOUNT ) - 1 ) /*0xF*/
 } ArithmeticCoding_Specs;
 register int i, j;  register unsigned int Temp, Temp2, InputValue;
 int ValueBandTableSize = 0, Octet=0x00, OctetBitCounter = 0, PendingBitCount = 0;
 unsigned int ProbabilityDenominator, ValueBandLow = 0, LowIndex, OutputSize = 0, High, Low, Range;  //avoid signed so even the full native word range may be used for intermediate calculations
 static unsigned int InputValueCounts [COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE];
 static unsigned int ValueBands [COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE]; //probability-valuebands/ranges/segments of input-values
 static unsigned int HighIndexes [COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE];  //probability indexes pointing to 'high' value of current value at next valid byte-value)
 int InOutSizeDiff=0, InOutSizeDiffMin = COMPRESSION_SIGNED_VALUE_MAX, InOutSizeDiffMax = 0;

 for (i=0; i < COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE; ++i) InputValueCounts[i] = 0;
 InputValueCounts[ COMPRESSION_ARITHMETIC_VALUE__END_OF_DATA ] = END_OF_DATA_BYTECOUNT;  //built-in End-of-Data signal appears only once (at the end of data)
 for (i=0, ProbabilityDenominator = inputsize + END_OF_DATA_BYTECOUNT; i < inputsize; ++i) ++InputValueCounts[ inputdata[i] ];  //for (i=0;i<COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE;++i)printf("%.2X:%d ",i,InputValueCounts[i]); printf("\n%d,\n",ProbabilityDenominator);  //count real byte-value occurrences of original input-datastream
 while (ProbabilityDenominator > PREFERRED_MAXIMUM_INPUTSIZE) {  //scaling down value-counts to fit valueband-range (worsens statistics-resolution an compression-ratio)
  for (i = Temp = 0; i < COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE - 1; ++i) {
   if ( InputValueCounts[i] > 0 ) {
    InputValueCounts[i] = ( (InputValueCounts[i] << INPUTSIZE_MULSHIFTS) / ProbabilityDenominator ) << PREFERRED_INPUTSIZE_MULSHIFTS;
    if (InputValueCounts[i] > VALUECOUNT_MAX) InputValueCounts[i] = VALUECOUNT_MAX;  //limit valuecount if exceeded (worsens statistics an compression-ratio)
    else if (InputValueCounts[i] < VALUECOUNT_MIN) InputValueCounts[i] = VALUECOUNT_MIN;  //ensure minimum valuecount (worsens statistics an compression-ratio)
    Temp += InputValueCounts[i];
  }}
  ProbabilityDenominator = Temp;  //for (i=0;i<COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE;++i) printf("%.2X:%d ",i,InputValueCounts[i]); printf("\n%d\n",ProbabilityDenominator);
 }

 for (i = 0, ValueBandLow = LowIndex = 0; i < COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE - 1; ++i) {  //create value-probability based model with valueband/range list (more frequent values have more span)
  if ( InputValueCounts[i] > 0 ) {
   ValueBands[i] = ValueBandLow / ProbabilityDenominator;  if (ValueBandLow > 0 && ValueBands[ LowIndex ] >= ValueBands[i] /*- 1*/) { if (COMPRESSION_DEBUG) printf("Compressor: Current arithmetic-coding probability-distribution model resolution is inadequate for the data, omitting arithmetic-coding.\n");  return COMPRESSION_RETURNVALUE__ABORT /*inputsize*/; }  //when too small resolution, by returning bigger output-size than input-size, cause an omission of arithmetic-coding compression
   ValueBandLow += InputValueCounts[i] << VALUEBAND_CALCSHIFTS;  if (ValueBands[i] > COMPRESSION_ARITHMETIC_PROBABILITY_MAX) { if (COMPRESSION_DEBUG) printf("Compressor: Current arithmetic-coding probability-distribution bandtable-value exceeded table's value-range, omitting arithmetic-coding.\n");  return COMPRESSION_RETURNVALUE__ABORT; }
   LowIndex = HighIndexes[ LowIndex ] = i;  if (i < COMPRESSION_ARITHMETIC_VALUE__END_OF_DATA) ValueBandTableSize = i + 1;
  }
  else ValueBands[i] = COMPRESSION_ARITHMETIC_VALUE__VALUEBANDTABLE_NONE;
 }
 HighIndexes[ COMPRESSION_ARITHMETIC_VALUE__END_OF_DATA ] = COMPRESSION_ARITHMETIC_VALUE__END_OF_DATA + 1;
 ValueBands[ COMPRESSION_ARITHMETIC_VALUE__END_OF_DATA + 1 ] = COMPRESSION_ARITHMETIC_PROBABILITY_MAX/*RANGE*/;  if (COMPRESSION_SHOW) { for (i=0; i < COMPRESSION_ARITHMETIC_VALUEBANDTABLE_SIZE; ++i) if (i >= COMPRESSION_ARITHMETIC_VALUE__END_OF_DATA || ValueBands[i]!=COMPRESSION_ARITHMETIC_VALUE__VALUEBANDTABLE_NONE) printf("%.2X:%.4X..[%.2X]  ",i,ValueBands[i],HighIndexes[i]); printf("\n"); }

 for (i = 0, OutputSize = 0; i <= ValueBandTableSize; ++i) {  //write the probability-model based table to output (byte/word band-values of all 256 byte-values including End-of-Data, non-existant ones got maximum value not normally assigned to existing ones, with this info the valueband-table can be reconstructed in decompressor)
  Temp = (i < ValueBandTableSize) ? ValueBands[i] : COMPRESSION_ARITHMETIC_VALUE__VALUEBANDTABLE_END;
  if (i < COMPRESSION_VALUERANGE__BYTE) {  //don't write the end-sign if the complete table was written (i.e. probability of input-value $FF was nonzero)
   Compression_writeValueBand( Temp, outputdata, &OutputSize );
 }} //write out End-of-Data band's 'low'
 Compression_writeValueBand( ValueBands[ COMPRESSION_ARITHMETIC_VALUE__END_OF_DATA ] , outputdata, &OutputSize );

 Low = COMPRESSION_ARITHMETIC_CALC_MIN; High = COMPRESSION_ARITHMETIC_CALC_MAX;
 for (i = Octet = OctetBitCounter = PendingBitCount = 0; i < inputsize + 1; ++i) {  //Itt a lenyeg:  :)
  InputValue = (i < inputsize) ? inputdata[i] : COMPRESSION_ARITHMETIC_VALUE__END_OF_DATA;  if(COMPRESSION_SHOW){Temp=Low;Temp2=High;}
  Range = ( (High - Low) /*+ 1*/ ) >> COMPRESSION_ARITHMETIC_RANGE_CALCSHIFTS;
  High = Low + ( ValueBands[ HighIndexes[ InputValue ] ] * Range ) - 1;  //High &= COMPRESSION_ARITHMETIC_CALC_MASK;
  Low  = Low + ( ValueBands[ InputValue ] * Range );  /*Low &= COMPRESSION_ARITHMETIC_CALC_MASK;*/  if(COMPRESSION_SHOW){printf("%6.6d ",i);for(j=0;j<CHARTWIDTH;++j) printf("%c",((int)(Low>>CHARTSHIFTS)<=j&&j<=(int)(High>>CHARTSHIFTS))||((int)(Temp>>CHARTSHIFTS)<=j&&j<=(int)(Temp2>>CHARTSHIFTS))?((int)(Low>>CHARTSHIFTS)<=j&&j<=(int)(High>>CHARTSHIFTS)&&(int)(Temp>>CHARTSHIFTS)<=j&&j<=(int)(Temp2>>CHARTSHIFTS)?'#':((int)(Low>>CHARTSHIFTS)<=j&&j<=(int)(High>>CHARTSHIFTS)?'+':'-')):(!(j&GRIDMASK)?'|':'.')); printf(" %2.2X(%.4X..%4.4X): %.4X..%.4X..%.4X..%.4X\n",InputValue,ValueBands[InputValue],ValueBands[HighIndexes[InputValue]]-/*1*/(COMPRESSION_ARITHMETIC_PROBABILITY_BYTECOUNT > 1),Temp>>DISPSHIFTS,Low>>DISPSHIFTS,High>>DISPSHIFTS,Temp2>>DISPSHIFTS);}  //set limints for value's band
   if (Low >= High /*- 1*/) { if (COMPRESSION_DEBUG) printf("Compressor: Current arithmetic-coding probability-distribution model resolution is inadequate for the data, aborting and cancelling arithmetic-coding.\n");  return COMPRESSION_RETURNVALUE__ABORT /*inputsize*/; }
  while (1) {  //(Invariants: Low only increases, High only decreases, and High is always greater than Low)
   if (High < COMPRESSION_ARITHMETIC_CALC_MID || COMPRESSION_ARITHMETIC_CALC_MID <= Low) {  if(COMPRESSION_SHOW)Temp2=High;  //High-low converged enough (around middle '0.5' of value-range) to shift out a 0/1: high dropped below mid, i.e its MSB (Carry) is 0 (and here Low's MSB is 0 as well as it's always less than High), or if Low's MSB became 1 (High's MSB is 1 too). As they're only getting closer, this situation would stay forever with any precision/length
    Temp=Low; Low <<= 1; High = (High << 1) | 1;   Low &= COMPRESSION_ARITHMETIC_CALC_MASK; High &= COMPRESSION_ARITHMETIC_CALC_MASK;  if(COMPRESSION_SHOW){printf("%6.6d ",i);for(j=0;j<CHARTWIDTH;++j) printf("%c",((int)(Low>>CHARTSHIFTS)<=j&&j<=(int)(High>>CHARTSHIFTS))||((int)(Temp>>CHARTSHIFTS)<=j&&j<=(int)(Temp2>>CHARTSHIFTS))?((int)(Low>>CHARTSHIFTS)<=j&&j<=(int)(High>>CHARTSHIFTS)&&(int)(Temp>>CHARTSHIFTS)<=j&&j<=(int)(Temp2>>CHARTSHIFTS)?'=':((int)(Temp>>CHARTSHIFTS)<=j&&j<=(int)(Temp2>>CHARTSHIFTS)?'-':'+')):(!(j&GRIDMASK)?'|':'.')); printf("   (%.4X..%.4X):",Low>>DISPSHIFTS,High>>DISPSHIFTS);}  //so we know its value 100% and can shift out the MSB to datastream and get rid of it, process the next  //discard MSB, halve/zoom to new range, shift 0 to Low LSB and 1 to High LSB (happens faster with higher ranges/probabilities emitting fewer bits, slower with less probability emitting more bits)
    Compression_writePendingBits( COMPRESSION_ARITHMETIC_CALC_MID <= Temp, &PendingBitCount, &Octet, &OctetBitCounter, outputdata, &OutputSize );  if(COMPRESSION_SHOW)printf("\n");  //shifting in every round is essentially inflating/zooming the range for next value when MSB determines the range's interesting half, not to run out of range-precision
   }
   else if (COMPRESSION_ARITHMETIC_CALC_1PER4 <= Low && High < COMPRESSION_ARITHMETIC_CALC_3PER4) {  if(COMPRESSION_SHOW){Temp=Low;Temp2=High;}  //check if upcoming (second MSB) differs from current MSB, the near-convergence problem (predicting not well defined output bit value)
    ++PendingBitCount;  //workaround for non-converging MSB or Low==High case (when range=High-Low is only 1, so Low would stay below middle (0.5), and High would stay above it):
    Low = (Low << 1) & COMPRESSION_ARITHMETIC_CALC_NOMSB_MASK;  Low &= COMPRESSION_ARITHMETIC_CALC_MASK;  //masking dicards 2nd MSB
    High = (High << 1) | (COMPRESSION_ARITHMETIC_CALC_BITVALUE_MSB | 1);  High &= COMPRESSION_ARITHMETIC_CALC_MASK;  if(COMPRESSION_SHOW){printf("%6.6d ",i);for(j=0;j<CHARTWIDTH;++j) printf("%c",((int)(Low>>CHARTSHIFTS)<=j&&j<=(int)(High>>CHARTSHIFTS))||((int)(Temp>>CHARTSHIFTS)<=j&&j<=(int)(Temp2>>CHARTSHIFTS))?((int)(Low>>CHARTSHIFTS)<=j&&j<=(int)(High>>CHARTSHIFTS)&&(int)(Temp>>CHARTSHIFTS)<=j&&j<=(int)(Temp2>>CHARTSHIFTS)?'=':((int)(Temp>>CHARTSHIFTS)<=j&&j<=(int)(Temp2>>CHARTSHIFTS)?'-':'+')):(!(j&GRIDMASK)?'|':'.')); printf("   (%.4X..%.4X) PendingBitCount:%d\n",Low>>DISPSHIFTS,High>>DISPSHIFTS,PendingBitCount);}
   }
   else break;  //not converging
  }
  if (i < inputsize) InOutSizeDiff = (i + 1) - OutputSize;
  if (InOutSizeDiff < InOutSizeDiffMin) InOutSizeDiffMin = InOutSizeDiff;
  if (InOutSizeDiff > InOutSizeDiffMax) InOutSizeDiffMax = InOutSizeDiff;  //if (InOutSizeDiff < 0) { printf("PatternMatch: in:%d - out:%d = %d   ",i,OutputSize,InOutSizeDiff); for(j=0;j<=10;++j) printf("%.2X ",inputdata[j]); printf("..."); for(j=i-10;i >= 10 && j<=i;++j) printf("%.2X ",inputdata[j]); printf(" -> "); for(j=0;j<10;++j) printf("%.2X ",outputdata[j]); printf("..."); for(j=OutputSize-10;OutputSize >= 10 && j<(int)OutputSize;++j) printf("%.2X ",outputdata[j]);  printf("\n"); }
 }
 ++PendingBitCount; Compression_writePendingBits( Low >= COMPRESSION_ARITHMETIC_CALC_1PER4, &PendingBitCount, &Octet, &OctetBitCounter, outputdata, &OutputSize );  if(COMPRESSION_SHOW)printf("\n");  //writing 01 or 10 is enough to create EOF signal value-range in decoder
 if (OctetBitCounter > 0) { Compression_writeOctet( Octet, outputdata, &OutputSize );  if(COMPRESSION_SHOW)printf("Last Output Octet: $%.2X\n",Octet); }
 if (COMPRESSION_VERBOSE) printf("Arithmetic:   in:%d - out:%d = diff:%d  (diff-min:%d, diff-max:%d)\n",inputsize,OutputSize,inputsize-OutputSize,InOutSizeDiffMin,InOutSizeDiffMax);

 return OutputSize;
}



int Compression_compressorCopyData (unsigned char* inputdata, int inputsize, unsigned char* outputdata) {
 int i, OutputSize = inputsize;
 for (i=0; i < inputsize; ++i) outputdata[i] = inputdata[i];
 return OutputSize;
}


inline int Compression_compareData (unsigned char* inputdata, int inputsize, unsigned char* outputdata) {
 int i, DifferenceIndex = -1;
 for (i=0; i < inputsize; ++i) { if ( outputdata[i] != inputdata[i] ) { DifferenceIndex = i; break; } }
 return DifferenceIndex;
}



static int Compression_getSYStarget (unsigned char* data, int size) {
 static enum { SYS_COMMAND_TOKEN=0x9E, SYS_COMMAND_OFFSET_MIN = 4, SYS_COMMAND_OFFSET_MAX = 32, SYS_OPERAND_SIZE_MAX = 32 } SYScommand_Specs;
 static int i, j;
 for (j=0x0000, i=SYS_COMMAND_OFFSET_MIN; i<SYS_COMMAND_OFFSET_MAX && i<size; ++i) if (data[i]==SYS_COMMAND_TOKEN) break;  //'SYS' BASIC-command found?
 if (i >= SYS_COMMAND_OFFSET_MAX) {  //fallback trial if no SYS-command found
  j = COMPRESSION_SFX_FALLBACK_JUMPADDRESS;  if (COMPRESSION_VERBOSE) printf( "Couldn't find SYS-command token, falling back to $%.4X.\n", COMPRESSION_SFX_FALLBACK_JUMPADDRESS );
  return j;
 }
 else {
  for (++i; i<SYS_OPERAND_SIZE_MAX && i<size; ++i) {
   if ('0'<=data[i] && data[i]<='9') j = j*10 + data[i]-'0';   else if (data[i]==0x00 || data[i]!=' ') break;
 }}
 if (j==0x0000) {  //another fallback
  j = COMPRESSION_SFX_FALLBACK_JUMPADDRESS;  if (COMPRESSION_VERBOSE) printf( "Couldn't find SYS-command target-address, falling back to $%.4X.\n", COMPRESSION_SFX_FALLBACK_JUMPADDRESS );
 }
 else if (COMPRESSION_VERBOSE) printf( "BASIC SYS starter target-address $%.4X found, setting self-extracion-finisher jump address to it.\n", j );
 return j;
}



static int Compression_compressDataInMode (unsigned char* data, int inputsize, unsigned char* data2) {
 static enum { BASIC_START_ADDRESS = 0x0801, BIT_ABS_opcode = 0x2C } SelfExtraction_specs;
 int i, OutputSize = 0, Temp, Type;
 #if (!COMPRESSION_DEBUG_VERIFICATION)
  static int Compression_CompressedDataIndexes [COMPRESSION_BUFFERSIZE_MAX];  //indexes in compressed data corresponding to indexes in output-data
 #endif
 int InOutSizeDiff=0, InOutSizeDiffMin = COMPRESSION_SIGNED_VALUE_MAX, InOutSizeDiffMax = 0;
 int InOutIndexDiff=0, InOutIndexDiffMin = COMPRESSION_SIGNED_VALUE_MAX;
 int CompressionSizeDiffMin = COMPRESSION_SIGNED_VALUE_MAX, CompressionSizeDiffMax = 0;
 int SizeDiffMin = COMPRESSION_SIGNED_VALUE_MAX, SizeDiffMax = 0;
 int InplaceExtractionOffset = 0, InplaceExtractionGap = 0, C64inplaceExtractionOffset = 0;  //compressed data of self-compressing file will be moved to this offset at the start of extraction
 int C64inputDataBaseAddress, C64inputDataSize, C64inputDataEndAddress;
 int C64intermediateDataBaseAddress, C64intermediateDataEndAddress;
 int C64outputDataBaseAddress = BASIC_START_ADDRESS, C64outputDataSize = inputsize - COMPRESSION_PRG_HEADER_SIZE,
     C64outputDataEndAddress = BASIC_START_ADDRESS;
 int C64initialMoveIndex, C64moveSourceEnd, C64moveTargetEnd, C64executionAddress = COMPRESSION_SFX_FALLBACK_JUMPADDRESS;

 Type = 0x00;

 if ( ( COMPRESSION_ENABLED_RLE && !Compression_compressModeSFX() ) || Compression_compressModeRLESFX() ) {
  if ( Compression_compressModeRLESFX() ) {
   C64outputDataBaseAddress = data[0] | (data[1] << COMPRESSION_HIGHBYTE_SHIFTS);
   C64executionAddress = (C64outputDataBaseAddress == BASIC_START_ADDRESS) ? Compression_getSYStarget( data + COMPRESSION_PRG_HEADER_SIZE, inputsize ) : C64outputDataBaseAddress;
   for (i=0; i < COMPRESSION_RLE_REPEATCOUNT_COMPRESSION_THRESHOLD; ++i) { if (data) break; }
   C64outputDataSize = inputsize - COMPRESSION_PRG_HEADER_SIZE;
  }
  Temp = Compression_compressModeRLESFX() ? COMPRESSION_PRG_HEADER_SIZE : 0;  //2byte PRG-header shouldn't be compressed into SFX (but should be stored in header, and should be used to determine/set decompressed app's execution-start address)
  Temp = Compression_compressValueRepeatsWithSizeStats( &data[Temp], inputsize - Temp, data2
                           , &CompressionSizeDiffMin, &CompressionSizeDiffMax, Compression_CompressedDataIndexes );
  if ( Temp == COMPRESSION_RETURNVALUE__ABORT || (COMPRESSION_ADAPTIVE_RLE && Temp >= inputsize && !Compression_compressModeRLESFX() ) ) {
   OutputSize = Compression_compressorCopyData( data, inputsize, data2 );  //reverting compression if RLE is inadequate
  }
  else { OutputSize = Temp; Type |= COMPRESSION_HEADER_BITVALUE__TYPE__USED_RLE; }
 }
 else OutputSize = Compression_compressorCopyData( data, inputsize, data2 );
 InOutSizeDiffMin = InOutSizeDiffMax = inputsize - OutputSize;


 if ( ( COMPRESSION_ENABLED_PATTERNMATCHING || Compression_compressModeSFX() ) && !Compression_compressModeRLESFX() ) {
  if ( Compression_compressModeSFX() ) {
   C64outputDataBaseAddress = data2[0] | (data2[1] << COMPRESSION_HIGHBYTE_SHIFTS);
   C64executionAddress = (C64outputDataBaseAddress == BASIC_START_ADDRESS) ? Compression_getSYStarget( data2 + COMPRESSION_PRG_HEADER_SIZE, OutputSize ) : C64outputDataBaseAddress;
   C64outputDataSize = OutputSize /*inputsize*/ - COMPRESSION_PRG_HEADER_SIZE;
  }
  Temp = Compression_compressModeSFX() ? COMPRESSION_PRG_HEADER_SIZE : 0;  //2byte PRG-header shouldn't be compressed into SFX (but should be stored in header, and should be used to determine/set decompressed app's execution-start address)
  Temp = Compression_compressPatternMatchesWithSizeStats( &data2[Temp], OutputSize - Temp, data, &CompressionSizeDiffMin, &CompressionSizeDiffMax, Compression_CompressedDataIndexes );
  if ( Temp == COMPRESSION_RETURNVALUE__ABORT || ( COMPRESSION_ADAPTIVE_PATTERNMATCHING && Temp >= OutputSize && !Compression_compressModeSFX() ) ) {
   Compression_compressorCopyData( data2, OutputSize, data );  //reverting compression if pattern-match is inadequate
  }
  else { OutputSize = Temp; Type |= COMPRESSION_HEADER_BITVALUE__TYPE__USED_PATTERNMATCHING; }  //in SFXmode, OutpuHeader reduced by 2 bytes (PRG-header)
 }
 else Compression_compressorCopyData( data2, OutputSize, data );
 InOutSizeDiff = inputsize - OutputSize;
 if (InOutSizeDiff < InOutSizeDiffMin) InOutSizeDiffMin = InOutSizeDiff;
 if (InOutSizeDiff > InOutSizeDiffMax) InOutSizeDiffMax = InOutSizeDiff;


 if ( COMPRESSION_ENABLED_ARITHMETIC_CODING && !( Compression_compressModeSFX() || Compression_compressModeRLESFX() ) ) {  //adaptive: there's an 512byte header with value-probability model, for small files it's a big overhead
  Temp = Compression_compressArithmetic( data, OutputSize, &data2[ COMPRESSION_HEADER_SIZE ] );
  if ( Temp == COMPRESSION_RETURNVALUE__ABORT || (COMPRESSION_ADAPTIVE_ARITHMETIC_CODING && Temp >= OutputSize) ) {  //revert arithmetic coding that increased size due to header (with very small files)
   Compression_compressorCopyData( data, OutputSize, &data2[ COMPRESSION_HEADER_SIZE ] );  //printf("Arithmetic-coded output would be %d bytes (vs %d without it), so omitting it.\n", Temp, OutputSize );
  }
  else {  //arithmetic-coding compresison is valid and really reduced size, fine  (adaptive enabling/disabling)
   OutputSize = Temp;
   Type |= COMPRESSION_HEADER_BITVALUE__TYPE__USED_ARITHMETIC_CODING;
   //if (COMPRESSION_ARITHMETIC_PROBABILITY_BYTECOUNT >= 3) Type |= COMPRESSION_HEADER_BITVALUE__TYPE__ARITHMETIC_CODING_3BYTE_VALUEBANDS;
  }
 }
 else Compression_compressorCopyData( data, OutputSize, &data2[
       Compression_compressModeSFX() ? COMPRESSION_SFX_HEADER_SIZE
        : ( Compression_compressModeRLESFX() ? COMPRESSION_RLESFX_HEADER_SIZE : COMPRESSION_HEADER_SIZE ) ] );  //or don't use arithmetic coding at all


 if (OutputSize > inputsize) { if (COMPRESSION_DEBUG) printf( "Compressor: Compressed payload (without header) output would be bigger than input, no point to compress as SFX. Doing it anyway.\n" ); } //return COMPRESSION_RETURNVALUE__ABORT; }


 if ( Compression_compressModeSFX() || Compression_compressModeRLESFX() ) { //SFX-mode: prepend and adjust self-extraction C64 code:
  InOutSizeDiff = inputsize - OutputSize;
  if (InOutSizeDiff < InOutSizeDiffMin) InOutSizeDiffMin = InOutSizeDiff;
  if (InOutSizeDiff > InOutSizeDiffMax) InOutSizeDiffMax = InOutSizeDiff;
  SizeDiffMin = (InOutSizeDiffMin > CompressionSizeDiffMin) ? CompressionSizeDiffMin : InOutSizeDiffMin;
  SizeDiffMax = (InOutSizeDiffMax < CompressionSizeDiffMax) ? CompressionSizeDiffMax : InOutSizeDiffMax;  if (COMPRESSION_VERBOSE) printf("Total(PRGheader): in:%d($%.4X) - out:%d($%.4X) = diff:%d  (diff-min:%d, diff-max:%d)\n",inputsize,inputsize,OutputSize,OutputSize,inputsize-OutputSize,SizeDiffMin,SizeDiffMax);

  C64inputDataBaseAddress = BASIC_START_ADDRESS
   + ( Compression_compressModeRLESFX() ? COMPRESSION_RLESFX_HEADER_SIZE : COMPRESSION_SFX_HEADER_SIZE )
   - COMPRESSION_PRG_HEADER_SIZE; //$8FF
  C64inputDataSize = OutputSize;
   C64inputDataEndAddress = C64inputDataBaseAddress + C64inputDataSize;
  C64outputDataEndAddress = C64outputDataBaseAddress + C64outputDataSize;  if (COMPRESSION_VERBOSE) printf( "C64 compressed data start:$%.4X, end:$%.4X, decompressed data will be at:$%.4X, ends at:$%.4X\n", C64inputDataBaseAddress, C64inputDataEndAddress, C64outputDataBaseAddress, C64outputDataEndAddress );

  InplaceExtractionOffset = inputsize - OutputSize + ( (SizeDiffMin < 0) * -SizeDiffMin ) + COMPRESSION_INPLACE_DECOMPRESSION_EXTRA_SAFETY_GAP;  //used only as default for PC-based decompression, dithcing it when setting C64 inplace-offset below   //ensure positive gap/slack during decompression too (non-overlapping current input-byte and output-byte addresses)
  C64inplaceExtractionOffset = (C64outputDataSize - C64inputDataSize)
    + ( Compression_compressModeSFX() ? COMPRESSION_PATTERNMATCH_PREFIX_SIZE__END_OF_DATA : 0 )
    + COMPRESSION_INPLACE_DECOMPRESSION_EXTRA_SAFETY_GAP;  if (COMPRESSION_VERBOSE) printf("In-place extraction Offset:  PC: %d ($%.4X) , C64: %d ($%.4X) (mindiff - PRGheader (+ endsignal))\n", InplaceExtractionOffset, InplaceExtractionOffset, C64inplaceExtractionOffset, C64inplaceExtractionOffset );  //to be on the safe side, always have at least a slack/gap between compressed and decompressed data in in-place decompression, e.g. with added end-of-data byte
  for (i=0; i < C64outputDataSize; ++i) {  //check overlaps between shifted source area vs decompression target area
   InOutIndexDiff = ( (Compression_CompressedDataIndexes[ i ] + C64inplaceExtractionOffset) - i ); //+ 1;
   if (InOutIndexDiff <= InOutIndexDiffMin) InOutIndexDiffMin = InOutIndexDiff;  //printf("%d-%d=%d  ", Compression_CompressedDataIndexes[ i ] + C64inplaceExtractionOffset, i, InOutIndexDiff );
  }  if (COMPRESSION_VERBOSE) printf( "Re-checked in-out index difference minimum with current inputdata-move target-index: %d\n", InOutIndexDiffMin );
  InplaceExtractionGap = (InOutIndexDiffMin </*=*/ 0) ? -InOutIndexDiffMin : 0;
  C64inplaceExtractionOffset += InplaceExtractionGap;  if (InOutIndexDiffMin </*=*/ 0 && COMPRESSION_VERBOSE) printf("In-out index-diff minimum %d is below 0, so increased C64 and PC move-offsets by it to %d($%.4X)\n",InOutIndexDiffMin,C64inplaceExtractionOffset,C64inplaceExtractionOffset);
  InplaceExtractionOffset = C64inplaceExtractionOffset;  //PC-based SFX-decompression should follow the re-checked offset instead of previous precalculation with just assumedly-right offset

  if ( C64outputDataEndAddress < C64inputDataEndAddress - InplaceExtractionGap
       - ( Compression_compressModeSFX() ? COMPRESSION_PATTERNMATCH_PREFIX_SIZE__END_OF_DATA : 0 )
       - COMPRESSION_INPLACE_DECOMPRESSION_EXTRA_SAFETY_GAP
      || C64outputDataBaseAddress >= C64inputDataEndAddress ) { //no need to move input data if output-endaddress is below or output-baseaddress is above input-endaddress, disable moving in the self-extractor
   if (COMPRESSION_VERBOSE) printf( "Decompressed output ($%.4X..$%.4X) will be safely below compressed input (endaddress $%.4X - $%.4X - EOF - safetygap = $%.4X) or above input-endaddress $%.4X, so not moving input-data in self-extractor.\n", C64outputDataBaseAddress, C64outputDataEndAddress-1, C64inputDataEndAddress,InplaceExtractionGap, C64inputDataEndAddress - InplaceExtractionGap - COMPRESSION_PATTERNMATCH_PREFIX_SIZE__END_OF_DATA - COMPRESSION_INPLACE_DECOMPRESSION_EXTRA_SAFETY_GAP, C64inputDataEndAddress );
   if ( Compression_compressModeSFX() ) {
    for (i=0; i < COMPRESSION_SFX_HEADER_SIZE; ++i) data2[i] = _binary_selfextract_nomove_prg_start[i];
   }
   /*else if ( Compression_compressModeRLESFX() ) {
    for (i=0; i < COMPRESSION_RLESFX_HEADER_SIZE; ++i) data2[i] = _binary_selfextract_rle_nomove_prg_start[i];
   }*/
   Compression_writeWord( data2, COMPRESSION_SFX_HEADER_INDEX__INPLACE_OFFSET, InplaceExtractionOffset ); //used by PC decompressor
   Compression_writeWord( data2, COMPRESSION_SFX_NOMOVE_HEADER_INDEX__RUN_ADDRESS, C64executionAddress );
   C64intermediateDataBaseAddress = C64inputDataBaseAddress;
   if ( Compression_compressModeRLESFX() ) {
    C64intermediateDataEndAddress = C64intermediateDataBaseAddress + C64inputDataSize;  if (COMPRESSION_VERBOSE) printf("Decompressed data end-addres will be: $%.4X, Intermediate compressed source-data end-address: $%.4X\n", C64outputDataEndAddress, C64intermediateDataEndAddress);
   }
  }
  else {  //will need to move input-data in self-extractor before decompression
   if ( Compression_compressModeSFX() ) {
    for (i=0; i < COMPRESSION_SFX_HEADER_SIZE; ++i) data2[i] = _binary_selfextract_prg_start[i];
   }
   /*else if ( Compression_compressModeRLESFX() ) {
    for (i=0; i < COMPRESSION_RLESFX_HEADER_SIZE; ++i) data2[i] = _binary_selfextract_rle_prg_start[i];
   }*/
   Compression_writeWord( data2, COMPRESSION_SFX_HEADER_INDEX__INPLACE_OFFSET, InplaceExtractionOffset );  //used by PC decompressor
   C64moveSourceEnd = C64inputDataEndAddress;
   C64intermediateDataBaseAddress = C64outputDataBaseAddress + C64inplaceExtractionOffset;  if (COMPRESSION_VERBOSE) printf("C64 output-address: $%.4X, intermediate compressed source address: $%.4X\n", C64outputDataBaseAddress, C64intermediateDataBaseAddress ); //first byte of moved input (intermediate compressed data read for in-place decompression)
   C64intermediateDataEndAddress = C64intermediateDataBaseAddress + C64inputDataSize;
   C64moveTargetEnd = C64intermediateDataEndAddress;  if (COMPRESSION_VERBOSE) printf("Decompressed data end-addres will be: $%.4X, Intermediate compressed source-data end-address: $%.4X\n", C64outputDataEndAddress, C64moveTargetEnd);
   if (C64moveTargetEnd > COMPRESSION_C64_ADDRESS_RANGE) { if (COMPRESSION_DEBUG) printf( "! *** Compressor: Moving compressed data-source by C64 self-extractor for in-place decompression would go over 64kbyte limit. Aborting.\n" ); return COMPRESSION_RETURNVALUE__ABORT;
    //Temp = C64moveTargetEnd - COMPRESSION_C64_ADDRESS_RANGE;  //Shaving off the extra byte(s) wouldn't work, it should be done properly before/inside compression
    //C64moveSourceEnd -= Temp; C64moveTargetEnd -= Temp; C64inputDataSize -= Temp;  //data2[ COMPRESSION_SFX_HEADER_SIZE + C64iputDataSize - 1 ] = Compression_convertPrefixType( COMPRESSION_PATTERNMATCH_PREFIX_BYTE1_VALUE__END_OF_DATA ); //forcing an end-signal at top of memory for the decompressor (possible last literal-sequence/pattern-prefix at the end is already truncated by compress64k.c)
   }
   C64initialMoveIndex = C64inputDataSize & COMPRESSION_LOWBYTE_MASK;
    data2[ COMPRESSION_SFX_HEADER_INDEX__MOVE_INDEXINIT ] = C64initialMoveIndex;
     data2[ COMPRESSION_SFX_HEADER_INDEX__MOVE_HIGHCOUNT ] = (C64inputDataSize >> COMPRESSION_HIGHBYTE_SHIFTS) + (C64initialMoveIndex != 0);  //+1 needed because 'BNE' is used in asm for loop-checking, but if initial index is 0, loops 1-time less
   C64moveSourceEnd -= C64initialMoveIndex + ( !C64initialMoveIndex * 0x100 ); //initial index 0 is a special case when copying starts a page lower (as index $00 becomes $FF at startup)
    Compression_writeWord( data2, COMPRESSION_SFX_HEADER_INDEX__MOVESOURCE_END, C64moveSourceEnd );  //last partial page of input (starting at it in asm)
   C64moveTargetEnd -= C64initialMoveIndex + ( !C64initialMoveIndex * 0x100 );  if (COMPRESSION_VERBOSE) printf("C64 mover code initial source-address: $%.4X, initial(top) index:$%.2X, initial target-address: $%.4X\n",C64moveSourceEnd,C64initialMoveIndex,C64moveTargetEnd);
    Compression_writeWord( data2, COMPRESSION_SFX_HEADER_INDEX__MOVETARGET_END, C64moveTargetEnd );  //last partial page of moved input ('itermediate' compressed data source area)
   Compression_writeWord( data2, COMPRESSION_SFX_HEADER_INDEX__RUN_ADDRESS, C64executionAddress );
  }
  Compression_writeWord( data2, COMPRESSION_SFX_HEADER_INDEX__INTERM_1ST, C64intermediateDataBaseAddress );
  Compression_writeWord( data2, COMPRESSION_SFX_HEADER_INDEX__OUTPUT_1ST, C64outputDataBaseAddress );  //(BASIC_START_ADDRESS /*- COMPRESSION_PRG_HEADER_SIZE*/);
  if ( Compression_compressModeRLESFX() ) {
   data2[ COMPRESSION_RLESFX_HEADER_INDEX__INTERM_END_L ] = C64intermediateDataEndAddress & COMPRESSION_LOWBYTE_MASK;
   data2[ COMPRESSION_RLESFX_HEADER_INDEX__INTERM_END_H ] = C64intermediateDataEndAddress >> COMPRESSION_HIGHBYTE_SHIFTS;
  }
  if ( C64outputDataSize < COMPRESSION_SFX__PROGRESS_COUNT_SIZE_THRESHOLD
       || ( C64outputDataBaseAddress <= COMPRESSION_SFX__PROGRESS_VRAM_ADDRESS
            && COMPRESSION_SFX__PROGRESS_VRAM_ADDRESS <= C64outputDataEndAddress ) ) {
   data2[ COMPRESSION_SFX_HEADER_INDEX__PROGRESS_VRAM_WRITE ] = BIT_ABS_opcode;  //if would overwrite output, substitute VRAM-writing STA with BIT
  }

  if ( Compression_compressModeRLESFX() ) {  //ensure that last 2byte repeat is not recognized as 3byte RLE escape-sequence if the next byte in memory has the same value: append a different value at the end
   Temp = data2[ COMPRESSION_RLESFX_HEADER_SIZE + C64inputDataSize - 1 ];
   for (i=0; i < COMPRESSION_RLE_REPEATCOUNT_COMPRESSION_THRESHOLD; ++i)  //(the decompressed size will still be the same as it's set in the RLESFX header above)
   { if ( data2[ COMPRESSION_RLESFX_HEADER_SIZE + C64inputDataSize - 1 - i ] != Temp ) break; }
   if (i == COMPRESSION_RLE_REPEATCOUNT_COMPRESSION_THRESHOLD) {  //the special case happened (2 identical bytes at the compressed datastream's end)
    data2[ COMPRESSION_RLESFX_HEADER_SIZE + C64inputDataSize ] = ~Temp; ++C64inputDataSize;  if (COMPRESSION_VERBOSE) printf("RLE-SFX/MEM last %d values are identical, so appending a different value at the end.\n",COMPRESSION_RLE_REPEATCOUNT_COMPRESSION_THRESHOLD);
   }
  }

  return ( Compression_compressModeRLESFX() ? COMPRESSION_RLESFX_HEADER_SIZE : COMPRESSION_SFX_HEADER_SIZE )
         + C64inputDataSize; //OutputSize;
 }


 else data2[ COMPRESSION_HEADER_INDEX__TYPE ] = Type;  //write 1byte header in normal PC-based (.cmp) compression mode


 return COMPRESSION_HEADER_SIZE + OutputSize;
}



int Compression_compressData (unsigned char* data, int inputsize, unsigned char* data2) {
 Compression_setCompressMode( COMPRESSION_MODE__NORMAL );
 return Compression_compressDataInMode( data, inputsize, data2 );
}

int Compression_compressReadOnlyData (unsigned char* source, int inputsize, unsigned char* target) {
 unsigned char *CompressionBuffer1 = NULL, *CompressionBuffer2 = NULL; int OutputSize;
 CompressionBuffer1 = malloc( COMPRESSION_BUFFERSIZE_MAX ); if (CompressionBuffer1 == NULL) return COMPRESSION_RETURNVALUE__ABORT;
 CompressionBuffer2 = malloc( COMPRESSION_BUFFERSIZE_MAX ); if (CompressionBuffer2 == NULL) return COMPRESSION_RETURNVALUE__ABORT;
 Compression_compressorCopyData( source, inputsize, CompressionBuffer1 );
 OutputSize = Compression_compressData( CompressionBuffer1, inputsize, CompressionBuffer2 );
 Compression_compressorCopyData( CompressionBuffer2, OutputSize, target );
 free( CompressionBuffer1 ); free( CompressionBuffer2 );
 return OutputSize;
}


int Compression_compressDataToInPlace (unsigned char* data, int inputsize, unsigned char* data2) {
 Compression_setCompressMode( COMPRESSION_MODE__IN_PLACE );
 return Compression_compressDataInMode( data, inputsize, data2 );
}


int Compression_compressDataToSFX (unsigned char* data, int inputsize, unsigned char* data2) {
 Compression_setCompressMode( COMPRESSION_MODE__SELF_EXTRACT );
 return Compression_compressDataInMode( data, inputsize, data2 );
}

int Compression_compressDataToRLESFX (unsigned char* data, int inputsize, unsigned char* data2) {
 Compression_setCompressMode( COMPRESSION_MODE__RLE_SELF_EXTRACT );
 return Compression_compressDataInMode( data, inputsize, data2 );
}


int Compression_compressReadOnlyDataToSFX (unsigned char* source, int inputsize, unsigned char* target) {
 unsigned char *CompressionBuffer1 = NULL, *CompressionBuffer2 = NULL; int OutputSize;
 CompressionBuffer1 = malloc( COMPRESSION_BUFFERSIZE_MAX ); if (CompressionBuffer1 == NULL) return COMPRESSION_RETURNVALUE__ABORT;
 CompressionBuffer2 = malloc( COMPRESSION_BUFFERSIZE_MAX ); if (CompressionBuffer2 == NULL) return COMPRESSION_RETURNVALUE__ABORT;
 Compression_compressorCopyData( source, inputsize, CompressionBuffer1 );
 OutputSize = Compression_compressDataToSFX( CompressionBuffer1, inputsize, CompressionBuffer2 );
 Compression_compressorCopyData( CompressionBuffer2, OutputSize, target );
 free( CompressionBuffer1 ); free( CompressionBuffer2 );
 return OutputSize;
}

int Compression_compressReadOnlyDataToRLESFX (unsigned char* source, int inputsize, unsigned char* target) {
 unsigned char *CompressionBuffer1 = NULL, *CompressionBuffer2 = NULL; int OutputSize;
 CompressionBuffer1 = malloc( COMPRESSION_BUFFERSIZE_MAX ); if (CompressionBuffer1 == NULL) return COMPRESSION_RETURNVALUE__ABORT;
 CompressionBuffer2 = malloc( COMPRESSION_BUFFERSIZE_MAX ); if (CompressionBuffer2 == NULL) return COMPRESSION_RETURNVALUE__ABORT;
 Compression_compressorCopyData( source, inputsize, CompressionBuffer1 );
 OutputSize = Compression_compressDataToRLESFX( CompressionBuffer1, inputsize, CompressionBuffer2 );
 Compression_compressorCopyData( CompressionBuffer2, OutputSize, target );
 free( CompressionBuffer1 ); free( CompressionBuffer2 );
 return OutputSize;
}
