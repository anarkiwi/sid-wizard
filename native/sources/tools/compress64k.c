//A simple RLE, dictionary-based pattern-matching, and arithmetic-coding based compressor by Hermit

#include "compressor.c"
#if (COMPRESSION_VERIFY)
 #include "decompressor.c"
#endif

//Common headers and functions for both 'compress' and 'decompress' runnables
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <unistd.h>
#include <dirent.h>  //for filesize-determination
#include <sys/stat.h>  //for filesize-determination
#include <math.h>  //for log2f() (used in entropy-calculation)
#include <libgen.h>  //for basename()


static enum {
 FILENAME_SIZE_MAX = 1024,
 //COMPRESSION_BUFFER_SLACK = 2,
 DECOMPRESSION_BUFFER_SIZES = COMPRESSION_BUFFERSIZE_MAX /*1000000*/ /*bytes*/, //INPUTDATA_SIZE_MAX = 65536, //OUTPUTDATA_SIZE_MAX = INPUTDATA_SIZE_MAX,  //keep in mind compression might actually increase size
 EXITCODE__OK = 0, EXITCODE__FILE_OPEN_ERROR = -1, EXITCODE__FILE_IS_EMPTY = -1, EXITCODE__FILE_TOO_BIG = -2,
 EXITCODE__MALLOC_ERROR = -3, EXITCODE__VERIFICATION_ERROR = -4
} CompressionApplication_Specifications;


typedef  uint8_t  byte;
typedef  uint16_t word;
typedef  uint32_t dword;



static char* getFilenameExtension (char *filename) //get pointer of file-extension from filename string
{  //if no '.' found, point to end of the string
 char* LastDotPos = strrchr( filename,'.' );
 if (LastDotPos == NULL) return ( filename+strlen(filename) ); //make strcmp not to find match, otherwise it would be segmentation fault
 return LastDotPos;
}
static void cutFilenameExtension (char *filename) { //cut the extension of the filename by putting 0 at position of '.'
 *getFilenameExtension( filename ) = '\0'; //cut original extension with 0 string-delimiter
}
static void changeFilenameExtension (char *filename, const char *new_extension) { //change the extension of the file 
 cutFilenameExtension( filename ); //omit original extension with 0 string-delimiter
 strcat( filename, new_extension ); //expand with new extension
}



static void* loadFileIntoMemory (char* filename, int* size) {
 unsigned char* Address = NULL; //static long FileSize;
 struct stat FileStats;
 if ( stat( filename, &FileStats ) < 0 ) { printf( "Couldn't find file \"%s\" or couldn't determine its size.\n", filename ); return NULL; }
 FILE *File = fopen( filename, "rb" );
 if (File == NULL) { printf( "Couldn't open file %s\n", filename ); return NULL; }
 //fseek( File, 0, SEEK_END ); FileSize = ftell( File ); rewind( File );
 *size = FileStats.st_size;
 Address = (unsigned char*) malloc( /*FileSize*/ /*FileStats.st_size*/ (*size) + 1 ); if (Address == NULL) { printf( "Couldn't allocate memory for file %s\n", filename ); return NULL; }
 size_t ReadCount = fread( Address, 1, /*FileSize*/ /*FileStats.st_size*/ *size, File );  //printf("%p,%d,%d,$%2.2X,$%2.2X\n",Address,FileStats.st_size,ReadCount,Address[0],Address[ReadCount-1]);
 Address[ ReadCount ] = '\0'; fclose( File ); //adding string termination considering case of a string-file
 return Address;
}



static float calculateEntropy (unsigned char* data, int datasize) {
 static enum { BITS_PER_BYTE = 8, BYTE_VALUE_RANGE = (1 << BITS_PER_BYTE) /*0x100*/ } Specs;
 int i, j;  float ByteValueProbability, Entropy;
 static int ByteValueCounts [BYTE_VALUE_RANGE];

 for (i=0; i < BYTE_VALUE_RANGE; ++i) ByteValueCounts[i] = 0;
 for (i=0; i < datasize; ++i) ++ByteValueCounts[ (int)data[i] ];

 for (i = Entropy = 0; i < BYTE_VALUE_RANGE; ++i) {
  if (ByteValueCounts[i] > 0) {
   ByteValueProbability = (float) ByteValueCounts[i] / (float) datasize;
   Entropy += -ByteValueProbability * log2f( ByteValueProbability );
  }
 }

 return Entropy;
}


#ifndef VERSION
 #define VERSION ""
#endif


int main (int argc, char *argv[]) {
 int i, SFXmode=0, MEMmode=0, RLESFXmode=0, RLEMEMmode=0;
 int ReadData, InputDataSize=0, PRGstartAddress = 0, SFXendAddress = 0, OutputDataSize=0;
 FILE *OutputFile;
 char InputFileName [FILENAME_SIZE_MAX];  //input-file described as 1st command-line parameter
 char OutputFileName [FILENAME_SIZE_MAX];  //output-file
 static byte /**Data = NULL,*/ *Data2 = NULL;  //static byte Data [INPUTDATA_SIZE_MAX * COMPRESSION_OVERHEAD_SLACK], Data2 [INPUTDATA_SIZE_MAX * COMPRESSION_OVERHEAD_SLACK];
 #if (COMPRESSION_VERIFY)
  int VerifyInputSize=0, VerifyOutputSize=0, DifferenceIndex = -1, CompressedVerifyIndex, HeaderSize;
  static byte *InputData = NULL, *OutputData = NULL, *VerifyData = NULL;
 #endif

 //mutually exclusive modes (with support for decompression on native C64):
 SFXmode = !strcmp( basename( argv[0] ), "compress64kSFX" ) || !strcmp( basename( argv[0] ), "cmp64sfx" )
           || !strcmp( basename( argv[0] ), "compress64kSFX.exe" ) || !strcmp( basename( argv[0] ), "cmp64sfx.exe" );  //based on Ian Coog's observations/mods
 MEMmode = !strcmp( basename( argv[0] ), "compress64kMEM" ) || !strcmp( basename( argv[0] ), "cmp64mem" )
           || !strcmp( basename( argv[0] ), "compress64kMEM.exe" ) || !strcmp( basename( argv[0] ), "cmp64mem.exe" );
 RLESFXmode = !strcmp( basename( argv[0] ), "compress64kRLESFX" ) || !strcmp( basename( argv[0] ), "cmp64rlesfx" )
           || !strcmp( basename( argv[0] ), "compress64kRLESFX.exe" ) || !strcmp( basename( argv[0] ), "cmp64rlesfx.exe" );
 RLEMEMmode = !strcmp( basename( argv[0] ), "compress64kRLEMEM" ) || !strcmp( basename( argv[0] ), "cmp64rlemem" )
           || !strcmp( basename( argv[0] ), "compress64kRLEMEM.exe" ) || !strcmp( basename( argv[0] ), "cmp64rlemem.exe" );

 if ( argc < 2 || !strcmp(argv[1],"-h") || !strcmp(argv[1],"--h") || !strcmp(argv[1],"-help") || !strcmp(argv[1],"--help") ) {
  printf ( "\n================================================\n"
             " Binary file compressor (for <64kByte size) " VERSION "\n"
             "------------------------------------------------\n"
             "Usage:  compress64k <inputfile> [outputfile]\n"
             "------------------------------------------------\n"
             "            2026 Hermit Software            \n"
             "================================================\n" );
  printf("\nPlease specify the input-file name.\n\n"); return EXITCODE__OK;
 }

 strcpy( InputFileName, argv[1] );
 InputData = loadFileIntoMemory( InputFileName, &InputDataSize );
 if (InputData == NULL) { printf( "\n! Could not open input-file...\n\n" ); exit( EXITCODE__FILE_OPEN_ERROR ); }
 if (InputDataSize == 0) { printf( "\n! Empty input-file...\n\n" ); exit( EXITCODE__FILE_IS_EMPTY ); }
 OutputData = malloc( DECOMPRESSION_BUFFER_SIZES );  //input might be just a few bytes, giving no room for prefixes, arithmetic-coding header, etc.
 if (/*Data == NULL ||*/ OutputData == NULL) { printf( "Couldn't allocate compression buffer, aborting.\n" ); exit( EXITCODE__MALLOC_ERROR ); }
 #if (COMPRESSION_VERIFY)
  if (InputData == NULL) { printf( "Couldn't allocate input-data verification buffer, aborting.\n" ); exit( EXITCODE__MALLOC_ERROR ); }
 #endif

 if (argc >= 3) strcpy( OutputFileName, argv[2] );
 else {
  strcpy( OutputFileName, InputFileName );
  changeFilenameExtension( OutputFileName, SFXmode ? ".sfx.prg" : ( MEMmode ? ".mem"
   : ( RLESFXmode ? ".rlesfx.prg" : ( RLEMEMmode ? ".rlemem" : ".cmp" ) ) ) );
 }

 printf( "File-size before compression \"%s\": %d ($%X) bytes  (Entropy: %.2f bits/byte)\n", InputFileName, InputDataSize, InputDataSize, calculateEntropy( InputData, InputDataSize ) );
 if (SFXmode || MEMmode || RLESFXmode || RLEMEMmode) {
  PRGstartAddress = InputData[0] + (InputData[1] << COMPRESSION_HIGHBYTE_SHIFTS);
  SFXendAddress = PRGstartAddress + (InputDataSize - COMPRESSION_PRG_HEADER_SIZE);
  if (SFXendAddress > COMPRESSION_SFX_EXTRACTED_DATA_ENDADDRESS_MAX) {
   printf( "! *** SFX extracted data end-address would be $%.4X.\n! *** Truncating input by %d byte(s) for 1byte compressed data end-signal at end of C64 memory.\n", SFXendAddress, SFXendAddress - COMPRESSION_SFX_EXTRACTED_DATA_ENDADDRESS_MAX);
   InputDataSize -= SFXendAddress - COMPRESSION_SFX_EXTRACTED_DATA_ENDADDRESS_MAX;  printf( "! *** New input-data size is %d($%.4X) (and end-address is $%.4X)\n", InputDataSize, InputDataSize, COMPRESSION_SFX_EXTRACTED_DATA_ENDADDRESS_MAX );
 }}
 OutputDataSize = (SFXmode || MEMmode) ? Compression_compressReadOnlyDataToSFX( InputData, InputDataSize, OutputData )  //for in-place decompression to self-extract on C64
                   : ( (RLESFXmode || RLEMEMmode) ? Compression_compressReadOnlyDataToRLESFX( InputData, InputDataSize, OutputData )  //for in-place decompression to self-extract on C64
                     : Compression_compressReadOnlyData( InputData, InputDataSize, OutputData ) );  //classic 'raw' compression

 if (OutputDataSize != COMPRESSION_RETURNVALUE__ABORT) {
  if (MEMmode) {  //removing SFX-header from 'MEM' output-data and adding 'MEM' header aka decompression target-address
   OutputData += COMPRESSION_SFX_HEADER_SIZE - COMPRESSION_MEM_HEADER_SIZE;  OutputDataSize -= COMPRESSION_SFX_HEADER_SIZE - COMPRESSION_MEM_HEADER_SIZE;
  }
  else if (RLEMEMmode) {  //removing SFX-header from 'MEM' output-data and adding 'MEM' header aka decompression target-address
   OutputData += COMPRESSION_RLESFX_HEADER_SIZE - COMPRESSION_MEM_HEADER_SIZE;  OutputDataSize -= COMPRESSION_RLESFX_HEADER_SIZE - COMPRESSION_MEM_HEADER_SIZE;
  }
  printf( "Compressed file \"%s\" size: %d ($%X) bytes (%d%%)  (Entropy: %.2f bits/byte)\n", OutputFileName, OutputDataSize, OutputDataSize, OutputDataSize * 100 / InputDataSize, calculateEntropy( OutputData, OutputDataSize ) );
  if (OutputDataSize > InputDataSize && COMPRESSION_DEBUG) printf( "Output-file size is bigger than input-file size. Saving anyway.\n" );
  OutputFile = fopen( OutputFileName, "wb" );
  if (OutputFile == NULL) { printf( "\n! Couldn't create output-file...\n\n" ); exit( EXITCODE__FILE_OPEN_ERROR ); }
  if (MEMmode || RLEMEMmode) { //writing out 'MEM'-format (PRG-)header (decompression target-address) and preparation for writing the rest
   fputc( PRGstartAddress & COMPRESSION_LOWBYTE_MASK, OutputFile ); fputc( PRGstartAddress >> COMPRESSION_HIGHBYTE_SHIFTS, OutputFile );
   OutputData += COMPRESSION_MEM_HEADER_SIZE;  OutputDataSize -= COMPRESSION_MEM_HEADER_SIZE;
  }
  for (i=0; i < OutputDataSize; ++i) fputc( OutputData[i], OutputFile );
  if (OutputFile != NULL) fclose( OutputFile );
  if (MEMmode) { OutputData -= COMPRESSION_SFX_HEADER_SIZE;  OutputDataSize += COMPRESSION_SFX_HEADER_SIZE; }  //restoring SFX-header for verification
  else if (RLEMEMmode) { OutputData -= COMPRESSION_RLESFX_HEADER_SIZE;  OutputDataSize += COMPRESSION_RLESFX_HEADER_SIZE; }  //restoring SFX-header for verification

  #if (COMPRESSION_VERIFY)  //verify AFTER writing output-file, so it can be checked for content if verification fails
   if (COMPRESSION_VERBOSE) printf( "Verifying by in-memory decompression whether the decompressed data would match the uncompressed original data.\n" );
   VerifyData = malloc( DECOMPRESSION_BUFFER_SIZES );
    if (VerifyData == NULL) { printf( "Couldn't allocate verification buffers, aborting.\n" ); exit(EXITCODE__MALLOC_ERROR); }
   VerifyInputSize = OutputDataSize;
   VerifyOutputSize = (SFXmode || MEMmode) ? Compression_decompressReadOnlySFXdata( OutputData, VerifyInputSize, &VerifyData )
                       : ( (RLESFXmode || RLEMEMmode) ? Compression_decompressReadOnlyRLESFXdata( OutputData, VerifyInputSize, &VerifyData )
                            : Compression_decompressReadOnlyData( OutputData, VerifyInputSize, &VerifyData ) );
   if ( ( DifferenceIndex = Compression_compareData( InputData, InputDataSize /*VerifyOutputSize*/, VerifyData ) ) >= 0 ) {  //debug difference's place (ignoring extra bytes that might be expectable end-of-data or similar)
    changeFilenameExtension( OutputFileName, ".vfy" );
    OutputFile = fopen( OutputFileName, "wb" );
    for (i=0; i < VerifyOutputSize; ++i) fputc( VerifyData[i], OutputFile );
    if (OutputFile != NULL) fclose( OutputFile );
    printf( "! *** Decompression self-verification failed at uncompressed-data index $%.4X!\n Compressed file and \"%s\" comparison file written anyway for comparison with diff/dhex/etc.\n", DifferenceIndex, OutputFileName );
    #if (COMPRESSION_DEBUG_VERIFICATION)
     #define VERIFY_DEBUG_LENGTH  10
     #define VERIFY_DEBUG_PREVCOUNT  6
     HeaderSize = (SFXmode || MEMmode) ? COMPRESSION_SFX_HEADER_SIZE
                    : ( (RLESFXmode || RLEMEMmode) ? COMPRESSION_RLESFX_HEADER_SIZE : COMPRESSION_HEADER_SIZE);
     CompressedVerifyIndex = Compression_CompressedDataIndexes[ DifferenceIndex ];
     printf( " The compressed data's corresponding index: $%.4X\n", CompressedVerifyIndex );
     printf( " Uncompressed data around($%.4X): ", DifferenceIndex ); for (i=0; DifferenceIndex>=VERIFY_DEBUG_PREVCOUNT && i<VERIFY_DEBUG_PREVCOUNT; ++i) printf( "$%.2X ", InputData[DifferenceIndex-VERIFY_DEBUG_PREVCOUNT+i] );  printf(", "); for (i=0; i<VERIFY_DEBUG_LENGTH; ++i) printf( "$%.2X ", InputData[DifferenceIndex+i] );  printf("  ...\n");
     printf( " Decompressed data around($%.4X): ", DifferenceIndex ); for (i=0; DifferenceIndex>=VERIFY_DEBUG_PREVCOUNT && i<VERIFY_DEBUG_PREVCOUNT; ++i) printf( "$%.2X ", VerifyData[DifferenceIndex-VERIFY_DEBUG_PREVCOUNT+i] );  printf(", "); for (i=0; i<VERIFY_DEBUG_LENGTH; ++i) printf( "$%.2X ", VerifyData[DifferenceIndex+i] );  printf("  ...\n");
     printf( " Compressed data around  ($%.4X,file:$%.4X): ", CompressedVerifyIndex, CompressedVerifyIndex+HeaderSize );
     for (i=0; CompressedVerifyIndex+HeaderSize>=VERIFY_DEBUG_PREVCOUNT && i<VERIFY_DEBUG_PREVCOUNT; ++i) printf( "$%.2X ", OutputData[ CompressedVerifyIndex+HeaderSize + i - VERIFY_DEBUG_PREVCOUNT ] );  printf(", ");
     for (i=0; i<VERIFY_DEBUG_LENGTH && CompressedVerifyIndex+HeaderSize+i < OutputDataSize; ++i) printf( "$%.2X ", OutputData[ CompressedVerifyIndex+HeaderSize + i ] );  if(CompressedVerifyIndex+HeaderSize+i<OutputDataSize) printf("  ...\n"); else printf("\n");
    #endif
    exit( EXITCODE__VERIFICATION_ERROR );
   }
   else if (COMPRESSION_VERBOSE) printf( "Decompression self-verification was successful. (Input-data and decompressed data matches.)\n" );
  #endif

 }
 else printf( "! *** Compression was inappropriate and aborted. (Enabled debugging shows details too.)\n" );

 printf("\n");  //separate different runs better

 if (InputData != NULL) free( InputData ); if (OutputData != NULL) free( OutputData );
 #if (COMPRESSION_VERIFY)
  if (VerifyData != NULL) free( VerifyData );
 #endif
 return EXITCODE__OK;
}


