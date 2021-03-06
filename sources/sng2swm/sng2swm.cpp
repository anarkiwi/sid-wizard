/********************************************
// $Id: sng2swm.cpp 360 2014-02-15 14:50:28Z soci $
// Goat2Wizard
//
// This program converts a GT2 .sng file into
// the Sid-Wizard V1 (SWM) format.
//
// Author:  Conrad/Samar and Hermit
//
// Source code if free of charge!
********************************************/

// Includes...
#include <cstdlib>
#include <cctype> //#include <iostream> //iostream was huge, using -s parameter for g++ and modifying this line reduces executable file-size significantly on windows
#include <cstdio>
#include <cstring>
#include "swm.h"
#include "sng.h"

        sng_table gt2_wavetable;
        sng_table gt2_pulsetable;
        sng_table gt2_filtertable;
        swm_instrument * sw_instruments = 0;

/*******************************************************************************
// Functions:
*******************************************************************************/

bool ParseFilename(char * arg, char ** dest, const char * what, char ** output_ext)
{
    char * ext = arg + (strlen(arg)-strlen(what)) ;
    
    if (!strcmp(ext,what))
    {
     *dest=arg; *output_ext=ext; return true;
    }

    return false;
}


void ReadGT2Orderlist(sng_orderlist * orderlist, FILE * f)
{
   fread(&orderlist->length, 1, 1, f);
   if (orderlist->length)
   {
       orderlist->data = new unsigned char[orderlist->length];
       fread(orderlist->data, 1, orderlist->length, f);
   }
   fread(&orderlist->restart, 1, 1, f);         
}


void ReadGT2Pattern(sng_pattern * pattern, FILE * f)
{
   fread(&pattern->rows, 1, 1, f);
   if (pattern->rows)
   {
       pattern->data = new sng_pattern_row[pattern->rows];
       fread(pattern->data, sizeof(sng_pattern_row), pattern->rows, f);
   }
}


void ReadGT2Table(sng_table * table, FILE * f, const char * type)
{
   fread(&table->length, 1, 1, f);
   if (table->length)
   {
      table->left = new unsigned char[table->length];
      table->right = new unsigned char[table->length];
      fread(table->left, 1, table->length, f);
      fread(table->right, 1, table->length, f);
   }
   printf("%s-table length: %i\n", type, table->length);
}


//slide (FREQMODL,FREQMODH) calculation in SID-Wizard 1.2 (if non-zero): 
// ExpTabIndex=(Input/2)+DPITCH; //(pitch-dependent) 
// if (ExpPabIndex<ExpTreshold) { FREQMODL=SWExpTabL[ExpTabIndex]; FREQMODH=0} 
// else { FREQMODL=SWExpTabL[ExpTabIndex-ExpTreshold]; FREQMODH=SWExpTabH[ExpTabIndex-ExpTreshold]
// this slide-value converter function has to do the inverse of this operation:
int GTsldToSWsld(int GTpos, unsigned char Dpitch, sng_table * tbl)
{
 int i, PitchTemp, SWslide, GTslide = (tbl->left[GTpos-1])*256 + tbl->right[GTpos-1];
 if (GTpos==0 || GTslide==0) return 0x00;
 else if (GTslide<0x100) for (i=EXPTRESHOLD-1; i>=0; i--) //if small pitch slide (fits in low byte of pitch)
 {
  if (SWexpTabH[i] <= GTslide) { SWslide=(i-Dpitch)*2; break; } //if found the matching index, calculate slide-value and exit the loop
 }
 else 
 { 
  for (i=FREQTB_SIZE-1; i>=0; i--) //if big pitch-slide using both low&high bytes of SID-pitch
  {
   PitchTemp=(SWexpTabH[i+FREQTBH_POS]*0x100+SWexpTabL[i]);
   if ( PitchTemp <= GTslide ) { SWslide=((i+EXPTRESHOLD)-Dpitch)*2; break; }  //if found the matching index, calculate slide-value and exit the loop
   else SWslide=(EXPTRESHOLD-Dpitch)*2; //smallest value for big pitch-slide
  }
 }
 //printf("%x-%X-%X-%x ",i,PitchTemp,GTslide, SWslide);
 if (SWslide < 0 ) SWslide=0; //take care of minimal limit which is 0 of course
 if (SWslide > 0xFF) SWslide=0xFF; //take care of maximal limit if dpitch causes overload
 return SWslide;
}

//vibrato-amplitude (FREQMODL,FREQMODH) calculation in SID-Wizard 1.2 (if non-zero): the same as slide but Input value is halved
int GTvibToSWvib(int GTpos, unsigned char Dpitch, sng_table * tbl)
{
 int i,SWamp, GTfreq=tbl->left[GTpos-1], GTamp = tbl->right[GTpos-1], SWvibr;
 if (GTpos==0 || GTamp==0) return 0x00;
 if (GTfreq>0xF) GTfreq=0xF; //take care of limit in SID-Wizard
 //GT has calculated vibrato if GTamp is bigger than $7F (bit7 set) - should be converted differently (omitting 'Dpitch' from the equation)
 for (i=EXPTRESHOLD-1; i>=0; i--) //convert GT-amplitude to SW-amplitude (i.e. slide-speed)
 {
  if (SWexpTabH[i] <= GTamp) { SWamp=(i-Dpitch)*4; break; } //if found the matching index, calculate slide-value and exit the loop
 }
 //printf("%x-%X-%X ",i,GTamp, SWamp);
 if (SWamp < 0x10 ) SWamp=0x10; //take care of minimal limit
 if (SWamp > 0xFF) SWamp=0xFF; //take care of maximal limit if dpitch causes overload
 SWvibr = (SWamp&0xF0) + SWvibFreq[GTfreq];
 return SWvibr;
}


unsigned char GTtblToSWtbl(sng_table * gt2_table, unsigned char GTstartIndex, swm_instrument * sw_instr, char TableType, bool debug)
{ //TableType: 0:Waveform-table, 1:Pulse-table, 2:Filter-table
 unsigned int SWtableStart, SWtableIndex;
 unsigned char GTvalLeft, GTvalRight, GTvalThird, prevGTvalLeft=0, PrevReso=0xF,PrevBand=0x9,PrevCutoff=0xFF, SkippedRowCount=0;
 int GTtableIndex;

 if (!(TableType&0x80))
 {
  SWtableStart=SWtableIndex = sw_instr->tables.size(); //init target (SW) index 
 }
 else
 {
  switch (TableType&0x7F)
  {
   case 0: SWtableStart=SWtableIndex = 0;
    break;
   case 1: SWtableStart=SWtableIndex = 0;
    break;
   case 2: SWtableStart=SWtableIndex = 0;
    break;
  }
  return 0;
 }

 
if ( (GTtableIndex=GTstartIndex) ) //convert GT-table
 {
  SkippedRowCount=0; //when more table-rows brought into one (for filter)
  for ( ;GTtableIndex<=0xFF; GTtableIndex++)
  {
   GTvalLeft=gt2_table->left[GTtableIndex-1]; GTvalRight=gt2_table->right[GTtableIndex-1]; GTvalThird=0;
   if (debug) printf("[%.2X: %.2X %.2X]", GTtableIndex, GTvalLeft, GTvalRight);
   if (!(TableType&0x7F) && GTvalLeft==0) GTvalLeft=prevGTvalLeft; //avoid inaudible waveform (when $FE loops)
   if (GTvalLeft==GT2_TABLE_JUMP) 
   {
    if (GTvalRight==0) {if (debug) printf(" -> [(%.2X)%.2X: %.2X\n",SWtableIndex, SW1_INSTRUMENT_PARAMSIZE+SWtableIndex, SW1_TABLE_END); break;} //$FF 00 ending
    else if (!(TableType&0x7F) && GTvalRight==GTtableIndex) { GTvalLeft=SW1_TABLE_JUMP; GTvalRight=SW1_INSTRUMENT_PARAMSIZE+0xC0; } //self-jumping in Wave-table needs special treatment 
    else if (GTvalRight < GTstartIndex || GTvalRight > GTtableIndex) // - or merge if jumped outside the already converted table
         { GTtableIndex=GTstartIndex=GTvalRight-1; SWtableStart=SWtableIndex-3; SkippedRowCount=0; if(debug)printf(" -> merging table-jump!\n"); continue; } 
    else {GTvalLeft=SW1_TABLE_JUMP; GTvalRight=SW1_INSTRUMENT_PARAMSIZE+SWtableStart + 3*((GTvalRight-GTstartIndex)-SkippedRowCount); } //$FF xx loop/jump ending -> convert to SW: $FE xx
   }
   else if (!(TableType&0x7F) && GTvalLeft>=GT2_WFTABLE_FX) GTvalLeft=GTvalRight=0; //no Wavetable-FX support in SW
   else if (!(TableType&0x7F)) //in case of normal waveform or speed-setting handle arp/pitch values
   {
    if(GT2_ARP_REL_MIN<=GTvalRight && GTvalRight<GT2_ARP_NOP) GTvalRight += (SW1_ARP_REL_MIN-GT2_ARP_REL_MIN); //convert negative relative pitches
    else if (GT2_ARP_ABS_MIN<=GTvalRight && GTvalRight<GT2_ARP_ABS_MAX) GTvalRight+=1; //convert absolute-pitches (starts from C#0 in GT, and from C-0 in SW)
   }
   else if ((TableType&0x7F)==2) //filtertable?
   {
    if (GTvalLeft==0) GTvalLeft=PrevBand*0x10 + PrevReso; //convert GT cutoff-setting to SW band+reso+cutoff setting
    else if (GTvalLeft&0x80) //band/reso/filterswitch setting in GT?
    { 
     PrevReso=GTvalRight/0x10; PrevBand=(GTvalLeft&0xF0)/0x10; GTvalLeft = PrevBand*0x10 + PrevReso;  GTvalThird=(GTvalRight&0xF)+0x80;
     if (gt2_table->left[(GTtableIndex-1)+1]==0) { GTvalRight=gt2_table->right[(GTtableIndex-1)+1]; GTtableIndex++; SkippedRowCount++; } //cutoff-setting in next row? set and skip next row
     else GTvalRight=PrevCutoff; //if no cutoff-setting follows, use previous cutoff
    } 
    else 
    { //sweep is only in 8 bit resolution, convert it to SW which sweeps in 11bit resolution
     if (GTvalRight<=0x7F) GTvalRight = (GTvalRight*8<=0x7F) ? GTvalRight*8 : 0x7F; //take care of maximal value
     else if (GTvalRight>=0x80) GTvalRight = ((0x100-GTvalRight)*8<0x80) ? 0x100-(0x100-GTvalRight)*8 :0x80; //take care of minimal value (can be overridden in GT's non-11bit-fine slides)
    }
   }
   if (!(TableType&0x80)) 
   {
    sw_instr->tables.push_back(GTvalLeft);
    sw_instr->tables.push_back(GTvalRight);
    sw_instr->tables.push_back(GTvalThird);
   }
   else
   {
    //sw_instr->tables.insert(SWtableIndex, GTvalThird);
    //sw_instr->tables.insert(SWtableIndex, GTvalRight);
    //sw_instr->tables.insert(SWtableIndex, GTvalLeft);
   }
   if (debug) printf(" -> [(%.2X)%.2X: %.2X %.2X %.2X]\n", SWtableIndex, SW1_INSTRUMENT_PARAMSIZE+SWtableIndex, sw_instr->tables[SWtableIndex], sw_instr->tables[SWtableIndex+1], sw_instr->tables[SWtableIndex+2]);
   SWtableIndex+=3; prevGTvalLeft=GTvalLeft;
   if (gt2_table->left[GTtableIndex-1]==GT2_TABLE_JUMP) break; //$FF xx loop/jump ending 
  }
 }
 sw_instr->tables.push_back(SW1_TABLE_END);SWtableIndex++; 
 return SWtableIndex;
}


void GTseqToSWseq (sng_orderlist * gt_seq , swm_sequence * sw_seq, bool debug)
{
 int j,k,size=0;
 if(debug) printf("\n\n*GT :  ");
 for (j=0; j<gt_seq->length; j++) 
 {
  if(debug) printf("%X  ",gt_seq->data[j]);
  if (gt_seq->data[j]==SW1_SEQUENCE_LOOPSONG) sw_seq->data.push_back(gt_seq->data[j]);
  else if ( gt_seq->data[j] >= GT2_ORDERLIST_TRANS_MIN ) sw_seq->data.push_back(gt_seq->data[j]-(GT2_ORDERLIST_TRANS-SW1_SEQUENCE_TRANS));
  else if (gt_seq->data[j] >= GT2_ORDERLIST_REPEAT) 
  {
   for (k=0;k<(gt_seq->data[j]&0xF);k++) sw_seq->data.push_back(gt_seq->data[j+1]+1); //SW pattern-numbers count from 1  
   size += k-1;
  }
  else sw_seq->data.push_back(gt_seq->data[j]+1); //SW pattern-numbers count from 1  
  size++;
 }
 sw_seq->data.push_back(gt_seq->restart); size++;
 sw_seq->size = size;
 if (debug) { printf("\n*SW :  "); for(j=0;j<sw_seq->size;j++) printf("%X  ",sw_seq->data.at(j)); }
}


void GTptnToSWptn (sng_pattern * gt_ptn , swm_pattern * sw_ptn , sng_table * spdtbl , bool debug)
{
 unsigned char j,GTnote,SWnote,GTins,SWins,GTfx,SWfx,GTval,SWval,SWindex=0;
 unsigned char Dpitch=(char)(SW1_NOTE_MAX/2); //buffer for valid notes (discrete pitch) - init to the middle of the note-region
 unsigned char PrevSWins=0, PrevSWfx=0, PrevSWval=0; //used to eliminate repetitive GT effects
 for (j=0; j<gt_ptn->rows-1; j++)
 {
  GTnote=gt_ptn->data[j].note;
  if (GTnote==GT2_PATTERN_REST) SWnote=SW1_PTNCOL1_REST;
  else if (GTnote==GT2_PATTERN_KEYOFF) SWnote=SW1_PTNCOL1_KEYOFF;
  else if (GTnote==GT2_PATTERN_KEYON) SWnote=SW1_PTNCOL1_KEYON;
  else if (GTnote>=GT2_PATTERN_FIRSTPACKEDREST) SWnote=SW1_PTNCOL1_REST;
  else if (GTnote>=GT2_PATTERN_NOTE) //register valid note
   { SWnote=Dpitch=GTnote-(GT2_PATTERN_NOTE-SW1_PTNCOL1_NOTE); PrevSWfx=PrevSWval=0; } 
  else SWnote=SW1_PTNCOL1_REST;
  
  SWins=SWfx=SWval=0; //init
  
  if (SWindex < SW1_PATTERNSIZE_MAX)
  {
   GTins=gt_ptn->data[j].instrument;
   if (SW1_INSTRUMENT_MIN<=GTins && GTins<=SW1_INSTRUMENT_MAX && PrevSWins!=GTins) 
   { SWins=PrevSWins=GTins; SWnote|=0x80; } //sign in note-column 
   else SWins=0;
   
   GTfx=gt_ptn->data[j].command; GTval=gt_ptn->data[j].parameter;
   
   switch (GTfx)
   {     //continuous GT commands - convert them to one-shot form of SW
    case GT2_COMMAND_NOP: //00?
     if (GTnote!=GT2_PATTERN_END && (PrevSWfx==SW1_BIGFX_PORTUP || PrevSWfx==SW1_BIGFX_PORTDOWN || PrevSWfx==SW1_BIGFX_VIBRATO) ) 
      { SWfx=PrevSWfx; SWval=0x00; } break; //stop slideup/slide-down/vibrato if ends in GT (portamento stops automatically)
    case GT2_COMMAND_PORTUP: 
     if (GTval) { SWfx=SW1_BIGFX_PORTUP; SWval=GTsldToSWsld(GTval,Dpitch,spdtbl); } break;
    case GT2_COMMAND_PORTDOWN: 
     if (GTval) { SWfx=SW1_BIGFX_PORTDOWN; SWval=GTsldToSWsld(GTval,Dpitch,spdtbl); } break;
    case GT2_COMMAND_TONEPORT: 
     if (GTval) { SWfx=SW1_BIGFX_TONEPORT; SWval=GTsldToSWsld(GTval,Dpitch,spdtbl); }
     else { SWfx=0; SWins=SW1_LEGATO; SWnote|=0x80; } //change fx-legato to instrument-column legato
     break;
    case GT2_COMMAND_VIBRATO: SWfx=SW1_BIGFX_VIBRATO; SWval=GTvibToSWvib(GTval,Dpitch,spdtbl); break;
    default: SWfx=SWval=0; break;
   }
   if (SWfx==PrevSWfx && SWval==PrevSWval) { SWfx=SWval=0; } //eliminate repetitive GT effects
   else { PrevSWfx=SWfx; PrevSWval=SWval; }

   switch (GTfx)
   {     //one-shot GT commands - table-jump commands should jump inside corresponding instrument-tables in SW
    case GT2_COMMAND_AD: SWfx=SW1_BIGFX_AD; SWval=GTval; break;
    case GT2_COMMAND_SR: SWfx=SW1_BIGFX_SR; SWval=GTval; break;
    case GT2_COMMAND_WAVEREG: SWfx=SW1_BIGFX_WAVEREG; SWval=GTval; break;
   
    case GT2_COMMAND_WAVETBL:
     SWfx=SW1_BIGFX_WAVETBL; //this FX was added in SW 1.4
     printf("\nInserting more into waveform-arp-table of SW instrument $%.2X:\n",PrevSWins);
     SWval = GTtblToSWtbl(&gt2_wavetable, GTval, &sw_instruments[PrevSWins], 0x80, debug); //TableType bit7 set means 'insert into the table' 
     break;
    case GT2_COMMAND_PULSETBL:
     SWfx=SW1_BIGFX_PULSETBL;
     printf("\nInserting more into pulse-table of SW instrument $%.2X:\n",PrevSWins);
     SWval = GTtblToSWtbl(&gt2_pulsetable, GTval, &sw_instruments[PrevSWins], 0x81, debug); //TableType bit7 set means 'insert into the table'
     break;
    case GT2_COMMAND_FILTERTBL:
     SWfx=SW1_BIGFX_FILTERTBL;
     printf("\nInserting more into filter-table of SW instrument $%.2X:\n",PrevSWins);
     SWval = GTtblToSWtbl(&gt2_filtertable, GTval, &sw_instruments[PrevSWins], 0x82, debug); //TableType bit7 set means 'insert into the table'
     break;
   
    case GT2_COMMAND_FILTERCTRL: 
     SWfx=SW1_BIGFX_FILT_CONTROL; //this FX was added in SW 1.4
     break;
    case GT2_COMMAND_FILTERCUT: 
     SWfx=SW1_BIGFX_FILT_CUTOFF; SWval=GTval; //this FX was added in SW 1.4
     break;
    case GT2_COMMAND_VOLUME: 
     SWfx=SW1_SMALLFX_MAINVOL|(GTval&0xF); 
     break;
    case GT2_COMMAND_FUNK: 
     SWfx=SW1_BIGFX_FUNKTEMPO; 
     SWval= (spdtbl->left[GTval-1]&0xF)*16 + (spdtbl->right[GTval-1]&0xF) ;
     break; 
    case GT2_COMMAND_TEMPO: 
     if (!(GTval&0x80)) {SWfx=SW1_BIGFX_TEMPO; SWval=GTval;} //tune-tempo setting
     else {SWfx=SW1_BIGFX_TRACKTEMPO; SWval=GTval&0x7F;}     //track-tempo setting
     break;
   };

   if (SWfx) { SWins|=0x80; SWnote|=0x80; }//sign FX column
  }
  
  if(debug) printf("%2X %2X %2X %2X , ",SWnote,SWins,SWfx,SWval);
  
  //push converted pattern-row to target
  sw_ptn->data.push_back(SWnote);SWindex++;
  if (SWnote&0x80) {sw_ptn->data.push_back(SWins);SWindex++;}
  if (SWins&0x80) {sw_ptn->data.push_back(SWfx);SWindex++;}
  if (0<SWfx && SWfx<SW1_SMALLFX_MIN) {sw_ptn->data.push_back(SWval);SWindex++;}
 }
 sw_ptn->data.push_back(SW1_PTNCOL1_END); //the closing $FF for SW pattern
 sw_ptn->length = gt_ptn->rows-1;
 sw_ptn->size=SWindex+1;
} 


/*******************************************************************************
// MAIN START
//
// Parameters:
// ["file.sng"] = GT2 input filename (REQUIRED!)
// ["file.swm"] / ["file.swm.prg"] = SWM output filename
// ["hr=0000"] = Hard-restart adsr value
// ["debug"] = Display debug/log of conversion

*******************************************************************************/
int main(int argc, char *argv[])
{
 printf ("\n===============================================\n"
           "Goattracker (sng) to SID-Wizard (swm) converter\n"
           "===============================================\n");

 if ( (argc==2 && !strcmp(argv[1],"-help")) || argc==1 )
 {
  printf ( "Converts GoatTracker .sng tune input-file to   \n"
           "to SID-Wizard .swm output-file. Usage syntax:  \n\n"

           "SWMconvert <inputfile> [outputfile] [options]  \n\n"

           "If output-file not given default is output.swm \n"
           "(Name will be lowercased and truncated if long)\n\n"

           "options: debug   - displays conversion logs    \n"
           "         hr=XXXX - set hard-restart  ADSR (hex)\n\n"

           "Authors: Conrad/Samar & Hermit in year 2013    \n"
           "===============================================\n");
  printf("\nPress any key...\n\n");
  getchar();
  exit(1);
 }

    int i,j;
    char * input_sng = 0;
    char * output_swm = 0;
    char sng_ext[] = ".sng";
    char swm_ext1[] = ".swm";
    char swm_ext2[] = ".swm.prg";
    char * input_ext = 0;
    char * output_ext = 0;
    char option_debug[] = "debug";
    char option_hr[] = "hr=";
    //bool fv = false, p0 = false, r0 = false; 
    unsigned short hr = 0x0f00;
    bool debug = false; //true;
    unsigned char Dpitch=(char)(SW1_NOTE_MAX/2); //default discrete pitch - init to the middle of the note-region
    unsigned char SWinsTableSize[SW1_INSTRUMENT_MAX+1];
    /*******************
    // Parse parameters:
    *******************/
    for (i = 1; i < argc; ++ i)
    {
        if (ParseFilename(argv[i], &input_sng, sng_ext,&input_ext)) continue;
        if (ParseFilename(argv[i], &output_swm, swm_ext1,&output_ext) || ParseFilename(argv[i], &output_swm, swm_ext2,&output_ext) ) continue;
//        if (!strncmp("fv", argv[i], sizeof("fv"))) { fv=true; continue; }
//        if (!strncmp("p0", argv[i], sizeof("p0"))) { p0=true; continue; }
//        if (!strncmp("r0", argv[i], sizeof("r0"))) { r0=true; continue; }
        if (!strncmp(option_debug, argv[i], strlen(option_debug))) { debug=true; continue; }
        if (!strncmp(option_hr, argv[i], strlen(option_hr)))
        {
           int hr_fromparam = 0;
           for (int ii = strlen(option_hr); argv[i][ii]; ++ ii)
           {
               hr_fromparam *= 16;
               switch (argv[i][ii])
               {
               case 'A':
               case 'B':
               case 'C':
               case 'D':
               case 'E':
               case 'F':
                    hr_fromparam += (CASTINT(argv[i][ii] - 'A') + 10);
                    break;
               default:
                    hr_fromparam = (argv[i][ii] >= '0' && argv[i][ii] <= '9') ? (hr_fromparam + CASTINT(argv[i][ii] & 0xf)) : -1;
                    break;
               };
               if (hr_fromparam < -1) break;
           };
           if (hr_fromparam >= 0) hr = static_cast<unsigned short>(hr_fromparam);
        }
    };


    if (output_swm)
    {
     char * onlyname=0; //filename without the folders
     for(i=strlen(output_swm);i>0;i--) if(output_swm[i]=='/' || output_swm[i]=='\\') {i++;break;} //skip file-path
     onlyname=output_swm+i;             //printf("%d,%s,%s\n",i,output_swm,onlyname);
     for(i=0; onlyname[i]; i++) onlyname[i]=tolower(onlyname[i]); //SW file-dialog doesn't like uppercase
     for (i=0; onlyname[i]!='.'; i++) ; //check length of base-filename by looking for 1st '.'   
     if(i>=12) { for(j=0;j<(int)strlen(output_ext);j++)onlyname[12+j]=output_ext[j]; onlyname[12+j]='\0'; } //avoid too long names, truncate them //expand with .swm and closing zero
    }
    else
    {
     output_swm = new char[sizeof(SWM_DEFAULT_FILENAME)];
     strncpy(output_swm, SWM_DEFAULT_FILENAME, sizeof(SWM_DEFAULT_FILENAME));
    }

    printf("Input file: \"%s\"\n", input_sng ? input_sng : "<no file>");
    printf("Output file: \"%s\"\n", output_swm);
    printf("HR value: $%.4X\n", hr);
//    printf("FV %sabled\n", fv ? "en" : "dis");
//    printf("P0 %sabled\n", p0 ? "en" : "dis");
//    printf("R0 %sabled\n\n", r0 ? "en" : "dis");


/*******************************************************************************
// FIRST, LOAD GT2 data into memory...
*******************************************************************************/
    printf("LOADING GT2 FILE\n-------------------\n");
    FILE * gt2file = fopen(/*input_sng*/input_sng, "rb");
    if (!gt2file)
    {
       // Failed to open file...
       printf("Error: GT2 file not found.\n");
    }
    else
    {
        // Declare Goat-Tracker 2 data pointers...
        sng_header * gt2_header;
        sng_orderlist * gt2_orderlist1;
        sng_orderlist * gt2_orderlist2;
        sng_orderlist * gt2_orderlist3;
        unsigned char gt2_instrumentcount;
        sng_instrument * gt2_instruments;
        sng_table gt2_speedtable;
        unsigned char gt2_patterncount;
        sng_pattern * gt2_patterns;

        // Begin reading GT2 file...
        fseek(gt2file, SEEK_SET, 0);
  
        // Read header data...
        gt2_header = new sng_header;
        fread(gt2_header, 1, sizeof(sng_header), gt2file);

        // Check tag...
        if (strncmp(GT2_TAG, gt2_header->tag, GT2_TAG_SIZE))
        {
           // GT2 tag is modified/corrupt...
           printf("Error: Invalid GT2 tag.\n");           
        }
        else
        {
           // OK...
           printf("Name: %s\n", gt2_header->name);
           printf("Author: %s\n", gt2_header->author);
           printf("Copyright: %s\n", gt2_header->copyright);

           // How many subtunes?
           if (!gt2_header->subtunes)
           {  
              // In 99.9% cases, this should never happen...
              printf("ERROR: No subtunes.\n");              
           }
           else
           {
               printf("Subtunes: %i\n", gt2_header->subtunes); 

               // Create orderlist pointers...
               gt2_orderlist1 = new sng_orderlist[gt2_header->subtunes];
               gt2_orderlist2 = new sng_orderlist[gt2_header->subtunes];
               gt2_orderlist3 = new sng_orderlist[gt2_header->subtunes];          

               // Read orderlist data per subtune...
               for (i = 0; i < CASTINT(gt2_header->subtunes); ++ i)
               {
                   // Voice 1, 2, 3...
                   ReadGT2Orderlist(&gt2_orderlist1[i], gt2file);
                   ReadGT2Orderlist(&gt2_orderlist2[i], gt2file);
                   ReadGT2Orderlist(&gt2_orderlist3[i], gt2file);
               };

               // Read instruments...
               fread(&gt2_instrumentcount, 1, 1, gt2file);
               if (gt2_instrumentcount)
               {
                   gt2_instruments = new sng_instrument[gt2_instrumentcount];
                   for (i = 0; i < CASTINT(gt2_instrumentcount); ++ i)
                   {
                       fread(&gt2_instruments[i], 1, sizeof(sng_instrument), gt2file);                                              
                   };
               }
               printf("Instruments: %i\n", gt2_instrumentcount);

               // Read tables...
               ReadGT2Table(&gt2_wavetable, gt2file, "Wave");
               ReadGT2Table(&gt2_pulsetable, gt2file, "Pulse"); 
               ReadGT2Table(&gt2_filtertable, gt2file, "Filter");
               ReadGT2Table(&gt2_speedtable, gt2file, "Speed");

               // Read patterns...
               fread(&gt2_patterncount, 1, 1, gt2file);
               if (gt2_patterncount)
               {
                   gt2_patterns = new sng_pattern[gt2_patterncount];
                   for (i = 0; i < CASTINT(gt2_patterncount); ++ i)
                   {
                       ReadGT2Pattern(&gt2_patterns[i], gt2file);
                   };
               }           
               printf("Patterns: %i\n\n", gt2_patterncount);
           }
        }
        fclose(gt2file);


/*******************************************************************************
// NOW CONVERT DATA TO SID-WIZARD V1 FORMAT...
********************************************************************************/

        printf("CONVERTING TO SWM\n-------------------\n");

// Convert Goattracker-header to SID-Wizard header ============================
        int j, titlepos; swm_header * sw_header = new swm_header;
        //init SWM header's part that doesn't have equivalent in GT
        for (i=0; i<SW1_TAG_SIZE; i++) sw_header->tag[i] = SW1_TAG[i];
        sw_header->framespd=1; sw_header->highlight=4; sw_header->auto_advance=1; 
        sw_header->conf_bits.binding=1; sw_header->conf_bits.hiderasters=0; sw_header->conf_bits.follow_play=0; sw_header->conf_bits.auto_instru=0;
        for (i=0; i<SID_CHNAMOUNT; i++) sw_header->mutesolo[i]=0xFF;
        sw_header->default_ptn=0x40; ; sw_header->chordtable_length=0; sw_header->tempotable_length=0;
        sw_header->colour_theme=0; sw_header->keyboard_type=0; sw_header->driver_type=0;
        //calculate SWM header's part that has Goattracker-source to be converted from
        sw_header->sequence_count = gt2_header->subtunes * 3; //check if SW is not overridden!
        sw_header->pattern_count = gt2_patterncount; //check if SW is not overridden!
        sw_header->instrument_count = gt2_instrumentcount; //check if SW is not overridden!
        for (i=0; i<SW1_AUTHORINFO_SIZE && i<GT2_AUTHOR_SIZE && gt2_header->author[i]!=0; i++) sw_header->authorinfo[i]=toupper(gt2_header->author[i]);
        if (i<SW1_AUTHORINFO_SIZE-1) {sw_header->authorinfo[i]=':'; i++;}
        titlepos=i;
        //if (titlepos<SW1_AUTHORINFO_SIZE)
        //{ //append title if fits
         for (i=titlepos;i<SW1_AUTHORINFO_SIZE;i++)
          { sw_header->authorinfo[i] = (i-titlepos<GT2_NAME_SIZE && gt2_header->name[i-titlepos]!=0) ? toupper(gt2_header->name[i-titlepos]) : 0x20; } //fill rest with spaces
        //}

// Convert instruments ... ====================================================
        if (gt2_instrumentcount)
        {
            // Create SID-WIZARD instrument objects:
            sw_instruments = new swm_instrument[gt2_instrumentcount];
            
            sng_instrument * gt_instr;
            swm_instrument * sw_instr;
            //std::map<unsigned char, unsigned char>::iterator it;

            // Convert them...
            for (i = 0; i < CASTINT(gt2_instrumentcount); i++)
            {
                gt_instr = &gt2_instruments[i];
                sw_instr = &sw_instruments[i];
                if (debug) printf("\nInstrument $%.2X - %s\n", i+1,gt_instr->name);

                if (debug) printf("Wave table  ->  (SW FORMAT)\n");
                GTtblToSWtbl(&gt2_wavetable,gt_instr->wv,sw_instr,0,debug); //0 means wavetable

                if (debug) printf("Pulse table -> (SW FORMAT)\n");
                sw_instr->parameter.pulsetb_index=SW1_INSTRUMENT_PARAMSIZE + sw_instr->tables.size();
                GTtblToSWtbl(&gt2_pulsetable,gt_instr->pl,sw_instr,1,debug); //1 means pulse-table

                if (debug) printf("Filter table -> (SW FORMAT)\n");
                sw_instr->parameter.filtertb_index=SW1_INSTRUMENT_PARAMSIZE +  sw_instr->tables.size();
                GTtblToSWtbl(&gt2_filtertable,gt_instr->fl,sw_instr,2,debug); //2 means filtertable

                SWinsTableSize[i]=sw_instr->tables.size(); if (debug) printf("Size of SW-tables together: $%.2X bytes\n",SWinsTableSize[i]);

                //convert main instrument-settings
                sw_instr->parameter.flag.hrtimer=(!(gt_instr->gtoff&0x80)) ? (gt_instr->gtoff>2)?2:gt_instr->gtoff : 0x00; //SW HR-timer max.2, if bit7 set in GT, no HR (timer=0)
                sw_instr->parameter.flag.staccato=0; //no equivalent in GT
                sw_instr->parameter.flag.wframe1=(gt_instr->hr) ? 1 : 0; //if 1st frame waveform is $00 in GT it leaves waveform untouched 
                sw_instr->parameter.flag.vibtype=1; //0b01; 
                sw_instr->parameter.flag.pwreset=0; sw_instr->parameter.flag.flreset=0; //no equivalents in GT
                sw_instr->parameter.hr_ad=(hr / 0x100); sw_instr->parameter.hr_sr=(hr % 0x100); //set by config file in GT can be directed to sng2swm through commandline arguments, default is $0F00
                sw_instr->parameter.ad=gt_instr->ad; sw_instr->parameter.sr=gt_instr->sr;
                sw_instr->parameter.vibrato=(gt_instr->vibspd) ? GTvibToSWvib(gt_instr->vibspd,Dpitch,&gt2_speedtable) : 0x00;
                sw_instr->parameter.vibrato_delay=gt_instr->vibtim;
                sw_instr->parameter.arpchord_speed=0x80; //pulsewidth and filtertable is always multispeed in GT
                sw_instr->parameter.default_chord=1; //no chord in GT btw. (only possible with WF-table pointer-setting FX)
                sw_instr->parameter.octave_shift=0; //no octave-shift in GT
                //pulse-table pointer was calculated earlier
                //filter-table pointer was calculated earlier
                sw_instr->parameter.wfgoff=0; //no gateoff-pointer in GT
                sw_instr->parameter.pwgoff=0; //no gateoff-pointer in GT
                sw_instr->parameter.fltgoff=0; //no gateoff-pointer in GT
                sw_instr->parameter.frame1_waveform=gt_instr->hr; //1st frame waveform is the same in both systems (00 treated differently in SW, converted to $09 for SWM1 to keep compatibility)
                
                for(j=0;j<SW1_INSTRUMENT_NAMESIZE;j++) sw_instr->name[j]=(gt_instr->name[j]) ? toupper(gt_instr->name[j]) : 0x20; //substitute zero (string-end) with spaces
                
            };
        }

// Convert Goattracker-orderlists to SID-Wizard sequences...===================
        swm_sequence * sw_sequence = new swm_sequence[sw_header->sequence_count];
        for (i=0; i<gt2_header->subtunes; i++)
        { //'i' counts subtunes, j is data-index
         if(debug) printf("\nSubtune %2X:",i);
         GTseqToSWseq(&gt2_orderlist1[i],&sw_sequence[i*3+0],debug);
         GTseqToSWseq(&gt2_orderlist2[i],&sw_sequence[i*3+1],debug);
         GTseqToSWseq(&gt2_orderlist3[i],&sw_sequence[i*3+2],debug);
        }

// Convert Goattracker-patterns to SID-Wizard patterns ========================
        swm_pattern * sw_pattern = new swm_pattern[sw_header->pattern_count+1];
        for (i=0; i<sw_header->pattern_count; i++)
        {
         if(debug) printf("\nPattern %2X: ",i);
         GTptnToSWptn(&gt2_patterns[i],&sw_pattern[i+1],&gt2_speedtable,debug); //swm-pattern numbering starts from 1
        }

//define subtune-tempo (default is 6 in GT?) ==================================
        swm_funktempo * sw_tempo = new swm_funktempo[gt2_header->subtunes];
        for(i=0;i<gt2_header->subtunes;i++) {sw_tempo[i].tempo1=0x86; sw_tempo[i].tempo2=0x84;}



//********************** generate SWM file *************************************
        FILE * sw1file = fopen(/*output_swm*/output_swm, "wb");
        unsigned char SWM_LoadAddress[2]={0x00,0x20};
        fwrite(SWM_LoadAddress,2,1,sw1file); //write .prg load-address
        fwrite(sw_header, sizeof(swm_header), 1, sw1file);
        unsigned char savebuffer[256]; //needed because it seems 'fwrite' doesn't handle structures&vectors well
        
        for (i=0; i<sw_header->sequence_count; i++)
        { 
         for (j=0;j<sw_sequence[i].size;j++) savebuffer[j]=sw_sequence[i].data[j]; 
         savebuffer[j]=sw_sequence[i].size;
         fwrite(savebuffer, sw_sequence[i].size+1, 1, sw1file); 
        }
        
        unsigned char PrevData,PattData,DataIndex,NOPcount; //variables used for NOP packing
        for (i=1; i<sw_header->pattern_count+1; i++)
        {
         //for (j=DataIndex=PrevData=0;j<0xFF;j++) savebuffer[j]=0; //clear buffer?
         for (j=DataIndex=PrevData=0;j<sw_pattern[i].size;j++) 
         { //Pack empty pattern-rows
          PattData=sw_pattern[i].data[j];
          //pack NOPs if enough found--------(mimic the packdepack.inc assembly pattern-packer algorithm) 
          if ( (PattData!=0) || (j==0) ) goto wrcmptr;
          else if (sw_pattern[i].data[j-1]) goto wrcmptr;
          NOPcount=SW1_PTNCOL1_PACKMIN-1; //init
packloop: if (sw_pattern[i].data[j+1]!=0) goto putprst;
          NOPcount++; j++; 
          if ( NOPcount < SW1_PTNCOL1_PACKMAX ) goto packloop; 
putprst:  if ((PattData=NOPcount)==(SW1_PTNCOL1_PACKMIN-1)) PattData=0;  //simple NOP in case of only 2 consequent zeroes
          //write Patterndata to target-----------------------
wrcmptr:  savebuffer[DataIndex]=PrevData=PattData; DataIndex++;  
         }
         savebuffer[DataIndex-1]=sw_pattern[i].size; //write out size of pattern to TUNEDATA - in place of 0xFF delimiter
         savebuffer[DataIndex]=sw_pattern[i].length; //write length of pattern to TUNEDATA after size
         fwrite( savebuffer, DataIndex+1, 1, sw1file);
        }
        
        for (i=0; i<sw_header->instrument_count; i++)
        {
         fwrite( &sw_instruments[i].parameter, SW1_INSTRUMENT_PARAMSIZE, 1, sw1file); 
         //fputc(0x21,sw1file);fputc(0x00,sw1file);fputc(0x00,sw1file);fputc(0xff,sw1file);
         //fputc(0xff,sw1file);fputc(0x15,sw1file); 
         for (j=0;j<SWinsTableSize[i];j++) savebuffer[j]=sw_instruments[i].tables[j];
         savebuffer[j-1]=SW1_INSTRUMENT_PARAMSIZE+SWinsTableSize[i]-1; //register size of instrument for depacker in last $FF
         fwrite( savebuffer, SWinsTableSize[i], 1, sw1file);
         fwrite( sw_instruments[i].name, SW1_INSTRUMENT_NAMESIZE, 1, sw1file);
        }
        
        for(i=0;i<gt2_header->subtunes;i++) 
        {
         fputc(sw_tempo[i].tempo1, sw1file);
         fputc(sw_tempo[i].tempo2, sw1file);
        }

        fclose(sw1file);
        printf("\nConversion Finished. Output SWM File '%s' generated.\n",output_swm);

        if (sw_instruments) delete [] sw_instruments;

        delete gt2_header;
    }


//************************** end of conversion *********************************
    if(debug) { printf("Press any key...\n\n"); getchar(); } else printf("\n");
    return 0;
}

//=====================================================================================
