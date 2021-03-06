# $Id: Makefile 382 2014-06-23 17:53:01Z hermitsoft $
AS=64tass
AFLAGS=-C -a
PACKER=exomizer
CBMDISK=c1541
X64=x64
XFLAGS= -autostartprgmode 1 +cart
 #-autostartprgmode 1 -autostart-warp
SOURCEDIR=.
BINARYDIR=../application
EXASWMDIR=../examples
EXASWIDIR=$(EXASWMDIR)/instruments
DOCDIR=../manuals
APPNAME1=SID-Wizard
APPVERSION=1.8
APPNAME2=SID-Maker
APPNAME3=SW-$(APPVERSION)-UserManual
DOCNAME1=$(APPNAME1)-$(APPVERSION)-UserManual.txt
KUSS= > /dev/null

SW-SOURCES=settings.cfg SWM-spec.src include/irq.inc include/startupmenu.inc include/altplayers.inc
SW-SOURCES+=include/displayer1.inc include/player.asm include/playadapter.inc
SW-SOURCES+=include/menu.inc include/packdepack.inc include/commonsubs.inc
SW-SOURCES+=graphics/graphics.inc include/displayer2.inc
SW-SOURCES+=include/keyhandler.inc include/datawriter.inc include/insdelcopy.inc
SW-SOURCES+=graphics/sw-charset.prg graphics/sw-header.prg
SW-SOURCES+=include/midisubs.inc MIDI-C64.asm HerMIDI/HerMIDI-C64.asm
SM-SOURCES=settings.cfg SWM-spec.src include/player.asm include/packdepack.inc
SM-SOURCES+=include/keyhandler.inc include/menu.inc include/altplayers.inc
UM-SOURCES=../manuals/SID-Wizard-$(APPVERSION)-UserManual.txt $(EXASWMDIR)/sid-exports/flashitback.sid
SWS-SOURCES=graphics/sw-splash-hires.prg
SNG-SOURCES=sng2swm/sng2swm.cpp sng2swm/sng.h sng2swm/swm.h
SW-SOURCE=editor.asm
SM-SOURCE=exporter.asm
UM-SOURCE=usermanual.asm
SWS-SOURCE=$(APPNAME1)-splashed.asm

SW-SPLASH=$(APPNAME1)-$(APPVERSION).prg
SW-SPLASH-KERNAL=$(APPNAME1)-$(APPVERSION)-KERNALload.prg
SW-EXEC=$(APPNAME1)-nopack.prg
SW-EXEC-KERNAL=$(APPNAME1)-nopack-KERNAL.prg
SM-EXEC=$(APPNAME2)-nopack.prg
SM-EXEC-KERNAL=$(APPNAME2)-nopack-KERNAL.prg
UM-EXEC=$(APPNAME3)-nopack.prg
SW-PACK=$(APPNAME1)-packed.prg
SW-PACK-KERNAL=$(APPNAME1)-packed-KERNAL.prg
SM-PACK=$(APPNAME2)-$(APPVERSION).prg
SM-PACK-KERNAL=$(APPNAME2)-$(APPVERSION)-KERNALload.prg
UM-PACK=$(APPNAME3).prg
SW-2SID-SPLASH=$(APPNAME1)-2SID.prg
SW-2SID-EXEC=$(APPNAME1)-2SID-nopack.prg
SM-2SID-EXEC=$(APPNAME2)-2SID-nopack.prg
SW-2SID-PACK=$(APPNAME1)-2SID-packed.prg
SM-2SID-PACK=$(APPNAME2)-2SID.prg
SW-3SID-SPLASH=$(APPNAME1)-3SID.prg
SW-3SID-EXEC=$(APPNAME1)-3SID-nopack.prg
SM-3SID-EXEC=$(APPNAME2)-3SID-nopack.prg
SW-3SID-PACK=$(APPNAME1)-3SID-packed.prg
SM-3SID-PACK=$(APPNAME2)-3SID.prg
SM-SFX-EXEC=$(APPNAME2)-SFX-nopack.prg
SM-SFX-PACK=$(APPNAME2)-SFX.prg
SM-SWP-EXEC=$(APPNAME2)-SWP-nopack.prg
SM-SWP-PACK=$(APPNAME2)-SWP.prg
SM-SWP-FX-EXEC=$(APPNAME2)-SWP-FX-nopack.prg
SM-SWP-FX-PACK=$(APPNAME2)-SWP-FX.prg
SWM-CONVERTER=SWMconvert
SNG-CONVERTER=sng2swm

CP?=cp -f
MD=mkdir
ICON=SID-Wizard.png
MENUENTRY=SID-Wizard.desktop
CC=gcc
CPPC=g++
APPDIR=/usr/local/bin
#APPDIR2=/usr/local/apps/$(APPNAME1)
APPDIR3=/opt/$(APPNAME1)
ICONDIR=/usr/share/icons
DESKDIR=/usr/share/applications
SW-ICON=SW-icon-32x32.png

all: $(BINARYDIR)/$(SW-SPLASH) $(BINARYDIR)/$(SW-SPLASH-KERNAL) $(BINARYDIR)/$(SM-PACK) $(BINARYDIR)/$(SM-PACK-KERNAL) $(BINARYDIR)/$(SM-SFX-PACK) $(BINARYDIR)/$(SM-SWP-PACK) $(BINARYDIR)/$(SM-SWP-FX-PACK) \
      $(BINARYDIR)/$(UM-PACK) 2SID 3SID $(BINARYDIR)/$(SWM-CONVERTER) $(BINARYDIR)/$(SNG-CONVERTER) \
      Conversion-Examples $(BINARYDIR)/$(APPNAME1)-$(APPVERSION)-disk1.d64 $(BINARYDIR)/$(APPNAME1)-$(APPVERSION)-disk2.d64 $(BINARYDIR)/$(APPNAME1)-instruments.d64
.PHONY: all clean install editor exporter 2SID 3SID
.INTERMEDIATE: $(BINARYDIR)/$(SW-PACK) $(BINARYDIR)/$(SW-EXEC) $(BINARYDIR)/$(SW-PACK-KERNAL) $(BINARYDIR)/$(SW-EXEC-KERNAL) \
                $(BINARYDIR)/$(SM-EXEC) $(BINARYDIR)/$(SM-EXEC-KERNAL) $(BINARYDIR)/$(SM-SFX-EXEC) $(BINARYDIR)/$(SM-SWP-EXEC) $(BINARYDIR)/$(SM-SWP-FX-EXEC) $(BINARYDIR)/$(UM-EXEC) \
                $(BINARYDIR)/$(SW-2SID-PACK) $(BINARYDIR)/$(SW-2SID-EXEC) $(BINARYDIR)/$(SM-2SID-EXEC) \
                $(BINARYDIR)/$(SW-3SID-PACK) $(BINARYDIR)/$(SW-3SID-EXEC) $(BINARYDIR)/$(SM-3SID-EXEC)
.INTERMEDIATE: editor.prg exporter.prg

#C64 executables
$(BINARYDIR)/$(SW-SPLASH): $(SWS-SOURCE) $(BINARYDIR)/$(SW-PACK) $(SWS-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -o $@ $<
$(BINARYDIR)/$(SW-PACK): $(BINARYDIR)/$(SW-EXEC)
	$(PACKER) sfx sys -q -o $@ $<
$(BINARYDIR)/$(SW-EXEC): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SW-SPLASH-KERNAL): $(SWS-SOURCE) $(BINARYDIR)/$(SW-PACK-KERNAL) $(SWS-SOURCES)
	$(AS) $(AFLAGS) -D KERNAL_LOADSAVE=1 -D SID_AMOUNT=1 -o $@ $<
$(BINARYDIR)/$(SW-PACK-KERNAL): $(BINARYDIR)/$(SW-EXEC-KERNAL)
	$(PACKER) sfx sys -q -o $@ $<
$(BINARYDIR)/$(SW-EXEC-KERNAL): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D KERNAL_LOADSAVE=1 -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SM-PACK): $(BINARYDIR)/$(SM-EXEC)
	$(PACKER) sfx sys -q -o $@ $<
$(BINARYDIR)/$(SM-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SM-PACK-KERNAL): $(BINARYDIR)/$(SM-EXEC-KERNAL)
	$(PACKER) sfx sys -q -o $@ $<
$(BINARYDIR)/$(SM-EXEC-KERNAL): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D KERNAL_LOADSAVE=1 -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SM-SFX-PACK): $(BINARYDIR)/$(SM-SFX-EXEC)
	$(PACKER) sfx sys -q -o $@ $<
$(BINARYDIR)/$(SM-SFX-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=1 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SM-SWP-PACK): $(BINARYDIR)/$(SM-SWP-EXEC)
	$(PACKER) sfx sys -q -o $@ $<
$(BINARYDIR)/$(SM-SWP-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SM-SWP-FX-PACK): $(BINARYDIR)/$(SM-SWP-FX-EXEC)
	$(PACKER) sfx sys -q -o $@ $<
$(BINARYDIR)/$(SM-SWP-FX-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=1 -D SLOWDOWN_SUPP=1 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(UM-PACK): $(BINARYDIR)/$(UM-EXEC)
	$(PACKER) sfx sys -q -o $@ $<
$(BINARYDIR)/$(UM-EXEC): $(UM-SOURCE) $(UM-SOURCES) $
	$(AS) $(AFLAGS) -o $@ $<

#C64 2SID-variant executables
$(BINARYDIR)/$(SW-2SID-SPLASH): $(SWS-SOURCE) $(BINARYDIR)/$(SW-2SID-PACK) $(SWS-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=2 -o $@ $<

$(BINARYDIR)/$(SW-2SID-PACK): $(BINARYDIR)/$(SW-2SID-EXEC)
	$(PACKER) sfx sys -q -o $@ $<

$(BINARYDIR)/$(SW-2SID-EXEC): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=2 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SM-2SID-PACK): $(BINARYDIR)/$(SM-2SID-EXEC)
	$(PACKER) sfx sys -q -o $@ $<

$(BINARYDIR)/$(SM-2SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=2 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<

2SID: $(BINARYDIR)/$(SW-2SID-SPLASH) $(BINARYDIR)/$(SM-2SID-PACK)

#C64 3SID-variant executables
$(BINARYDIR)/$(SW-3SID-SPLASH): $(SWS-SOURCE) $(BINARYDIR)/$(SW-3SID-PACK) $(SWS-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=3 -o $@ $<

$(BINARYDIR)/$(SW-3SID-PACK): $(BINARYDIR)/$(SW-3SID-EXEC)
	$(PACKER) sfx sys -q -o $@ $<

$(BINARYDIR)/$(SW-3SID-EXEC): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=3 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SM-3SID-PACK): $(BINARYDIR)/$(SM-3SID-EXEC)
	$(PACKER) sfx sys -q -o $@ $<

$(BINARYDIR)/$(SM-3SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=3 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<

3SID: $(BINARYDIR)/$(SW-3SID-SPLASH) $(BINARYDIR)/$(SM-3SID-PACK)

#SWMconvert
.IGNORE:
$(BINARYDIR)/$(SWM-CONVERTER): $(SWM-CONVERTER).c SWM-spec.src
	$(CC) -m32 -w $< -o $@ -lm -s
	wine gcc.exe -m32 -w $< -o "$@.exe" -lm -s
.IGNORE = $(NULL)

#sng2swm
.IGNORE:
$(BINARYDIR)/$(SNG-CONVERTER): $(SNG-CONVERTER)/$(SNG-CONVERTER).cpp $(SNG-SOURCES) 
	$(CPPC) $< -m32 -o $@ -lm -s
	wine g++.exe $< -m32 -s -o "$@.exe" -lm -s
.IGNORE = $(NULL)

#generate XM/MID exports and import example XM/MID/SNG files
.IGNORE:
Conversion-Examples: $(BINARYDIR)/$(SWM-CONVERTER) $(BINARYDIR)/$(SNG-CONVERTER) $(EXASWMDIR)/
	for inputfile in $(EXASWMDIR)/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(BINARYDIR)/$(SWM-CONVERTER) $$inputfile $(EXASWMDIR)/xm-exports/$${outputfile%.*}.xm $(KUSS) ; done ;
	for inputfile in $(EXASWMDIR)/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(BINARYDIR)/$(SWM-CONVERTER) $$inputfile $(EXASWMDIR)/mid-exports/$${outputfile%.*}.mid $(KUSS) ; done ;
	for inputfile in $(EXASWMDIR)/xm-imports/*.xm ; do $(BINARYDIR)/$(SWM-CONVERTER) $$inputfile $${inputfile%.*}.swm ; done ;
	for inputfile in $(EXASWMDIR)/mid-imports/*.mid ; do $(BINARYDIR)/$(SWM-CONVERTER) $$inputfile $${inputfile%.*}.swm $(KUSS) ; done ;
	for inputfile in $(EXASWMDIR)/sng-imports/*.sng ; do $(BINARYDIR)/$(SNG-CONVERTER) $$inputfile $${inputfile%.*}.swm $(KUSS) ; done ;
	for inputfile in $(EXASWMDIR)/sng-imports/todisk/*.sng ; do $(BINARYDIR)/$(SNG-CONVERTER) $$inputfile $${inputfile%.*}.swm $(KUSS) ; done ;

.IGNORE = $(NULL)

#SID-Wizard disk
$(BINARYDIR)/$(APPNAME1)-$(APPVERSION)-disk1.d64: $(BINARYDIR)/$(SW-SPLASH) $(BINARYDIR)/$(SM-PACK) $(BINARYDIR)/$(SW-2SID-SPLASH) $(BINARYDIR)/$(SM-2SID-PACK) $(BINARYDIR)/$(UM-PACK) $(EXASWMDIR)/ $(EXASWMDIR)/sng-imports/todisk
	$(CBMDISK) -format "sid-wizard $(APPVERSION),s1" d64 $@
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SW-SPLASH) sid-wizard-$(APPVERSION)
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SM-PACK) sid-maker-$(APPVERSION)
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SW-2SID-SPLASH) sid-wizard-2sid
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SM-2SID-PACK) sid-maker-2sid
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(UM-PACK) sw-$(APPVERSION)-manual
	$(CBMDISK) -attach $@ -write ./dummyfile CCCCCCCCCCCCCCCC
	for inputfile in $(EXASWMDIR)/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCsng-importCCC"
	for inputfile in $(EXASWMDIR)/sng-imports/todisk/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCCC 2sid CCCCC"
	for inputfile in $(EXASWMDIR)/*.sws ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;

$(BINARYDIR)/$(APPNAME1)-$(APPVERSION)-disk2.d64: $(BINARYDIR)/$(SW-3SID-SPLASH) $(BINARYDIR)/$(SM-3SID-PACK) $(EXASWMDIR)/ $(EXASWMDIR)/swm-more $(EXASWMDIR)/swm-more/light  $(EXASWMDIR)/swm-more/extra $(EXASWMDIR)/swm-more/bare $(EXASWMDIR)/swm-more/demo
	$(CBMDISK) -format "sid-wizard $(APPVERSION),s2" d64 $@
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SW-3SID-SPLASH) sid-wizard-3sid
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SM-3SID-PACK) sid-maker-3sid
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCCCCCCCCCCCCCC"
	for inputfile in $(EXASWMDIR)/swm-more/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCCCextraCCCCCC"
	for inputfile in $(EXASWMDIR)/swm-more/extra/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCCClightCCCCCC"
	for inputfile in $(EXASWMDIR)/swm-more/light/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCCCCbareCCCCCC"
	for inputfile in $(EXASWMDIR)/swm-more/bare/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCCCCdemoCCCCCC"
	for inputfile in $(EXASWMDIR)/swm-more/demo/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCCC 3sid CCCCC"
	for inputfile in $(EXASWMDIR)/*.swt ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;

#SID-Wizard instrument-disk
$(BINARYDIR)/$(APPNAME1)-instruments.d64: $(EXASWIDIR)/
	$(CBMDISK) -format "sw$(APPVERSION)-instrument,s3" d64 $@
	for inputfile in $(EXASWIDIR)/*.swi ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $(BINARYDIR)/$(APPNAME1)-instruments.d64 -write $$inputfile $${outputfile} $(KUSS) ; done;

#for test builds
editor: editor.prg
	$(X64) $(XFLAGS) $<

editor.prg: $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o editor-nopack.prg $<
	$(PACKER) sfx sys -q -o editor.prg editor-nopack.prg

exporter: exporter.prg
	$(X64) $(XFLAGS) $<

exporter.prg: $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o exporter-nopack.prg $<
	$(PACKER) sfx sys -q -o exporter.prg exporter-nopack.prg

SID-Wizard-splashed: SID-Wizard-splashed.prg
	$(X64) $(XFLAGS) $<

SID-Wizard-splashed.prg: $(SWS-SOURCE) $(SWS-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -o $@ $<


clean:
	$(RM) $(BINARYDIR)/$(SW-SPLASH) $(BINARYDIR)/$(SW-SPLASH-KERNAL) $(BINARYDIR)/$(SW-2SID-SPLASH) $(BINARYDIR)/$(SW-3SID-SPLASH)
	$(RM) $(BINARYDIR)/$(SW-PACK) $(BINARYDIR)/$(SW-EXEC) $(BINARYDIR)/$(SM-PACK) $(BINARYDIR)/$(SM-PACK-KERNAL) $(BINARYDIR)/$(UM-PACK) $(BINARYDIR)/$(SM-EXEC) $(BINARYDIR)/$(UM-EXEC)
	$(RM) $(BINARYDIR)/$(SW-2SID-EXEC) $(BINARYDIR)/$(SW-2SID-PACK) $(BINARYDIR)/$(SM-2SID-EXEC) $(BINARYDIR)/$(SM-2SID-PACK)
	$(RM) $(BINARYDIR)/$(SW-3SID-EXEC) $(BINARYDIR)/$(SW-3SID-PACK) $(BINARYDIR)/$(SM-3SID-EXEC) $(BINARYDIR)/$(SM-3SID-PACK)
	$(RM) $(BINARYDIR)/$(SM-SFX-PACK) $(BINARYDIR)/$(SM-SWP-PACK) $(BINARYDIR)/$(SM-SWP-FX-PACK)
	$(RM) $(BINARYDIR)/$(APPNAME1)-$(APPVERSION)-disk1.d64 $(BINARYDIR)/$(APPNAME1)-$(APPVERSION)-disk2.d64 $(BINARYDIR)/$(APPNAME1)-instruments.d64
	$(RM) $(BINARYDIR)/$(SWM-CONVERTER) $(BINARYDIR)/$(SNG-CONVERTER)
	$(RM) $(BINARYDIR)/$(SWM-CONVERTER).exe $(BINARYDIR)/$(SNG-CONVERTER).exe
	$(RM) $(EXASWMDIR)/xm-exports/*.xm $(EXASWMDIR)/mid-exports/*.mid
	$(RM) $(EXASWMDIR)/xm-imports/*.swm.prg $(EXASWMDIR)/xm-imports/*.swm.P00 $(EXASWMDIR)/xm-imports/*.swm 
	$(RM) $(EXASWMDIR)/mid-imports/*.swm.prg $(EXASWMDIR)/mid-imports/*.swm.P00 $(EXASWMDIR)/mid-imports/*.swm
	$(RM) $(EXASWMDIR)/sng-imports/*.swm $(EXASWMDIR)/sng-imports/todisk/*.swm
	$(RM) editor.prg exporter.prg SID-Wizard-splashed.prg


install: 
	$(CP) $(BINARYDIR)/$(SWM-CONVERTER) $(APPDIR)/
	$(CP) $(BINARYDIR)/$(SNG-CONVERTER) $(APPDIR)/
#Create application-entry and folder for SID-Wizard for easy access on Linux
	$(CP) $(APPNAME1).desktop $(DESKDIR)/
	$(CP) ./graphics/$(SW-ICON) $(ICONDIR)/
	$(MD) -p $(APPDIR3)
	$(CP) $(BINARYDIR)/$(SW-SPLASH) $(APPDIR3)/
	$(CP) $(BINARYDIR)/$(SM-PACK) $(APPDIR3)/
	$(CP) $(BINARYDIR)/$(SW-2SID-SPLASH) $(APPDIR3)/
	$(CP) $(BINARYDIR)/$(SM-2SID-PACK) $(APPDIR3)/
	$(CP) $(BINARYDIR)/$(SW-3SID-SPLASH) $(APPDIR3)/
	$(CP) $(BINARYDIR)/$(SM-3SID-PACK) $(APPDIR3)/
	$(CP) $(DOCDIR)/$(DOCNAME1) $(APPDIR3)/
	for filename in $(EXASWIDIR)/*.swi ; do $(CP) $$filename $(APPDIR3)/ ; done;
	for filename in $(EXASWMDIR)/*.swm; do onlyname=$${filename##*/} ; $(CP) $$filename $(APPDIR3)/$${onlyname%.*} ; done;
	for filename in $(EXASWMDIR)/*.sws ; do $(CP) $$filename $(APPDIR3)/ ; done;

uninstall:
	$(RM) $(DESKDIR)/$(APPNAME1).desktop $(ICONDIR)/$(SW-ICON)
	$(RM) $(APPDIR)/$(SWM-CONVERTER) $(APPDIR)/$(SNG-CONVERTER) 
	$(RM) $(APPDIR3)/$(SW-SPLASH) $(APPDIR3)/$(SM-PACK)
	$(RM) $(APPDIR3)/$(SW-2SID-SPLASH) $(APPDIR3)/$(SM-2SID-PACK) 
	$(RM) $(APPDIR3)/$(SW-3SID-SPLASH) $(APPDIR3)/$(SM-3SID-PACK) 
	$(RM) $(APPDIR3)/$(DOCNAME1)
#	$(RM) -rf $(APPDIR3)/  #not recommended to delete the whole folder as user can have workfiles there


#========================================================================================================
