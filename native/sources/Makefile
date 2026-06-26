#these basic settings can be overridden from outside by emulator-code:
EMULATOR=0
TUI_ONLY=0
UNCOMPRESSED_NATIVES=0
OSCILLOSCOPE_SHOWS_ALL_CHANNELS=0

AS=64tass
AFLAGS=-C -a -Wno-deprecated -DEMULATOR=$(EMULATOR)  -DTUI_ONLY=$(TUI_ONLY)  -DOSCILLOSCOPE_SHOWS_ALL_CHANNELS=$(OSCILLOSCOPE_SHOWS_ALL_CHANNELS)
ifeq ($(EMULATOR),1)
 ifeq ($(TUI_ONLY),1)      # this only applies to TUI (eventually used in standalone TUI app and SDL+TUI app's TUI-mode as well )
 AFLAGS+=-Drepspd1=6 #5     # should be greater than or equal to TUI_KEYHOLD_TIME/FRAMETIME (60ms = 3 frames) in SIDwizard.c
# AFLAGS+=-Drepspd2=2 #3    # should be less than TUI_KEYHOLD_TIME/FRAMETIME (60ms = 3 frames) in SIDwizard.c
# AFLAGS+=-DrelocRepSpd2=2  # in SID-Maker relocation-address setting repeat would be a bit instable without this override
 endif
endif
PACKER=tools/compress64kSFX
RAWPACKER=tools/compress64k
CBMDISK=c1541
X64=x64
XFLAGS= -autostartprgmode 1 +cart
 #-autostartprgmode 1 -autostart-warp
SOURCEDIR=.
BINARYDIR=../application
EXASWMDIR=../examples
EXASWIDIR=$(EXASWMDIR)/instruments
DOCDIR=../manuals
EMUDIR=../..
EMURESDIR=../../resources
APPNAME1=SID-Wizard
APPVERSION=1.9
SUBVERSION=7
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
SW-4SID-SPLASH=$(APPNAME1)-4SID.prg
SW-4SID-EXEC=$(APPNAME1)-4SID-nopack.prg
SM-4SID-EXEC=$(APPNAME2)-4SID-nopack.prg
SW-4SID-PACK=$(APPNAME1)-4SID-packed.prg
SM-4SID-PACK=$(APPNAME2)-4SID.prg
SM-SFX-EXEC=$(APPNAME2)-SFX-nopack.prg
SM-SFX-PACK=$(APPNAME2)-SFX.prg
SM-SWP-EXEC=$(APPNAME2)-SWP-nopack.prg
SM-SWP-PACK=$(APPNAME2)-SWP.prg
SM-echo-EXEC=$(APPNAME2)-echo-nopack.prg
SM-echo-PACK=$(APPNAME2)-echo.prg
SM-SWP-2SID-EXEC=$(APPNAME2)-SWP-2SID-nopack.prg
SM-SWP-2SID-PACK=$(APPNAME2)-SWP-2SID.prg
SM-SWP-3SID-EXEC=$(APPNAME2)-SWP-3SID-nopack.prg
SM-SWP-3SID-PACK=$(APPNAME2)-SWP-3SID.prg
SM-SWP-4SID-EXEC=$(APPNAME2)-SWP-4SID-nopack.prg
SM-SWP-4SID-PACK=$(APPNAME2)-SWP-4SID.prg
SM-SWP-FX-EXEC=$(APPNAME2)-SWP-FX-nopack.prg
SM-SWP-FX-PACK=$(APPNAME2)-SWP-FX.prg
SM-ROM-EXEC=$(APPNAME2)-ROM-nopack.prg
SM-ROM-PACK=$(APPNAME2)-ROM.prg
SM-ROM-SFX-EXEC=$(APPNAME2)-ROM-SFX-nopack.prg
SM-ROM-SFX-PACK=$(APPNAME2)-ROM-SFX.prg
SM-ROM-SWP-EXEC=$(APPNAME2)-ROM-SWP-nopack.prg
SM-ROM-SWP-PACK=$(APPNAME2)-ROM-SWP.prg
SM-ROM-SWP-FX-EXEC=$(APPNAME2)-ROM-SWP-FX-nopack.prg
SM-ROM-SWP-FX-PACK=$(APPNAME2)-ROM-SWP-FX.prg
SM-ROM-2SID-EXEC=$(APPNAME2)-ROM-2SID-nopack.prg
SM-ROM-2SID-PACK=$(APPNAME2)-ROM-2SID.prg
SM-ROM-3SID-EXEC=$(APPNAME2)-ROM-3SID-nopack.prg
SM-ROM-3SID-PACK=$(APPNAME2)-ROM-3SID.prg
SM-ROM-4SID-EXEC=$(APPNAME2)-ROM-4SID-nopack.prg
SM-ROM-4SID-PACK=$(APPNAME2)-ROM-4SID.prg
SM-ROM-SWP-2SID-EXEC=$(APPNAME2)-ROM-SWP-2SID-nopack.prg
SM-ROM-SWP-2SID-PACK=$(APPNAME2)-ROM-SWP-2SID.prg
SM-ROM-SWP-3SID-EXEC=$(APPNAME2)-ROM-SWP-3SID-nopack.prg
SM-ROM-SWP-3SID-PACK=$(APPNAME2)-ROM-SWP-3SID.prg
SM-ROM-SWP-4SID-EXEC=$(APPNAME2)-ROM-SWP-4SID-nopack.prg
SM-ROM-SWP-4SID-PACK=$(APPNAME2)-ROM-SWP-4SID.prg
SWM-CONVERTER=SWMconvert
SNG-CONVERTER=sng2swm

CP?=cp -f
MV=mv
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

all: compressor
all: $(BINARYDIR)/$(SW-SPLASH) $(BINARYDIR)/$(SW-SPLASH-KERNAL) $(BINARYDIR)/$(SM-PACK) $(BINARYDIR)/$(SM-PACK-KERNAL) \
     2SID 3SID 4SID \
      $(BINARYDIR)/$(SM-SFX-PACK) $(BINARYDIR)/$(SM-SWP-PACK) $(BINARYDIR)/$(SM-SWP-FX-PACK) \
       $(BINARYDIR)/$(SM-echo-PACK) $(BINARYDIR)/$(SM-ROM-PACK) \
       $(BINARYDIR)/$(SM-ROM-SFX-PACK) $(BINARYDIR)/$(SM-ROM-SWP-PACK) $(BINARYDIR)/$(SM-ROM-SWP-FX-PACK) \
       $(BINARYDIR)/$(SM-SWP-2SID-PACK) $(BINARYDIR)/$(SM-SWP-3SID-PACK) $(BINARYDIR)/$(SM-SWP-4SID-PACK) \
       $(BINARYDIR)/$(SM-ROM-2SID-PACK) $(BINARYDIR)/$(SM-ROM-3SID-PACK) $(BINARYDIR)/$(SM-ROM-4SID-PACK) \
       $(BINARYDIR)/$(SM-ROM-SWP-2SID-PACK) $(BINARYDIR)/$(SM-ROM-SWP-3SID-PACK) $(BINARYDIR)/$(SM-ROM-SWP-4SID-PACK) \
       $(BINARYDIR)/$(UM-PACK) $(BINARYDIR)/$(SWM-CONVERTER) $(BINARYDIR)/$(SNG-CONVERTER) Conversion-Examples \
      $(BINARYDIR)/$(APPNAME1)-$(APPVERSION)$(SUBVERSION)-disk1.d64 $(BINARYDIR)/$(APPNAME1)-$(APPVERSION)$(SUBVERSION)-disk2.d64 $(BINARYDIR)/$(APPNAME1)-instruments.d64
.PHONY: all clean install editor exporter 2SID 3SID 4SID
.INTERMEDIATE: $(BINARYDIR)/$(SW-PACK) $(BINARYDIR)/$(SW-EXEC) $(BINARYDIR)/$(SW-PACK-KERNAL) $(BINARYDIR)/$(SW-EXEC-KERNAL) \
                $(BINARYDIR)/$(SM-EXEC) $(BINARYDIR)/$(SM-EXEC-KERNAL) $(BINARYDIR)/$(SM-SFX-EXEC) \
                $(BINARYDIR)/$(SM-SWP-EXEC) $(BINARYDIR)/$(SM-SWP-FX-EXEC) \
                 $(BINARYDIR)/$(SM-echo-EXEC) $(BINARYDIR)/$(SM-ROM-EXEC) $(BINARYDIR)/$(UM-EXEC) \
                 $(BINARYDIR)/$(SM-ROM-SFX-EXEC) $(BINARYDIR)/$(SM-ROM-SWP-EXEC) $(BINARYDIR)/$(SM-ROM-SWP-FX-EXEC) \
                 $(BINARYDIR)/$(SM-SWP-2SID-EXEC) $(BINARYDIR)/$(SM-SWP-3SID-EXEC) $(BINARYDIR)/$(SM-SWP-4SID-EXEC) \
                 $(BINARYDIR)/$(SM-ROM-2SID-EXEC) $(BINARYDIR)/$(SM-ROM-3SID-EXEC) $(BINARYDIR)/$(SM-ROM-4SID-EXEC) \
                 $(BINARYDIR)/$(SM-ROM-SWP-2SID-EXEC) $(BINARYDIR)/$(SM-ROM-SWP-3SID-EXEC) $(BINARYDIR)/$(SM-ROM-SWP-4SID-EXEC) \
                $(BINARYDIR)/$(SW-2SID-PACK) $(BINARYDIR)/$(SW-2SID-EXEC) $(BINARYDIR)/$(SM-2SID-EXEC) \
                $(BINARYDIR)/$(SW-3SID-PACK) $(BINARYDIR)/$(SW-3SID-EXEC) $(BINARYDIR)/$(SM-3SID-EXEC) \
                $(BINARYDIR)/$(SW-4SID-PACK) $(BINARYDIR)/$(SW-4SID-EXEC) $(BINARYDIR)/$(SM-4SID-EXEC)
.INTERMEDIATE: editor.prg exporter.prg

upload: $(BINARYDIR)/$(APPNAME1)-$(APPVERSION).prg
	ncftpput -p "" -Cv c64 $(BINARYDIR)/$(APPNAME1)-$(APPVERSION).prg /Temp/$(APPNAME1)-$(APPVERSION).prg

#C64 executables
$(BINARYDIR)/$(SW-SPLASH): $(SWS-SOURCE) $(BINARYDIR)/$(SW-PACK) $(SWS-SOURCES)
ifeq ($(EMULATOR),0)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -o $@ $<
endif
$(BINARYDIR)/$(SW-PACK): $(BINARYDIR)/$(SW-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SW-EXEC): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SW-SPLASH-KERNAL): $(SWS-SOURCE) $(BINARYDIR)/$(SW-PACK-KERNAL) $(SWS-SOURCES)
ifeq ($(EMULATOR),0)
	$(AS) $(AFLAGS) -D KERNAL_LOADSAVE=1 -D SID_AMOUNT=1 -o $@ $<
endif
$(BINARYDIR)/$(SW-PACK-KERNAL): $(BINARYDIR)/$(SW-EXEC-KERNAL)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SW-EXEC-KERNAL): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D KERNAL_LOADSAVE=1 -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<

$(BINARYDIR)/$(SM-PACK): $(BINARYDIR)/$(SM-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-PACK-KERNAL): $(BINARYDIR)/$(SM-EXEC-KERNAL)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-EXEC-KERNAL): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D KERNAL_LOADSAVE=1 -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<


$(BINARYDIR)/$(SM-SFX-PACK): $(BINARYDIR)/$(SM-SFX-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-SFX-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=1 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-SWP-PACK): $(BINARYDIR)/$(SM-SWP-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-SWP-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif


$(BINARYDIR)/$(SM-SWP-FX-PACK): $(BINARYDIR)/$(SM-SWP-FX-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-SWP-FX-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=1 -D SLOWDOWN_SUPP=1 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif


$(BINARYDIR)/$(SM-echo-PACK): $(BINARYDIR)/$(SM-echo-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-echo-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D ECHO_SUPPORT=1 -D ECHO_DIVISOR=4.0 -D ECHO_MILLISECONDS=400 -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif


$(BINARYDIR)/$(SM-ROM-PACK): $(BINARYDIR)/$(SM-ROM-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-ROM-SFX-PACK): $(BINARYDIR)/$(SM-ROM-SFX-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-SFX-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=1 -D SFX_SUPPORT=1 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-ROM-SWP-PACK): $(BINARYDIR)/$(SM-ROM-SWP-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-SWP-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-ROM-SWP-FX-PACK): $(BINARYDIR)/$(SM-ROM-SWP-FX-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-SWP-FX-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=1 -D SFX_SUPPORT=1 -D SLOWDOWN_SUPP=1 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-ROM-2SID-PACK): $(BINARYDIR)/$(SM-ROM-2SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-2SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=2 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-ROM-3SID-PACK): $(BINARYDIR)/$(SM-ROM-3SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-3SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=3 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-ROM-4SID-PACK): $(BINARYDIR)/$(SM-ROM-4SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-4SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=4 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif


$(BINARYDIR)/$(SM-SWP-2SID-PACK): $(BINARYDIR)/$(SM-SWP-2SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-SWP-2SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=2 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-SWP-3SID-PACK): $(BINARYDIR)/$(SM-SWP-3SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-SWP-3SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=3 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-SWP-4SID-PACK): $(BINARYDIR)/$(SM-SWP-4SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-SWP-4SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=4 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif


$(BINARYDIR)/$(SM-ROM-SWP-2SID-PACK): $(BINARYDIR)/$(SM-ROM-SWP-2SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-SWP-2SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=2 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-ROM-SWP-3SID-PACK): $(BINARYDIR)/$(SM-ROM-SWP-3SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-SWP-3SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=3 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-ROM-SWP-4SID-PACK): $(BINARYDIR)/$(SM-ROM-SWP-4SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(SM-ROM-SWP-4SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D PLAYER_FOR_ROM=1 -D SID_AMOUNT=4 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=1 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif


$(BINARYDIR)/$(UM-PACK): $(BINARYDIR)/$(UM-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif
$(BINARYDIR)/$(UM-EXEC): $(UM-SOURCE) $(UM-SOURCES)
	$(AS) $(AFLAGS) -o $@ $<


#C64 2SID-variant executables
$(BINARYDIR)/$(SW-2SID-SPLASH): $(SWS-SOURCE) $(BINARYDIR)/$(SW-2SID-PACK) $(SWS-SOURCES)
ifeq ($(EMULATOR),0)
	$(AS) $(AFLAGS) -D SID_AMOUNT=2 -o $@ $<
endif

$(BINARYDIR)/$(SW-2SID-PACK): $(BINARYDIR)/$(SW-2SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif

$(BINARYDIR)/$(SW-2SID-EXEC): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=2 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-2SID-PACK): $(BINARYDIR)/$(SM-2SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif

$(BINARYDIR)/$(SM-2SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=2 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

2SID: $(BINARYDIR)/$(SW-2SID-SPLASH) $(BINARYDIR)/$(SM-2SID-PACK)


#C64 3SID-variant executables
$(BINARYDIR)/$(SW-3SID-SPLASH): $(SWS-SOURCE) $(BINARYDIR)/$(SW-3SID-PACK) $(SWS-SOURCES)
ifeq ($(EMULATOR),0)
	$(AS) $(AFLAGS) -D SID_AMOUNT=3 -o $@ $<
endif

$(BINARYDIR)/$(SW-3SID-PACK): $(BINARYDIR)/$(SW-3SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif

$(BINARYDIR)/$(SW-3SID-EXEC): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=3 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-3SID-PACK): $(BINARYDIR)/$(SM-3SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif

$(BINARYDIR)/$(SM-3SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=3 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

3SID: $(BINARYDIR)/$(SW-3SID-SPLASH) $(BINARYDIR)/$(SM-3SID-PACK)


#C64 4SID-variant executables
$(BINARYDIR)/$(SW-4SID-SPLASH): $(SWS-SOURCE) $(BINARYDIR)/$(SW-4SID-PACK) $(SWS-SOURCES)
ifeq ($(EMULATOR),0)
	$(AS) $(AFLAGS) -D SID_AMOUNT=4 -o $@ $<
endif

$(BINARYDIR)/$(SW-4SID-PACK): $(BINARYDIR)/$(SW-4SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif

$(BINARYDIR)/$(SW-4SID-EXEC): $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=4 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

$(BINARYDIR)/$(SM-4SID-PACK): $(BINARYDIR)/$(SM-4SID-EXEC)
ifeq ($(EMULATOR),0)
	$(PACKER) $< $@
else
	cp $< $@
endif

$(BINARYDIR)/$(SM-4SID-EXEC): $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=4 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o $@ $<
ifeq ($(EMULATOR),1)
	$(eval CMPNAME = $(subst -nopack,,$(basename $@)).cmp )
	$(RAWPACKER)  $@  $(CMPNAME)
 ifeq ($(TUI_ONLY),0)
	$(MV)  $(CMPNAME)  $(EMURESDIR)/
 else
	$(MV)  $(CMPNAME)  $(EMURESDIR)/$(basename $(notdir $(CMPNAME)))-for-TUI.cmp
 endif
endif

4SID: $(BINARYDIR)/$(SW-4SID-SPLASH) $(BINARYDIR)/$(SM-4SID-PACK)


#SWMconvert
.IGNORE:
$(BINARYDIR)/$(SWM-CONVERTER): $(SWM-CONVERTER).c SWM-spec.src
	$(CC) -w $< -o $@ -lm -s
	#-i686-w64-mingw32-gcc -m32 -w $< -o "$@.exe" -lm -s
.IGNORE = $(NULL)

#sng2swm
.IGNORE:
$(BINARYDIR)/$(SNG-CONVERTER): $(SNG-CONVERTER)/$(SNG-CONVERTER).cpp $(SNG-SOURCES)
	$(CPPC) $< -o $@ -lm -s
	#-i686-w64-mingw32-g++ $< -m32 -s -o "$@.exe" -lm -s
.IGNORE = $(NULL)

#generate XM/MID exports and import example XM/MID/SNG files
.IGNORE:
Conversion-Examples: $(BINARYDIR)/$(SWM-CONVERTER) $(BINARYDIR)/$(SNG-CONVERTER) $(EXASWMDIR)/
ifeq ($(EMULATOR),0)
	for inputfile in $(EXASWMDIR)/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(BINARYDIR)/$(SWM-CONVERTER) $$inputfile $(EXASWMDIR)/xm-exports/$${outputfile%.*}.xm $(KUSS) ; done ;
	for inputfile in $(EXASWMDIR)/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(BINARYDIR)/$(SWM-CONVERTER) $$inputfile $(EXASWMDIR)/mid-exports/$${outputfile%.*}.mid $(KUSS) ; done ;
	for inputfile in $(EXASWMDIR)/xm-imports/*.xm ; do $(BINARYDIR)/$(SWM-CONVERTER) $$inputfile $${inputfile%.*}.swm ; done ;
	for inputfile in $(EXASWMDIR)/mid-imports/*.mid ; do $(BINARYDIR)/$(SWM-CONVERTER) $$inputfile $${inputfile%.*}.swm $(KUSS) ; done ;
	for inputfile in $(EXASWMDIR)/sng-imports/*.sng ; do $(BINARYDIR)/$(SNG-CONVERTER) $$inputfile $${inputfile%.*}.swm $(KUSS) ; done ;
	for inputfile in $(EXASWMDIR)/sng-imports/todisk/*.sng ; do $(BINARYDIR)/$(SNG-CONVERTER) $$inputfile $${inputfile%.*}.swm $(KUSS) ; done ;
endif

.IGNORE = $(NULL)

#SID-Wizard disk
$(BINARYDIR)/$(APPNAME1)-$(APPVERSION)$(SUBVERSION)-disk1.d64: $(BINARYDIR)/$(SW-SPLASH) $(BINARYDIR)/$(SM-PACK) $(BINARYDIR)/$(SW-2SID-SPLASH) $(BINARYDIR)/$(SM-2SID-PACK) $(BINARYDIR)/$(UM-PACK) $(EXASWMDIR)/ $(EXASWMDIR)/sng-imports/todisk
	$(CBMDISK) -format "sid-wizard $(APPVERSION)$(SUBVERSION),s1" d64 $@
ifeq ($(EMULATOR),0)
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SW-SPLASH) sid-wizard-$(APPVERSION)
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SM-PACK) sid-maker-$(APPVERSION)
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SW-2SID-SPLASH) sid-wizard-2sid
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(SM-2SID-PACK) sid-maker-2sid
	$(CBMDISK) -attach $@ -write $(BINARYDIR)/$(UM-PACK) sw-$(APPVERSION)-manual
	$(CBMDISK) -attach $@ -write ./dummyfile CCCCCCCCCCCCCCCC
	for inputfile in $(EXASWMDIR)/*.swm ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
#	$(CBMDISK) -attach $@ -write ./dummyfile "CCCsng-importCCC"
#	for inputfile in $(EXASWMDIR)/sng-imports/todisk/*.swm ; do outputfile=$${inputfile##*/} ; \
#         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
	$(CBMDISK) -attach $@ -write ./dummyfile "CCCCC 2sid CCCCC"
	for inputfile in $(EXASWMDIR)/*.sws ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $@ -write $$inputfile $${outputfile} ; done;
endif

$(BINARYDIR)/$(APPNAME1)-$(APPVERSION)$(SUBVERSION)-disk2.d64: $(BINARYDIR)/$(SW-3SID-SPLASH) $(BINARYDIR)/$(SM-3SID-PACK) $(EXASWMDIR)/ $(EXASWMDIR)/swm-more $(EXASWMDIR)/swm-more/light  $(EXASWMDIR)/swm-more/extra $(EXASWMDIR)/swm-more/bare $(EXASWMDIR)/swm-more/demo
	$(CBMDISK) -format "sid-wizard $(APPVERSION)$(SUBVERSION),s2" d64 $@
ifeq ($(EMULATOR),0)
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
endif

#SID-Wizard instrument-disk
$(BINARYDIR)/$(APPNAME1)-instruments.d64: $(EXASWIDIR)/
	$(CBMDISK) -format "sw$(APPVERSION)-instrument,s3" d64 $@
ifeq ($(EMULATOR),0)
	for inputfile in $(EXASWIDIR)/*.swi ; do outputfile=$${inputfile##*/} ; \
         $(CBMDISK) -attach $(BINARYDIR)/$(APPNAME1)-instruments.d64 -write $$inputfile $${outputfile} $(KUSS) ; done;
endif

#for test builds
editor: editor.prg
	$(X64) $(XFLAGS) $<

editor.prg: $(SW-SOURCE) $(SW-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o editor-nopack.prg $<
	$(PACKER) editor-nopack.prg editor.prg

exporter: exporter.prg
	$(X64) $(XFLAGS) $<

exporter.prg: $(SM-SOURCE) $(SM-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o exporter-nopack.prg $<
	$(PACKER) exporter-nopack.prg exporter.prg

SID-Wizard-splashed: SID-Wizard-splashed.prg
	$(X64) $(XFLAGS) $<

SID-Wizard-splashed.prg: $(SWS-SOURCE) $(SWS-SOURCES)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -o $@ $<

test: $(SW-SOURCE) $(SW-SOURCES)
ifeq ($(UNCOMPRESSED_NATIVES),0)
ifeq ($(TUI_ONLY),0)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o ../../resources/SID-Wizard-nopack.prg $<
	$(RAWPACKER)  ../../resources/SID-Wizard-nopack.prg  ../../resources/SID-Wizard.cmp
	$(RM) ../../resources/SID-Wizard-nopack.prg
else
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o ../../resources/SID-Wizard-nopack-for-TUI.prg $<
	$(RAWPACKER)  ../../resources/SID-Wizard-nopack-for-TUI.prg  ../../resources/SID-Wizard-for-TUI.cmp
	$(RM) ../../resources/SID-Wizard-nopack-for-TUI.prg
endif
else
ifeq ($(TUI_ONLY),0)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o ../../resources/SID-Wizard.prg $<
else
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SWversion="'$(APPVERSION)'" -o ../../resources/SID-Wizard-for-TUI.prg $<
endif
endif

mtest: $(SM-SOURCE) $(SM-SOURCES)
ifeq ($(TUI_ONLY),0)
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o ../../resources/SID-Maker-nopack.prg $<
	$(RAWPACKER)  ../../resources/SID-Maker-nopack.prg  ../../resources/SID-Maker.cmp
	$(RM) ../../resources/SID-Maker-nopack.prg
else
	$(AS) $(AFLAGS) -D SID_AMOUNT=1 -D SFX_SUPPORT=0 -D SLOWDOWN_SUPP=0 -D SWP_EXPORT=0 -D SWversion="'$(APPVERSION)'" -o ../../resources/SID-Maker-nopack-for-TUI.prg $<
	$(RAWPACKER)  ../../resources/SID-Maker-nopack-for-TUI.prg  ../../resources/SID-Maker-for-TUI.cmp
	$(RM) ../../resources/SID-Maker-nopack-for-TUI.prg
endif


compressor: $(RAWPACKER)

$(RAWPACKER): $(RAWPACKER).c  FORCE
	cd tools && $(LD)  -r  -b binary  selfextract.prg  -o selfextract.prg.o  #make prg linkable by CC
	cd tools && $(LD)  -r  -b binary  selfextract_nomove.prg  -o selfextract_nomove.prg.o  #make prg linkable by CC
	 $(CC) -O3 -s $(RAWPACKER).c tools/selfextract.prg.o tools/selfextract_nomove.prg.o -o $(RAWPACKER) -lm
	rm -f  tools/*.prg.o


clean:
	$(RM) $(BINARYDIR)/$(SW-SPLASH) $(BINARYDIR)/$(SW-SPLASH-KERNAL) $(BINARYDIR)/$(SW-2SID-SPLASH) $(BINARYDIR)/$(SW-3SID-SPLASH) $(BINARYDIR)/$(SW-4SID-SPLASH)
	$(RM) $(BINARYDIR)/$(SW-PACK) $(BINARYDIR)/$(SW-EXEC) $(BINARYDIR)/$(SM-PACK) $(BINARYDIR)/$(SM-PACK-KERNAL) $(BINARYDIR)/$(UM-PACK) $(BINARYDIR)/$(SM-EXEC) $(BINARYDIR)/$(UM-EXEC)
	$(RM) $(BINARYDIR)/$(SW-2SID-EXEC) $(BINARYDIR)/$(SW-2SID-PACK) $(BINARYDIR)/$(SM-2SID-EXEC) $(BINARYDIR)/$(SM-2SID-PACK)
	$(RM) $(BINARYDIR)/$(SW-3SID-EXEC) $(BINARYDIR)/$(SW-3SID-PACK) $(BINARYDIR)/$(SM-3SID-EXEC) $(BINARYDIR)/$(SM-3SID-PACK)
	$(RM) $(BINARYDIR)/$(SW-4SID-EXEC) $(BINARYDIR)/$(SW-4SID-PACK) $(BINARYDIR)/$(SM-4SID-EXEC) $(BINARYDIR)/$(SM-4SID-PACK)
	$(RM) $(BINARYDIR)/$(SM-SFX-PACK) $(BINARYDIR)/$(SM-SWP-PACK) $(BINARYDIR)/$(SM-SWP-FX-PACK)
	$(RM) $(BINARYDIR)/$(SM-echo-PACK) $(BINARYDIR)/$(SM-ROM-PACK)
	 $(RM) $(BINARYDIR)/$(SM-ROM-SFX-PACK) $(BINARYDIR)/$(SM-ROM-SWP-PACK) $(BINARYDIR)/$(SM-ROM-SWP-FX-PACK) \
	 $(RM) $(BINARYDIR)/$(SM-SWP-2SID-PACK) $(BINARYDIR)/$(SM-SWP-3SID-PACK) $(BINARYDIR)/$(SM-SWP-4SID-PACK)
	 $(RM) $(BINARYDIR)/$(SM-ROM-2SID-PACK) $(BINARYDIR)/$(SM-ROM-3SID-PACK) $(BINARYDIR)/$(SM-ROM-4SID-PACK)
	 $(RM) $(BINARYDIR)/$(SM-ROM-SWP-2SID-PACK) $(BINARYDIR)/$(SM-ROM-SWP-3SID-PACK) $(BINARYDIR)/$(SM-ROM-SWP-4SID-PACK)
	$(RM) $(BINARYDIR)/$(APPNAME1)-$(APPVERSION)$(SUBVERSION)-disk1.d64 $(BINARYDIR)/$(APPNAME1)-$(APPVERSION)$(SUBVERSION)-disk2.d64 $(BINARYDIR)/$(APPNAME1)-instruments.d64
	$(RM) $(BINARYDIR)/$(SWM-CONVERTER) $(BINARYDIR)/$(SNG-CONVERTER)
	#$(RM) $(BINARYDIR)/$(SWM-CONVERTER).exe $(BINARYDIR)/$(SNG-CONVERTER).exe
	$(RM) $(EXASWMDIR)/xm-exports/*.xm $(EXASWMDIR)/mid-exports/*.mid
	$(RM) $(EXASWMDIR)/xm-imports/*.swm.prg $(EXASWMDIR)/xm-imports/*.swm.P00 $(EXASWMDIR)/xm-imports/*.swm
	$(RM) $(EXASWMDIR)/mid-imports/*.swm.prg $(EXASWMDIR)/mid-imports/*.swm.P00 $(EXASWMDIR)/mid-imports/*.swm
	$(RM) $(EXASWMDIR)/sng-imports/*.swm $(EXASWMDIR)/sng-imports/todisk/*.swm
	$(RM) editor.prg exporter.prg SID-Wizard-splashed.prg
#	$(RM) $(RAWPACKER)


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
	$(CP) $(BINARYDIR)/$(SW-4SID-SPLASH) $(APPDIR3)/
	$(CP) $(BINARYDIR)/$(SM-4SID-PACK) $(APPDIR3)/
	$(CP) $(DOCDIR)/$(DOCNAME1) $(APPDIR3)/
	for filename in $(EXASWIDIR)/*.swi ; do $(CP) $$filename $(APPDIR3)/ ; done;
	for filename in $(EXASWMDIR)/*.swm; do onlyname=$${filename##*/} ; $(CP) $$filename $(APPDIR3)/$${onlyname%.*} ; done;
	for filename in $(EXASWMDIR)/*.sws ; do $(CP) $$filename $(APPDIR3)/ ; done;
	for filename in $(EXASWMDIR)/*.swt ; do $(CP) $$filename $(APPDIR3)/ ; done;
	for filename in $(EXASWMDIR)/*.swq ; do $(CP) $$filename $(APPDIR3)/ ; done;

uninstall:
	$(RM) $(DESKDIR)/$(APPNAME1).desktop $(ICONDIR)/$(SW-ICON)
	$(RM) $(APPDIR)/$(SWM-CONVERTER) $(APPDIR)/$(SNG-CONVERTER)
	$(RM) $(APPDIR3)/$(SW-SPLASH) $(APPDIR3)/$(SM-PACK)
	$(RM) $(APPDIR3)/$(SW-2SID-SPLASH) $(APPDIR3)/$(SM-2SID-PACK)
	$(RM) $(APPDIR3)/$(SW-3SID-SPLASH) $(APPDIR3)/$(SM-3SID-PACK)
	$(RM) $(APPDIR3)/$(SW-4SID-SPLASH) $(APPDIR3)/$(SM-4SID-PACK)
	$(RM) $(APPDIR3)/$(DOCNAME1)
#	$(RM) -rf $(APPDIR3)/  #not recommended to delete the whole folder as user can have workfiles there


purge: clean uninstall


FORCE:


#========================================================================================================
