#===========================================================#
#               HerMIDI makefile for Linux                  #
#===========================================================#

PICASM = gpasm
C64ASM = 64tass
APPNAME= HerMIDI
APPNAM2= receiver
SOURCE1= $(APPNAME)-PIC.gpasm
SOURCE2= $(APPNAME)-C64.asm
SOURCE3= $(APPNAM2).asm
BINARY1= $(APPNAME)-PIC.hex
BINARY2= $(APPNAME)-C64.prg
BINARY3= $(APPNAM2).prg
TESTCMD= testbench.stc
TESTGEN= stimgen.py
BINDIR = .
REMOVE = rm -f
COPY   = cp -f
SIMPIC = gpsim
PICKIT = pk2cmd
PICDIR = /usr/share/pk2
PICTYPE= PIC12F509

.PHONY: all all-before all-after install clean
all: all-before BINARY all-after
	chmod 755 *

help: 
	@echo "possible targets: all, BINARY, sim, install, clean, help"

#generate .hex,.lst,.cod file for the PIC and C64 binaries
BINARY: $(SOURCE1) $(SOURCE2) $(SOURCE3) $(TESTGEN)
	$(PICASM) $(SOURCE1) -o $(BINDIR)/$(BINARY1)
	$(C64ASM) -C -a $(SOURCE2) -o $(BINDIR)/$(BINARY2)
	$(C64ASM) -C -a $(SOURCE3) -o $(BINDIR)/$(BINARY3)
	python $(TESTGEN)

#simulate PIC & PCB
sim: $(TESTCMD) $(BINDIR)/$(APPNAME)-PIC.cod
	$(SIMPIC) -c $(TESTCMD)

#burn the .hex file to the PIC
install: $(BINDIR)/$(BINARY1)
	@echo "Checking factory internal RC-oscillator calibration value (OSCCAL, the last 2 digits.)"
	@echo "(So it can be recovered in case a PIC-programming error occurs and it gets lost.)"
	@echo "(To re-program OSCCAL you have to add -U0Cxx to burner parameters, where xx is OSCCAL)"
	$(PICKIT) -P$(PICTYPE) -GP3ff-3ff -B$(PICDIR) 
	@echo "(To note down OSCCAL you can write it on the 8 legs of the PIC with felt-tip pen in binary form)"
	$(PICKIT) -P$(PICTYPE) -M -B$(PICDIR) -F$(BINDIR)/$(BINARY1)

clean:
	${REMOVE} $(BINDIR)/$(APPNAME)-PIC.lst
	${REMOVE} $(BINDIR)/$(APPNAME)-PIC.cod
	${REMOVE} $(BINDIR)/$(BINARY1)
	${REMOVE} $(BINDIR)/$(BINARY2)
	${REMOVE} $(BINDIR)/$(BINARY3)
#	${REMOVE} stimulus.stc

#===========================================================#
