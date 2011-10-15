include ../common.mk

all: sumeuler.exe

sumeuler.exe : $(wildcard *.hs) $(DPRPARSRC)
	$(GHC) $(GHC_FLAGS) --make -i../.. SumEuler.hs -o $@ -rtsopts

clean: defaultclean
