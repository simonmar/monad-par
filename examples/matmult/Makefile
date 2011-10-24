include ../common.mk

all: matmult.exe 

matmult.exe : $(wildcard *.hs) $(DPRPARSRC)
	$(GHC) $(GHC_FLAGS) -cpp --make -i../.. MatMult.hs -o $@  -rtsopts

clean: defaultclean
