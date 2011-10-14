include ../common.mk

all: partree.exe

partree.exe : $(wildcard *.hs) $(DPRPARSRC)
	$(GHC) $(GHC_FLAGS)  --make -i../.. partree.hs -o $@ -rtsopts

clean: defaultclean
