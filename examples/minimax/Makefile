include ../common.mk

all: minimax.exe

minimax.exe : $(wildcard *.hs) $(DPRPARSRC)
	$(GHC) $(GHC_FLAGS) --make -i../.. Main.hs -o $@ -rtsopts


clean: defaultclean
