include ../common.mk

all: minimax.exe

minimax.exe : $(wildcard *.hs) ../../Control/Monad/Par.hs
	$(GHC) -O2 -cpp --make -i../.. Main.hs -o $@ -threaded -rtsopts


clean: defaultclean
