include ../common.mk

all: matmult.exe 

matmult.exe : $(wildcard *.hs) ../../Control/Monad/Par.hs
	$(GHC) -O2 -cpp --make -i../.. MatMult.hs -o $@ -threaded -rtsopts

clean: defaultclean
