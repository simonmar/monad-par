include ../common.mk

matmult.exe : $(wildcard *.hs) ../../Control/Monad/Par.hs
	$(GHC) -O2 -cpp --make -i../.. MatMult.hs -o $@ -threaded -rtsopts
