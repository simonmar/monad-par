include ../common.mk

sumeuler.exe : $(wildcard *.hs) ../../Control/Monad/Par.hs
	$(GHC) -O2 -cpp --make -i../.. SumEuler.hs -o $@ -threaded -rtsopts
