include ../common.mk

partree.exe : $(wildcard *.hs) ../../Control/Monad/Par.hs
	$(GHC) -O2 -cpp --make -i../.. partree.hs -o $@ -threaded -rtsopts
