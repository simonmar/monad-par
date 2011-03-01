include ../common.mk

minimax : $(wildcard *.hs) ../../Control/Monad/Par.hs
	$(GHC) -O2 -cpp --make -i../.. Main.hs -o $@ -threaded -rtsopts
