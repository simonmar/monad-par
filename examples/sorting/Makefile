GHC=ghc
CC=icc

all: cilkWrap 

cilksort: cilksort.o
	$(CC) -Wall $^ -o $@

cilksort-nomain.o: cilksort.c
	$(CC) -Wall -c $^ -o $@

cilksort.o: cilksort.c
	$(CC) -Wall -DUSE_MAIN=1 -c $^ -o $@

cilkWrap: cilkWrap.hs cilksort-nomain.o
	$(GHC) --make $^ -lm -lcilkrts -lirc -limf -o $@

clean:
	rm -f cilkWrap cilksort *.hi *.o

.PHONY: clean


# /u/rrnewton/opt/cuda/bin/nvcc  -gencode=arch=compute_10,code=\"sm_10,compute_10\"  -gencode=arch=compute_20,code=\"sm_20,compute_20\"  -m64 --compiler-options -fno-strict-aliasing  -I. -I/u/rrnewton/opt/cuda/include -I../../common/inc -I../../../shared//inc -DUNIX -O2   -o obj/x86_64/release/radixSortThrust.cu.o -c radixSortThrust.cu

