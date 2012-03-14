ifeq ($(GHC),)
  GHC=ghc
endif

# enable ICC with this:
# module load intel/composer_xe_2011_sp1.7.256-intel64

ifeq ($(CC),gcc)
  CC=gcc
  LDFLAGS=-lm 
else
  CC=icc
  LDFLAGS=-lm -lcilkrts -lirc -limf
endif

# Hack to control which CPU sort gets called at the bottom of the div/conq.
# Choices are:
#
# * CPU_SORT=HASKELL_SEQ for vector-algorithms
# * CPU_SORT=CILK_SEQ for the cilk sequential quicksort
# * CPU_SORT=CILK_PAR for the cilk parallel runtime

CFLAGS= -Wall -O3

ifeq ($(CPU_SORT),)
  CPU_SORT=HASKELL_SEQ
endif

all: mergesort cilkWrap

mergesort: mergesort.hs cilksort-nomain.o
	$(GHC) /nobackup/afoltzer/NVIDIA_GPU_Computing_SDK/C/src/mergeSort/obj/x86_64/release/mergeSort.cu.o -i../.. --make -O2 -rtsopts -threaded $^ $(LDFLAGS) -D$(CPU_SORT) -o $@

cilksort: cilksort.o
	$(CC) $(CFLAGS) $^ -o $@

cilksort-nomain.o: cilksort.c
	$(CC) $(CFLAGS) -c $^ -o $@

cilksort.o: cilksort.c
	$(CC) $(CFLAGS) -DUSE_MAIN=1 -c $^ -o $@

cilkWrap: cilkWrap.hs cilksort-nomain.o
	$(GHC) --make -O2 $^ $(LDFLAGS) -o $@

clean:
	rm -f cilkWrap mergesort cilksort *.hi *.o

.PHONY: clean


# /u/rrnewton/opt/cuda/bin/nvcc  -gencode=arch=compute_10,code=\"sm_10,compute_10\"  -gencode=arch=compute_20,code=\"sm_20,compute_20\"  -m64 --compiler-options -fno-strict-aliasing  -I. -I/u/rrnewton/opt/cuda/include -I../../common/inc -I../../../shared//inc -DUNIX -O2   -o obj/x86_64/release/radixSortThrust.cu.o -c radixSortThrust.cu

