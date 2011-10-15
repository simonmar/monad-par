include ../common.mk

# This directory provides several different benchmarks:
SRC= simple1_measureSrc.hs \
     disjoint_working_sets_pipeline.hs \
     fft_pipeline.hs
EXES= $(SRC:.hs=.exe)

NOTHREAD= $(SRC:.hs=.nothread.exe)

all: $(EXES)

%.exe : %.hs $(DPRPARSRC)
	$(GHC) $(GHC_FLAGS)  --make -i../../ $< -o $@  -rtsopts


#------------------------------------------------------------
# Build the non-threaded versions.

nothread: $(NOTHREAD)

%.nothread.exe: %.hs $(DPRPARSRC)
	$(GHC) $(GHC_FLAGS) --make -i../../ $< -o $@ -rtsopts

#------------------------------------------------------------
runtests: 
	../run_tests.sh $(EXES) 

clean: defaultclean
