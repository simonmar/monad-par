include ../common.mk

SRC= simple1_measureSrc.hs \
     disjoint_working_sets_pipeline.hs \
     fft_pipeline.hs
EXES= $(SRC:.hs=.exe)

NOTHREAD= $(SRC:.hs=.nothread.exe)

# Par source code with relative paths updated for this deeper directory:
RELPARSRC= $(subst ../,../../,$(ALLPARSRC))

all: $(EXES)

%.exe : %.hs $(RELPARSRC)
	$(GHC) -O2 --make -i../../ $< -o $@ -threaded -rtsopts


#------------------------------------------------------------
# Build the non-threaded versions.

nothread: $(NOTHREAD)

%.nothread.exe: %.hs $(RELPARSRC)
	$(GHC) -O2 --make -i../../ $< -o $@ -rtsopts

#------------------------------------------------------------
runtests: 
	../run_tests.sh $(EXES) 

