include ../common.mk

SRC= simple1_measureSrc.hs \
     fft_pipeline.hs
EXES= $(SRC:.hs=.exe)

# Par source code with relative paths updated for this deeper directory:
RELPARSRC= $(subst ../,../../,$(ALLPARSRC))

all: $(EXES)

%.exe : %.hs $(RELPARSRC)
	$(GHC) -O2 --make -i../../ $< -o $@ -threaded -rtsopts

runtests: 
	../run_tests.sh $(EXES) 

clean:
	rm -f $(EXES) *.o *.hi
