include common.mk

# 9 benchmarks
BENCHMARKS= queens.hs mandel.hs blackscholes.hs nbody.hs parfib.hs \
            primes.hs cholesky.hs parfib.hs coins.hs

# binomial_lattice.hs
EXES= $(BENCHMARKS:.hs=.exe)

# Another four in their own subdirectories:
OTHEREXES= matmult/matmult.exe minimax/minimax.exe sumeuler/sumeuler.exe \
           stream/simple1_measureSrc.exe
# Note: not using all of the stream benchmarks at the moment.

ALLEXES= $(EXES) $(OTHEREXES)

all: $(EXES)
	(cd sumeuler; $(MAKE))
	(cd matmult;  $(MAKE))
	(cd minimax;  $(MAKE))
	(cd stream;   $(MAKE) simple1_measureSrc.exe)
	(cd partree;  $(MAKE))

%.exe : %.hs $(ALLPARSRC)
	$(GHC) -O2 --make -i.. $< -o $@ -threaded -rtsopts

test: runtests
runtests: 
	./run_tests.sh $(ALLEXES) 

clean:
	rm -f $(ALLEXES) *.o *.hi *.out
	(cd sumeuler; $(MAKE) clean)
	(cd matmult;  $(MAKE) clean)
	(cd minimax;  $(MAKE) clean)
	(cd stream;   $(MAKE) clean)
	(cd partree;  $(MAKE) clean)
