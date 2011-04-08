include common.mk

BENCHMARKS= queens.hs mandel.hs blackscholes.hs nbody.hs parfib.hs primes.hs \
            cholesky.hs parfib.hs coins.hs

# binomial_lattice.hs
EXES= $(BENCHMARKS:.hs=.exe)

OTHEREXES= matmult/matmult.exe minimax/minimax.exe sumeuler/sumeuler.exe \
           stream/simple1_measureSrc.exe

ALLEXES= $(EXES) $(OTHEREXES)

all: $(EXES)
	(cd sumeuler; $(MAKE))
	(cd matmult;  $(MAKE))
	(cd minimax;  $(MAKE))
	(cd stream;   $(MAKE))
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
