include common.mk

BENCHMARKS= queens.hs mandel.hs blackscholes.hs nbody.hs parfib.hs primes.hs cholesky.hs

# binomial_lattice.hs
EXES= $(BENCHMARKS:.hs=.exe)

OTHEREXES= matmult/matmult.exe minimax/minimax.exe sumeuler/sumeuler.exe

ALLEXES= $(EXES) $(OTHEREXES)

PARSRC= ../Control/Monad/Par.hs ../Control/Monad/Par/AList.hs ../Control/Monad/Par/OpenList.hs

all: $(EXES)
	(cd sumeuler; $(MAKE))
	(cd matmult;  $(MAKE))
	(cd minimax;  $(MAKE))

%.exe : %.hs $(PARSRC)
	$(GHC) -O2 --make -i.. $< -o $@ -threaded -rtsopts

runtests: 
	./run_tests.sh $(ALLEXES) 

clean:
	rm -f $(ALLEXES) *.o *.hi
	(cd sumeuler; $(MAKE) clean)
	(cd matmult;  $(MAKE) clean)
	(cd minimax;  $(MAKE) clean)
