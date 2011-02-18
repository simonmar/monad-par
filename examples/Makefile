GHC = ghc


BENCHMARKS= queens.hs mandel.hs blackscholes.hs
EXES= $(BENCHMARKS:.hs=.exe)

PARSRC= ../Control/Monad/Par.hs ../Control/Monad/Par/AList.hs ../Control/Monad/Par/OpenList.hs

all: $(EXES)

%.exe : %.hs $(PARSRC)
	$(GHC) -O2 --make -i.. $< -o $@ -threaded -rtsopts

runtests: 
	echo $(EXES) | xargs -n1 bash -c 

# queens : queens.hs ../Control/Monad/Par.hs
# 	$(GHC) -O2 --make -i.. $< -o $@ -threaded -rtsopts

# mandel : mandel.hs ../Control/Monad/Par.hs
# 	$(GHC) -O2 --make -i.. $< -o $@ -threaded -rtsopts

clean:
	rm -f $(EXES) *.o *.hi
