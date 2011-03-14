include common.mk

BENCHMARKS= ./queens.hs ./mandel.hs ./blackscholes.hs ./nbody.hs
EXES= $(BENCHMARKS:.hs=.exe)

PARSRC= ../Control/Monad/Par.hs ../Control/Monad/Par/AList.hs ../Control/Monad/Par/OpenList.hs

all: $(EXES)

%.exe : %.hs $(PARSRC)
	$(GHC) -O2 --make -i.. $< -o $@ -threaded -rtsopts

# test: runtests
# runtests: 
# 	echo $(EXES) | xargs -n1 bash -c 

runtests: 
	./run_tests.sh $(EXES) 

clean:
	rm -f $(EXES) *.o *.hi
