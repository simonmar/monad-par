include common.mk

# This Makefile is largely redundant with the benchmark script, but it
# provides a simple (and parallel) way to build and test the examples.
# 
# Entrypoints:
#   make all
#   make test

# ================================================================================

BENCHMARKS= queens.hs mandel.hs blackscholes.hs nbody.hs \
            parfib_monad.hs parfib_pseq.hs randomGen.hs \
            primes.hs cholesky/cholesky.hs coins.hs \
            quicksort/parquicksort_monad.hs quicksort/parquicksort_pseq.hs \
            sumeuler/sumeuler.hs \
            matmult/MatMult.hs \
            minimax/Main.hs \
            stream/simple1_measureSrc.hs \
            partree/partree.hs \
	    kmeans/kmeans.hs
# binomial_lattice.hs
# Note: not using all of the stream benchmarks at the moment.

DISTEXES= distributed/parfib_dist.exe

ALLEXES= $(BENCHMARKS:.hs=.exe)

# FILE EXTENSIONS USED:
#   .hs   -- example source
#   .exe  -- binary (used on all platforms)
#   .out  -- output from "make test"
#   .err  -- indicates a test has failed.

OUTS  = $(ALLEXES:.exe=.out)

.SUFFIXES: .o .c .exe .out
.PRECIOUS: $(ALLEXES) $(OUTS)

all: $(ALLEXES) datasets 

dist: $(DISTEXES)

%.exe : %.hs $(ALLPARSRC)
	@echo "Building executable: $@"
	$(GHC) $(GHC_FLAGS) --make -i.. -i$(shell dirname $<) $< -o $@ -rtsopts

# ================================================================================
# Testing:

test: all $(OUTS)
	@echo 
	@echo ========================================
	@echo "Finally, checking for .err files:"
	find -name "*.err"

.exe.out:
# We touch a .err file and then remove it if there is no error.
	@touch "$(<:.exe=.err)"
	@echo 
	@echo ====================================================================================================
	@echo RUNNING TEST $<
	@echo "(Displaying up 20 lines of output):"
	@echo ----------------------------------------	
# This makes it difficult to get the error code:
#	./$<  &> /dev/stdout | tee "$@" | head -n 20
	"./$<"  &> $@ 
	@head -n 20 $@
#	@tail -F $@ | head -n 20
	@echo Done Running test.  Exited successfully.
	@echo ----------------------------------------
	@rm -f "$(<:.exe=.err)"

# DANGER DANGER:
wiperesults:
	rm -f results_*.dat results_*.dat.*
	rm -f bench_*.log bench_*.log.*


# ================================================================================
# Benchmarking infrastructure:

bench: benchmark.run clusterbench.run
benchmark.run: benchmark.hs
	ghc --make benchmark.hs -o benchmark.run -threaded -rtsopts

clusterbench.run: clusterbench.hs
	ghc --make clusterbench.hs -o clusterbench.run -threaded -rtsopts

# ================================================================================
datasets:
	(cd cholesky; ./get_cholesky_data.sh)
	(cd kmeans;   ./get_kmeans_data.sh)

# ================================================================================
# Install dependencies used by test programs.
#
# These are not included in the .cabal file because we don't want to
# increase the dependencies for the monad-par package as a whole.
install-deps: deps
deps:
	cabal install parallel fft carray --with-ghc=$(GHC)
# For kmeans:
	cabal install mersenne-random --with-ghc=$(GHC)
	cabal install binary --with-ghc=$(GHC)

# Make sure we have comparable installed configurations (-O2)
alldeps:
	cabal install --with-ghc=$(GHC) -O2 mersenne-random fft carray vector-algorithms --reinstall
	@echo
alldepvers:
	GHC=ghc-6.12.3 $(MAKE) alldeps
	GHC=ghc-7.0.4  $(MAKE) alldeps
	GHC=ghc-7.2.1  $(MAKE) alldeps
	GHC=ghc-7.4.1  $(MAKE) alldeps

# Redifen clean from common.mk
clean:
	rm -f $(ALLEXES) *.o *.hi *.out *.exe
	rm -rf build_*_threaded*
	rm -rf build_*_serial*
	rm -f $(ALLEXES) */*.o */*.hi */*.out */*.exe
	rm -rf */build_*_threaded*
	rm -rf */build_*_serial*

distclean: clean
	rm -f benchmark.run

# ================================================================================

fib: parfib_monad.exe parfib_pseq.exe
