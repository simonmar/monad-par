include common.mk

# 9 benchmarks
BENCHMARKS= queens.hs mandel.hs blackscholes.hs nbody.hs \
            parfib_monad.hs parfib_pseq.hs \
            primes.hs cholesky.hs coins.hs \
           quicksort/parquicksort_monad.exe quicksort/parquicksort_pseq.exe
# binomial_lattice.hs

EXES= $(BENCHMARKS:.hs=.exe)

# Another four in their own subdirectories:
OTHEREXES= matmult/matmult.exe minimax/minimax.exe sumeuler/sumeuler.exe \
           stream/simple1_measureSrc.exe 
# Note: not using all of the stream benchmarks at the moment.

ALLEXES= $(EXES) $(OTHEREXES)

# FILE EXTENSIONS USED:
#   .hs   -- example source
#   .exe  -- binary (used on all platforms)
#   .out  -- output from "make test"
#   .err  -- indicates a test has failed.

OUTS  = $(ALLEXES:.exe=.out)

.SUFFIXES: .o .c .exe .out
.PRECIOUS: $(ALLEXES) $(OUTS)

all: $(EXES) buildsubs
buildsubs: $(EXES)
	(cd sumeuler; $(MAKE))
	(cd matmult;  $(MAKE))
	(cd minimax;  $(MAKE))
	(cd stream;   $(MAKE) simple1_measureSrc.exe)
	(cd partree;  $(MAKE))

%.exe : %.hs $(ALLPARSRC)
	$(GHC) $(GHC_FLAGS) --make -i.. $< -o $@ -rtsopts

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
# Install dependencies used by test programs.
#
# These are not included in the .cabal file because we don't want to
# increase the dependencies for the monad-par package as a whole.
install-deps: deps
deps:
	cabal install parallel
	cabal install fft
	cabal install carray

# Redifen clean from common.mk
clean:
	rm -f $(ALLEXES) *.o *.hi *.out *.exe
	(cd sumeuler; $(MAKE) clean)
	(cd matmult;  $(MAKE) clean)
	(cd minimax;  $(MAKE) clean)
	(cd stream;   $(MAKE) clean)
	(cd partree;  $(MAKE) clean)


# ================================================================================

fib: parfib_monad.exe parfib_pseq.exe
