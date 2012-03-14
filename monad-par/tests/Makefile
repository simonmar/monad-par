
# NOTE!! THIS IS REDUNDANT WITH THE CABAL TEST-SUITE IN monad-par.cabal
#
# This is simply an alternative way to run the tests.


# These are HUnit/QuickCheck tests, factored out of their respective files:
SRC = ParTests.hs AListTest.hs AsyncTests.hs TestHelpers.hs Makefile
TESTS= AllTests.hs
EXES= $(TESTS:.hs=.exe)

all: $(EXES) test

.SUFFIXES: .hs .exe

.hs.exe: $< $(SRC)
	ghc -i.. --make $< -o $@ -rtsopts -threaded

deps:
	cabal install QuickCheck HUnit test-framework test-framework-hunit test-framework-quickcheck2 test-framework-th

test:
	@echo
	@echo "Running tests:"
	@echo "=================================="
	./AllTests.exe +RTS -N2
#	"for t in $(EXES); do $t; done"
# 	./ParTests.exe
# 	./AListTest.exe

clean:
	rm -f *.hi *.exe *.o
