
# These are HUnit/QuickCheck tests, factored out of their respective files.

# TESTS= ParTests.hs AListTest.hs
TESTS= AllTests.hs
EXES= $(TESTS:.hs=.exe)

all: $(EXES) test

.SUFFIXES: .hs .exe

.hs.exe: $<
	ghc -i.. --make $< -o $@

deps:
	cabal install QuickCheck HUnit test-framework test-framework-hunit test-framework-quickcheck2

test:
	@echo
	@echo "Running tests:"
	@echo "=================================="
	./AllTests.exe
#	"for t in $(EXES); do $t; done"
# 	./ParTests.exe
# 	./AListTest.exe

clean:
	rm -f *.hi *.exe *.o
