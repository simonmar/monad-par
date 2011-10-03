
# These are HUnit tests, factored out of their respective files.

TESTS= ParTests.hs AListTest.hs
EXES= $(TESTS:.hs=.exe)

all: $(EXES) test

.SUFFIXES: .hs .exe

.hs.exe:
	ghc -i.. --make $< -o $@

test:
	@echo
	@echo "Running tests:"
	@echo "=================================="
	./ParTests.exe
	./AListTest.exe

clean:
	rm -f *.hi *.exe *.o
