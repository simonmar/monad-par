

SRC= issue21.hs
EXES= $(SRC:.hs=.exe)

.SUFFIXES: .hs .exe

.hs.exe: $< $(SRC)
	ghc -i../monad-par/ --make $< -o $@ -rtsopts -threaded -with-rtsopts -N $(GHC_ARGS)

all: $(EXES)

test: 
	(cd ../examples; $(MAKE) benchmark.run)
	BENCHLIST=testlist.txt ../examples/benchmark.run

# These extensionless executables appear if people do a raw ghc
# --make.  I like to wipe these out for cleanliness:
RAWPROGS= $(SRC:.hs=)
clean:
	rm -f *~ *.o *.hi $(RAWPROGS) $(EXES)

distclean: clean
	rm -f results*.dat bench*.log *.bak
