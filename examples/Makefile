
# This is just here as a reminder of what to run, or for people whose
# fingers are in the habit of typing "make".

# Run "make install-all" in the parent directory before using the below:

all: install
install:
	./generate_cabal.sh
	cabal install

# This can't reuse the vanilla "cabal install" version above because
# it passes a bunch of different flags.
test: 
	./generate_cabal.sh
	ghc -threaded --make benchmark.hs -o benchmark.run
	SHORTRUN=1 THREADS="1" ./benchmark.run

clean:
	cabal clean
	rm -rf bin


test:
	./bin/parfib.exe
	./bin/mandel.exe
