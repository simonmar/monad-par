
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
	ghc -rtsopts -threaded --make benchmark.hs -o benchmark.run
	SHORTRUN=1 THREADS="1" ./benchmark.run --par --no-cabal +RTS -N

validate: 
	./generate_cabal.sh
	cabal-dev install -s ../cabal-dev/
	ghc -threaded --make benchmark.hs -o benchmark.run
	SHORTRUN=1 THREADS="1" ./benchmark.run --no-recomp --par +RTS -N

clean:
	cabal clean || echo 
	rm -rf bin

# TODO: A cheap way to run everything for a quick test without having to
# rely on the benchmark script.
# runall:
# 	./bin/parfib.exe
# 	./bin/mandel.exe
