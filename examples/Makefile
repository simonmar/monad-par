
# This is just here as a reminder of what to run, or for people whose
# fingers are in the habit of typing "make".

all:
	./generate_cabal.sh
	cabal install

test: all runtest
runtest:
	ghc --make benchmark.hs -o benchmark.run
	SHORTRUN=1 THREADS="1" ./benchmark.run

clean:
	cabal clean
