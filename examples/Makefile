
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
	$(MAKE) quicktest
#	ghc -threaded --make benchmark.hs -o benchmark.run
#	SHORTRUN=1 THREADS="1" ./benchmark.run --no-recomp --par +RTS -N

clean:
	cabal clean || echo 
	rm -rf bin

BIN="../cabal-dev/bin/"
# SUFFIX=".exe"
SUFFIX=""

# TEMP: A cheap way to run everything for a quick test without having to
# rely on the benchmark script.
quicktest:
	$(BIN)/parfib_monad$(SUFFIX)
	$(BIN)/parfib_pseq$(SUFFIX)
	$(BIN)/mandel$(SUFFIX)
# needs to run with no args:
#	$(BIN)/MatMult$(SUFFIX)
	$(BIN)/blackscholes$(SUFFIX)  
	$(BIN)/coins$(SUFFIX)
# needs data:
#	$(BIN)/kmeans$(SUFFIX)
	$(BIN)/minimax$(SUFFIX)  
	$(BIN)/nbody$(SUFFIX)  
	$(BIN)/partree$(SUFFIX)
	$(BIN)/primes$(SUFFIX)
	$(BIN)/queens$(SUFFIX)
# needs to run with no args:
#	$(BIN)/sumeuler$(SUFFIX)

#	$(BIN)/randomGen$(SUFFIX)


