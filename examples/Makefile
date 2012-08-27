
# This is just here as a reminder of what to run, or for people whose
# fingers are in the habit of typing "make".

# Run "make install-all" in the parent directory before using the below:

all: install
install:
	./generate_cabal.sh
	cabal install


# This is a quick test that assumes dependencies are already there.
test: 
	ghc -rtsopts -threaded --make benchmark.hs -o benchmark.run
	SHORTRUN=1 THREADS="1" ./benchmark.run --par --no-cabal +RTS -N

clean:
	cabal clean || echo 
	rm -rf bin

linBuildRun: benchmark.run
	SHORTRUN=1 THREADS="1" ./benchmark.run 

# HSH has ugly dependencies... let's install it locally for now.
benchmark.run: benchmark.hs
	cabal install HSH
#	cabal-dev install HSH
	ghc -threaded --make benchmark.hs -o benchmark.run

#================================================================================
# Hacks to build on Windows:

# BAD way to check for Windows:
ifeq ($(SYSTEMROOT),)
  BUILDCMD=linBuildRun
else
  BUILDCMD=winBuildRun
endif

validate: 
	./generate_cabal.sh
	cabal install --only-dependencies
	$(MAKE) $(BUILDCMD)

CBLDEV=	cabal-dev install -s ..\\cabal-dev
BIN="../cabal-dev/bin/"
# SUFFIX=""

# We don' yet have the benchmark script running on Windows.  So we do
# things a bit more manually.
winBuildRun:
	$(CBLDEV) -ftrace    --program-suffix=_Trace_threaded.exe 
	$(CBLDEV) -fdirect   --program-suffix=_Direct_threaded.exe 
	$(CBLDEV) -fmeta-smp --program-suffix=_SMP_threaded.exe 
	$(CBLDEV) -fsparks   --program-suffix=_Sparks_threaded.exe 
	cabal-dev install -s ../cabal-dev/ -fdirect
	$(MAKE) quicktest SUFFIX="_Trace_threaded.exe"
	$(MAKE) quicktest SUFFIX="_Direct_threaded.exe"
	$(MAKE) quicktest SUFFIX="_SMP_threaded.exe"
        # No way to run Sparks version with this little hack.

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


