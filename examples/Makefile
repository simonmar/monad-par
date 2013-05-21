
# This is just here as a reminder of what to run, or for people whose
# fingers are in the habit of typing "make".

# Run "make install" in the parent directory before using the below:
#================================================================================

ifeq ($(GHC),)
  GHC=`which ghc`
endif 
ifeq ($(CABAL),)
  CABAL= cabal
endif
ifeq ($(CABALDEV),)
  CABALDEV= cabal-dev
endif

BENCHES= src/mandel/ src/primes/ src/sorting/ src/nbody/ src/queens/ src/blackscholes/ src/coins/ src/parfib/ \
  src/randomGen/ src/sumeuler/ src/MatMult/ src/minimax/ src/partree/ src/kmeans/

# disabling due to dependency hell with deepseq and containers:
# src/cholesky/ 

all: test

install: 
	./generate_cabal.sh
	$(CABAL) install -j -fdirect --prefix=`pwd` $(BENCHES) $(CABAL_ARGS)

clean:
	rm -rf bin *.hi *.o *.exe
	$(CABAL) clean || echo 

# This is a dependency but isn't yet published to hackage [2013.05.09]
# UPDATE [2013.05.10]: Assume it's in the global path:
HSBENCHER=hsbencher

# This is a quick test that assumes dependencies are already there.
# TODO -- should probably merge this with 'validate'.
# But right now it's SOO much faster than using cabal that it's nice to have around:
test: deps
        # Building with no cabal should put the same executables in the same
        # places as building with cabal:
	SHORTRUN=1 THREADS="1" $(HSBENCHER) --par --no-cabal +RTS -N

monad-par-examples.cabal:
	./generate_cabal.sh

deps: monad-par-examples.cabal
	$(MAKE) install CABAL_ARGS="--only-dependencies --disable-library-profiling --enable-tests --disable-documentation -j"

#================================================================================
# Validation / regression testing:

# "Validation" uses cabal-dev for isolation:
CBLDEV=$(CABALDEV) -s $(MAINDEVDIR)
BIN="../cabal-dev/bin/"

# BAD way to check for Windows:
ifeq ($(SYSTEMROOT),)
  BUILDCMD=linValidate
  MAINDEVDIR=../cabal-dev
else
  BUILDCMD=winValidate
  MAINDEVDIR=..\\cabal-dev
endif

validate: monad-par-examples.cabal 
	$(MAKE) $(BUILDCMD)

# This is just like a regular test but with cabal-dev.
#linValidate: 
#	$(MAKE) CABAL=$(CBLDEV) test

# OR... we could build through cabal and forgo parallelism:
linValidate: monad-par-examples.cabal
	$(MAKE) CABAL="$(CBLDEV)" deps
	SHORTRUN=1 THREADS="1" $(HSBENCHER) --with-cabal-install="$(CBLDEV)" --with-ghc="$(GHC)"

# --------------------------------------------------
# Hacks to build on Windows:

# We don' yet have the benchmark script running on Windows.  So we do
# things a bit more manually.
winValidate: allvers
	cabal-dev install -s ../cabal-dev/ -fdirect
	$(MAKE) winhelper SUFFIX="_Trace_threaded.exe"
	$(MAKE) winhelper SUFFIX="_Direct_threaded.exe"
	$(MAKE) winhelper SUFFIX="_SMP_threaded.exe"
        # No way to run Sparks version with this little hack.

tracevers:
	$(CBLDEV) install --flags="-ftrace"    --program-suffix=_Trace_threaded.exe 

directvers:
	$(CBLDEV) install  --flags="-fdirect"   --program-suffix=_Direct_threaded.exe 

smpvers:
	$(CBLDEV) install  --flags="-fmeta-smp" --program-suffix=_SMP_threaded.exe 

sparksvers:
	$(CBLDEV) install  --flags="-fsparks"   --program-suffix=_Sparks_threaded.exe 

# Should be able to get make -j parallelism here:
allvers: tracevers directvers smpvers sparksvers


# TEMP: A cheap way to run everything for a quick test without having to
# rely on the benchmark script.
winhelper:
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


