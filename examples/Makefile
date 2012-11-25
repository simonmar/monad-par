
# This is just here as a reminder of what to run, or for people whose
# fingers are in the habit of typing "make".

# Run "make install-all" in the parent directory before using the below:

all: install
install:
	./generate_cabal.sh
	cabal install

clean:
	cabal clean || echo 
	rm -rf bin *.hi *.o *.exe

distclean: clean
	rm -f benchmark.run

# This is a quick test that assumes dependencies are already there.
# TODO -- should probably merge this with 'validate'.
# But right now it's SOO much faster than using cabal that it's nice to have around:
test: benchmark.run
        # Building with no cabal should put the same executables in the same
        # places as building with cabal:
	SHORTRUN=1 THREADS="1" ./benchmark.run --par --no-cabal +RTS -N

linValidate: benchmark.run
	SHORTRUN=1 THREADS="1" ./benchmark.run +RTS -N

benchmark.run: benchmark.hs
	ghc -rtsopts -threaded --make benchmark.hs -o benchmark.run

#================================================================================
# Hacks to build on Windows:

# BAD way to check for Windows:
ifeq ($(SYSTEMROOT),)
  BUILDCMD=linValidate
  MAINDEVDIR=../cabal-dev
else
  BUILDCMD=winValidate
  MAINDEVDIR=..\\cabal-dev
endif

validate: 
	./generate_cabal.sh
	cabal install --only-dependencies
	$(MAKE) $(BUILDCMD)

CBLDEV=	cabal-dev install -s $(MAINDEVDIR)
BIN="../cabal-dev/bin/"
# SUFFIX=""

tracevers:
	$(CBLDEV) -ftrace    --program-suffix=_Trace_threaded.exe 

directvers:
	$(CBLDEV) -fdirect   --program-suffix=_Direct_threaded.exe 

smpvers:
	$(CBLDEV) -fmeta-smp --program-suffix=_SMP_threaded.exe 

sparksvers:
	$(CBLDEV) -fsparks   --program-suffix=_Sparks_threaded.exe 

# Should be able to get make -j parallelism here:
allvers: tracevers directvers smpvers sparksvers

# temphack:
# 	./generate_cabal.sh
# 	$(CBLDEV) --only-dependencies
# 	$(MAKE) -j allvers

# We don' yet have the benchmark script running on Windows.  So we do
# things a bit more manually.
winValidate: allvers
	cabal-dev install -s ../cabal-dev/ -fdirect
	$(MAKE) winhelper SUFFIX="_Trace_threaded.exe"
	$(MAKE) winhelper SUFFIX="_Direct_threaded.exe"
	$(MAKE) winhelper SUFFIX="_SMP_threaded.exe"
        # No way to run Sparks version with this little hack.

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


