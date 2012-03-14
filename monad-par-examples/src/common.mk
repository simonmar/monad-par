
# RRN: allow setting it via the command line.  (I don't have a ghc-testing2 ;-). )
ifeq (,$(GHC))
  GHC = ghc
  # GHC = ghc-testing2
  # GHC = ghc-stable-nightly2
endif

ifeq (,$(GHC_FLAGS))
  GHC_FLAGS = -O2 -threaded
endif

# This is hard to keep up to date:
ALLPARSRC= ../Control/Monad/Par.hs ../Control/Monad/Par/AList.hs ../Control/Monad/Par/OpenList.hs \
           ../Control/Monad/Par/IList.hs  ../Control/Monad/Par/Stream.hs ../Control/Monad/Par/Logging.hs

# Par source code with relative paths updated for a deeper directory:
DPRPARSRC= $(subst ../,../../,$(ALLPARSRC))

default: all

defaultclean:
	rm -f $(EXES) *.o *.hi *.out *.exe
