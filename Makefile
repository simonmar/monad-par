
# This provides some convenient shorthands for installing many cabal packages.

# It is essentially a more compact way of bundling what would be a
# handful of shell scripts.

default: reinstall

# --------------------------------------------------------------------------------
# Set up some Variables
# --------------------------------------------------------------------------------

# The packages unique to this repository, not including third-party
# dependencies (submodules).
OUR_PKGS= abstract-par/ monad-par-extras/ monad-par/ 

ifneq ($(GHC_VER),)
  GHC=ghc-$(GHC_VER)
  GHC_PKG=ghc-pkg-$(GHC_VER)
endif

ifeq ($(GHC),)
  GHC=`which ghc`
endif 

ifeq ($(GHC_PKG),)
  GHC_PKG=`which ghc-pkg`
endif

ifeq ($(HADDOCK),)
  HADDOCK=haddock
endif

ifeq ($(CABAL),)
  CABAL= cabal
endif

# [2013.09.16] TODO: Replace me with "cabal sandbox".
ifeq ($(CABALDEV),)
  CABALDEV= cabal-dev
endif

ifeq ($(CABAL_ARGS),)
  CABAL_ARGS=--force-reinstalls
endif

CABAL_INSTALL= ${CABAL} install --with-ghc=${GHC} --with-ghc-pkg=${GHC_PKG} \
  --with-haddock=${HADDOCK} ${CABAL_ARGS} 

# --------------------------------------------------------------------------------
# Installation 
# --------------------------------------------------------------------------------

# The main entrypoint assumes that you already have third party
# dependencies installed, and installs the core packages only:
install: install-ours

install-ours:
	${CABAL_INSTALL} ${OUR_PKGS}

# Example of how to reinstall:
reinstall:
	CABAL_ARGS="--force-reinstalls" ${MAKE} install

check-submodules:
	@if [ ! -f ./bench_results/README.md ]; then echo "!!ERROR: submodule missing.  You need to run 'git submodule update --init --recursive'! "; exit 1; fi 

# --------------------------------------------------------------------------------
# Testing 
# --------------------------------------------------------------------------------

# The short way.  Install everything and test at the same time.
test:
	$(MAKE) install CABAL_ARGS='--enable-tests --disable-documentation --force-reinstalls'

# The longer way.  Run a full test uses cabal-dev to sandbox the build.
validate: 
	$(MAKE) mega-install CABAL='$(CABALDEV)' CABAL_ARGS='$(CABAL_ARGS) --disable-library-profiling --enable-tests --disable-documentation --force-reinstalls'
	(cd examples; $(MAKE) validate CABALDEV='$(CABALDEV)')

# --------------------------------------------------------------------------------
# Build the Documentation 
# --------------------------------------------------------------------------------

doc:
	rm -rf docs
	mkdir docs
        # Link EVERYTHING to Haddock:
	${CABAL_INSTALL} ${OUR_PKGS} --enable-documentation \
           --haddock-html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' \
           --with-haddock=${HADDOCK} --force-reinstalls
	mv */dist/doc/html/* docs/

# Ryan specific:
# This publishes docs on the interwebs.
# 
# TODO: If we also moved over all the other docs from the global and
# user DBs we could create a complete documentation collection on this
# website and no longer depend on hackage:
install-doc:
	rsync -vrplt docs/ ~/.hyplan/haddock/
	cd ~/.hyplan/haddock/
	makedirindex > index.html
	chmod ugo+rX -R .

# --------------------------------------------------------------------------------

# Pinging the server here can manually trigger a build-bot build:
trigger-jenkins:
	wget tester-lin.soic.indiana.edu:8080/job/monad-par_QUICKtest_github/build?token=gogomonadparbuild
