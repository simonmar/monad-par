
# This provides some convenient shorthands for installing many cabal packages.

# It is essentially a more compact way of bundling what would be a
# handful of shell scripts.

# --------------------------------------------------------------------------------
# Set up some Variables
# --------------------------------------------------------------------------------

# The packages unique to this repository, not including third-party
# dependencies (submodules).
OUR_PKGS= abstract-par/ monad-par-extras/ monad-par/ meta-par/
# Direct CUDA support isn't really meant to be distributed:
# meta-par-cuda/

# Third party dependency: concurrent data structures
DEQUE_PKGS= Deques/CAS/ Deques/AbstractDeque/ Deques/MichaelScott/ 
#  Deques/ChaseLev/ Deques/MegaDeque/ 

# Third party dependency: network-transport layer
NETWORK_PKGS=   RPC/ meta-par-dist-tcp/ \
  distributed-process/network-transport \
  distributed-process/network-transport-pipes/

# Third party dependency: Accelerate
ACC_PKGS= accelerate/ abstract-par-accelerate/ meta-par-accelerate/
# accelerate/accelerate-io/
# All of the above can work CPU-only.  The following really requires CUDA:
ACC_GPU_PKGS= accelerate/accelerate-cuda/ 

MAIN_PKGS= ${DEQUE_PKGS} ${ACC_PKGS} ${OUR_PKGS} 

ifeq ($(GHC),)
  GHC=`which ghc`
endif 

ifeq ($(GHC_PKG),)
  GHC_PKG=`which ghc-pkg`
endif

ifeq ($(HADDOCK),)
  HADDOCK=haddock
#  HADDOCK= "$(HOME)/.cabal/bin/haddock"
endif

ifeq ($(CABAL),)
  CABAL= cabal
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

# Example of how to reinstall:
reinstall:
	CABAL_ARGS="--force-reinstalls" ${MAKE} install

install-ours:
	${CABAL_INSTALL} ${OUR_PKGS}

# Install everything you need for distributed meta-par.
dist-install:  check-submodules
	${CABAL_INSTALL} ${MAIN_PKGS} ${NETWORK_PKGS}

mega-install:  check-submodules
	${CABAL_INSTALL} ${MAIN_PKGS} 

# This one is CUDA SPECIFIC:
mega-install-cuda: check-submodules
	${CABAL_INSTALL} -fcuda ${MAIN_PKGS} ${ACC_GPU_PKGS}

check-submodules:
	if [ ! -d ./Deques/README.md ]; echo "ERROR, no submod.  You need to run git 'submodule update --init --recursive'! "; exit 1; fi 

# For Jenkins testing of old GHC versions we are only interested in meta-par and monad-par:
jenkins-all-versions:
	${CABAL_INSTALL} ${OUR_PKGS} Deques/AbstractDeque/

uninstall:
	ghc-pkg unregister network-transport-pipes --force || echo
	ghc-pkg unregister network-transport-tcp   --force || echo
	ghc-pkg unregister network-transport    --force || echo
	ghc-pkg unregister RPC                  --force || echo

	ghc-pkg unregister meta-par-dist-tcp    --force || echo

	ghc-pkg unregister accelerate-cuda      --force || echo 
	ghc-pkg unregister accelerate-io        --force || echo 
	ghc-pkg unregister accelerate           --force || echo 
	ghc-pkg unregister meta-par-accelerate  --force || echo 
	ghc-pkg unregister abstract-par-accelerate --force || echo 

	ghc-pkg unregister meta-par             --force || echo 
	ghc-pkg unregister monad-par            --force || echo 
	ghc-pkg unregister monad-par-extras     --force || echo

	ghc-pkg unregister mega-deque           --force || echo 
	ghc-pkg unregister lockfree-queue       --force || echo
	ghc-pkg unregister chaselev-deques      --force || echo
	ghc-pkg unregister abstract-deque       --force || echo 
	ghc-pkg unregister IORefCAS             --force || echo



# --------------------------------------------------------------------------------
# Testing 
# --------------------------------------------------------------------------------

# The short way.  Install everything and test at the same time.
test:
	$(MAKE) mega-install CABAL_ARGS='--enable-tests --disable-documentation'

# The longer way.  Run a full test uses cabal-dev to sandbox the build.
validate: 
	$(MAKE) mega-install CABAL='cabal-dev' CABAL_ARGS='$(CABAL_ARGS) --disable-library-profiling --enable-tests --disable-documentation --force-reinstalls'
	(cd examples; $(MAKE) validate)
# force-reinstalls
# SANDBOX=`pwd`/cabal-dev
# pushd monad-par
# cabal-dev -s $SANDBOX configure --with-ghc=$GHC --with-ghc-pkg=$GHC_PKG --enable-tests
# cabal-dev -s $SANDBOX build
# cabal-dev -s $SANDBOX test --test-options='--jxml=dist/test/$test-suite.xml'
# popd


# --------------------------------------------------------------------------------
# Build the Documentation 
# --------------------------------------------------------------------------------

doc:
	rm -rf docs
	mkdir docs
        # Link EVERYTHING to Haddock:
	${CABAL_INSTALL} ${ALL_GPU_PKGS} --enable-documentation \
           --haddock-html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' \
           --with-haddock=${HADDOCK} --force-reinstalls
	mv */dist/doc/html/* docs/
	mv ./Deques/*/dist/doc/html/* docs/
	mv ./accelerate/*/dist/doc/html/* docs/

# Ryan specific:
# This publishes docs on the interwebs.
# 
# TODO: If we also moved over all the otherd docs from the global and
# user DBs we could create a complete documentation collection on this
# website and no longer depend on hackage:
install-doc:
	rsync -vrplt docs/ ~/.hyplan/haddock/
	cd ~/.hyplan/haddock/
	makedirindex > index.html
	chmod ugo+rX -R .

# --------------------------------------------------------------------------------



