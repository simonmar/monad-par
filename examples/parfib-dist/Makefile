
# A way to configure arguments and make sure cabal-dev is used when it
# should be.

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

ifeq ($(CABAL_ARGS),)
        CABAL_ARGS=
	CABAL_ARGS_TWO=
endif

ifeq ($(SANDBOX),)
	SANDBOX=cabal-dev
endif

ifeq ($(CABAL),cabal-dev)
	CABAL_ARGS_TWO=${CABAL_ARGS} --sandbox=${SANDBOX}
endif

CABAL_INSTALL= ${CABAL} install --with-ghc=${GHC} --with-ghc-pkg=${GHC_PKG} \
  --with-haddock=${HADDOCK} ${CABAL_ARGS_TWO}

# -------------------------------------------------------------------

# Actually install the thing

install:
	${CABAL_INSTALL}

test:
	bash simple_parfib_test.sh

# user responsible for their own $SANDBOX
clean:
	rm -r *addr dist
