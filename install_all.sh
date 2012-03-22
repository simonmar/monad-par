#!/bin/bash

set -o errexit
set -x

# Optionally configure which programs to use:
if [ "$HADDOCK" == "" ];
then HADDOCK=`which haddock`
fi
if [ "$CABAL" == "" ];
then CABAL=`which cabal`
fi
if [ "$GHC" == "" ];
then GHC=`which ghc`
fi

function doall () {
  CMD=$1
  (cd abstract-par;      $CMD)
  (cd monad-par;         $CMD)
  (cd meta-par;          $CMD)
  (cd RPC;               $CMD)
  (cd meta-par-dist-tcp; $CMD)
  (cd meta-par-cuda;     $CMD)
}

# Pass through extra command line arguments to cabal:
# doall "cabal haddock --with-haddock=$HADDOCK"
# doall "cabal install --haddock"
doall "$CABAL install --with-ghc=$GHC $*"
