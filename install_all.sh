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


# # Pass through extra command line arguments to cabal:
# # doall "cabal haddock --with-haddock=$HADDOCK"
# # doall "cabal install --haddock"


$CABAL install  --with-ghc=$GHC $*     \
  abstract-par/ monad-par/ meta-par/  \
  RPC/ meta-par-dist-tcp/             \
  meta-par-cuda/ abstract-par-accelerate/ meta-par-accelerate/
 
