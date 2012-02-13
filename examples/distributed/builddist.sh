#!/bin/bash

set -e 

# Comment to disable debug mode:
if [ "$DEBUG" != "" ]; then 
  OPTS="-DDEBUG"
else
  OPTS=""
fi

ghc -i../.. -hide-package remote --make parfib_dist.hs -O2 -threaded -rtsopts $OPTS $@
# -fforce-recomp 
cp parfib_dist worker/

echo "Next run this command here and in the worker/ dir:"
echo "./parfib_dist monad 10 +RTS -N2"

