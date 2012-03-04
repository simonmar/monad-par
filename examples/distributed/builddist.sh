#!/bin/bash

set -e 
# set -x

if [ "$APP" = "" ]; then 
  APP=parfib_dist
fi

if [ "$DEBUG" != "" ]; then 
  OPTS="-DDEBUG"
else
  OPTS=""
fi

if [ "$GHC" = "" ]; then 
  GHC=ghc
fi

# -hide-package remote
set -x
$GHC ../../dist/build/cbits/pin.o -i../..  --make $APP.hs -o $APP.exe -O2 -threaded -rtsopts $OPTS $@
set +x

echo "  Next run this command here:"
echo "MACHINE_LIST=... ./parfib_dist.exe master 10 +RTS -N2"
echo 
echo "  And run this somewhere else:"
echo "./parfib_dist.exe slave +RTS -N2"
