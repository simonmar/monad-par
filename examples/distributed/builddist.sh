#!/bin/bash

set -e 
set -x

# Comment to disable debug mode:
if [ "$DEBUG" != "" ]; then 
  OPTS="-DDEBUG"
else
  OPTS=""
fi

if [ "$GHC" = "" ]; then 
  GHC=ghc
fi

$GHC -i../.. -hide-package remote --make parfib_dist.hs -o parfib_dist.exe -O2 -threaded -rtsopts $OPTS $@

echo "  Next run this command here:"
echo "MACHINE_LIST=... ./parfib_dist.exe master 10 +RTS -N2"
echo 
echo "  And run this somewhere else:"
echo "./parfib_dist.exe slave +RTS -N2"
