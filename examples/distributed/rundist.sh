#!/bin/bash

N=$1
shift

# Error if any command fails:
set -e 

pushd worker
./parfib_dist monad $N +RTS -N2 -RTS & 
popd

time ./parfib_dist monad $N +RTS -N2 -RTS
echo "Done running master computation.  Killing worker if it's still running."
kill -9 $!
