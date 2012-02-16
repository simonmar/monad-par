#!/bin/bash

N=$1
shift
if [ "$N" == "" ]; then
 N=10
fi

# Error if any command fails:
set -e 
set -x

HOST=`hostname`

export MACHINE_LIST="localhost $HOST"

# Launch master asynchronously:
./parfib_dist.exe master $N +RTS -N2 -RTS &

MASTERPID=$!
sleep 1

# Launch worker asynchronously:
./parfib_dist.exe slave +RTS -N2 -RTS  & 
WORKERPID=$!

# Now wait until the master is done.
wait $MASTERPID

echo "Done running master computation.  Killing worker if it's still running."
kill -9 $WORKERPID
