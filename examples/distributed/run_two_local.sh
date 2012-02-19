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

# Use -xc here under profiling mode:
OPTS="-N2 $*"

export MACHINE_LIST="$HOST $HOST"

# Launch master asynchronously:
./parfib_dist.exe master $N +RTS $OPTS -RTS &

MASTERPID=$!
sleep 0.3

# Launch worker asynchronously:
./parfib_dist.exe slave +RTS $OPTS -RTS &> worker.log & 
WORKERPID=$!

# Now wait until the master is done.
wait $MASTERPID
echo "Done running master computation."

# Don't worry about errors in the slave process.
set +e
wait $WORKERPID

exit 0 
