#!/bin/bash


ARGS=$*

# Error if any command fails:
set -e 
# set -x

HOST=`hostname`

# Use -xc here under profiling mode:
OPTS="-N2 "

export MACHINE_LIST="$HOST $HOST"

if [ "$APP" = "" ]; then 
  APP=parfib_dist
fi

# Launch master asynchronously:
time ./$APP.exe master $ARGS +RTS $OPTS -RTS &

MASTERPID=$!
sleep 0.5

# Launch worker asynchronously:
# ./$APP.exe slave +RTS $OPTS -RTS &> worker.log & 
./$APP.exe slave +RTS $OPTS -RTS & 
WORKERPID=$!

# Now wait until the master is done.
wait $MASTERPID
echo "Done running master computation."

# Don't worry about errors in the slave process.
# set +e
# wait $WORKERPID

kill -9 $WORKERPID

exit 0 
