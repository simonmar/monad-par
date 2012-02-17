#!/bin/bash

THERE=$1
shift
if [ "$THERE" == "" ]; then
   echo "Usage: run_two_here_two_there.sh DOMAINNAME"
   echo "  Where DOMAINNAME is the 'there' machine on which to run two workers."
   exit 1
fi

# Error if any command fails:
set -e 
set -x

ROOT=`pwd`
HERE=`hostname`

export MACHINE_LIST="$HERE $HERE $THERE $THERE"

# Launch master asynchronously:
./parfib_dist.exe master 10 +RTS -N2 -RTS &
MASTERPID=$!

sleep 0.3
# Launch 1st worker asynchronously:
./parfib_dist.exe slave +RTS -N2 -RTS  & 
WORKER1PID=$!

# Launch 2nd and 3rd remotely (asynchronously):
ssh $THERE "(cd $ROOT; ./parfib_dist.exe slave +RTS -N2 -RTS)" & 
WORKER2PID=$!
ssh $THERE "(cd $ROOT; ./parfib_dist.exe slave +RTS -N2 -RTS)" & 
WORKER3PID=$!

# Now wait until the master is done.
wait $MASTERPID

echo "Done running master computation.  Killing worker if it's still running."

# This shouldn't be necessary now that there is a proper shutdown system.
# kill -9 $WORKER1PID
# kill -9 $WORKER2PID
# kill -9 $WORKER3PID
