#!/bin/bash

THERE=$1
shift
if [ "$THERE" == "" ]; then
   echo "Usage: run_two_here_two_there.sh DOMAINNAME [other-args]"
   echo "  Where DOMAINNAME is the 'there' machine on which to run two workers."
   exit 1
fi

# Extra arguments passed through to the executable:
ARGS=$*

# Consider passing -xc when compiled in profiling mode:
if [ "$ARGS" == "" ]; then
  ARGS=10 
fi

if [ "$APP" = "" ]; then 
  APP=parfib_dist
fi

# Error if any command below fails:
set -e 
set -x

#----------------------------------------
rm -f worker1.log worker2.log worker3.log *.addr

ROOT=`pwd`
HERE=`hostname`

export MACHINE_LIST="$HERE $HERE $THERE $THERE"

# Launch master asynchronously:
./$APP.exe master tcp $ARGS  &
MASTERPID=$!

sleep 0.4
# Launch 1st worker asynchronously:
./$APP.exe slave tcp $ARGS &> worker1.log & 
WORKER1PID=$!

# Launch 2nd and 3rd remotely (asynchronously):
ssh $THERE "(cd $ROOT; VERBOSITY=$VERBOSITY ./$APP.exe slave tcp $ARGS )" &> worker2.log & 
WORKER2PID=$!
ssh $THERE "(cd $ROOT; VERBOSITY=$VERBOSITY ./$APP.exe slave tcp $ARGS )" &> worker3.log & 
WORKER3PID=$!

# Now wait until the master is done.
wait $MASTERPID

echo "Done running master computation.  Killing worker if it's still running."

# This shouldn't be necessary now that there is a proper shutdown system.
# kill -9 $WORKER1PID
# kill -9 $WORKER2PID
# kill -9 $WORKER3PID
