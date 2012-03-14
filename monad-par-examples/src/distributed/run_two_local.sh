#!/bin/bash

# INPUTS
#----------------------------------------

ARGS=$*

if [ "$APP" = "" ]; then 
  APP=parfib_dist
fi

if [ "$SLEEP" = "" ]; then 
  SLEEP=0.5
fi
#----------------------------------------

# Error if any command fails:
set -e 
# set -x


# Some hygiene:
#rm -f /tmp/pipe_*

HOST=`hostname`

# Use -xc here under profiling mode:
# OPTS="-N2 "
# OPTS=" -N1 "

export MACHINE_LIST="$HOST $HOST"

# Launch master asynchronously:
time ./$APP.exe master $ARGS +RTS $OPTS -RTS &
MASTERPID=$!

sleep $SLEEP

# Launch worker asynchronously, and in its own directory:
# ----------------------------------------
# This is lame but we copy the executable to get a DIFFERENT eventlog:
cp -f $APP.exe "$APP"_slave.exe

./"$APP"_slave.exe slave $ARGS & 
WORKERPID=$!

# Now wait until the master is done.
wait $MASTERPID
echo "Done running master computation."

# Don't worry about errors in the slave process.
set +e
# wait $WORKERPID
kill $WORKERPID

exit 0 
