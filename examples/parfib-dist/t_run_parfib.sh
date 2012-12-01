#!/bin/bash

##
## A script to run both a master and slave locally. Selects the parfib
## algorithm.
##

## This was written by toddaaro, primarily for his own uses, but it
## should be generally useful.

HOST=`hostname`
export MACHINE_LIST="$HOST $HOST"

ARGS="tcp 25 1 1"
OPTS="-N2"

EXE=dist/build/parfib_dist/parfib_dist

./$EXE master $ARGS +RTS $OPTS -RTS &
MASTER_PID=$!

sleep 1

./$EXE slave $ARGS +RTS $OPTS -RTS &
WORKER_PID=$!

wait $MASTER_PID
kill $WORKER_PID
