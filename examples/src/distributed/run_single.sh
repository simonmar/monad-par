#!/bin/bash

VERBOSITY=0
ROLE=master

# Ugh, it looks like it's impossible to pass env vars over
# when launching jobs using SLURM, unless you install a 
# separate plugin. so we do a little arg loop.

while [ $# -gt 0 ]
do
    case "$1" in
        -v)  VERBOSITY="$2"; shift;;
        -r)  ROLE="$2"; shift;;
        --)     shift; break;;
        -*)
            echo >&2 \
            "usage: $0 [-v verbosity] [-r role] [args ...]"
            exit 1;;
        *)  break;;     # terminate while loop
    esac
    shift
done

APP=$1
shift
if [ "$APP" = "" ]; then 
  APP=parfib_dist
fi

# Extra arguments passed through to the executable:
ARGS=$*

export MACHINE_LIST=`./cluster-machinelist`
export VERBOSITY=$VERBOSITY

time ./$APP.exe $ROLE tcp $ARGS &
PID=$!
wait $PID
