#!/bin/bash

N=$1
shift

# Error if any command fails:
# set -e 

pushd worker
(./parfib_dist monad $N +RTS -N2 -RTS &> ../worker.log) & 
WORKER=$!
echo "Worker pid $WORKER"
popd

# sleep 0.3s
# tail -F worker.log &
# TAIL1=$1

(time ./parfib_dist monad $N +RTS -N2 -RTS &> master.log) &
MASTER=$!
echo "MASTER pid $MASTER"

# sleep 0.3s
# tail -F master.log &
# TAIL2=$1

echo "Waiting for master to finish... (writing to master.log)."
wait $MASTER

echo "Done running master computation.  Killing worker if it's still running."
# kill -9 $TAIL1
# kill -9 $TAIL2
kill -9 $WORKER
