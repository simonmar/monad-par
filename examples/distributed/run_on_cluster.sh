#!/bin/bash

#SBATCH --job-name=monad-par

#
# This script runs the dist tests on a cluster. It assumes that
# you have already requested a job allocation to run these tests.
#

# Extra arguments passed through to the executable:
ARGS=$*

# Consider passing -xc when compiled in profiling mode:
if [ "$ARGS" == "" ]; then
  ARGS=10 
fi

if [ "$APP" = "" ]; then 
  APP=parfib_dist
fi

if [ "$SLEEP" = "" ]; then 
  SLEEP=0.5
fi

if [ "$VERBOSITY" = "" ]; then 
  VERBOSITY=0
fi

# Error if any command below fails:
#set -x

#----------------------------------------
rm -f *.addr

# Launch master remotely asynchronously:
srun -N1 -o master-$SLURM_NNODES-%N.log ./run_single.sh -v $VERBOSITY $APP $ARGS &
sleep $SLEEP

# Launch workers remotely (asynchronously):
srun -N$((SLURM_NNODES - 1)) -o worker-$SLURM_NNODES-%N.log ./run_single.sh -v $VERBOSITY -r slave $APP $ARGS
