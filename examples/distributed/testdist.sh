#!/bin/bash

N=$1
shift

# Error if any command fails:
set -e 

./builddist.sh
./rundist.sh $N

