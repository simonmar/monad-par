#!/bin/bash

ht-disable

export SCHEDS="Trace Direct Sparks SMP"
export GHC_RTS="-qa -A1M" 
export BENCHLIST=benchlist_server.txt  
export KEEPGOING=1 
export GHC=ghc-7.4.1 
export THREADS="1 2 4 8 12 16 20 24 28 32" 
# export THREADS="1 2 4 8 12 16 20 24 28 32" 

export TRIALS=5

make bench
./benchmark.run --par +RTS -N

unlock_for_experiments
