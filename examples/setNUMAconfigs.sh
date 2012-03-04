#!/bin/bash

# run `source setNUMAconfigs.sh`, then run your benchmarks

# this config uses the logical core numbers
#ENVS='[[("NUMA_TOPOLOGY","[[0,1,2,3,4,5,6,7],[8,9,10,11,12,13,14,15],[16,17,18,19,20,21,22,23],[24,25,26,27,28,29,30,31]]")]]'

# this config uses the physical core numbers
ENVS='[[("NUMA_TOPOLOGY","[[0,4,8,12,16,20,24,28],[1,5,9,13,17,21,25,29],[2,6,10,14,18,22,26,30],[3,7,11,15,19,23,27,31]]")]]'
export ENVS
