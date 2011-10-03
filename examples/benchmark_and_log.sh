#!/bin/bash

# TODO: make robust to lack of /dev/stdout or "tee"

(./benchmark.sh &> /dev/stdout) | tee bench_$HOSTNAME.log &
