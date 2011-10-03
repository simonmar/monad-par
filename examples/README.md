
The examples/ directory
=======================

This directory contains a collection of programs using the Par monad.
It is called "examples" but it also serves as most of our tests and
benchmarks as well.

The primary commands of interest in this directory are:

    make
    make test

As well as more extensive benchmarking.  Ranging from the simple:

    SHORTRUN=1 THREADSETTINGS="1" ./benchmark.sh

To the involved:

    KEEPGOING=1 TRIALS=3 THREADSETTINGS="1 2 3 4 5 6 7 8" ./benchmark.sh 

Both of which will populate a file named results_$HOSTNAME.dat.  

You may also want to keep the raw output so that you'll have the full
GHC "+RTS -s" output for future detective work.  In this case use:

    ./benchmark_and_log.sh 

(Again, prefixing with environment variable settings.)


