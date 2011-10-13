
The examples/ directory
=======================

This directory contains a collection of programs using the Par monad.
It is called "examples" but it also serves as most of our tests and
benchmarks as well.

The primary commands of interest in this directory are:

    make deps
    make
    make test

Make deps will install package dependencies that aren't already
installed by monad-par itself.

Also, this directory provides benchmarking.  Ranging from the simple:

    SHORTRUN=1 THREADSETTINGS="1" ./benchmark.hs

To the involved:

    KEEPGOING=1 TRIALS=3 THREADSETTINGS="1 2 3 4 5 6 7 8" ./benchmark.hs

Both of which will populate a file named results_$HOSTNAME.dat.  



HOW TO ADD A NEW BENCHMARK
==========================

It is important that new benchmarks conform to all the conventions.
Here is the recipe:

  * Add your benchmark here (single .hs file) or in a subdirectory.
  * If the latter, give it a Makefile (supporting "all" and "clean").
  * Add the benchmark and its input parameters to benchlist.txt, as
    well as the _laptop and _server variants.
  * Make sure the benchmark runs with no arguments.  This is "test"
    mode and should run quickly.

