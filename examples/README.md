
The examples/ directory
=======================

This directory contains a collection of programs using the Par monad.
It is called "examples" but it also serves as most of our tests and
benchmarks as well.

QUICK START
===========

The easy way to get started is to simply run:

    make
    make test

This will compile all the examples, and then run each of them using the settings:

    SHORTRUN=1 THREADS="1"
    
This will produce a fair bit of profiling output.

DETAILS
=======

Each benchmark/test program requires a cabal file. The
'generate_cabal.sh' script creates these files for you.

    ./generate_cabal.sh
    
Once generated 'cabal install' will compile each benchmark.

    cabal install
    
The benchmarks are now ready to be run.

BENCHMARK SCRIPT
================

To build the benchmark script the haskell shell scripting module 'HSH'
is required. Some say this is a dangerous module, so you may want to
install it using cabal-dev.

TESTING / BENCHMARKING
======================

This directory provides benchmarking.  Ranging from the simple:

    SHORTRUN=1 THREADS="1" ./benchmark.hs

To the more involved:

    KEEPGOING=1 TRIALS=3 THREADS="0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16" ./benchmark.hs

Both of the above will populate a file named results_$HOSTNAME.dat.
Typically that will be followed by the following call to file the
results:

    ./file_away HOST ../results/MyMachineDescr_X.YGHz_Ncore


NOTES ON BENCHLIST TEXT FILES
=============================

The bechmarking configurations are stored in simple text files.  There
are three of them currently [2011.10.13] checked in which are tuned
for different sized runs:

  * benchlist.txt -- desktop version, e.g. 4 core workstation
  * benchlist_server.txt -- big multicore, e.g. 16-32 core server
  * benchlist_laptop.txt -- ~dual processor mobile processor

These files have hash-prefixed comments and use the following Schema:

    NAME MODE ARGS...

Name is the name of the benchmark, the root to which ".hs" and ".exe"
are appended.  Mode determines which Monad Par schedulers can be used
and is one of the following:

  * default -- whatever is provided by Control.Monad.Par and only that
  * future  -- all schedulers supporting ParFuture
  * ivars   -- all schedulers supporting ParIVar
  * chans   -- all schedulers supporting ParChan

Note that this mode setting could in theory be inferred automatically
by looking at the code for each example and what Par methods it uses.


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
