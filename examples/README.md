
The examples/ directory
=======================

This directory contains a collection of programs using the Par monad.
It is called "examples" but it also serves as most of our tests and
benchmarks as well.

QUICK START
===========

First thing first you need to install "hsbencher", which is an
executable program used to run the benchamarks.

    cd ../HSBenchScaling; cabal install
    
(Or, in the future, just "cabal install hsbencher")

Then, the easy way to get started is to simply run:

    make test

This will produce a lot of output.  It installs dependencies, then
builds and runs all examples in a "quick" configuration and in
parallel.


MORE DETAILS
============

Each benchmark/test can be built directly or through a cabal file.
'generate_cabal.sh' script creates the cabal file for you:

    ./generate_cabal.sh
    
Once generated, 'cabal install' will compile and install all
benchmarks (globally):

    cabal install


The benchmarks are now ready to be run.

BENCHMARK SCRIPT
================

This directory provides benchmarking.  We used to run the script
"benchmark.hs" from source, but now it is highly encouraged to compile
it (and the Makefile does so).

Benchmarking can range from the simple:

    SHORTRUN=1 THREADS="1" hsbencher

To the more involved:

    KEEPGOING=1 TRIALS=3 THREADS="0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16" hsbencher

Both of the above will populate a file named results_$HOSTNAME.dat.
Typically that will be followed by the following call to file the
results:

    ./file_away HOST ../results/MyMachineDescr_X.YGHz_Ncore


NOTES ON BENCHLIST TEXT FILES
=============================

The bechmarking configurations are stored in simple text files.  There
are three of them currently [2011.10.13] checked in which are tuned
for different sized runs:

  * benchlist.txt        -- desktop version, e.g. 4 core workstation
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
  * Add an entry to generate_cabal.sh, listing the benchmark and its
    dependencies.  
  * Add the benchmark and its input parameters to benchlist.txt, as
    well as the _laptop and _server variants.
  * Make sure the benchmark runs with no arguments.  This is "test"
    mode and should run quickly.
