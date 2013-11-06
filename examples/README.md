
The examples/ directory
=======================

This directory contains a collection of programs using the Par monad.
It is called "examples" but it also serves as most of our tests and
benchmarks as well.

QUICK START
===========

The easy way to get started is to simply run:

    make test

This will produce a lot of output.  It installs dependencies, then
builds and runs all examples in a "quick" configuration and in
parallel.


MORE DETAILS
============

Each benchmark/test can be built directly or through a cabal file.
'generate_cabal.sh' script creates the cabal file for you:

    ./generate_cabal.sh
    
Once generated, 'cabal install' can compile and install any
of the individual benchmarks (globally), e.g.:

    cabal install src/mandel/

`make install` will build and install them all in such a fashion.

BENCHMARK SCRIPT
================

This directory also provides benchmarking.  It is based on
`hsbencher`, which will be installed as one of the dependencies.
The benchmarking configuration information is found in `run_benchmark.hs`.

Type this for help on additional command line options:

    ./run_benchmark -h 

HSBencher has different modes of output.  The simplesti is that it
will populate a file named results_$HOSTNAME.dat.  


<DEPRECATED>
For the monad-par project itself, we typically file away these textual
result files:

    ./file_away HOST ../results/MyMachineDescr_X.YGHz_Ncore
</DEPRECATED>


NOTES ON BENCHLIST TEXT FILES
=============================

When perusing `run_benchmark.hs`, you will see different benchmark
configurations corresponding to different-sized runs:

  * quick    -- just for testing
  * desktop  -- desktop version, e.g. 4 core workstation
  * server   -- big multicore, e.g. 16-32 core server

For more details, read the rest of that script.

HOW TO ADD A NEW BENCHMARK
==========================

It is important that new benchmarks conform to all the conventions.
Here is the recipe:

  * Add your benchmark in its own directory under src/
  * Add an entry to generate_cabal.sh, listing the benchmark and its
    dependencies.  
  * Add the benchmark and its input parameters to run_benchmark.hs, 
    including all three variants
  * Make sure the benchmark runs with no arguments.  This is "test"
    mode and should run quickly.
