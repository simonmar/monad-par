
Experimental Results Directory
==============================

This directory contains results from the benchmarks.sh script found in
the `examples/` directory.  Currently the schema for results is:

     results_MACHINE_YYYY_MM_DD_[server|desktop|laptop]_vN[.M].dat

Which in turn should reside in a directory named
"MachineDescription/ghc-X.Y.Z/".  MachineDescription is a
human-generated informal description of the hardware platform.

A typical example of something you would do in this directory would be
to create plots from a raw benchmark result file, like so:

    ./plot_scaling.hs Nehalem_3.33GHz_4coreHT/ghc-7.2.1/results_wasp_2011_10_03_v1.dat 


Pull requests are welcome including runs from new architectures, as
long as the results conform to these conventions. Please also run the
following on your Linux machine to record some machine details:

    cd MyMachineDescr; ../characterize_machine.sh

(We would love it if someone could implement an analogous script for Windows and MacOS!)


TODO 
====

We need to add scripts for crawling all results over time to visualize
improvements and regressions.

