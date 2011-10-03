#!/bin/bash

# Runs benchmarks.  (Note Simon Marlow has his another setup, but this
# one is self contained.)

# ---------------------------------------------------------------------------
# Usage: [set env vars] ./benchmark.sh

# Call it with environment variable...

#   SHORTRUN=1 to get a shorter run for testing rather than benchmarking.

#   THREADSETTINGS="1 2 4" to run with # threads = 1, 2, or 4.

#   KEEPGOING=1 to keep going after the first error.

#   TRIALS=N to control the number of times each benchmark is run.

#   BENCHLIST=foo.txt to select the benchmarks and their arguments
#                     (uses benchlist.txt by default)

# Additionally, this script will propagate any flags placed in the
# environment variables $GHC_FLAGS and $GHC_RTS.  It will also use
# $GHC, if available, to select the $GHC executable.

# ---------------------------------------------------------------------------


ENVSETTINGS="BENCHLIST=$BENCHLIST  THREADSETTINGS=$THREADSETTINGS  TRIALS=$TRIALS  SHORTRUN=$SHORTRUN  KEEPGOING=$KEEPGOING  GHC=$GHC  GHC_FLAGS=$GHC_FLAGS  GHC_RTS=$GHC_RTS"

# ----------------------------------------
# Default Settings:
# ----------------------------------------

if [ "$GHC" == "" ];  then GHC=ghc; fi

# HACK: with all the inter-machine syncing and different version
# control systems I run into permissions problems sometimes:
chmod +x ./ntime* ./*.sh


# Where to put the timing results:
RESULTS=results_"$HOSTNAME".dat
if [ -e $RESULTS ];
then BAK="$RESULTS".bak.`date +%s`
     echo "Backing up old results to $BAK"
     mv $RESULTS $BAK
fi

# How many times to run a process before taking the best time:
if [ "$TRIALS" == "" ]; then 
  TRIALS=1
fi

# Determine number of hardware threads on the machine:
  if [ -d /sys/devices/system/cpu/ ]; # linux
  then 
       MAXTHREADS=`ls  /sys/devices/system/cpu/ | grep "cpu[0123456789]*$" | wc -l`
  elif [ `uname` == "Darwin" ];
  then MAXTHREADS=`sysctl -n hw.ncpu`
  else MAXTHREADS=2
  # TODO: Windows?
  fi 

if [ "$THREADSETTINGS" == "" ] 
then THREADSETTINGS="$MAXTHREADS"
#then THREADSETTINGS="0 1 2 3 4"
fi

GHC_FLAGS="$GHC_FLAGS -O2 -rtsopts"
GHC_RTS="$GHC_RTS -qa"

# ================================================================================
echo "# TestName Variant NumThreads   MinTime MedianTime MaxTime" > $RESULTS
echo "#    "        >> $RESULTS
echo "# "`date`     >> $RESULTS
echo "# "`uname -a` >> $RESULTS
echo "# Determined machine to have $MAXTHREADS hardware threads."  >> $RESULTS
echo "# "`$GHC -V`                                                 >> $RESULTS
echo "# "                                               >> $RESULTS
echo "# Running each test for $TRIALS trials."          >> $RESULTS
echo "#  ... with default compiler options: $GHC_FLAGS" >> $RESULTS
echo "#  ... with default runtime options: $GHC_RTS"    >> $RESULTS
echo "# Using the following settings from the benchmarking environment:" >> $RESULTS
echo "# $ENVSETTINGS"                                   >> $RESULTS

cnt=0

function check_error() 
{
  CODE=$1
  MSG=$2
  # Error code 143 was a timeout
  if [ "$CODE" == "143" ]
  then echo " #      Return code $CODE Params: $GHC_FLAGS, RTS $GHC_RTS " | tee -a $RESULTS
       echo " #      Process TIMED OUT!!" | tee -a $RESULTS
  elif [ "$CODE" != "0" ]
  then echo " # $MSG" | tee -a $RESULTS
       echo " #      Error code $CODE Params: $GHC_FLAGS, RTS $GHC_RTS "  | tee -a $RESULTS
       echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
       if [ "$KEEPGOING" == "" ];
       then exit $CODE
       fi
  fi
}


function runit() 
{
  cnt=$((cnt+1))
  echo 
  echo "--------------------------------------------------------------------------------"
  echo "  Running Config $cnt: $test variant   threads $NUMTHREADS "
  echo "--------------------------------------------------------------------------------"
  echo 
  echo "(In directory `pwd`)"
  if [ "$NUMTHREADS" != "0" ] && [ "$NUMTHREADS" != "" ]
  then export RTS=" $GHC_RTS -s -N$NUMTHREADS "
       export FLAGS=" $GHC_FLAGS -threaded "
  else export RTS=""
       export FLAGS=" $GHC_FLAGS "
  fi

  CONTAININGDIR=`dirname $test`

  # We compile the test case using runcnc:
  if [ -e "$test".hs ]; then 
     FINALARGS="-i../  -i`dirname $test` $FLAGS $test.hs -o $test.exe"
     echo "Compiling with a single GHC command: "
     echo "   $GHC $FINALARGS"
     $GHC $FINALARGS
     check_error $? "ERROR: compilation failed."

  elif [ -d "$CONTAININGDIR" ] && 
       [ "$CONTAININGDIR" != "." ] && 
       [ -e "$CONTAININGDIR/Makefile" ]; then
     echo " ** Benchmark appears in a subdirectory with Makefile.  Using it."
     echo " ** WARNING: Can't currently control compiler options for this benchmark!"
     (cd "$CONTAININGDIR/"; make)

  else
     echo "ERROR: File does not exist: $test.hs"
     exit 1
  fi


  echo "Executing $NTIMES $TRIALS $test.exe $ARGS +RTS $RTS -RTS "
  if [ "$SHORTRUN" != "" ]; then export HIDEOUTPUT=1; fi

  # One option woud be dynamic feedback where if the first one
  # takes a long time we don't bother doing more trials.

  times=`$NTIMES "$TRIALS" ./$test.exe $ARGS +RTS $RTS -RTS`
  CODE=$?

  echo " >>> MIN/MEDIAN/MAX TIMES $times"

  check_error $CODE "ERROR: run_all_tests this test failed completely: $test.exe"

  set -- $ARGS

  if [ "$CODE" == "143" ];
  then echo "$test.exe" $1 "$NUMTHREADS"  "TIMEOUT TIMEOUT TIMEOUT" >> $RESULTS
  elif [ "$CODE" != "0" ] ;
  then echo "$test.exe" $1 "$NUMTHREADS"  "ERR ERR ERR" >> $RESULTS
  else 
       echo "$test.exe" $1 "$NUMTHREADS"  "$times" >> $RESULTS
  fi
}


echo "Running all tests, for THREADSETTINGS in {$THREADSETTINGS}"
echo

# Hygiene:
make clean
echo; echo;

#====================================================================================================

function run_benchmark() 
{
  set -- $line
  test=$1; shift

  if [ "$SHORTRUN" == "" ];
  # If we're in SHORTRUN mode we run each executable with no
  # arguments causing it to go to its default (small) problem size.
  then ARGS=$*
  else ARGS=
  fi

  echo "================================================================================"
  echo "                   Running Test: $test.exe $ARGS                        "
  echo "================================================================================"

  echo "# *** Config [$cnt ..], testing with command/args: $test.exe $ARGS " >> $RESULTS


  for NUMTHREADS in $THREADSETTINGS; do
    runit
    echo >> $RESULTS;
  done # threads

  export NUMTHREADS=0

 echo >> $RESULTS;
 echo >> $RESULTS;
}

# Read $line and do the benchmark with ntimes_binsearch.sh
function run_binsearch_benchmark() 
{
   NTIMES=./ntimes_binsearch.sh
   run_benchmark
   NTIMES=UNSET
}

# Read $line and do the benchmark with ntimes_minmedmax
function run_normal_benchmark() 
{
   NTIMES=./ntimes_minmedmax
   run_benchmark
   NTIMES=UNSET
}


#====================================================================================================


if [ "$BENCHLIST" == "" ];
then BENCHLIST="./benchlist.txt"
fi

echo "Reading benchmarks from $BENCHLIST ..."
cat $BENCHLIST | grep -v "\#" | 
while read line
do
  if [ "$line" == "" ]; then continue; fi
  echo 
  echo RUNNING BENCH:  $line
  run_normal_benchmark
done

echo "Finished with all test configurations."
