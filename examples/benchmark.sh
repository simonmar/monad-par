#!/bin/bash

# Runs benchmarks.  Simon Marlow has his own setup.  This is just temporary.

# ---------------------------------------------------------------------------
# Usage: [set env vars] ./benchmark.sh

# Call it with environment variable SHORTRUN=1 to get a shorter run for
# testing rather than benchmarking.

# Call it with THREADSETTINGS="1 2 4" to run with # threads = 1, 2, or 4.

# Call it with KEEPGOING=1 to keep going after the first error.

# Call it with TRIALS=N to control the number of times each benchmark is run.
# ---------------------------------------------------------------------------


# Settings:
# ----------------------------------------

if [ "$THREADSETTINGS" == "" ] 
then THREADSETTINGS="4"
#then THREADSETTINGS="0 1 2 3 4"
fi

if [ "$GHC" == "" ];  then GHC=ghc; fi

# HACK: with all the intermachine syncing and different version control systems I run into permissions problems sometimes.
chmod +x ./ntime* ./*.sh


# Where to put the timing results:
RESULTS=results.dat
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
       echo "Detected the number of CPUs on the machine to be $MAXTHREADS"
  elif [ `uname` == "Darwin" ];
  then MAXTHREADS=`sysctl -n hw.ncpu`
  else MAXTHREADS=2
  fi 

GHC_FLAGS="$GHC_FLAGS -O2 -rtsopts"
GHC_RTS="$GHC_RTS -qa"

# ================================================================================
echo "# TestName Variant NumThreads   MinTime MedianTime MaxTime" > $RESULTS
echo "# "`date`     >> $RESULTS
echo "# "`uname -a` >> $RESULTS
echo "# "`$GHC -V`  >> $RESULTS
echo "# "
echo "# Running each test for $TRIALS trials."          >> $RESULTS
echo "#  ... with default compiler options: $GHC_FLAGS" >> $RESULTS
echo "#  ... with default runtime options: $GHC_RTS"    >> $RESULTS

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

  # We compile the test case using runcnc:
  $GHC -i../  $FLAGS "$test".hs

  check_error $? "ERROR: compilation failed."

  echo "Executing $NTIMES $TRIALS $test.exe $ARGS +RTS $RTS -RTS "
  if [ "$LONGRUN" == "" ]; then export HIDEOUTPUT=1; fi

  # One option woud be dynamic feedback where if the first one
  # takes a long time we don't bother doing more trials.

  times=`$NTIMES "$TRIALS" ./$test.exe $ARGS +RTS $RTS -RTS`
  CODE=$?

  echo " >>> MIN/MEDIAN/MAX TIMES $times"

  check_error $CODE "ERROR: run_all_tests this test failed completely: $test.exe"

  if [ "$CODE" == "143" ];
  then echo "$test.exe" _ "$NUMTHREADS" "TIMEOUT TIMEOUT TIMEOUT" >> $RESULTS
  elif [ "$CODE" != "0" ] ;
  then echo "$test.exe" _ "$NUMTHREADS" "ERR ERR ERR" >> $RESULTS
  else 
       echo "$test.exe" _ "$NUMTHREADS" "$times" >> $RESULTS
  fi
}


echo "Running all tests, for THREADSETTINGS in {$THREADSETTINGS}"
echo

# Hygiene:
make clean

#====================================================================================================

function run_benchmark() 
{
  set -- $line
  test=$1; shift

  if [ "$LONGRUN" == "" ];
  # If we're not in LONGRUN mode we run each executable with no
  # arguments causing it to go to its default problem size.
  then ARGS=
  else ARGS=$*
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
  echo RUNNING BENCH:  $line
  run_normal_benchmark
done

echo "Finished with all test configurations."
