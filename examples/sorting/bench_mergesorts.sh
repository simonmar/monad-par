#!/bin/bash

function myresfile {
    date=`date +"%Y.%m.%d"`
    secs=`date +"%s"`
    #base=`basename $1`
    base="mergesort"
    mkdir -p ./runs/
    file=./runs/results-"$base"_"$date"_"$secs".dat
    echo > $file
    echo $file
}

RESULTS=`myresfile`
#CILKSEQRESULTS=`myresfile ./mergesort.cilk_seq.exe`
#CILKPARRESULTS=`myresfile ./mergesort.cilk_par.exe`
#HASKSEQRESULTS=`myresfile ./mergesort.haskell_seq.exe`

HEADER="# SORTSIZE   GpuMode CpuMode HaskellThreads CilkThreads SeqsortThreshold GpuLowerBound GpuUpperBound  Time1Min Time2Med Time3Max Prod1 Prod2 Prod3"

echo $HEADER > $RESULTS

TRIALS=9
NUMTOSORT=24
THREADCOUNTS="1 2 3 4"
GPUMODES="cpu static static_blocking dynamic"
CPUTHRESHES="1024 2048 4096 8192 16384 65536 262144 1048576"
GPULOWERS="14 15 16 17 18 19 20 21 22"
GPUUPPER=22

# Total number of configurations (I think): (4*3*8) + (4*8*9) + (4*8)
CONFIGS=416
CURCONFIG=1

ntimes="../ntimes_minmedmax"
RTS="+RTS -s -qa -A1M -N"

lock_for_experiments

for threads in $THREADCOUNTS; do
  for gpumode in $GPUMODES; do
    for cputhresh in $CPUTHRESHES; do
      if [ $gpumode == 'dynamic' ]; then
        for gpulower in $GPULOWERS; do

          # Keep track of configurations for progress
          echo -n -e "\n"
          echo "CONFIGURATION $CURCONFIG of $CONFIGS"
          echo "------------------------------------------------------------"
          CURCONFIG=$(($CURCONFIG+1))

          # CILK_SEQ with GPU bounds
          echo -n "16777216 $gpumode cilk_seq $threads 1 $cputhresh $gpulower $GPUUPPER" >> $RESULTS
          cmd="$ntimes $TRIALS ./mergesort.cilk_seq.exe $gpumode \
                $NUMTOSORT $cputhresh $gpulower $GPUUPPER $RTS$threads"
          $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
             | tr -d '\n' >> $RESULTS
          echo -n -e "\n" >> $RESULTS


          # Keep track of configurations for progress
          echo -n -e "\n"
          echo "CONFIGURATION $CURCONFIG of $CONFIGS"
          echo "------------------------------------------------------------"
          CURCONFIG=$(($CURCONFIG+1))

          # CILK_PAR with GPU bounds
          echo -n "16777216 $gpumode cilk_par $threads 1 $cputhresh $gpulower $GPUUPPER" >> $RESULTS
          cmd="$ntimes $TRIALS ./mergesort.cilk_par.exe $gpumode \
                 $NUMTOSORT $cputhresh $gpulower $GPUUPPER $RTS$threads"
          $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
            | tr -d '\n' >> $RESULTS
          echo -n -e "\n" >> $RESULTS
        done
      else
        # Keep track of configurations for progress
          echo -n -e "\n"
        echo "CONFIGURATION $CURCONFIG of $CONFIGS"
        echo "------------------------------------------------------------"
        CURCONFIG=$(($CURCONFIG+1))

        # CILK_SEQ without GPU bounds
        echo -n "16777216 $gpumode cilk_seq $threads 1 $cputhresh _ _" >> $RESULTS
        cmd="$ntimes $TRIALS ./mergesort.cilk_seq.exe $gpumode \
               $NUMTOSORT $cputhresh $RTS$threads"
        $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
            | tr -d '\n' >> $RESULTS
        echo -n -e "\n" >> $RESULTS


        # Keep track of configurations for progress
        echo -n -e "\n"
        echo "CONFIGURATION $CURCONFIG of $CONFIGS"
        echo "------------------------------------------------------------"
        CURCONFIG=$(($CURCONFIG+1))
  
        # CILK_PAR without GPU bounds
        echo -n "16777216 $gpumode cilk_par $threads 1 $cputhresh _ _" >> $RESULTS
        cmd="$ntimes $TRIALS ./mergesort.cilk_par.exe $gpumode \
               $NUMTOSORT $cputhresh $RTS$threads"
        $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
            | tr -d '\n' >> $RESULTS
        echo -n -e "\n" >> $RESULTS
      fi
    done
  done
done

# HASKELL_SEQ runs (no GPU, no GPU bounds)
for threads in $THREADCOUNTS; do
  for cputhresh in $CPUTHRESHES; do
    # Keep track of configurations for progress
    echo -n -e "\n"
    echo "CONFIGURATION $CURCONFIG of $CONFIGS"
    echo "------------------------------------------------------------"
    CURCONFIG=$(($CURCONFIG+1))

    echo -n "16777216 cpu haskell_seq $threads _ $cputhresh _ _" >> $RESULTS
    cmd="$ntimes $TRIALS ./mergesort.haskell_seq.exe "cpu" $NUMTOSORT \
           $cputhresh $RTS$threads"
    $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
        | tr -d '\n' >> $RESULTS
    echo -n -e "\n" >> $RESULTS 
  done
done

unlock_for_experiments
