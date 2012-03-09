#!/bin/bash

## TODO: Record outputs somewhere in the form sent in e-mail

function myresfile {
    date=`date +"%Y.%m.%d"`
    secs=`date +"%s"`
    base=`basename $1`
    mkdir -p ./runs/
    file=./runs/results-"$base"_"$date"_"$secs".dat
    echo > $file
    echo $file
}

CILKSEQRESULTS=`myresfile ./mergesort.cilk_seq.exe`
CILKPARRESULTS=`myresfile ./mergesort.cilk_par.exe`
HASKSEQRESULTS=`myresfile ./mergesort.haskell_seq.exe`

HEADER="# SORTSIZE   GpuMode CpuMode HaskellThreads CilkThreads SeqsortThreshold GpuLowerBound GpuUpperBound  Time1Min Time2Med Time3Max Prod1 Prod2 Prod3"

echo $HEADER > $CILKSEQRESULTS
echo $HEADER > $CILKPARRESULTS
echo $HEADER > $HASKSEQRESULTS

TRIALS=3
NUMTOSORT=24
THREADCOUNTS="1 2 3 4"
GPUMODES="cpu static static_blocking dynamic"
CPUTHRESHES="1024 2048 4096 8192 16384 65536 262144 1048576"
GPULOWERS="14 15 16 17 18 19 20 21 22"
GPUUPPER=22

ntimes="../ntimes_minmedmax"
RTS="+RTS -s -qa -A1M -N"

#lock_for_experiments

for threads in $THREADCOUNTS; do
  for gpumode in $GPUMODES; do
    for cputhresh in $CPUTHRESHES; do
      if [ $gpumode == 'dynamic' ]; then
        for gpulower in $GPULOWERS; do
          # CILK_SEQ with GPU bounds
          echo -n "16777216 $gpumode cilk_seq $threads 1 $cputhresh $gpulower $GPUUPPER" >> $CILKSEQRESULTS
          cmd="$ntimes $TRIALS ./mergesort.cilk_seq.exe $gpumode \
                $NUMTOSORT $cputhresh $gpulower $GPUUPPER $RTS$threads"
          $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
             | tr '\n' ' ' >> $CILKSEQRESULTS
          echo -e "\n" >> $CILKSEQRESULTS

          # CILK_PAR with GPU bounds
          echo -n "16777216 $gpumode cilk_par $threads 1 $cputhresh $gpulower $GPUUPPER" >> $CILKPARRESULTS
          cmd="$ntimes $TRIALS ./mergesort.cilk_par.exe $gpumode \
                 $NUMTOSORT $cputhresh $gpulower $GPUUPPER $RTS$threads"
          $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
            | tr '\n' ' ' >> $CILKPARRESULTS
          echo -e "\n" >> $CILKPARRESULTS
        done
      else
        # CILK_SEQ without GPU bounds
        echo -n "16777216 $gpumode cilk_seq $threads 1 $cputhresh _ _" >> $CILKSEQRESULTS
        cmd="$ntimes $TRIALS ./mergesort.cilk_seq.exe $gpumode \
               $NUMTOSORT $cputhresh $RTS$threads"
        $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
            | tr '\n' ' ' >> $CILKSEQRESULTS
        echo -e "\n" >> $CILKSEQRESULTS

        # CILK_PAR without GPU bounds
        echo -n "16777216 $gpumode cilk_par $threads 1 $cputhresh _ _" >> $CILKPARRESULTS
        cmd="$ntimes $TRIALS ./mergesort.cilk_par.exe $gpumode \
               $NUMTOSORT $cputhresh $RTS$threads"
        $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
            | tr '\n' ' ' >> $CILKPARRESULTS
        echo -e "\n" >> $CILKPARRESULTS
      fi
    done
  done
done

# HASKELL_SEQ runs (no GPU, no GPU bounds)
for threads in $THREADCOUNTS; do
  for cputhresh in $CPUTHRESHES; do
    echo -n "16777216 cpu haskell_seq $threads _ $cputhresh _ _" >> $HASKSEQRESULTS
    cmd="$ntimes $TRIALS ./mergesort.haskell_seq.exe "cpu" $NUMTOSORT \
           $cputhresh $RTS$threads"
    $cmd | tail -n2 | sed 's/REALTIME//' | sed 's/PRODUCTIVITY//' \
        | tr '\n' ' ' >> $HASKSEQRESULTS
    echo -e "\n" >> $HASKSEQRESULTS 
  done
done

#unlock_for_experiments
