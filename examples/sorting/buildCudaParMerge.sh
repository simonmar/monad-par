#!/bin/bash

ghc --make -O2 cudaParMerge.hs -i../.. ../../dist/build/cbits/pin.o /nobackup/afoltzer/NVIDIA_GPU_Computing_SDK/C/src/mergeSort/obj/x86_64/release/mergeSort.cu.o -lcudart -rtsopts -threaded 
