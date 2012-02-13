#!/bin/bash

N=$1
shift

set -e

ghc -i../.. --make parfib_dist.hs -O2 -threaded -rtsopts -fforce-recomp $@
time ./parfib_dist monad $N +RTS -N2 -RTS
