#!/bin/bash

# This is a LAME way of eliminating boilerplate in our .cabal files.

COMMON_DEPS="base == 4.*, deepseq == 1.3.*, vector >= 0.10, abstract-par, monad-par-extras"
REGULAR_BENCHS="mandel/mandel sorting/mergesort"
PREFIX=monad-par-test

function executable_header() {
cat >> $CABALFILE <<EOF
--------------------------------------------------------------------------------
executable $PREFIX-$NAME
  main-is:           $NAME.hs

  if  !(flag(trace)  || flag(direct)   || flag(contfree)\
     || flag(sparks) || flag(meta-smp) || flag(meta-numa))
    buildable:       False

  build-depends:     $COMMON_DEPS
EOF
}

function header() {
CABALFILE=$DIR/generated.cabal
cat > $CABALFILE <<EOF
name:                $NAME
version:             0.3.9
build-type:          Simple
cabal-version:       >=1.8

flag trace
  default:           False

flag direct
  default:           False

flag contfree
  default:           False

flag sparks
  default:           False

flag meta-smp
  default:           False

flag meta-numa
  default:           False

EOF
executable_header
}

function boilerplate() {
cat >> $CABALFILE <<EOF

  -- Only one of these flags should be turned on at once:
  if flag(trace)
     build-depends:   monad-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Scheds.Trace

  if flag(direct)
     build-depends:   monad-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Scheds.Direct

  if flag(contfree)
     build-depends:   monad-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Scheds.ContFree

  if flag(sparks)
     build-depends:   monad-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Scheds.Sparks

  if flag(meta-smp)
     build-depends:   meta-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Meta.SMP

  if flag(meta-numa)
     build-depends:   meta-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Meta.NUMAOnly

  if  !(flag(trace)  || flag(direct)   || flag(contfree)\
     || flag(sparks) || flag(meta-smp) || flag(meta-numa))
     build-depends:   monad-par
     -- uses Control.Monad.Par by default

  ghc-options:       -O2 -threaded -rtsopts -with-rtsopts=-N
  extensions:        CPP
EOF
}

#--------------------------------------------------------------------------------

NAME=mandel
DIR=src/$NAME
header 
boilerplate 


NAME=mergesort
DIR=src/sorting
header
cat >> $CABALFILE <<EOF
  build-depends:     mwc-random,
                     QuickCheck, split >= 0.2, time, transformers, vector, vector-algorithms
EOF
boilerplate 


NAME=nbody
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  build-depends:    array
  if flag(sparks)
     buildable:      False
EOF
boilerplate 


NAME=queens
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  build-depends:    parallel
EOF
boilerplate 


NAME=blackscholes
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  build-depends:    array, parallel
EOF
boilerplate 


NAME=coins
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  build-depends:    parallel
EOF
boilerplate 

NAME=parfib-monad
DIR=src/parfib/
header
boilerplate 

# [2013.05.21] Not well setup for sharing yet:

# NAME=parfib_pseq
# DIR=src/$NAME
# header
# cat >> $CABALFILE <<EOF
#   build-depends:     abstract-par, monad-par-extras
# EOF
# boilerplate 


NAME=randomGen
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  build-depends:  random, transformers
EOF
boilerplate 


NAME=primes
DIR=src/$NAME
header
boilerplate 


NAME=cholesky
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  -- disabling due to dependency hell with deepseq and containers
  buildable: False
  build-depends:     
                     array, bytestring, containers, time, unix
                     , deepseq > 1.2
  if flag(sparks)
     buildable:      False
EOF
boilerplate 


NAME=sumeuler
DIR=src/$NAME
header
boilerplate 


NAME=matmult
DIR=src/$NAME
header
boilerplate 


NAME=minimax
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  main-is:           Main.hs
  build-depends:     parallel, random
EOF
boilerplate 


NAME=partree
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  build-depends:     parallel
EOF
boilerplate


NAME=kmeans
DIR=src/$NAME
header
cat >> $CABALFILE <<EOF
  build-depends:     array, bytestring, cereal, cereal-vector >= 0.2.0.0, mwc-random, 
                     parallel, time, transformers, vector
EOF
boilerplate 


# # Disabled until Control.Monad.Par.Stream is imported
# #
# # cat >> monad-par-examples.cabal <<EOF
# # --------------------------------------------------------------------------------
# # executable simple1_measureSrc
# #   main-is:           simple1_measureSrc.hs
# #   hs-source-dirs:    src/stream
# #   build-depends:     $COMMON_DEPS , 
# #                      abstract-par, monad-par-extras
# # EOF
# # boilerplate 


