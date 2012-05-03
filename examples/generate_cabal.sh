#!/bin/bash

# This is a LAME way of eliminating boilerplate in our .cabal file.

cat > monad-par-examples.cabal <<EOF
name:                monad-par-examples
version:             0.3
build-type:          Simple
cabal-version:       >=1.8
EOF

COMMON_DEPS="base == 4.*, deepseq == 1.3.*, vector == 0.9.* "

function boilerplate() {
cat >> monad-par-examples.cabal <<EOF
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


cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable mandel
  main-is:           src/mandel.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 





cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable mergesort
  main-is:           src/sorting/mergesort.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     mwc-random,
                     QuickCheck, split, time, transformers, vector, vector-algorithms
  if  !(flag(trace)  || flag(direct)   || flag(contfree)\
     || flag(sparks) || flag(meta-smp) || flag(meta-numa))
    buildable:       False
EOF
boilerplate 




cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable nbody
  main-is:           src/nbody.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     array
  if flag(sparks)
     buildable:      False
EOF
boilerplate 


cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable queens
  main-is:           src/queens.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras, 
                     parallel
EOF
boilerplate 



cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable blackscholes
  main-is:           src/blackscholes.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras, 
                     array, parallel
EOF
boilerplate 




cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable coins
  main-is:           src/coins.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     parallel
EOF
boilerplate 




cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable parfib_monad
  main-is:           src/parfib_monad.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 




cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable parfib_pseq
  main-is:           src/parfib_pseq.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 



cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable randomGen
  main-is:           src/randomGen.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     random, transformers
EOF
boilerplate 





cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable primes
  main-is:           src/primes.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 





cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable cholesky
  -- disabling due to dependency hell with deepseq and containers
  buildable: False
  main-is:           src/cholesky/cholesky.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     array, bytestring, containers, time, unix
                     , deepseq > 1.2
  if flag(sparks)
     buildable:      False
EOF
boilerplate 



cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable sumeuler
  main-is:           sumeuler.hs
  hs-source-dirs:    src/sumeuler
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 




cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable MatMult
  main-is:           MatMult.hs
  hs-source-dirs:    src/matmult
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 




cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable minimax
  main-is:           Main.hs
  hs-source-dirs:    src/minimax
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     parallel, random
EOF
boilerplate 



cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable partree
  main-is:           partree.hs
  hs-source-dirs:    src/partree
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     parallel
EOF
boilerplate




cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------
executable kmeans
  main-is:           kmeans.hs
  hs-source-dirs:    src/kmeans
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     array, bytestring, cereal, cereal-vector, mwc-random, 
                     parallel, time, transformers, vector
EOF
boilerplate 




# Disabled until Control.Monad.Par.Stream is imported
#
# cat >> monad-par-examples.cabal <<EOF
# --------------------------------------------------------------------------------
# executable simple1_measureSrc
#   main-is:           simple1_measureSrc.hs
#   hs-source-dirs:    src/stream
#   build-depends:     $COMMON_DEPS , 
#                      abstract-par, monad-par-extras
# EOF
# boilerplate 




cat >> monad-par-examples.cabal <<EOF
--------------------------------------------------------------------------------  

-- BUILDABLE??


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