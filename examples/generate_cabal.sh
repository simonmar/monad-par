#!/bin/bash

# This is a LAME way of eliminating boilerplate in our .cabal file.

cat > foo.cabal <<EOF
name:                monad-par-examples
version:             0.3
build-type:          Simple
cabal-version:       >=1.8
EOF

COMMON_DEPS="base == 4.*, deepseq == 1.3.*, vector == 0.9.* "

function boilerplate() {
cat >> foo.cabal <<EOF
  if flag(trace)
     build-depends:   monad-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Scheds.Trace

  if flag(direct)
     build-depends:   monad-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Scheds.Direct

  if flag(sparks)
     build-depends:   monad-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Scheds.Sparks

  if flag(meta-smp)
     build-depends:   meta-par
     cpp-options:     -DPARSCHED=Control.Monad.Par.Meta.SMP

  if !(flag(trace) || flag(direct) || flag(sparks) || flag(meta-smp))
     build-depends:   monad-par
     -- uses Control.Monad.Par by default

  ghc-options:       -O2 -threaded -rtsopts -with-rtsopts=-N
  extensions:        CPP
EOF
}


cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable nbody.exe
  main-is:           src/nbody.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     array
  if flag(sparks)
     buildable:      False
EOF
boilerplate 


cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable queens.exe
  main-is:           src/queens.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras, 
                     parallel
EOF
boilerplate 



cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable blackscholes.exe
  main-is:           src/blackscholes.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras, 
                     array, parallel
EOF
boilerplate 




cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable coins.exe
  main-is:           src/coins.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     parallel
EOF
boilerplate 




cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable parfib_monad.exe
  main-is:           src/parfib_monad.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 




cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable parfib_pseq.exe
  main-is:           src/parfib_pseq.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 



cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable randomGen.exe
  main-is:           src/randomGen.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     random, transformers
EOF
boilerplate 





cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable primes.exe
  main-is:           src/primes.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 





cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable cholesky.exe
  main-is:           src/cholesky/cholesky.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     array, bytestring, containers, time, unix
  if flag(sparks)
     buildable:      False
EOF
boilerplate 



cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable sumeuler.exe
  main-is:           sumeuler.hs
  hs-source-dirs:    src/sumeuler
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 




cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable MatMult.exe
  main-is:           MatMult.hs
  hs-source-dirs:    src/matmult
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras
EOF
boilerplate 




cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable minimax.exe
  main-is:           Main.hs
  hs-source-dirs:    src/minimax
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     parallel, random
EOF
boilerplate 



cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable partree.exe
  main-is:           partree.hs
  hs-source-dirs:    src/partree
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     parallel
EOF
boilerplate




cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable kmeans.exe
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
# cat >> foo.cabal <<EOF
# --------------------------------------------------------------------------------
# executable simple1_measureSrc.exe
#   main-is:           simple1_measureSrc.hs
#   hs-source-dirs:    src/stream
#   build-depends:     $COMMON_DEPS , 
#                      abstract-par, monad-par-extras
# EOF
# boilerplate 




cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------  

-- BUILDABLE??


flag trace
  default:           False

flag direct
  default:           False

flag sparks
  default:           False

flag meta-smp
  default:           False

EOF