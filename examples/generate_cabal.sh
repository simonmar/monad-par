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
executable nbody
  main-is:           src/nbody.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras,
                     array
EOF
boilerplate 


cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable queens
  main-is:           src/queens.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras, 
                     parallel
EOF
boilerplate 



cat >> foo.cabal <<EOF
--------------------------------------------------------------------------------
executable blackscholes
  main-is:           src/blackscholes.hs
  build-depends:     $COMMON_DEPS , 
                     abstract-par, monad-par-extras, 
                     array, parallel
EOF
boilerplate 




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