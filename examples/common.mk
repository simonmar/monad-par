
# RRN: allow setting it via the command line.  (I don't have a ghc-testing2 ;-). )
ifeq (,$(GHC))
  # GHC = ghc
  GHC = ghc-testing2
  # GHC = ghc-stable-nightly2
endif
