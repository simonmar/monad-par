GHC = ghc

queens : queens.hs ../Control/Monad/Par.hs
	$(GHC) -O2 --make -i.. $< -o $@ -threaded -rtsopts

mandel : mandel.hs ../Control/Monad/Par.hs
	$(GHC) -O2 --make -i.. $< -o $@ -threaded -rtsopts
