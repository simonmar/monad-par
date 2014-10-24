

REPORTS= -o report_monadpar.html --csv=report_monadpar.csv

bench:
	cabal install --bindir=. --program-suffix=.exe --ghc-options="-ddump-simpl -ddump-to-file"
	./bench-monad-par.exe -v2 --regress=allocated:iters $(REPORTS) +RTS -T -s -RTS
