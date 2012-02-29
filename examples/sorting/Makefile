GHC=ghc
CC=icc

all: cilkWrap 

cilksort: cilksort.o
	$(CC) -Wall $^ -o $@

cilksort-nomain.o: cilksort.c
	$(CC) -Wall -c $^ -o $@

cilksort.o: cilksort.c
	$(CC) -Wall -DUSE_MAIN=1 -c $^ -o $@

cilkWrap: cilkWrap.hs cilksort-nomain.o
	$(GHC) --make $^ -lm -lcilkrts -lirc -limf -o $@

clean:
	rm -f cilkWrap cilksort *.hi *.o

.PHONY: clean

