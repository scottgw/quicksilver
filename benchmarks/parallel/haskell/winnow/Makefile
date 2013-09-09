main: Main.hs
	ghc --make Main.hs -o main -threaded -O2 -Odph -rtsopts \
	-fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 \
	-fllvm -optlo-O3 -eventlog

clean:
	@rm -f main Main.o Main.hi
