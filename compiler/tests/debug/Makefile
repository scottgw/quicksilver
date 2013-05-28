main: debug.o prodcons.o debug.bc debug.ll
	gcc debug.o prodcons.o -o main  `pkg-config --libs --cflags libqs` -O3

clean:
	@rm -f debug.bc debug.o prodcons.o prodcons.s prodcons.bc main

debug.ll: debug.bc
	llvm-dis debug.bc -o debug.ll

debug.bc: debug.c
	clang -emit-llvm -std=c99 `pkg-config --libs --cflags libqs` debug.c -c -pthread -o debug.bc -O3

debug.o: debug.c
	gcc -std=c99 `pkg-config --libs --cflags libqs` debug.c -c -pthread -O3

prodcons.o: prodcons.s
	gcc prodcons.s -c

prodcons.s: prodcons.bc
	llc prodcons.bc -o prodcons.s

prodcons.bc: prod_cons_test.ll
	llvm-as prod_cons_test.ll -o prodcons.bc
