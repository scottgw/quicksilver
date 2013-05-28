main: debug.o prodcons.o
	gcc debug.o prodcons.o -o main  `pkg-config --libs --cflags libqs`

debug.bc: debug.c
	clang -emit-llvm -std=c99 `pkg-config --libs --cflags libqs` debug.c -c -pthread -o debug.bc -O3

debug.o: debug.c
	gcc -std=c99 `pkg-config --libs --cflags libqs` debug.c -c -pthread

prodcons.o: prodcons.s
	gcc prodcons.s -c

prodcons.s: prodcons.bc
	llc prodcons.bc -o prodcons.s

prodcons.bc: prod_cons_test.ll
	llvm-as prod_cons_test.ll -o prodcons.bc
