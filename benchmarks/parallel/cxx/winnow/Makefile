.PHONY: test

main: main.cc

test: main
	./$< --threads 4 < $<.in > main.out
	diff main.out main.gold
	./$< --threads 4 < $<2.in > main2.out
	diff main2.out main2.gold
	./$< --threads 4 < $<3.in > main3.out
	diff main3.out main3.gold
	./$< --threads 4 < $<4.in > main4.out
	diff main4.out main4.gold
	./$< --threads 4 < $<.in
	./$< --threads 4 --is_bench < $<.in

CXXFLAGS+=-std=c++11 -O2 -ltbb -lpthread -lrt -lstdc++
