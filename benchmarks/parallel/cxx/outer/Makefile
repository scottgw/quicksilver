.PHONY: test

main: main.cc

test:
	./$< < $<.in > main.out --threads 4
	diff main.out main.gold
	./$< < $<.in --threads 4
	./$< < $<.in --threads 4 --is_bench

clean:
	@rm -f main

CXXFLAGS+=-std=c++11 -O2 -ltbb -lpthread -lrt
