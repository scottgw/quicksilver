.PHONY: test

main: main.cc

test: main
	echo "10 9 8" | ./main --threads 4
	echo "1 4 8" | ./main --threads 4
	echo "1 5 8" | ./main --threads 4
	echo "2 5 8" | ./main --threads 4
	echo "2 5 9" | ./main --threads 4
	echo "2 5 9" | ./main --is_bench --threads 4

clean:
	@rm -f main

CXXFLAGS+=-std=c++11 -O2 -ltbb -lpthread -lrt
