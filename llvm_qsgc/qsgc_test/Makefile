.PHONY: all
.SUFFIXES: .o .s .cpp
LIBS=-lunwind-x86_64 -lunwind
CXX_FLAGS=-std=c++11 -g

all: main

sample.s: sample.ll
	llvm-as < $< | llc -load=/home/scott/tmp/llvm-3.3.src/Release+Asserts/lib/QsGC.so -o $@

main: main.o stack_map.o sample.o
	g++ ${LIBS} $^ -o $@

%.o: %.cpp
	g++ ${CXX_FLAGS} -c $<

sample.o: sample.s
	gcc ${OPTS} $< -c

clean:
	@rm -f *.o main *.s
