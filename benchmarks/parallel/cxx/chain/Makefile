main: main.o randmat.o thresh.o winnow.o outer.o product.o
	g++ -ltbb $^ -o $@

clean:
	@rm -f *.o main

CXXFLAGS += -std=c++11 -O2 -ltbb
