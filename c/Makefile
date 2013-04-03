main: main.o private_queue.o private_queue.h
	${CC} -O3 -pthread $^ -o $@

sink_test: sink_test.o sink_queue.o sink_queue.h
	${CC} -O3 -pthread $^ -o $@

%.o: %.c
	${CC} -O3 $< -c -Wall -pthread -g -std=c99

clean:
	@rm -f *.o main sink_test
