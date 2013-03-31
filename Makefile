main: main.o private_queue.o sink_queue.o
	${CC} -O3 -pthread $^ -o $@

%.o: %.c
	${CC} -O3 $< -c -Wall -pthread -g -std=c99

clean:
	@rm -f *.o main
