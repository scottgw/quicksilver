CC_OPTS=-O3 -Wall -g -pthread -std=gnu99

main: main.o executor.o notifier.o processor.o list.o
	${CC} ${CC_OPTS} $^ -o $@

%.o: %.c
	${CC} ${CC_OPTS} $< -c 

clean:
	@rm -f *.o main
