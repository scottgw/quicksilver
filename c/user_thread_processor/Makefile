CC_OPTS=-O3 -Wall -g -pthread -std=gnu99

main: main.o executor.o notifier.o processor.o work_list.o
	${CC} ${CC_OPTS}  $^ -o $@ `pkg-config glib-2.0 --libs`

%.o: %.c
	${CC} ${CC_OPTS} `pkg-config glib-2.0 --cflags` $< -c 

clean:
	@rm -f *.o main
