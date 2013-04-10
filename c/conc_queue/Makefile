# without -mcx16 gcc doesn't know it can use the cmpxchg16b
# to do 16-byte CAS operations, which the concurrent queue needs.
CC_OPTS=-O0 -Wall -g -pthread -std=gnu99 -mcx16

test: test.o conc_queue.o
	${CC} ${CC_OPTS} $^ -o $@

%.o: %.c
	${CC} ${CC_OPTS} $< -c 

clean:
	@rm -f *.o main
