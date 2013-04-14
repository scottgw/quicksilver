MODULES=executor list task processor main notifier
SRCS=$(MODULES:=.c)
OBJS=$(MODULES:=.o)

CCFLAGS=-O3 -Wall -std=gnu99 -pthread -g

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CC) $(CFLAGS) -MM $^ > ./.depend;

include .depend

main: $(OBJS) liblfds.a
	${CC} ${CCFLAGS} $^ -o $@

sink_test: sink_test.o sink_queue.o sink_queue.h
	${CC} ${CCFLAGS} $^ -o $@

%.o: %.c
	${CC} ${CCFLAGS} $< -c

clean:
	@rm -f *.o main sink_test .depend

# to build the concurrent data strutures library
liblfds.a: liblfds6.1.1/liblfds611/bin/liblfds611.a
	cp $< $@

liblfds6.1.1/liblfds611/bin/liblfds611.a: liblfds6.1.1/liblfds611/makefile.linux
	$(MAKE) -f makefile.linux -C liblfds6.1.1/liblfds611/
