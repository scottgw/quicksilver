MODULES=executor list task processor main notifier sync_ops
SRCS=$(MODULES:=.c)
OBJS=$(MODULES:=.o)

INCLUDES=-I./liblfds6.1.1/liblfds611/inc
CFLAGS=-O0 -Wall -std=gnu99 -lrt -pthread -g $(INCLUDES)

main: $(OBJS) liblfds.a
	${CC} $^ -o $@ ${CFLAGS}

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CC) -MM $^ $(CFLAGS) > ./.depend;

include .depend

sink_test: sink_test.o sink_queue.o sink_queue.h
	${CC} $^ -o $@ $(CFLAGS)

%.o: %.c
	${CC} $< -c ${CFLAGS}

clean:
	@rm -f *.o main sink_test .depend

# to build the concurrent data strutures library
liblfds.a: liblfds6.1.1/liblfds611/bin/liblfds611.a
	cp $< $@

liblfds6.1.1/liblfds611/bin/liblfds611.a: liblfds6.1.1/liblfds611/makefile.linux
	$(MAKE) -f makefile.linux -C liblfds6.1.1/liblfds611/
