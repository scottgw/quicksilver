MODULES=executor list task processor main notifier \
        sync_ops task_mutex task_condition
SRCS=$(MODULES:=.c)
OBJS=$(MODULES:=.o)

INCLUDES=-I./liblfds6.1.1/liblfds611/inc
CFLAGS=-O3 -Wall -std=gnu99 -lrt -pthread -g $(INCLUDES) -L./ -fPIC

libqs.so: $(OBJS) liblfds.so
	${CC} $(OBJS) -L./ -llfds -shared -o libqs.so

main: main.c liblfds.so libqs.so
	${CC}  -llfds -lqs  ${CFLAGS} main.c -o $@

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
	@rm -f *.o main sink_test .depend libqs.so liblfds.so
	$(MAKE) -f makefile.linux -C liblfds6.1.1/liblfds611/ clean

# to build the concurrent data strutures library
liblfds.so: liblfds6.1.1/liblfds611/bin/liblfds611.so
	cp $< $@

liblfds6.1.1/liblfds611/bin/liblfds611.so: liblfds6.1.1/liblfds611/makefile.linux
	$(MAKE) -f makefile.linux -C liblfds6.1.1/liblfds611/ sodbg
