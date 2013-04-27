MODULES=executor list task processor notifier \
        sync_ops task_mutex task_condition ctx bounded_queue \
	closure
SRCS=$(MODULES:=.c)
OBJS=$(MODULES:=.o)

include common.mk
INCLUDES=-I./liblfds6.1.1/liblfds611/inc
CFLAGS=-O3 -Wall -std=gnu99 -lrt -pthread -g $(INCLUDES) -lqs -llfds -fPIC -U_FORTIFY_SOURCE

libqs.so: $(OBJS) liblfds.so
	$(CC) $(OBJS) -L./ -lrt -llfds -shared -o libqs.so

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CC) -MM $^ $(CFLAGS) > ./.depend;

include .depend

clean:
	@rm -f *.o main .depend libqs.so liblfds.so
	$(MAKE) -f makefile.linux -C liblfds6.1.1/liblfds611/ clean

# to build the concurrent data strutures library
liblfds.so: liblfds6.1.1/liblfds611/bin/liblfds611.so
	cp $< $@

liblfds6.1.1/liblfds611/bin/liblfds611.so: liblfds6.1.1/liblfds611/makefile.linux
	$(MAKE) -f makefile.linux -C liblfds6.1.1/liblfds611/ sodbg
