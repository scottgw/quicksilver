MODULES=executor list task processor notifier \
        sync_ops task_mutex task_condition ctx bounded_queue \
	closure private_queue
CXX_MODULES=queue_impl
SRCS=$(MODULES:=.c)
CXX_SRCS=$(CXX_MODULES:=.cpp)
OBJS=$(MODULES:=.o) $(CXX_MODULES:=.o)
LIBS=rt qs lfds :libffi.so.6 tbb
LLIBS=$(addprefix -l, $(LIBS))

include common.mk
INCLUDES=-I./liblfds6.1.1/liblfds611/inc
FLAGS=-O3 -Wall -pthread -g $(INCLUDES) $(LLIBS) -fPIC -U_FORTIFY_SOURCE
CFLAGS=-std=gnu99 $(FLAGS)
CXXFLAGS=$(FLAGS)

libqs.so: $(OBJS) liblfds.so
	$(CC) $(OBJS) -L./ -lrt -llfds -l:libffi.so.6 -shared -o libqs.so

depend: .depend

.depend: $(SRCS) $(CXX_SRCS)
	rm -f ./.depend
	$(CXX) -MM $^ $(CXXFLAGS) > ./.depend;

include .depend

ctags:
	ctags -e *.c *.h *.cpp

clean:
	@rm -f *.o main .depend libqs.so liblfds.so
	$(MAKE) -f makefile.linux -C liblfds6.1.1/liblfds611/ clean

# to build the concurrent data strutures library
liblfds.so: liblfds6.1.1/liblfds611/bin/liblfds611.so
	cp $< $@

liblfds6.1.1/liblfds611/bin/liblfds611.so: liblfds6.1.1/liblfds611/makefile.linux
	$(MAKE) -f makefile.linux -C liblfds6.1.1/liblfds611/ sorel
