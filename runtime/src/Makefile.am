lib_LTLIBRARIES = libqs.la

libqs_la_SOURCES = \
	bounded_queue.c \
	closure.c \
	ctx.c \
	debug_log.cpp \
	executor.c \
	internal/bounded_queue.h \
	internal/ctx.h \
	internal/debug_log.h \
	internal/debug_log.h \
	internal/executor.h \
	internal/mpscq.h \
	internal/queue_impl.h \
	internal/spsc_queue.h \
	internal/task.h \
	internal/task_condition.h \
	internal/task_mutex.h \
	io_manager.c \
	mpscq.c \
	notifier.c \
	private_queue.c \
	processor.c \
	qoq.c \
	queue_impl.cpp \
	sched_task.c \
	spsc_queue.cpp \
	sync_ops.c \
	task.c \
	task_condition.c \
	task_mutex.c \
	trace.h \
	ws_deque.c

BUILT_SOURCES = config.h

if ENABLE_SYSTEMTAP
libqs_la_SOURCES += probes.h
BUILT_SOURCES += probes.h
endif



# we use pkginclude because it'll put these in a separate directory
# undef the $(includedir).
pkginclude_HEADERS = \
	libqs/closure.h \
	libqs/io_manager.h \
	libqs/notifier.h \
	libqs/private_queue.h \
	libqs/processor.h \
	libqs/sched_task.h \
	libqs/sync_ops.h \
	libqs/types.h

FLAGS = -pthread -U_FORTIFY_SOURCE -DENABLE_LOG \
	-Wall -Wno-unused-value -Wno-unused-variable -Werror -march=native
libqs_la_CFLAGS = -std=gnu99 $(FLAGS) $(DEPS_CFLAGS)
libqs_la_CXXFLAGS = -std=c++0x $(FLAGS) $(DEPS_CFLAGS)
libqs_la_LIBADD =

# systemtap related directives
if ENABLE_SYSTEMTAP
probes.h: probes.d
	$(DTRACE) -h -C -s $< -o $@

probes.o: probes.d
	$(DTRACE) -G -C -s $< -o $@
libqs_la_LIBADD += probes.o
endif

# TODO: add dependencies for (?) LIBS=rt qs lfds :libffi.so.6 tbb
# TODO: investigate how to use compilation optimization options, debugging info
