lib_LTLIBRARIES = libqs.la

libqs_la_SOURCES = \
	executor.c \
	task.c \
	processor.c \
	notifier.c \
	sched_task.c \
	sync_ops.c \
	task_mutex.c \
	task_condition.c \
	ctx.c \
	bounded_queue.c \
	closure.c \
	private_queue.c \
	queue_impl.cpp \
	spsc_queue.c \
	ws_deque.c \
	mpscq.c \
	qoq.c \
	debug_log.cpp \
	libqs/debug_log.h \
	internal/ctx.h

# we use pkginclude because it'll put these in a separate directory
# undef the $(includedir).
HEADER_FILES = \
	executor.h \
	task.h \
	processor.h \
	notifier.h \
	sync_ops.h \
	task_mutex.h \
	task_condition.h \
	bounded_queue.h \
	sched_task.h \
	closure.h \
	private_queue.h \
	queue_impl.h \
	spsc_queue.h \
	types.h \
	debug_log.h \
	mpscq.h

pkginclude_HEADERS = $(addprefix libqs/, $(HEADER_FILES))

FLAGS = -pthread -U_FORTIFY_SOURCE -DENABLE_LOG\
	-Wall -Wno-unused-value -Wno-unused-variable -Werror
libqs_la_CFLAGS = -std=gnu99 $(FLAGS) $(DEPS_CFLAGS)
libqs_la_CXXFLAGS = -std=c++11 $(FLAGS) $(DEPS_CFLAGS)

# TODO: add dependencies for (?) LIBS=rt qs lfds :libffi.so.6 tbb
# TODO: investigate how to use compilation optimization options, debugging info
