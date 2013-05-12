lib_LTLIBRARIES = libqs.la

libqs_la_SOURCES = executor.c list.c task.c processor.c notifier.c \
	sync_ops.c task_mutex.c task_condition.c ctx.c bounded_queue.c \
	closure.c private_queue.c queue_impl.cpp \
	debug_log.cpp libqs/debug_log.h

# we use pkginclude because it'll put these in a separate directory
# undef the $(includedir).
HEADER_FILES = executor.h list.h task.h processor.h notifier.h \
	sync_ops.h task_mutex.h task_condition.h ctx.h bounded_queue.h \
	closure.h private_queue.h queue_impl.h types.h

pkginclude_HEADERS = $(addprefix libqs/, $(HEADER_FILES))

FLAGS = -pthread -U_FORTIFY_SOURCE -Wall -Werror -Wno-unused-variable
libqs_la_CFLAGS = -std=gnu99 $(FLAGS)
libqs_la_CXXFLAGS = -std=c++11 $(FLAGS)

# TODO: add dependencies for (?) LIBS=rt qs lfds :libffi.so.6 tbb
# TODO: investigate how to use compilation optimization options, debugging info
