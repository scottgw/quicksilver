lib_LTLIBRARIES = libqs.la

libqs_la_SOURCES = executor.c list.c task.c processor.c notifier.c \
	sync_ops.c task_mutex.c task_condition.c ctx.c bounded_queue.c \
	closure.c private_queue.c queue_impl.cpp

# we use pkginclude because it'll put these in a separate directory
# undef the $(includedir).
pkginclude_HEADERS = executor.h list.h task.h processor.h notifier.h \
	sync_ops.h task_mutex.h task_condition.h ctx.h bounded_queue.h \
	closure.h private_queue.h queue_impl.h

FLAGS = -pthread -O3 -U_FORTIFY_SOURC
libqs_la_CFLAGS = -std=gnu99 $(FLAGS)
libqs_la_CXXFLAGS = -std=c++11 $(FLAGS)

# TODO: add dependencies for (?) LIBS=rt qs lfds :libffi.so.6 tbb
# TODO: investigate how to use compilation optimization options, debugging info
