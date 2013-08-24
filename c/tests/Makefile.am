TESTS = task_layer_test sched_layer_test

task_layer_test_SOURCES = task_layer/task_layer.cpp
sched_layer_test_SOURCES = sched_layer/sched_layer.cpp

LDADD=$(top_builddir)/src/libqs.la
CXXFLAGS = `pkg-config glib-2.0 --libs --cflags` \
	`gtest-config --libs` \
	-lffi -ltbb \
	-I../../src

check_PROGRAMS = task_layer_test sched_layer_test
