TESTS = task_layer_test\
	mpscq_test\
	work_stealing_q_test\
	wake_test\
	sched_layer_test\
	sync_data_test\
	stask_test\
	executor_test\
	mutex_test \
	process_test \
	condvar_test \
	stack_walk \
	io_mgr

task_layer_test_SOURCES = task_layer/task_layer.cpp
mpscq_test_SOURCES = data_layer/mpscq.cpp
wake_test_SOURCES = sched_layer/wake.cpp
work_stealing_q_test_SOURCES = data_layer/work_stealing.cpp
sched_layer_test_SOURCES = sched_layer/sched_layer.cpp
sync_data_test_SOURCES = sched_layer/sync_data.cpp
stask_test_SOURCES = sched_layer/stask.cpp
executor_test_SOURCES = sched_layer/executor.cpp
mutex_test_SOURCES = sync_layer/mutex.cpp
condvar_test_SOURCES = sync_layer/condvar.cpp
process_test_SOURCES = process_layer/process.cpp
stack_walk_SOURCES = unwind/stack_walk.cpp
io_mgr_SOURCES = io_mgr/io_mgr.cpp

LDADD=$(top_builddir)/src/libqs.la
AM_CXXFLAGS = `pkg-config glib-2.0 --libs --cflags` \
	`gtest-config --libs` \
	-pthread \
	-lffi -ltbb -lunwind -lunwind-x86_64 \
	-I../../src

check_PROGRAMS = ${TESTS}

