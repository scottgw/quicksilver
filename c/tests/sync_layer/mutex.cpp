#include <gtest/gtest.h>

#include <libqs/sync_ops.h>
#include <libqs/sched_task.h>
#include <internal/task_mutex.h>

#define SCHEDULE_MULTIPLE_NUM 32
#define NUM_ITERS 20000

int32_t finished;
task_mutex_t mutex;

void
exec_tester(int num_tasks, int num_execs, void (*f)(void*))
{
  sync_data_t sync_data = sync_data_new(num_tasks); 
  sched_task_t *task_array = 
    (sched_task_t*) malloc(num_tasks * sizeof(sched_task_t));

  mutex = task_mutex_new();

  finished = 0;

  for (int i = 0; i < num_tasks; i++)
    {
      sched_task_t stask = stask_new(sync_data);
      task_array[i] = stask;
      stask_set_func(stask, f, stask);
    }

  sync_data_create_executors (sync_data, num_execs);

  ASSERT_EQ(sync_data_num_processors(sync_data), num_tasks);

  for (int i = 0; i < num_tasks; i++)
    {
      sync_data_enqueue_runnable(sync_data, task_array[i]);
    }

  sync_data_join_executors(sync_data);
  ASSERT_EQ(finished, num_tasks * NUM_ITERS);
  ASSERT_EQ(sync_data_num_processors(sync_data), 0);
  sync_data_free(sync_data);

  task_mutex_free(mutex);
  free(task_array);
}

void
mutex_task(void* data)
{
  sched_task_t stask = (sched_task_t) data;

  for (int i = 0; i < NUM_ITERS; i++)
    {
      task_mutex_lock(mutex, stask);
      finished++;
      task_mutex_unlock(mutex, stask);
    }
}


TEST(Mutex, Task1SingleExecutor)
{
  exec_tester(1, 1, mutex_task);
}

TEST(Mutex, TaskSingleExecutor)
{
  exec_tester(SCHEDULE_MULTIPLE_NUM, 1, mutex_task);
}

TEST(Mutex, TaskMultipleExecutors)
{
  exec_tester(SCHEDULE_MULTIPLE_NUM, 4, mutex_task);
}

int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
