#include <gtest/gtest.h>
#include <libqs/sync_ops.h>
#include <libqs/sched_task.h>
#include <internal/task_mutex.h>
#include <internal/task_condition.h>

#define SCHEDULE_MULTIPLE_NUM 32
#define NUM_ITERS 2000

int32_t finished;
task_mutex_t mutex;
task_condition_t cv;

void
worker_task(sched_task_t stask, int flag)
{
  for (int i = 0; i < NUM_ITERS; i++)
    {
      task_mutex_lock(mutex, stask);
      while (finished % 2 == flag)
	{
	  task_condition_wait(cv, mutex, stask);
	}
      finished++;
      task_condition_signal_all(cv, stask);
      task_mutex_unlock(mutex, stask);
    }
}

void
even_task(void* data)
{
  worker_task((sched_task_t)data, 0);
}


void
odd_task(void* data)
{
  worker_task((sched_task_t)data, 1);
}

void
exec_tester(int num_tasks, int num_execs)
{
  sync_data_t sync_data = sync_data_new(2 * num_tasks); 
  sched_task_t *task_array = 
    (sched_task_t*) malloc(2 * num_tasks * sizeof(sched_task_t));

  mutex = task_mutex_new();
  cv = task_condition_new();

  finished = 0;

  for (int i = 0; i < num_tasks; i++)
    {
      sched_task_t stask = stask_new(sync_data);
      task_array[2*i] = stask;
      stask_set_func(stask, even_task, stask);
    }

  for (int i = 0; i < num_tasks; i++)
    {
      sched_task_t stask = stask_new(sync_data);
      task_array[2*i + 1] = stask;
      stask_set_func(stask, odd_task, stask);
    }

  sync_data_create_executors (sync_data, num_execs);

  ASSERT_EQ(sync_data_num_processors(sync_data), 2 * num_tasks);

  for (int i = 0; i < 2 * num_tasks; i++)
    {
      sync_data_enqueue_runnable(sync_data, task_array[i]);
    }

  sync_data_join_executors(sync_data);
  ASSERT_EQ(finished, 2 * num_tasks * NUM_ITERS);
  ASSERT_EQ(sync_data_num_processors(sync_data), 0);
  sync_data_free(sync_data);

  // task_condition_free(cv);
  task_mutex_free(mutex);
  free(task_array);
}

TEST(CondVar, Task1SingleExecutor)
{
  exec_tester(1, 1);
}

TEST(CondVar, TaskSingleExecutor)
{
  exec_tester(SCHEDULE_MULTIPLE_NUM, 1);
}

TEST(CondVar, TaskMultipleExecutors)
{
  exec_tester(SCHEDULE_MULTIPLE_NUM, 4);
}

int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
