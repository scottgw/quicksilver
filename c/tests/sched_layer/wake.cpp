#include <gtest/gtest.h>
#include <libqs/sync_ops.h>
#include <libqs/sched_task.h>

#include <internal/executor.h>
#include <internal/queue_impl.h>
#include <internal/task.h>

#define WORKERS 32
#define N 2000
int32_t finished;

queue_impl_t q;

void
sleep_task(void* data)
{
  sched_task_t stask = (sched_task_t) data;

  for (int i = 0; i < N; i++)
    {
      task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
      queue_impl_enqueue(q, stask);
      stask_yield_to_executor(stask);
    }

  __sync_fetch_and_add(&finished, 1); 
}

void
wake_task(void* data)
{
  sched_task_t stask = (sched_task_t) data;

  for (int i = 0; i < N * WORKERS; i++)
    {
      sched_task_t wakee;
      while (!queue_impl_dequeue(q, (void**)&wakee));
      stask_wake(wakee, stask->executor);
    }

  __sync_fetch_and_add(&finished, 1); 
}

void
exec_tester(int num_tasks, int num_execs)
{
  // volatile means we can ask gdb about it later
  int num_tasks_1 = num_tasks + 1;
  volatile sync_data_t sync_data = sync_data_new(num_tasks_1);
  sched_task_t *task_array = 
    (sched_task_t*) malloc(num_tasks_1 * sizeof(sched_task_t));

  q = queue_impl_new(N);

  finished = 0;


  {
    sched_task_t stask = stask_new(sync_data);
    task_array[num_tasks] = stask;
    stask_set_func(stask, wake_task, stask);
  }

  for (int i = 0; i < num_tasks; i++)
    {
      sched_task_t stask = stask_new(sync_data);
      task_array[i] = stask;
      stask_set_func(stask, sleep_task, stask);
    }

  sync_data_create_executors (sync_data, num_execs);

  ASSERT_EQ(sync_data_num_processors(sync_data), num_tasks_1);

  for (int i = 0; i < num_tasks_1; i++)
    {
      sched_task_t stask = task_array[i];
      sync_data_enqueue_runnable(sync_data, stask);
    }

  sync_data_join_executors(sync_data);
  ASSERT_EQ(finished, num_tasks_1);
  ASSERT_EQ(sync_data_num_processors(sync_data), 0);
  sync_data_free(sync_data);

  free(task_array);
}

TEST(Wake, WakeSnooze)
{
  exec_tester(WORKERS, 4);
}


int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
