#include <gtest/gtest.h>
#include <libqs/sync_ops.h>
#include <libqs/sched_task.h>
#include <internal/executor.h>
#include <internal/task.h>

#define SCHEDULE_MULTIPLE_NUM 1000
int32_t finished;

void
exec_tester(int num_tasks, int num_execs, void (*f)(void*))
{
  // volatile means we can ask gdb about it later
  volatile sync_data_t sync_data = sync_data_new(num_tasks);
  sched_task_t *task_array = 
    (sched_task_t*) malloc(num_tasks * sizeof(sched_task_t));

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
      sched_task_t stask = task_array[i];
      sync_data_enqueue_runnable(sync_data, stask);
    }

  sync_data_join_executors(sync_data);
  ASSERT_EQ(finished, num_tasks);
  ASSERT_EQ(sync_data_num_processors(sync_data), 0);
  sync_data_free(sync_data);

  free(task_array);
}

void
schedule_multiple(void* data)
{
  __sync_add_and_fetch(&finished, 1);
}

TEST(SchedTask, ScheduleSingleTask)
{
  exec_tester(1, 1, schedule_multiple);
}

TEST(SchedTask, ScheduleMultipleTasksSingleExecutor)
{
  exec_tester(SCHEDULE_MULTIPLE_NUM, 1, schedule_multiple);
}

TEST(SchedTask, ScheduleMultipleTasksMultipleExecutors)
{
  exec_tester(SCHEDULE_MULTIPLE_NUM, 8, schedule_multiple);
}

void
preempt_task(void* data)
{
  sched_task_t stask = (sched_task_t) data;
  task_set_state(stask->task, TASK_TRANSITION_TO_RUNNABLE);
  stask_yield_to_executor(stask);
  schedule_multiple(NULL);
  task_set_state(stask->task, TASK_TRANSITION_TO_RUNNABLE);
  stask_yield_to_executor(stask);
}


TEST(SchedTask, SchedulePreempt1SingleExecutor)
{
  exec_tester(1, 1, preempt_task);
}

TEST(SchedTask, SchedulePreemptSingleExecutor)
{
  exec_tester(SCHEDULE_MULTIPLE_NUM, 1, preempt_task);
}

TEST(SchedTask, SchedulePreemptMultipleExecutors)
{
  exec_tester(SCHEDULE_MULTIPLE_NUM, 8, preempt_task);
}


int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
