#include <gtest/gtest.h>
#include <libqs/sync_ops.h>
#include <internal/sched_task.h>
#include <internal/task.h>

TEST(SchedTask, SyncMakeFree)
{
  sync_data_t sync_data = sync_data_new(1);

  sync_data_free(sync_data);

  ASSERT_TRUE(true);
}

TEST(SchedTask, SchedTaskMakeFree)
{
  sync_data_t sync_data = sync_data_new(1);
  sched_task_t stask = stask_new(sync_data);

  stask_free(stask);
  sync_data_free(sync_data);

  ASSERT_TRUE(true);
}

TEST(SchedTask, ExecutorsMakeFree)
{
  sync_data_t sync_data = sync_data_new(1);
  sync_data_create_executors (sync_data, 1);

  sync_data_join_executors(sync_data);
  sync_data_free(sync_data);

  ASSERT_TRUE(true);
}

int schedSingleRan;

void
schedule_single(void* data)
{
  sched_task_t stask = (sched_task_t) stask;
  schedSingleRan = true;
}

TEST(SchedTask, ScheduleSingle)
{
  schedSingleRan = false;
  sync_data_t sync_data = sync_data_new(1);
  sync_data_create_executors (sync_data, 1);
  sched_task_t stask = stask_new(sync_data);
  task_set_func(stask->task, schedule_single, stask);

  sync_data_enqueue_runnable(sync_data, stask);

  // We do not free the stask because it is cleaned up as it finishes
  // execution.
  sync_data_join_executors(sync_data);
  sync_data_free(sync_data);
  ASSERT_EQ(schedSingleRan, true);
}


#define SCHEDULE_MULTIPLE_NUM 20

bool sched_multi_array[SCHEDULE_MULTIPLE_NUM];

void
schedule_multiple(void* data)
{
  bool* flag = (bool*) data;
  *flag = true;
}

TEST(SchedTask, ScheduleMultiple)
{
  schedSingleRan = false;
  sync_data_t sync_data = sync_data_new(SCHEDULE_MULTIPLE_NUM);
  sync_data_create_executors (sync_data, 1);

  for (int i = 0; i < SCHEDULE_MULTIPLE_NUM; i++)
    {
      sched_task_t stask = stask_new(sync_data);
      sched_multi_array[i] = false;
      task_set_func(stask->task, schedule_multiple, &sched_multi_array[i]);
      
      sync_data_enqueue_runnable(sync_data, stask);
    }


  sync_data_join_executors(sync_data);
  sync_data_free(sync_data);

  for (int i = 0; i < SCHEDULE_MULTIPLE_NUM; i++)
    {
      ASSERT_EQ(sched_multi_array[i], true);
    }
}


int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
