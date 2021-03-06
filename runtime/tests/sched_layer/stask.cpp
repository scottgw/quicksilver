#include <gtest/gtest.h>
#include <libqs/sync_ops.h>
#include <libqs/sched_task.h>

TEST(SchedTask, SchedTaskMakeFree)
{
  sync_data_t sync_data = sync_data_new(1);
  sched_task_t stask = stask_new(sync_data);

  stask_free(stask);
  sync_data_free(sync_data);

  ASSERT_TRUE(true);
}

int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
