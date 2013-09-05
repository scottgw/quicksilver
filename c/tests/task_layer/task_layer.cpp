#include <gtest/gtest.h>
#include <internal/task.h>

TEST(Task, MakeFree)
{
  task_t task = task_make();
  task_free(task);
  ASSERT_TRUE(true);
}

int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
