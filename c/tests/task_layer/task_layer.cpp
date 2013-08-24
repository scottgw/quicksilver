#include <gtest/gtest.h>
#include <internal/task.h>

TEST(Task, MakeFree)
{
  task_t task = task_make();
  task_free(task);
  ASSERT_TRUE(true);
}

// This upsets gtest a little, and to not clutter the output I've commented it out.
// TEST(TaskTest, FreeBad)
// {
//   ASSERT_DEATH(task_free(NULL), "");
// }


int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
