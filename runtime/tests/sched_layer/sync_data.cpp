#include <gtest/gtest.h>
#include <libqs/sync_ops.h>

TEST(SchedTask, SyncMakeFree)
{
  sync_data_t sync_data = sync_data_new(1);

  sync_data_free(sync_data);

  ASSERT_TRUE(true);
}

int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
