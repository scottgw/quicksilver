#include <gtest/gtest.h>

#include <libqs/sync_ops.h>
#include <libqs/processor.h>

void
root_func(processor_t proc)
{

}

TEST(Process, Basic)
{
  sync_data_t sync_data = sync_data_new (1);
  volatile processor_t proc = proc_new_root (sync_data, root_func);

  sync_data_create_executors(sync_data, 1);

  sync_data_join_executors(sync_data);
  sync_data_free(sync_data);
}

int
main(int argc, char **argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
