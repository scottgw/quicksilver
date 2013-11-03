#include <gtest/gtest.h>
#include <iostream>

#include <internal/task.h>


#define UNW_LOCAL_ONLY
#include <libunwind.h>

void stack_walk(std::string tag)
{
  unw_cursor_t cursor; unw_context_t uc;
  unw_word_t ip, sp;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);

  std::cout << tag << " walk\n";
  while (unw_step(&cursor) > 0) 
    {
      unw_get_reg(&cursor, UNW_REG_IP, &ip);
      unw_get_reg(&cursor, UNW_REG_SP, &sp);
      printf ("ip = %lx, sp = %lx\n", (long) ip, (long) sp);
    }
  
  return;
}

void walk_2()
{
  stack_walk("Second task");
}

void walk_1 (void* data)
{
  stack_walk("First task");
  walk_2();
}

TEST(Unwind, WalkStack)
{
  task_t task = task_make();
  stack_walk("Outer");
  task_set_func_and_run(task, walk_1, NULL);

  task_free(task);
  ASSERT_TRUE(true);
}

int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

