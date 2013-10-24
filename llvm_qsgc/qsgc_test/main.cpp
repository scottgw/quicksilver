#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <libunwind.h>
#include "stack_map.h"
#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>

extern "C"
void f(int*);

stack_map_t stk_map;

int main (int argc, char** argv)
{
  char c = 'a';
  int i = 42;
  f(&i);
  printf("done %c\n", c);
  return 0;
}

extern "C"
void fakegc()
{
  std::cout << "GC\n";
  unw_cursor_t cursor;
  unw_word_t ip, sp;
  unw_context_t uc;
  root_info_t root_info_ptr;

  unw_getcontext(&uc);
  unw_init_local(&cursor, &uc);

  do
    {
      unw_get_reg(&cursor, UNW_REG_IP, &ip);
      unw_get_reg(&cursor, UNW_REG_SP, &sp);
      // printf("ip=%p sp=%p\n", ip, sp);

      if (stk_map.find_root_info((void*)ip, root_info_ptr))
        {
          for(auto it : root_info_ptr)
            {
              int *cptr = *((int**)sp - it);
              printf("x=%p\n", cptr);
              printf("i=%d\n", *cptr);
              *cptr = *cptr + 1;
            }
        }
    } while (unw_step(&cursor) > 0);
}
