#include <stdio.h>

#include "conc_queue.h"

int
main(int argc, char **argv)
{
  int x = 42;
  int y = 10;
  int* res;
  cqueue_t q = cq_make();

  cq_enqueue(q, &x);
  cq_enqueue(q, &y);

  cq_dequeue(q, (void**)&res);
  printf("First in: %d\n", *res);

  cq_dequeue(q, (void**)&res);
  printf("First in: %d\n", *res);

  // cq_free(q);

  return 0;
}
