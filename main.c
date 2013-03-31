#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <assert.h>

#include "private_queue.h"

#define N 1000000

void
*in_test(void *q_in)
{
  private_queue *q = (private_queue *) q_in;
 
  for (int i = 0; i < N; i++)
    {
      int *x = (int*) malloc(sizeof(int));
      *x = i;
      pq_enqueue(q, x);
    }
 
  pthread_exit(NULL);
}

int out_count = 0;

void
*out_test(void *q_in)
{
  private_queue *q = (private_queue *) q_in;
 
  for (int i = 0; i < N; i++)
    {
      int *x;
      
      if (pq_dequeue(q, (void**)&x))
        {
          out_count++;
          free(x);
        }
        
    }

  pthread_exit(NULL);
}

int
threaded_test()
{
  private_queue *q = pq_create();
  pthread_t thr1;
  pthread_t thr2;

  pthread_create(&thr1, NULL, in_test, (void *) q);
  pthread_create(&thr2, NULL, out_test, (void *) q);

  pthread_join(thr1, NULL);
  pthread_join(thr2, NULL);

  {
    int i = 0;
    int *tmp;
    while (pq_dequeue(q, (void**)&tmp))
      {
        free(tmp);
        i++;
      }

    // The remaining elements plus the ones removed should add
    // up to the original number of inserted entries.
    assert (i + out_count == N);
  }

  pq_free(q);

  pthread_exit(NULL);
}

int
unthreaded_test()
{
  int x = 42;
  int y = 10;
  int *z;
  private_queue *q = pq_create();

  pq_enqueue(q, &x);
  pq_enqueue(q, &y);

  pq_dequeue(q, (void**)&z);
  assert (*z == 42);

  pq_dequeue(q, (void**)&z);
  assert (*z == 10);

  pq_free(q);
  return 0;
}


int
main (int argc, char** argv)
{
  threaded_test();
  unthreaded_test();
  return 0;
}
