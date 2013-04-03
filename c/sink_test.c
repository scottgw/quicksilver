#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <assert.h>

#include "sink_queue.h"

#define N 1000000

void
*in_priv_test(void *q_in)
{
  sink_queue *q = (sink_queue *) q_in;
 
  for (int i = 0; i < N; i++)
    {
      int *x = (int*) malloc(sizeof(int));
      *x = i;
      sinkq_enqueue(q, x);
    }
 
  pthread_exit(NULL);
}

int out_count = 0;

void
*out_priv_test(void *q_in)
{
  sink_queue *q = (sink_queue *) q_in;
 
  for (int i = 0; i < 2*N; i++)
    {
      int *x;
      
      if (sinkq_dequeue(q, (void**)&x))
        {
          out_count++;
          free(x);
        }
        
    }

  pthread_exit(NULL);
}

int
threaded_priv_test()
{
  sink_queue *q = sinkq_create();
  pthread_t thr1;
  pthread_t thr2;
  pthread_t thr3;

  pthread_create(&thr1, NULL, in_priv_test, (void *) q);
  pthread_create(&thr2, NULL, in_priv_test, (void *) q);
  pthread_create(&thr3, NULL, out_priv_test, (void *) q);

  pthread_join(thr1, NULL);
  pthread_join(thr2, NULL);
  pthread_join(thr3, NULL);

  {
    int i = 0;
    int *tmp;
    while (sinkq_dequeue(q, (void**)&tmp))
      {
        free(tmp);
        i++;
      }

    // The remaining elements plus the ones removed should add
    // up to the original number of inserted entries.
    assert (i + out_count == 2*N);
  }

  sinkq_free(q);

  pthread_exit(NULL);
}

int
unthreaded_priv_test()
{
  int x = 42;
  int y = 10;
  int *z;
  sink_queue *q = sinkq_create();

  sinkq_enqueue(q, &x);
  sinkq_enqueue(q, &y);

  sinkq_dequeue(q, (void**)&z);
  assert (*z == 42);

  sinkq_dequeue(q, (void**)&z);
  assert (*z == 10);

  sinkq_free(q);
  return 0;
}


int
main (int argc, char** argv)
{
  threaded_priv_test();
  unthreaded_priv_test();
  return 0;
}
