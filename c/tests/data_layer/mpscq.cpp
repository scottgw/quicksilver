#include <gtest/gtest.h>
#include <pthread.h>

#include <internal/sched_task.h>
#include <internal/mpscq.h>

#define PUSHERS 32
#define PUSHES 20000

pthread_barrier_t barrier;
int count;

void*
q_pusher(void* data)
{
  mpscq_t *q = (mpscq_t*) data;
  pthread_barrier_wait(&barrier);

  for (int i = 0; i < PUSHES; i++)
    {
      sched_task_t stask = (sched_task_t)malloc(sizeof(*stask)); // leak it
      mpscq_push(q, stask);
    }
}



void*
q_popper(void* data)
{
  mpscq_t *q = (mpscq_t*) data;
  sched_task_t stask;
  pthread_barrier_wait(&barrier);

  for (int i = 0; i < PUSHERS * PUSHES; i++)
    {
      while ((stask = mpscq_pop(q)) == NULL);
      count++;
    }
}


TEST(Data, WorkStealingPushPopSteal)
{
  pthread_t threads[PUSHERS + 1];
  mpscq_t q;
  count = 0;
 
  mpscq_create(&q, NULL);

  pthread_barrier_init(&barrier, NULL, PUSHERS + 1);
  
  {
    pthread_create(&threads[PUSHERS], NULL, q_popper, &q);
  }

  for (int i = 0; i < PUSHERS; i++)
    {
      pthread_create(&threads[i], NULL, q_pusher, &q);
    }

  for (int i = 0; i < PUSHERS + 1; i++)
    {
      pthread_join(threads[i], NULL);
    }

  ASSERT_EQ(count, PUSHERS*PUSHES);
}


int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
