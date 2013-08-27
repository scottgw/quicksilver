#include <gtest/gtest.h>
#include <pthread.h>

#include <internal/ws_deque.h>

#define POPPERS 8
#define POPS 40000

pthread_barrier_t barrier;

void*
ws_pusher(void* data)
{
  ws_deque_t wsd = (ws_deque_t) data;
  pthread_barrier_wait(&barrier);

  for (int i = 0; i < POPS * POPPERS; i++)
    {
      ws_deque_push_bottom(wsd, (void*)i);
    }
}

void*
ws_pushpopper(void* data)
{
  ws_deque_t wsd = (ws_deque_t) data;
  pthread_barrier_wait(&barrier);

  int64_t tmp = 0;

  for (int i = 0; i < POPS * (POPPERS + 1); i++)
    {
      ws_deque_push_bottom(wsd, (void*)i);
    }

  for (int i = 0; i < POPS; i++)
    {
      while (!ws_deque_steal(wsd, (void**) &tmp));
    }
}

void*
ws_stealer(void* data)
{
  ws_deque_t wsd = (ws_deque_t) data;
  int64_t result;
  pthread_barrier_wait(&barrier);

  for (int i = 0; i < POPS; i++)
    {
      while (!ws_deque_steal(wsd, (void**) &result));
    }
}

TEST(Data, WorkStealingPushPopSteal)
{
  pthread_t threads[POPPERS + 1];
  volatile ws_deque_t wsd;
  wsd = ws_deque_new();
  pthread_barrier_init(&barrier, NULL, POPPERS + 1);
  
  {
    pthread_create(&threads[POPPERS], NULL, ws_pushpopper, wsd);
  }

  for (int i = 0; i < POPPERS; i++)
    {
      pthread_create(&threads[i], NULL, ws_stealer, wsd);
    }

  for (int i = 0; i < POPPERS + 1; i++)
    {
      pthread_join(threads[i], NULL);
    }

  ASSERT_EQ(ws_deque_size(wsd), 0);
}


TEST(Data, WorkStealingPushSteal)
{
  pthread_t threads[POPPERS + 1];
  volatile ws_deque_t wsd;
  wsd = ws_deque_new();
  pthread_barrier_init(&barrier, NULL, POPPERS + 1);
  
  {
    pthread_create(&threads[POPPERS], NULL, ws_pusher, wsd);
  }

  for (int i = 0; i < POPPERS; i++)
    {
      pthread_create(&threads[i], NULL, ws_stealer, wsd);
    }

  for (int i = 0; i < POPPERS + 1; i++)
    {
      pthread_join(threads[i], NULL);
    }

  ASSERT_EQ(ws_deque_size(wsd), 0);
}

int
main(int argc, char** argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
