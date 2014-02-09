#include <stdbool.h>

struct priv_queue_
{
  int x;
};

typedef struct priv_queue_ priv_queue;

bool
pred (priv_queue*);

void
priv_queue_sync (priv_queue* p);

void
priv_queue_routine (priv_queue* p);

int
test1 (priv_queue *p)
{
  int i = 0;
  priv_queue_sync (p);
  if (pred (p))
    {
      i = 1;
      priv_queue_sync (p);
    }
  else
    {
      i = 2;
      priv_queue_sync (p);
    }
  priv_queue_sync (p);
  return i;
}


int
test2 ()
{
  priv_queue p_ = {};
  priv_queue q_ = {};
  priv_queue *p = &p_;
  priv_queue *q = &q_;

  int i = 0;
  priv_queue_sync (p);

  if (pred (p))
    {
      i = 1;
      priv_queue_routine (q);
      priv_queue_sync (p);
    }
  else
    {
      i = 2;
      priv_queue_sync (p);
    }

  priv_queue_sync (p);

  return i;
}



int
test3 (priv_queue *p)
{
  int i = 0;
  int s = 0;

  for (i = 0; i < 200000; i++)
    {
      priv_queue_sync (p);
      s += i*i - (i >> 2);
    }

  priv_queue_sync (p);

  return s;
}


