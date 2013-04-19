#include "task_condition.h"

#define INIT_WAIT_QUEUE_SIZE 16

struct task_condition
{
  conc_queue_t wait_queue;
};

task_condition_t
task_condition_new(processor_t proc)
{
task_condition_t cv = (task_condition_t)malloc(sizeof(struct task_condition));

  assert(lfds611_queue_new(&cv->wait_queue, INIT_WAIT_QUEUE_SIZE) == 1);

  return cv;
}

void
task_condition_free(task_condition_t cv)
{
  lfds611_queue_delete(cv->wait_queue, NULL, NULL);
  free(cv);
}

void
task_condition_wait(task_condition_t cv, processor_t proc)
{
  lfds611_queue_enqueue(cv->wait_queue, proc);
}
