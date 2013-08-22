#include <stdlib.h>

#include "libqs/sched_task.h"

sched_task_t
sched_task_new(task_t task)
{
  sched_task_t sched_task = malloc(sizeof(struct sched_task));
  
  sched_task->task = task;

  return sched_task;
}
