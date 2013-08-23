#include <stdlib.h>

#include "libqs/executor.h"
#include "libqs/sched_task.h"
#include "libqs/task.h"

sched_task_t
stask_new(sync_data_t sync_data)
{
  sched_task_t stask = malloc(sizeof(struct sched_task));
  
  stask->task = task_make();
  stask->sync_data = sync_data;
  stask->executor = NULL;
  stask->next = NULL;

  return stask;
}

void
stask_free(sched_task_t stask)
{
  task_free(stask->task);
  free(stask);
}


void
stask_wake(sched_task_t stask, executor_t exec)
{ 
  while(task_get_state(stask->task) != TASK_WAITING);
  task_set_state(stask->task, TASK_RUNNABLE);
  exec_push(exec, stask);
  /* sync_data_enqueue_runnable(proc->task->sync_data, proc); */
}
