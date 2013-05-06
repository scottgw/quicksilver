#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>

#include "executor.h"
#include "notifier.h"
#include "sync_ops.h"

//
// Notifier implementation 
//

struct notifier
{
  sync_data_t sync_data;
  pthread_t thread;
};


volatile int time_is_up = 0;
volatile int notifier_done = 0;

// Handler that sends each executor a signal.
void
timer_handler(int x)
{
  switch(x)
    {
    case SIGINT:
      printf("notifier: SIGINT\n");
      pthread_exit(NULL);
      break;
    default:
      printf("notifier: unknown signal\n");
      break;
    }
}

// Setup the handler for thsi thread;
void
setup_handler()
{
  struct sigaction alarm_action;
  sigset_t mask;
  sigemptyset(&mask);

  alarm_action.sa_handler = timer_handler;
  alarm_action.sa_mask    = mask;
  alarm_action.sa_flags   = 0;

  sigaction (SIGINT, &alarm_action, NULL);
}

static
void
reschedule_awoken(notifier_t notifier)
{
  uint64_t num_awoken;
  processor_t *procs;

  sync_data_get_sleepers(notifier->sync_data, &procs, &num_awoken);

  for(int i = 0; i < num_awoken; i++)
    {
      sync_data_enqueue_runnable(notifier->sync_data, procs[i]);
    }

  free(procs);
}

// This thread continually waits on the
// timeout and sends the SIGUSR1 to the executors
// so they will switch to another processor.
void*
notifier_run(void* ptr)
{
  notifier_t notifier = (notifier_t)ptr;
  while(notifier_done == 0)
    {
      usleep(10000);
      reschedule_awoken(notifier);
      time_is_up = 1;
    }

  return NULL;
}

notifier_t
notifier_new(sync_data_t sync_data)
{
  notifier_t notifier = (notifier_t)malloc(sizeof(struct notifier));
  notifier->sync_data = sync_data;

  return notifier;
}

notifier_t
notifier_spawn(sync_data_t sync_data)
{
  notifier_t notifier = notifier_new(sync_data);
  pthread_create(&notifier->thread, NULL, notifier_run, notifier);
  return notifier;
}

void
notifier_join(notifier_t notifier)
{
  pthread_join(notifier->thread, NULL);
  free(notifier);
}
