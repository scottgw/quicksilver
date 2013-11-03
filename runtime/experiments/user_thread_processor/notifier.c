#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>

#include "executor.h"
#include "notifier.h"

//
// Notifier implementation 
//

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

// This thread continually waits on the
// timeout and sends the SIGUSR1 to the executors
// so they will switch to another processor.
void*
notifier(void* ptr)
{
  while(notifier_done == 0)
    {
      usleep(1000);
      time_is_up = 1;
    }

  return NULL;
}

pthread_t
create_notifier()
{
  pthread_t thr;
  pthread_create(&thr, NULL, notifier, NULL);
  return thr;
}
