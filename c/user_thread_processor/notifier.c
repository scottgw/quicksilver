#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <glib.h>

#include "executor.h"
#include "notifier.h"

//
// Notifier implementation 
//

// Callback to send signal to an executor.
static
void
timer_send_usr1(gpointer elem, gpointer user)
{
  executor_t *exec = (executor_t*) elem;
  pthread_kill(exec->thread, SIGUSR1);
}


// Handler that sends each executor a signal.
void
timer_handler(int x)
{
  switch(x)
    {
    case SIGALRM:
      g_list_foreach(executors, timer_send_usr1, NULL);
      break;
    case SIGINT:
      printf("notifier: SIGINT\n");
      pthread_exit(NULL);
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

  sigaction (SIGALRM, &alarm_action, NULL);
  sigaction (SIGINT, &alarm_action, NULL);
}

// This thread continually waits on the
// timeout and sends the SIGUSR1 to the executors
// so they will switch to another processor.
void*
notifier(void* ptr)
{
  sigset_t mask, old_mask;
  setup_handler();
  
  // We only listem for SIGALRM, may want to also listen
  // for general interrupts.
  sigemptyset(&mask);
  sigaddset(&mask, SIGALRM);
  sigaddset(&mask, SIGINT);

  // Block the signal we want, so we don't miss it
  // while the loop iterates.
  pthread_sigmask(SIG_UNBLOCK, &mask, NULL);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
  while(1) // FIXME: this should have a real condition to end the
           // thread eventually
    {
      sigsuspend(&old_mask);
    }

  // Unblock the signals to continue as normal.
  pthread_sigmask(SIG_UNBLOCK, &mask, NULL);

  return NULL;
}

pthread_t
create_notifier()
{
  pthread_t thr;
  pthread_create(&thr, NULL, notifier, NULL);
  return thr;
}

void
setup_timer()
{
  struct itimerval timer_param;
  
  // Setup a 500 microsecond repeating timer
  timer_param.it_interval.tv_sec = 0;
  timer_param.it_interval.tv_usec = 500;
  timer_param.it_value.tv_sec = 0;
  timer_param.it_value.tv_usec = 500;

  setitimer(ITIMER_REAL,
            &timer_param,
            NULL);
}
