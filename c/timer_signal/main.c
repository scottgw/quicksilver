#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>
#include <unistd.h>

pthread_t *sigaction_threads;

int
fib(int n)
{
  if (n < 2)
    return 1;
  else
    return fib (n-1) + fib (n-2);
}

void
timer_handler(int x)
{
  pthread_kill(sigaction_threads[0], SIGUSR1);
  pthread_kill(sigaction_threads[1], SIGUSR1);
  printf("Timer handled\n");
  pthread_exit(NULL);
}

void
setup_handler(pthread_t *threads)
{
  struct sigaction alarm_action;
  sigset_t mask;
  sigemptyset(&mask);

  alarm_action.sa_handler = timer_handler;
  alarm_action.sa_mask    = mask;
  alarm_action.sa_flags   = 0;

  sigaction_threads = threads;
  sigaction (SIGALRM, &alarm_action, NULL);
}

void*
notifier(void* ptr)
{
  sigset_t mask, old_mask;
  pthread_t *threads = (pthread_t*) ptr;  

  setup_handler(threads);
  
  // We only listem for SIGALRM, may want to also listen
  // for general interrupts.
  sigemptyset(&mask);
  sigaddset(&mask, SIGALRM);

  // Block the signal we want, so we don't miss it
  // while the loop iterates.
  pthread_sigmask(SIG_UNBLOCK, &mask, NULL);
  pthread_sigmask(SIG_BLOCK, &mask, &old_mask);
  //  while(1)
  sigsuspend(&old_mask);
  // Unblock the signals to continue as normal.
  pthread_sigmask(SIG_UNBLOCK, &mask, NULL);

  return NULL;
}


void
usr_handler(int x, siginfo_t *info, void* data)
{
  printf("USR1 Handled\n");
  pthread_exit(NULL);
}

void
setup_usr_handler()
{
  struct sigaction usr_action;
  sigset_t mask;
  sigemptyset(&mask);

  usr_action.sa_sigaction = usr_handler;
  usr_action.sa_mask      = mask;
  usr_action.sa_flags     = 0;

  sigaction (SIGUSR1, &usr_action, NULL);
}

void*
thread_run(void* ptr)
{
  sigset_t mask;

  // Setup the mask so that regular threads do not
  // catch the alarm, only the notifier does.

  sigemptyset(&mask);
  sigaddset(&mask, SIGALRM);

  pthread_sigmask(SIG_BLOCK, &mask, NULL);

  setup_usr_handler();

  fib(45);

  printf("After thread\n");
  
  return NULL;
}

void
setup_threads()
{
  pthread_t threads[2];
  pthread_t pause_thr;

  pthread_create(threads + 0, NULL, thread_run, NULL);
  pthread_create(threads + 1, NULL, thread_run, NULL);
  pthread_create(&pause_thr, NULL, notifier, threads);

  pthread_join(threads[0], NULL);
  pthread_join(threads[1], NULL);
  pthread_join(pause_thr, NULL);
  printf("Done with threads\n");
}

int
main(int argc, char **argv)
{
  struct itimerval timer_param;
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGALRM);
  
  pthread_sigmask(SIG_BLOCK, &mask, NULL);

  timer_param.it_interval.tv_sec = 0;
  timer_param.it_interval.tv_usec = 0;
  timer_param.it_value.tv_sec = 1;
  timer_param.it_value.tv_usec = 0;

  setitimer(ITIMER_REAL,
            &timer_param,
            NULL);

  setup_threads();

  return 0;
}
