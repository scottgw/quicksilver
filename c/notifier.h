
// Creates the notifier thread and returns the thread_t.
// The caller can then join it when desired.
pthread_t
create_notifier();

extern volatile int time_is_up;
extern volatile int notifier_done;

