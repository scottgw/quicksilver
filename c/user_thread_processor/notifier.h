
// Creates the notifier thread and returns the thread_t.
// The caller can then join it when desired.
pthread_t
create_notifier();

void
setup_timer();


extern int time_is_up;
