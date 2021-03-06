#ifndef _NOTIFIER_H
#define _NOTIFIER_H

#include "../libqs/types.h"
// Creates the notifier thread and returns the thread_t.
// The caller can then join it when desired.
notifier_t
notifier_new(sync_data_t);

notifier_t
notifier_spawn(sync_data_t);

void
notifier_join(notifier_t notifier);

extern volatile int time_is_up;
extern volatile int notifier_done;

#endif // _NOTIFIER_H
