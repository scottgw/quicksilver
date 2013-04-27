#ifndef _CLOSURE_H
#define _CLOSURE_H

struct closure;
typedef struct closure* closure_t;

closure_t
closure_new(void (*func)(void*), void* ptr);

void closure_apply(closure_t clos);

#endif // _CLOSURE_H
