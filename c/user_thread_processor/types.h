#ifndef _TYPES_H
#define _TYPES_H


struct processor;
typedef struct processor* processor_t;

typedef void (*proc_func)(processor_t);

struct executor;
typedef struct executor* executor_t;


#endif // _TYPES_H
