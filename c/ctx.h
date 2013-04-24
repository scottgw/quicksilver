#ifndef _CTX_H
#define _CTX_H

#include <stdbool.h>
#include <stdint.h>

struct ctx;
typedef struct ctx * ctx_t;

ctx_t
ctx_new();

void
ctx_make(ctx_t ctx, void (*func)(void*), void* ptr);

void
ctx_set_next(ctx_t ctx, ctx_t next_ctx);

volatile
bool
ctx_save(volatile ctx_t ctx);

uint32_t
ctx_get(ctx_t ctx);

uint32_t
ctx_set(ctx_t ctx);

void
ctx_set_stack_ptr(ctx_t ctx, void* ptr);

void
ctx_set_stack_size(ctx_t ctx, size_t sz);

#endif // _CTX_H
