#include <ucontext.h>
#include <stdlib.h>

#include "ctx.h"

struct ctx
{
  ucontext_t ctx;
};

ctx_t
ctx_new()
{
  ctx_t ctx = (ctx_t)malloc(sizeof(struct ctx));
  return ctx;
}

void
ctx_make(ctx_t ctx, void (*func)(void*), void* ptr)
{
  makecontext(&ctx->ctx, (void (*)())func, 1, ptr);
}

void
ctx_set_next(ctx_t ctx, ctx_t next_ctx)
{
  ctx->ctx.uc_link = &next_ctx->ctx;
}

uint32_t
ctx_get(ctx_t ctx)
{
  return getcontext(&ctx->ctx);
}

uint32_t
ctx_set(ctx_t ctx)
{
  return setcontext(&ctx->ctx);
}

void
ctx_set_stack_ptr(ctx_t ctx, void* ptr)
{
  ctx->ctx.uc_stack.ss_sp = ptr;
}

void
ctx_set_stack_size(ctx_t ctx, size_t sz)
{
  ctx->ctx.uc_stack.ss_size = sz;
}
