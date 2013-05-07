#include <assert.h>
#include <ucontext.h>
#include <setjmp.h>
#include <stdlib.h>


#include "libqs/ctx.h"

struct ctx
{
  jmp_buf jbuf;
  ucontext_t uctx;
};


static
uint32_t
ctx_get(ctx_t ctx)
{
  return getcontext(&ctx->uctx);
}


ctx_t
ctx_new()
{
  ctx_t ctx = (ctx_t)malloc(sizeof(struct ctx));
  ctx_get(ctx);
  return ctx;
}

void
ctx_free(ctx_t ctx)
{
  free(ctx);
}

typedef struct
{
  ctx_t ctx;
  void (*func)(void*);
  void *ptr;
} ctx_wrapper_data;

static
void
ctx_wrapper_f(ctx_wrapper_data* data, ucontext_t *next_uctx)
{
  ctx_t ctx = data->ctx;
  void (*func)(void*) = data->func;
  void *ptr = data->ptr;

  free(data);

  if (setjmp(ctx->jbuf) == 0)
    {
      // First time through we want to jump back to
      // the setup routine that just made us.
      ucontext_t wrapper_uctx;
      swapcontext(&wrapper_uctx, next_uctx);
    }
  func(ptr);
}

void
ctx_make(ctx_t ctx, void (*func)(void*), void* ptr)
{
  ucontext_t next_uctx;
  ctx_wrapper_data* data =
    (ctx_wrapper_data*)malloc(sizeof(ctx_wrapper_data));
  data->ctx = ctx;
  data->func = func;
  data->ptr = ptr;
  makecontext(&ctx->uctx, (void (*)())ctx_wrapper_f, 2, data, &next_uctx);
  swapcontext(&next_uctx, &ctx->uctx);
}

void
ctx_set_next(ctx_t ctx, ctx_t next_ctx)
{
  ctx->uctx.uc_link = &next_ctx->uctx;
}

bool
ctx_save(ctx_t ctx)
{
  /* volatile int flag = true; */
  /* assert (ctx_get(ctx) == 0); */
  if (setjmp(ctx->jbuf) == 0)
    {
      return true;
    }
  else
    {
      return false;
    }
}

uint32_t
ctx_set(ctx_t ctx)
{
  longjmp(ctx->jbuf, 1);
  assert (false && "ctx_set: should not reach this point after longjmp");
  return 0;
}

void
ctx_set_stack_ptr(ctx_t ctx, void* ptr)
{
  ctx->uctx.uc_stack.ss_sp = ptr;
}

void
ctx_set_stack_size(ctx_t ctx, size_t sz)
{
  ctx->uctx.uc_stack.ss_size = sz;
}
