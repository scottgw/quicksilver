#include "stack_map.h"

extern "C"
uint32_t*
stack_map_count_addr();

// Function that returns the address of the count of
// the stack maps. After this is the start of the 
uint32_t
stack_map_count()
{
  return *stack_map_count_addr();
}

char*
stack_map_top()
{
  return (char*)(stack_map_count_addr() + 2);
}

stack_map_t stack_map;

// Intermediate function table, lacking the arrays.
struct func_table_inter {
  uint32_t num_safe_points;
  uint32_t stack_frame_size;
  uint32_t stack_arity;
  uint32_t live_root_count;
};

typedef struct func_table_inter func_table_inter_t;

// Final function table
struct func_table {
  uint32_t num_safe_points;
  uint32_t stack_frame_size;
  uint32_t stack_arity;
  uint32_t live_root_count;
  void** safe_points;
  uint32_t *root_stack_index;
};

typedef struct func_table func_table_t;

static
void
print_func_table(func_table_t ftable)
{
  printf("num_safe_points %d\n", ftable.num_safe_points);
  for (int i = 0; i < ftable.num_safe_points; i++)
    {
      printf("safe_point %d %p\n", i, ftable.safe_points[i]);
    }
  
  printf("stack_frame_size %d\n", ftable.stack_frame_size);
  printf("stack_arity %d\n", ftable.stack_arity);
  
  printf("live_root_count %d\n", ftable.live_root_count);
  for (int i = 0; i < ftable.live_root_count; i++)
    {
      printf("root_index %d %d\n", i, ftable.root_stack_index[i]);
    }
}

static
func_table_t
get_next_func_table(char** p)
{
  func_table_inter_t *ftable_inter = *(func_table_inter_t**)p;
  char* tmp_p = *p;
  func_table_t ftable;
  ftable = *(func_table_t*)ftable_inter;

  tmp_p += sizeof(func_table_inter_t);

  ftable.safe_points =
    (void**)malloc(sizeof(void*) * ftable.num_safe_points);
  ftable.root_stack_index =
    (uint32_t*)malloc(sizeof(uint32_t) * ftable.live_root_count);

  for (int i = 0; i < ftable.num_safe_points; i++)
    {
      ftable.safe_points[i] = *(void**)tmp_p;
      tmp_p = (char*)((void**)tmp_p + 1);
    }

  for (int i = 0; i < ftable.live_root_count; i++)
    {
      ftable.root_stack_index[i] = *(uint32_t*)tmp_p;
      tmp_p = (char*)((uint32_t*)tmp_p + 1);
    }

  *p = tmp_p;
  return ftable;
}

static
void
free_func_table(func_table_t t)
{
  free(t.safe_points);
  free(t.root_stack_index);
}

static
void
merge_func_table(stack_map_t &stack_map, func_table_t ftable)
{
  root_info_t *root_info = new root_info_t();
  std::unique_ptr<root_info_t> root_info_ptr (root_info);

  for (int i = 0; i < ftable.live_root_count; i++)
    {
      // Divide by the size of a pointer because the sizes here are in
      // bytes.
      root_info_ptr->push_back(ftable.root_stack_index[i] / sizeof(void*));
    }

  for (int i = 0; i < ftable.num_safe_points; i++)
    {
      stack_map.root_infos.push_back(std::move(root_info_ptr));
      stack_map[ftable.safe_points[i]] = root_info;
    }

  free_func_table(ftable);
}

stack_map_t::stack_map_t() : root_infos ()
{
  int func_count = stack_map_count();
  char* p = stack_map_top();
  for (int i = 0; i < func_count; i++)
    {
      merge_func_table(*this, get_next_func_table(&p));
    }
}

