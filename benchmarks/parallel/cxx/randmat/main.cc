/* randmat: random number generator
 *
 * input:
 *   nrows, ncols: number of rows and columns
 *   s: random number generation seed
 *
 * output:
 *   matrix: random nrows x ncols integer matrix
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "tbb/parallel_for.h"
#include "tbb/blocked_range.h"
#include "tbb/task_scheduler_init.h"

using namespace tbb;

typedef blocked_range<size_t> range;

static unsigned char* matrix;

int is_bench = 0;
int n_threads = task_scheduler_init::default_num_threads();

void randmat(int n, unsigned int s) {
  const int LCG_A = 1664525, LCG_C = 1013904223;
  parallel_for(
    range(0, n),
    [=](range r) {
      auto end = r.end (); 
      for (size_t i = r.begin(); i != end; ++i) {
        unsigned int seed = s + i;
        for (int j = 0; j < n; j++) {
          seed = LCG_A * seed + LCG_C;
          matrix[i * n + j] = seed % 100;
        }
      }
  });
}

int main(int argc, char** argv) {
  int n, s;
  int param_num = 0;

  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-')
      {
        if (!strcmp(argv[i], "--is_bench")) {
          is_bench = 1;
        } else if (!strcmp(argv[i], "--threads")) {
          n_threads = atoi(argv[i+1]);
          i++;
        }
      }
    else
      {
        if (param_num == 0)
          {
            n = atoi(argv[i]);
            param_num++;
          }
        else
          {
            s = atoi(argv[i]);
            break;
          }
      }
  }

  task_scheduler_init init(n_threads);

  matrix = (unsigned char*) malloc (sizeof (unsigned char) * n * n);
  randmat(n, s);

  if (!is_bench) {
    printf("%d %d\n", n, n);
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        printf("%d ", matrix[i*n + j]);
      }
      printf("\n");
    }
    printf("\n");
  }

  return 0;
}
