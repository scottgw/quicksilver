/* chain: chain all problems
 *
 * input:
 *   nelts: the number of elements
 *   randmat_seed: random number generator seed
 *   thresh_percent: percentage of cells to retain
 *   winnow_nelts: the number of points to select
 *
 * output:
 *  result: a real vector, whose values are the result of the final product
 */
#include <cstdio>
#include <cstring>
#include <cstdlib>

#include <vector>

#include "tbb/task_scheduler_init.h"

using namespace std;
using namespace tbb;

int is_bench = 0;
int n_threads = task_scheduler_init::default_num_threads();

void randmat(int, int, unsigned int);
void thresh(int, int, int);
void winnow(int, int, int);
void outer(int);
void product(int);

extern double *product_result;

int main(int argc, char** argv) {
  int nelts, randmat_seed, thresh_percent, winnow_nelts;
  int param_start = 0;

  for (int i = 1; i < argc; i++) 
    {
      if (argv[i][0] == '-')
        {
          if (strcmp(argv[i], "--is_bench") == 0)
            {
              is_bench = 1;
              param_start = i+1;
            }
          else if (!strcmp(argv[i], "--threads"))
            {
              i++;
              n_threads = atoi(argv[i]);
              param_start = i + 1;
            }
        }
    }

  nelts = atoi(argv[param_start]);
  randmat_seed = atoi(argv[param_start + 1]);
  thresh_percent = atoi(argv[param_start + 2]);
  winnow_nelts = atoi(argv[param_start + 3]);

  task_scheduler_init init(n_threads);

  randmat(nelts, nelts, randmat_seed);
  thresh(nelts, nelts, thresh_percent);
  winnow(nelts, nelts, winnow_nelts);
  outer(winnow_nelts);
  product(winnow_nelts);

  if (!is_bench) {
    for (int i = 0; i < winnow_nelts; i++) {
      printf("%g ", product_result[i]);
    }
    printf("\n");
  }

  return 0;
}
