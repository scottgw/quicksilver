/* product: matrix-vector product
 *
 * input:
 *   matrix: a real matrix
 *   vec: a real vector
 *   nelts: the number of elements
 *
 * output:
 *   result: a real vector, whose values are the result of the product
 */
#include <cstdio>
#include <cstring>

#include <iostream>
#include <vector>

#include "tbb/tbb.h"

using namespace std;
using namespace tbb;

static int is_bench = 0;
int n_threads = task_scheduler_init::default_num_threads();

static double *matrix;
static double *vec;
static double *result;

typedef blocked_range<size_t> range;

void product(int nelts) {
  parallel_for(
    range(0, nelts),
    [&, nelts](range r) {
      auto r_end = r.end();
      for (size_t i = r.begin(); i != r_end; ++i) {
        double sum = 0;
        for (int j = 0; j < nelts; ++j) {
          sum += matrix [i*nelts + j] * vec [j];
        }
        result [i] = sum;
      }
  });
}

int main(int argc, char** argv) {
  int nelts;


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
        nelts = atoi(argv[i]);
        break;
      }
  }

  task_scheduler_init init(n_threads);

  struct timespec start;
  struct timespec end;
 
  matrix = (double *) malloc (sizeof(double) * nelts * nelts);
  vec = (double *) malloc (sizeof (double) * nelts);
  result = (double *) malloc (sizeof (double) * nelts);

  if (!is_bench) {
    for (int i = 0; i < nelts; i++) {
      for (int j = 0; j < nelts; j++) {
        cin >> matrix[i*nelts + j];
      }
    }

    for (int i = 0; i < nelts; i++) {
      cin >> vec[i];
    }
  }

  clock_gettime(CLOCK_MONOTONIC, &start);
  product(nelts);
  clock_gettime(CLOCK_MONOTONIC, &end);
  printf("%f\n",
         (double)(end.tv_sec - start.tv_sec) +
         (end.tv_nsec - start.tv_nsec)/1000000000.0);

  if (!is_bench) {
    printf("%d\n", nelts, nelts);
    for (int i = 0; i < nelts; i++) {
      printf("%g ", result[i]);
    }
    printf("\n");
  }

  return 0;
}
