/* thresh: histogram thresholding
 *
 * input:
 *   matrix: the integer matrix to be thresholded
 *   nrows, ncols: number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   mask: a boolean matrix filled with true for cells that are kept
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <algorithm>

#include "tbb/tbb.h"

using namespace std;
using namespace tbb;

int is_bench = 0;
int n_threads = task_scheduler_init::default_num_threads();

static unsigned char *matrix;
static unsigned char *mask;

struct view {
  int h[100];
  view() {fill_n (h, 100, 0);}
};

typedef combinable<view> histogram_type;

typedef tbb::blocked_range<size_t> range;

void thresh(int n, int percent) {
  int nmax = 0;
  histogram_type histogram;

  nmax = tbb::parallel_reduce(
      range(0, n), 0,
      [=,&histogram](range r, int result)->int {
        view& v =  histogram.local ();
        for (size_t i = r.begin(); i != r.end(); i++) {
          for (int j = 0; j < n; j++) {
            int val;
            if (is_bench) {
              matrix[i*n + j] = (i * j) % 100;
            }
            val = (int)matrix[i*n + j];

            result = max(result, val);
            v.h[val]++;
          }
        }
        return result;
      },
      [](int x, int y)->int {
        return max(x, y);
      });

  view v;
  histogram.combine_each( [=, &v] (const view& x) {
      for (int i = 0; i <= nmax; ++i)
        v.h[i] += x.h[i];
    });

  int count = (n * n * percent) / 100;

  int prefixsum = 0;
  int threshold = nmax;

  for (int i = nmax; i >= 0 && prefixsum <= count; i--) {
    prefixsum += v.h[i];
    threshold = i;
  }

  tbb::parallel_for(
      range(0, n),
      [=](range r) {
        for (size_t i = r.begin(); i != r.end(); ++i) {
          for (int j = 0; j < n; j++) {
            mask[i*n + j] = matrix[i*n + j] >= threshold;
          }
        }
      });
}

int main(int argc, char** argv) {
  int n, percent;

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
            percent = atoi(argv[i]);
            break;
          }
      }
  }

  task_scheduler_init init(n_threads);
  
  matrix = (unsigned char*) malloc (sizeof (unsigned char) * n * n);
  mask = (unsigned char*) malloc (sizeof (unsigned char) * n * n);

  if (!is_bench) {
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        scanf("%hhu", &matrix[i*n + j]);
      }
    }
  }

  thresh(n, percent);

  if (!is_bench) {
    printf("%d %d\n", n, n);
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        printf("%hhu ", mask[i*n + j]);
      }
      printf("\n");
    }
    printf("\n");
  }

  return 0;
}
