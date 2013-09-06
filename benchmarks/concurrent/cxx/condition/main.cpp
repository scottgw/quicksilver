#include <cstdlib>
#include <iostream>
#include <thread>
#include <vector>
#include <mutex>
#include <condition_variable>

using namespace std;

int x = 0;
int num_iters;
mutex mtx;
condition_variable cv;

void
work(int flag)
{
  for(int i = 0; i < num_iters; i++)
    {
      unique_lock<mutex> lck (mtx);
      while (!(x % 2 == flag))
        {
          cv.notify_one();
          cv.wait(lck);
        }
      x++;
      cv.notify_one();
    }
}

int
main(int argc, char *argv[])
{
  num_iters = atoi(argv[1]);
  auto num_workers = atoi(argv[2]);
  auto workers = vector<thread*>();

  for(int i = 0; i < 2*num_workers; i++)
    {
      auto worker = new thread(work, i % 2);
      workers.push_back(worker);
    }

  for(auto worker : workers)
    {
      worker->join();
      delete worker;
    }

  cout << x << endl;
  
  return 0;
}
