#include <cstdlib>
#include <iostream>
#include <thread>
#include <vector>
#include <mutex>

using namespace std;

int x = 0;
int num_iters;
mutex mtx;

void
work()
{
  for(int i = 0; i < num_iters; i++)
    {
      lock_guard<mutex> lck (mtx);
      x++;
    }
}

int
main(int argc, char *argv[])
{
  num_iters = atoi(argv[1]);
  auto num_workers = atoi(argv[2]);
  auto workers = vector<thread*>();

  for(int i = 0; i < num_workers; i++)
    {
      auto worker = new thread(work);
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
