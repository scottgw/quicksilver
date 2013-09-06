#include <cstdlib>
#include <iostream>
#include <thread>
#include <deque>
#include <vector>
#include <mutex>
#include <condition_variable>

using namespace std;

int x = 0;
int num_iters;
mutex mtx;
condition_variable cv;
deque<int> q;

void
producer()
{
  for(int i = 0; i < num_iters; i++)
    {
      unique_lock<mutex> lck (mtx);
      q.push_front(i);
      if (q.size() == 1)
        {
          cv.notify_one();
        }
    }
}

void
consumer()
{
  for(int i = 0; i < num_iters; i++)
    {
      unique_lock<mutex> lck (mtx);
      while (q.empty())
        {
          cv.wait(lck);
        }
      x = q.back();
      q.pop_back();
      if (q.size() > 0)
        {
          cv.notify_one();
        }
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
      auto worker = new thread(consumer);
      workers.push_back(worker);
    }

  for(int i = 0; i < num_workers; i++)
    {
      auto worker = new thread(producer);
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
