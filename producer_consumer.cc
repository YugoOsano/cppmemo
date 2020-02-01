// transcribed from
// https://cutlassfish.wordpress.com/2016/09/11/c-%E3%81%A7-producer-consumer-%E3%83%91%E3%82%BF%E3%83%BC%E3%83%B3/

// g++ -pthread [this file]

// debugging mutex lock
// https://46dogs.blogspot.com/2012/06/debugging-mutex-locks-in-threaded.html

#include <vector>
#include <deque>
#include <string>
#include <mutex>
#include <thread>
#include <functional>
// A condition variable is an object able to block the calling thread until notified to resume.(cplusplus.com)
#include <condition_variable>
#include <iostream>
#include <cassert>

template <typename T>
class Queue {
public:
  Queue(int size) : size_(size) {}
  bool Put(T&& data) {
    if (size_ <= deque_.size()) return false;
    deque_.emplace_back(std::move(data));
    return true;
  }
  bool Get(T& data) {
    if (deque_.empty()) return false;

    data = std::move(deque_.front());
    deque_.pop_front();
    return true;
  }
  bool Empty() const {
    return deque_.empty();
  }
private:
  int           size_;
  std::deque<T> deque_;
};

// the constructor of std::thread is:
// template <class F, class ...Args>
// explicit thread(F&& f, Args&&... args);

class Printer {
public:
  // The consumer side (Printer) is multi-threaded
  explicit Printer(int threadCount, int queueSize) :
    isTerminationRequested_(false),
    queue_(queueSize) {
    for (int n=0; n<threadCount; n++) {
      threads_.emplace_back(std::thread(main_, n));
    }
  }
  ~Printer() {
    {
      std::unique_lock<std::mutex> ul(mutex_);
      isTerminationRequested_ = true;
    }
    cv_.notify_all();
    const int size = threads_.size();
    for (int n=0; n < size; n++) {
      threads_.at(n).join();
    }
  }
  bool Append(std::string&& str) {
    {
      std::unique_lock<std::mutex> ul(mutex_);
      if (!queue_.Put(std::move(str))) return false;
    }
    cv_.notify_all();
    return true;
  }
private:
  std::function<void(int)> main_ =
    [this](int num) {
      while(1) {
	std::string str;
	{
	  std::unique_lock<std::mutex> ul(mutex_);
	  while (queue_.Empty()) {
	    if (isTerminationRequested_) return;
	    cv_.wait(ul);
	  }
	  const bool result = queue_.Get(str);
	  assert(result);
	}
	std::cout << "num=" << num << " "
		  << str    << std::endl;
      }
    };
  bool                     isTerminationRequested_;
  Queue<std::string>       queue_;
  std::mutex               mutex_;
  std::condition_variable  cv_;
  std::vector<std::thread> threads_;
};

#include <chrono>
#include <sstream>

int main () {
  Printer printer(2,10);// thread count, queue size
  for (int i=0; i<100; i++) {
    std::stringstream ss;
    ss << "hello world " << i;
    while (!printer.Append(std::move(ss.str()))) {
      std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
  }	
  return 0;
}
// Note:
// strace -c [executable] lists system calls in summary;
// it presents clone() is used to create a new thread.
// see follows for thread
// https://postd.cc/raw-linux-threads-via-system-calls/
