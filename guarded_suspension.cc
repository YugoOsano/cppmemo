//-- updated version of guarded suspension example at
//   https://melpon.hatenadiary.org/entry/20071230/p1
//   (design pattern for multithread) 
#include <string>
#include <memory>
#include <thread>
#include <queue>
#include <mutex>
#include <condition_variable>

class request {
private:
  const std::string name_;

public:
  request(std::string name)
    : name_(name) {}
  std::string name(){
    return name_;
  }
  std::string to_string(){
    return "[ Request " + name_ + " ]";
  }
};
class request_queue {
private:
  std::queue<std::shared_ptr<request>> queue_;
  std::mutex mutex_;
  std::condition_variable condition_;

public:
  std::shared_ptr<request> get_request(){
    std::unique_lock<std::mutex> lock(mutex_);

    while (queue_.empty()){
      condition_.wait(lock);
    }
    // ↓こんな風に書くことも出来る
    // condition_.wait(lock, !boost::bind(&std::queue<boost::shared_ptr<request> >::empty, &queue_));

    std::shared_ptr<request> r = queue_.front();
    queue_.pop();
    return r;
  }
  void put_request(std::shared_ptr<request> r){
    std::unique_lock<std::mutex> lock(mutex_);

    queue_.push(r);
    condition_.notify_all();
  }
};
int main () {
  return 0;
}

