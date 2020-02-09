//-- updated version of guarded suspension example at
//   https://melpon.hatenadiary.org/entry/20071230/p1
//   (design pattern for multithread) 
#include <string>
#include <memory>
#include <thread>
#include <queue>
#include <mutex>
#include <condition_variable>
#include <chrono>
#include <random>
#include <iostream>

struct thread_helper
{
  static void sleep(std::size_t ms){
    std::this_thread::sleep_for(std::chrono::milliseconds(ms));
  }
  // take a mutex also at output
  template<class T>
  static void shared_cout(const T& value)
  {
    static std::mutex mutex;

    std::unique_lock<std::mutex> lock(mutex);
    std::cout << value;
  }
};

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
class client_thread {
private:
  const std::string name_;
  const std::shared_ptr<request_queue> request_queue_;
  std::mt19937 mt_;
  const size_t n_repeat_;

public:
  client_thread(std::shared_ptr<request_queue> rq,
		std::string                    name,
		size_t                         seed,
		const size_t                   n_repeat)
    : name_(name), request_queue_(rq), mt_(seed),
      n_repeat_(n_repeat) {}
  void run(){
    std::uniform_int_distribution<> random(0,1000);
    for (int i = 0; i < n_repeat_; i++) {
      std::shared_ptr<request> r = request_queue_->get_request();
      thread_helper::shared_cout(name_ + " handles " +
				 r->to_string() + "\n");
      thread_helper::sleep(random(mt_));
    }
  }
};
class server_thread {
private:
  const std::string name_;
  const std::shared_ptr<request_queue> request_queue_;
  std::mt19937 mt_;
  const size_t n_repeat_;

public:
  server_thread(std::shared_ptr<request_queue> rq,
		std::string                    name,
		size_t                         seed,
		const size_t                   n_repeat)
    : name_(name), request_queue_(rq), mt_(seed),
      n_repeat_(n_repeat){}
  void run(){
    std::uniform_int_distribution<> random(0,1000);
    for (int i = 0; i < n_repeat_; i++)
      {
	std::shared_ptr<request> r(new request("No." + std::to_string(i)));
	thread_helper::shared_cout(name_ + " requests " +
				   r->to_string() + "\n");
	request_queue_->put_request(r);
	thread_helper::sleep(random(mt_));
      }
  }
};

int main () {
  const size_t n_repeat = 15;
  std::shared_ptr<request_queue> rq(new request_queue());

  // no longer exists equivalent to boost::thread_group
  //https://stackoverflow.com/questions/9894263/boostthread-group-in-c11
  std::thread client_th(
       &client_thread::run,
       std::make_shared<client_thread>(rq, "Alice", 3141592, n_repeat));
  std::thread server_th(
       &server_thread::run,
       std::make_shared<server_thread>(rq, "Bobby", 6535897, n_repeat));
  client_th.join();
  server_th.join();
  return 0;
}

