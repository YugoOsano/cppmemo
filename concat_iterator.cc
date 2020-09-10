// see https://stackoverflow.com/questions/757153/concatenating-c-iterator-ranges-into-a-const-vector-member-variable-at-constru
#include <set>
#include <vector>
#include <utility>
#include <iostream>

class ConcatIterator {
public:
  ConcatIterator() :
    iter_pair_list_({}),
    begin_(nullptr) {}
  //    end_(nullptr) {}
  void PushBack(const std::set<size_t>::const_iterator begin,
		const std::set<size_t>::const_iterator end) {
    if (iter_pair_list_.size() == 0) {
      begin_ = begin;
    }
    //    end_ = end;
    iter_pair_list_.push_back(std::make_pair(begin, end));
  }
  void Initialize() {
    current_       = begin_;
    current_range_ = iter_pair_list_.cbegin();
  }
  void Forward() {
    if (current_ == iter_pair_list_.crbegin()->second) return;
    if (current_ == current_range_->second) {
      current_range_++;
      current_ = current_range_->first;
      return;
    }
    current_++;
  }
  // const std::set<size_t>::const_iterator& Begin() const {
  //   return begin_;
  // }
  const std::set<size_t>::const_iterator& End() const {
    return iter_pair_list_.crbegin()->second;
  }
  const std::set<size_t>::const_iterator& Current() const {
    return current_;
  }
private:
  std::vector<std::pair<std::set<size_t>::const_iterator,
			std::set<size_t>::const_iterator>> iter_pair_list_;
  std::set<size_t>::const_iterator begin_;
  //  std::set<size_t>::const_iterator end_;

  std::set<size_t>::const_iterator          current_;
  decltype(iter_pair_list_)::const_iterator current_range_;
};

int main() {
  std::set<size_t> example {0,1,2,3,4,5};
  std::set<size_t> example2 {10,11,12,13,14,15};

  ConcatIterator coniter;
  coniter.PushBack(example.cbegin(),
		   example.cend());
  coniter.PushBack(example2.cbegin(),
		   example2.cend());
  for (coniter.Initialize() ;coniter.Current() != coniter.End();
       coniter.Forward()) {
    std::cout << *coniter.Current() << ", ";
  }
  std::cout << std::endl;
  return 0;
}
