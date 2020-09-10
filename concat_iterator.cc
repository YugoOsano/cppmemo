// see https://stackoverflow.com/questions/757153/concatenating-c-iterator-ranges-into-a-const-vector-member-variable-at-constru
#include <set>
#include <vector>
#include <utility>

class ConcatIterator {
public:
  ConcatIterator() :
    iter_pair_list_({}),
    begin_(nullptr),
    end_(nullptr) {}
  void PushBack(const std::set<size_t>::const_iterator begin,
		const std::set<size_t>::const_iterator end) {
    iter_pair_list_.push_back(std::make_pair(begin, end));
    if (iter_pair_list_.size() == 0) {
      begin_ = begin;
    }
    end_ = end;
    iter_pair_list_.push_back(std::make_pair(begin, end));
  }
  void Initialize() {
    current_       = begin_;
    current_range_ = iter_pair_list_.cbegin();
  }
  bool Forward() {
    if (current_ != current_range_->second) {
      current_ = std::next(current_);
      return true;
    }
    if (std::next(current_range_) == iter_pair_list_.cend())
      return false;
    current_range_++;
    current_ = current_range_->first;
  }
  const std::set<size_t>::const_iterator& Begin() const {
    return begin_;
  }
  const std::set<size_t>::const_iterator& End() const {
    return end_;
  }
  
private:
  std::vector<std::pair<std::set<size_t>::const_iterator,
			std::set<size_t>::const_iterator>> iter_pair_list_;
  std::set<size_t>::const_iterator begin_;
  std::set<size_t>::const_iterator end_;

  std::set<size_t>::const_iterator          current_;
  decltype(iter_pair_list_)::const_iterator current_range_;
};

int main() {
  std::set<size_t> example {0,1,2,3,4,5};
  ConcatIterator coniter;
  coniter.PushBack(example.cbegin(),
		   example.cend());
  coniter.Initialize();
  // for (auto iter = coniter.Begin();
  //      iter     != coniter.End(); iter.Forward()) {
  // // }
  return 0;
}
