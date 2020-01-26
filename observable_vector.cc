// transcribed from Modern C++ Challenge Q71
#include <vector>
#include <iostream>

enum class ActionList {
   add,
   remove,
   clear,
   assign
};

struct CollectionChangeNotification {
  ActionList          action_;
  std::vector<size_t> item_indices_;
};

class CollectionObserver {
public:
  virtual void CollectionChanged(
    CollectionChangeNotification notification)=0;
  virtual ~CollectionObserver()=default;
};

class Observer : public CollectionObserver {
public:
  virtual void CollectionChanged(
    CollectionChangeNotification notification) override {
    std::cout << "action: "
	      << static_cast<int>(notification.action_);
    if (!notification.item_indices_.empty()) {
      std::cout << ", indices: ";
      for (auto const i : notification.item_indices_)
	std::cout << i << ' ';
      std::cout << std::endl;
    }
  }
};

template <typename T>
class ObservableVector final {
public:
  void PushBack(T&& value) {
    data_.push_back(value);

    for (auto observer : observers_) {
      if (observer != nullptr) {
	observer->CollectionChanged({
	    ActionList::add,
	      std::vector<size_t>{data_.size() - 1}
	  });
      }
    }
  }
  void AddObserver(CollectionObserver* const observer) {
    observers_.push_back(observer);
  }
private:
  std::vector<T>                   data_;
  std::vector<CollectionObserver*> observers_;
};

int main () {
  ObservableVector<int> v;
  Observer observer;
  v.AddObserver(&observer);
  v.PushBack(1);
  
  return 0;
}
