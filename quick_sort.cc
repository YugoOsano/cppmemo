#include <list>
#include <iostream>
#include <algorithm>
#include <vector>

template <typename T>
void QuickSort(const std::list<T>& list) {
  if (list.size() == 0) return;
  if (list.size() == 1) {
    std::cout << list.front() << std::endl;
    return;
  }
  const auto& minmax = std::minmax_element(list.cbegin(),
					   list.cend());
  const T middle = (*minmax.first + *minmax.second) / 2;
  std::list<T> upper_list, lower_list;

  for (const T element : list) {
    if (element > middle) upper_list.push_back(element);
    else                  lower_list.push_back(element);
  }
  QuickSort(upper_list);
  QuickSort(lower_list);
}
//-- functional version --
template <typename T>
std::list<T> QuickSortFunctional(const std::list<T>& list,
				 std::list<T>&       sorted_list) {
  if (list.size() == 0) return std::move(sorted_list);
  if (list.size() == 1) {
    sorted_list.push_back(list.front());
    return std::move(sorted_list);
  }
  using IterType = decltype(list.cbegin());
  const std::pair<IterType, IterType>& minmax
             = std::minmax_element(list.cbegin(),
				   list.cend());
  const T middle = (*minmax.first + *minmax.second) / 2;
  std::list<T> upper_list, lower_list;

  for (const T element : list) {
    if (element > middle) upper_list.push_back(element);
    else                  lower_list.push_back(element);
  }
  sorted_list = QuickSortFunctional(upper_list,
				    sorted_list);
  sorted_list = QuickSortFunctional(lower_list,
				    sorted_list);
  return std::move(sorted_list);
}
//-- wrapper class to prevent copy
//   ref: https://stackoverflow.com/questions/19826376/insert-into-vector-having-objects-without-copy-constructor
template <typename T>
struct SortedList {
  SortedList()=default;
  SortedList(const SortedList&)=delete;
  SortedList& operator=(const SortedList&)=delete;
  SortedList(SortedList&&) noexcept =default;
  SortedList& operator=(SortedList&&) noexcept=default;
  std::list<T> list_;
};
template <typename T>
SortedList<T> QuickSortWrappedClass(
		const std::list<T>& list,
		SortedList<T>&      sorted_list) {
  if (list.size() == 0) return std::move(sorted_list);
  if (list.size() == 1) {
    sorted_list.list_.push_back(list.front());
    return std::move(sorted_list);
  }
  using IterType = typename std::list<T>::const_iterator;
  const std::pair<IterType, IterType>& minmax
              = std::minmax_element(list.cbegin(),
				    list.cend());
  const T middle = (*minmax.first + *minmax.second) / 2;
  std::list<T> upper_list, lower_list;

  for (const T element : list) {
    if (element > middle) upper_list.push_back(element);
    else                  lower_list.push_back(element);
  }
  SortedList<T> sorted_list_update0
              = QuickSortWrappedClass(upper_list,
				      sorted_list);
  SortedList<T> sorted_list_update1
              = QuickSortWrappedClass(lower_list,
				      sorted_list_update0);
  return std::move(sorted_list_update1);
}

//-- version of using itertator to prevent new vector construction
//   (reference stlprac.cc)
template <typename T>
void QuickSortIter(typename std::vector<T>::iterator iter_begin,
		   typename std::vector<T>::iterator iter_end) {
  const size_t distance = std::distance(iter_begin,
					iter_end);
  if (distance == 0) return;
  if (distance == 1) {
    std::cout << *iter_begin << std::endl;
    return;
  }
  const std::pair<typename std::vector<T>::iterator,
		  typename std::vector<T>::iterator>& minmax
    = std::minmax_element(iter_begin,
			  iter_end);
  const int middle = (*minmax.first + *minmax.second) / 2;

  typename std::vector<T> buffer(distance);
  typename std::vector<T>::iterator         buffer_front = buffer.begin();
  typename std::vector<T>::reverse_iterator buffer_back  = buffer.rbegin();
  for (auto iter = iter_begin; iter != iter_end; iter++) {
    if (*iter > middle) {
      *buffer_front = *iter; buffer_front++;
    }
    else {
      *buffer_back = *iter; buffer_back++;
    }
  }
  std::copy(buffer.begin(), buffer.end(),
	    iter_begin);
  const size_t upper_size = std::distance(buffer.begin(),
					  buffer_front);
  QuickSortIter<T>(iter_begin, iter_begin + upper_size);
  QuickSortIter<T>(iter_begin + upper_size, iter_end);
}

int main () {
  const std::list<int> list{-100, -300, 987, -567, 100, 256, -1298, 675};
  std::list<int> sorted_list;
  sorted_list = QuickSortFunctional(list, sorted_list);

  for (const int element : sorted_list)
    std::cout << element << std::endl;

  SortedList<int>        sorted_list_initial;
  const SortedList<int>& sorted_list_wrapped
     = QuickSortWrappedClass(list, sorted_list_initial);

  for (const int element : sorted_list_wrapped.list_)
    std::cout << element << std::endl;

  std::vector<int> vector(list.begin(), list.end());
  std::cout << "--- QuickSortIter ---" << std::endl;
  QuickSortIter<int>(vector.begin(),
		     vector.end());
  return 0;
}


