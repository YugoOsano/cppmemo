#include <list>
#include <iostream>
#include <algorithm>

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
int main () {
  const std::list<int> list{-100, -300, 987, -567, 100, 256, -1298, 675};
  std::list<int> sorted_list;
  sorted_list = QuickSortFunctional(list, sorted_list);

  for (const int element : sorted_list)
    std::cout << element << std::endl;
  
  return 0;
}


