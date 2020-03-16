#include <vector>
#include <algorithm>
#include <iostream>

void MergeSort(std::vector<int>::iterator iter_begin,
	       std::vector<int>::iterator iter_end) {
  const size_t size = std::distance(iter_begin,
				    iter_end);
  if (size == 1) return; // do nothing when the length is one

  const size_t half = size / 2;
  MergeSort(iter_begin,
	    iter_begin + half);
  MergeSort(iter_begin + half,
	    iter_end);

  std::vector<int> buffer;
  std::vector<int>::iterator iter1 = iter_begin;
  std::vector<int>::iterator iter2 = iter_begin + half;
  auto PushAndForward =
    [&buffer](std::vector<int>::iterator& iter)->void {
      buffer.push_back(*iter);
      iter++;
    };
  while(1) {
    // finish loop when the both iter reach the end
    if (iter1 == iter_begin + half &&
	iter2 == iter_end) break;
    else if (iter1 == iter_begin + half)
      PushAndForward(iter2);
    else if (iter2 == iter_end)
      PushAndForward(iter1);
    else
      PushAndForward((*iter1 > *iter2) ? iter1 : iter2);
  }
  std::copy(std::make_move_iterator(buffer.begin()),
	    std::make_move_iterator(buffer.end()),
	    iter_begin);
}
int main () {
  std::vector<int> list{-100, -300, 987, -567, 100, 256, -1298, 675};
  MergeSort(list.begin(), list.end());
  for (const int elem : list)
    std::cout << elem << ",";
  std::cout << std::endl;
  return 0;
}
