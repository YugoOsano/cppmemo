// https://en.wikipedia.org/wiki/Bitonic_sorter
// C++ version of the example code in Python

#include <vector>
#include <utility>
#include <iostream>

void BitonicCompareAndSwap(
		    const bool           is_up,
		    std::vector<double>& array) {
  const size_t dist = array.size() / 2;
  for (size_t i = 0; i < dist; i++) {
    if ((array[i] > array[i + dist]) == is_up) {
      std::swap(array[i], array[i + dist]);
    }
  }
}

std::vector<double> BitonicMerge(const bool           is_up,
				 std::vector<double>& array) {
  if (array.size() == 1) return array;

  const size_t half_size = array.size() / 2;
  BitonicCompareAndSwap(is_up, array);
  std::vector<double> initial_half(array.begin(),
				   array.begin() + half_size);
  std::vector<double> initial_half_recurred
    = BitonicMerge(is_up, initial_half);

  std::vector<double> latter_half(array.begin() + half_size,
				  array.end());
  std::vector<double> latter_half_recurred
    = BitonicMerge(is_up, latter_half);
  initial_half_recurred.insert(initial_half_recurred.end(),
			       latter_half_recurred.begin(),
			       latter_half_recurred.end());  
  return initial_half_recurred;
}

std::vector<double> BitonicSort(const bool           is_up,
				std::vector<double>& array) {
  if (array.size() <= 1) return array;
  const size_t half_size = array.size() / 2;

  std::vector<double> initial_half(array.begin(),
				   array.begin() + half_size);
  std::vector<double> initial_half_recurred
    = BitonicSort(true /* is_up */, initial_half);

  std::vector<double> latter_half(array.begin() + half_size,
				  array.end());
  std::vector<double> latter_half_recurred
    = BitonicSort(false /* is_up */, latter_half);
  initial_half_recurred.insert(
	  initial_half_recurred.end(),
	  std::make_move_iterator(latter_half_recurred.begin()),
	  std::make_move_iterator(latter_half_recurred.end()));
  return std::move(BitonicMerge(is_up, initial_half_recurred));
}

int main () {
  std::vector<double> list{10, 30, 11, 20, 4, 330, 21, 110};
  std::vector<double> sorted = BitonicSort(true, list);

  for (const double value : sorted)
    std::cout << value << " ";
  std::cout << std::endl;
  return 0;
}
