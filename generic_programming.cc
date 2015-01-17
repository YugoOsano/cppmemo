// C++ template technique p-69
// sample program
// compiled with C++11 mode
// (g++ -std=c++0x generic_programming.cc)

#include <iostream>
#include <iterator>

/*
template <class T>
void selection_sort(T data[], int n)
{
  for (int i = 0; i < n; i++){
    int min_index = i;
    for (int j = i; j < n; j++){
      if (data[min_index] > data[j]){
	min_index = j;
      }
    }
    T tmp = data[min_index];
    data[min_index] = data[i];
    data[i] = tmp;
  }
}
*/
template <class T>
void selection_sort(T first, T last)
{
  for (T p = first; p != last; p++){
    T min_position = p;
    for (T q = p; q != last; q++){
      if (*min_position > *q){
	min_position = q;
      }
    }
    auto tmp = *min_position;
    *min_position = *p;
    *p = tmp;
  }
}


int main()
{
  int data[] = {9, 7, 5, 3, 1, 8, 6, 4, 2, 0 };
  //double data[] = {9.3, 7.2, 5.5, 3.2, 1.9, 8.4, 6.5, 4.3, 2.1, 0.23 };

  std::cout << "begin: " << std::begin(data)
	    << "\tend: " << std::end(data) << std::endl;
  std::cout << "*begin: " << *std::begin(data)
	    << "\t*end: " << *std::end(data)  << std::endl;

  //selection_sort(data, 10);
  selection_sort(std::begin(data), std::end(data));

  for (auto x : data){
    std::cout << x << ' ';
  }
  std::cout << std::endl;

  

  return 0;
}
