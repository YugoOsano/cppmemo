// http://www.cplusplus.com/reference/algorithm/make_heap/

// range heap example
#include <iostream>     // std::cout
#include <algorithm>    // std::make_heap, std::pop_heap, std::push_heap, std::sort_heap
#include <vector>       // std::vector

/* max-heap property */
void insert(int val, int heap[], int counter) {
  int i = counter + 1;

  while ((i != 1) && (heap[i/2] < val)) {

    heap[i] = heap[i/2];
    i = i/2;
  }
  heap[i] = val;
}


int main () {
  int myints[] = {10,20,30,5,15};
  std::vector<int> v(myints,myints+5);

  for (int elem : v) std::cout << elem << ' ';
  std::cout << '\n';
  std::cout << "v.front() before make_heap : "
	    << v.front() << '\n';

  
  std::make_heap (v.begin(),v.end());
  for (int elem : v) std::cout << elem << ' ';
  std::cout << '\n';

  std::cout << "initial max heap (make_heap -> v.front())  : "
	    << v.front() << '\n';

  std::pop_heap (v.begin(),v.end()); v.pop_back();
  for (int elem : v) std::cout << elem << ' ';
  std::cout << '\n';


  std::cout << "max heap after pop : " << v.front() << '\n';

  v.push_back(99); std::push_heap (v.begin(),v.end());
  for (int elem : v) std::cout << elem << ' ';
  std::cout << '\n';

  
  std::cout << "max heap after push: " << v.front() << '\n';

  std::sort_heap (v.begin(),v.end());

  std::cout << "final sorted range :";
  for (unsigned i=0; i<v.size(); i++)
    std::cout << ' ' << v[i];

  std::cout << '\n';

  //-----
  int initial_int[] = {10,20,30,5,15};
  int *heap;
  for (int i=0; i<5; i++) {
    insert(initial_int[i], heap, i+1);

    for (int j=0; j<=i; j++) std::cout << heap[j] << ' ';
    std::cout << '\n';
  } 
  
  return 0;
}
