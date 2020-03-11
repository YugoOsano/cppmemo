// http://www.cplusplus.com/reference/algorithm/make_heap/

// range heap example
#include <iostream>     // std::cout
#include <algorithm>    // std::make_heap, std::pop_heap, std::push_heap, std::sort_heap
#include <vector>       // std::vector

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define HEAP_SIZE 10

/* max-heap property 
   https://www.codereading.com/algo_and_ds/ds/heap.html */
void insert(int val, int heap[], int counter) {
  int i = counter + 1;

  printf("insert called; counter: %d\ti: %d\n", counter, i);

  while ((i != 1) && (heap[i/2] < val)) {

    heap[i] = heap[i/2];
    printf("value %d transfered from index %d to %d\n", heap[i], (i/2), i); 
    i = i/2;
  }
  heap[i] = val;
  
}
void print_structure(int heap[], int start_index)
{
    for(int i = start_index; i < HEAP_SIZE; i++) {
        printf("parent [%d]: %d\n", i, heap[i]);
        /* left node */
        if (i*2 < HEAP_SIZE) {
            printf("\tchild L [%d]: %d\n", i*2, heap[i*2]);
        }
        /* right node */
        if (i*2+1 < HEAP_SIZE) {
            printf("\tchild R [%d]: %d\n", i*2+1, heap[i*2+1]);
        }
    }
}
//https://stackoverflow.com/questions/53161/find-the-highest-order-bit-in-c
int FindHighestOrderOfBit(int num) {
  if (!num) return 0;

  int ret = 1;
  while (num >>= 1)
    ret <<= 1;
  return ret;
}
void PrintHeapAsTree(const std::vector<int>& heap) {
  // skip 0th element
  std::vector<int>::const_iterator iter = heap.cbegin()+1;
  size_t distance_for_return = 2;
  while (iter != heap.cend()) {
    std::cout << *iter << " ";
    iter++;
    if (std::distance(heap.cbegin(), iter) ==
	distance_for_return) {
      std::cout << std::endl;
      distance_for_return <<= 1;//return at 2^n
    }
  }
  std::cout << std::endl;
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

  std::cout << "\n\n";

  //-----
  
  int heap[HEAP_SIZE];
  int val = 0; /* insert data */

  srand(getpid());

  int myints10[] = {10,20,30,5,15,25,2,7,12,18};
  
  for(int i = 0; i < HEAP_SIZE; i++) {
    /* random integer */
    val = myints10[i];//rand();
    
    insert(val, heap, i);
    printf("Insert [%d]: %d\n", i, val);
  }
  printf("\n");
  print_structure(heap, 1);

  //---- made from scratch ----
  std::cout << "--- hand made heap ---" << std::endl;
  std::vector<int> intvec10(myints10, myints10+10);
  PrintHeapAsTree(intvec10);
  
  return 0;
}
