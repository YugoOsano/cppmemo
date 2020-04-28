#include <atomic>
#include <iostream>

int main () {
  
  std::atomic<int> atm(1234);
  std::cout << "atm: " << atm.load() << std::endl;

  atm = 4567;

  std::cout << "atm: " << atm.load() << std::endl;


  int  b   = 4567;
  int* ptr = &b;

  /* if object is equal to expected in byte level,
   * the object is replaced by desired. */
  bool is_equal =
    std::atomic_compare_exchange_weak(&atm,  // object
				      ptr,   // expected
				      1111); // desired
  std::cout << "is_equal: " << is_equal << std::endl;
  
  std::cout << "ptr: " << ptr << "\t&b: " << &b << std::endl;
  std::cout << "*ptr: " << *ptr << std::endl;
  std::cout << "atm: " << atm.load() << std::endl;  
  
  return 0;
}
/*
memory model coming with using std::atomic is well presented at:
https://stackoverflow.com/questions/6319146/c11-introduced-a-standardized-memory-model-what-does-it-mean-and-how-is-it-g

-> If the system requires atomicity but not ordering
  ((x,y)=(0,37) can be accepted below), 'memory_order_relaxed' works well
  to secure performance.

Global
atomic<int> x, y;

Thread 1                            Thread 2
x.store(17,memory_order_relaxed);   cout << y.load(memory_order_relaxed) << " ";
y.store(37,memory_order_relaxed);   cout << x.load(memory_order_relaxed) << endl;
*/
