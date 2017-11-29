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
