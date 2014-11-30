// Compiled with:
// g++ -std=c++0x smart_pointer.cc 

// Reference:
// http://kaworu.jpn.org/cpp/std::unique_ptr

#include <memory>
#include <iostream>

int main()
{
  const int size = 100;

  std::unique_ptr<int[]> p(new int[size]);

  p[10] = 7;
  std::cout << p[10] << std::endl;
  

  return 0;
}
