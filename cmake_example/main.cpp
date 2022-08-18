#include <iostream>
#include <cassert>
#include "func.hpp"

int main() {
  std::cout << "Hello, CMake!" << std::endl;
  func();

  // assert statements are for ctest
  assert(true);
  // assert(false); // ctest will end with fail
  return 0;
}
