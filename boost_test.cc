#include <vector>
#include <iostream>
#include <boost/range/algorithm.hpp>

void output(const int a) {
  std::cout << a << std::endl;
}
int main () {
  std::vector<int> alist{2,3,4};
  // second argument is an unary function
  // (single argument function)
  boost::for_each(alist, output);

  //-- lambda
  auto outputlambda = [](const int a) {
    std::cout << a << std::endl;
  };
  boost::for_each(alist, outputlambda);
  return 0;
}
