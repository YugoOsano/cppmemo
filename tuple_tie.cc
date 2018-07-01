#include <tuple>
#include <iostream>

// tuple requires c++11 

std::tuple<int, bool> ReturnTuple() {
  return std::make_tuple(10, true);
}

int main () {

  int a,b;
  bool is1, is2;
  std::tie(a,is1) = ReturnTuple();

  std::cout << a << "," << is1 << std::endl;

  std::forward_as_tuple(b,is2) = ReturnTuple();

  std::cout << b << "," << is2 << std::endl;
  
  std::tie(a,is1) = std::forward_as_tuple(b,is2);

  std::forward_as_tuple(b,is2) = std::make_tuple(a,is1);

  std::forward_as_tuple(b,is2) = std::tie(a,is1);

  return 0;
}
