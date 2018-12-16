// install of c++17 (g++-7):
// https://qiita.com/forno/items/a877c3fd9d2056217042
// compile:
// g++-7 -std=c++1z structured_bindings.cc

#include <tuple>
#include <string>
#include <iostream>

std::tuple<int, std::string> ReturnTuple() {
  return std::make_tuple(int (3), std::string("Hello"));
}

int main () {
  const auto [a, s] = ReturnTuple();
  std::cout << a << ", " << s << std::endl;
  return 0;
}
