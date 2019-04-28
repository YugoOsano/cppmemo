// g++-7 optional.cc -std=c++17

// g++ optional.cc -I/usr/include/boost169 (on CentOS)

#include <optional>
#include <iostream>
#include <boost/optional.hpp>

int main() {
  std::optional<int> x(std::nullopt_t);

  std::optional<int> y(std::make_optional(100));

  std::cout << *y << std::endl;
  
  boost::optional<int> maybe_x = boost::none;

  return 0;
}

