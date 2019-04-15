// g++-7 optional.cc -std=c++17

#include <optional>
#include <iostream>

int main() {
  std::optional<int> x(std::nullopt_t);

  std::optional<int> y(std::make_optional(100));

  std::cout << *y << std::endl;
  
  return 0;
}

