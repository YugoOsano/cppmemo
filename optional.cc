// g++-7 optional.cc -std=c++17

// g++ optional.cc -I/usr/include/boost169 (on CentOS)

#include <optional>
#include <iostream>
#include <boost/optional.hpp>
#include <fstream>
#include <string>

int main() {
  std::optional<int> x(std::nullopt_t);

  std::optional<int> y(std::make_optional(100));

  std::cout << *y << std::endl;
  
  boost::optional<int> maybe_x = boost::none;

  //-- optional for file stream
  const bool is_output = false;//true;
  boost::optional<std::ofstream> maybe_ofs =
    [](const bool is_out)->boost::optional<std::ofstream>
    {
     if (!is_out) return boost::none;
     return std::move(std::ofstream("output.dat"));
    }(is_output);

  if (maybe_ofs != boost::none) {
    maybe_ofs.get() << "hello maybe ofs!" << std::endl;
  }
  return 0;
}

