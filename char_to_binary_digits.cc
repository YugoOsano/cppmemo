#include <vector>
#include <string>
#include <iostream>
#include <algorithm>

const std::vector<char> bit_set{
  static_cast<char>(1),
    static_cast<char>(2),
    static_cast<char>(4),
    static_cast<char>(8),
    static_cast<char>(16),
    static_cast<char>(32),
    static_cast<char>(64),
  static_cast<char>(128)
    };


std::string GetBinaryDigitsFromChar(const char x) {

  std::vector<char> bit_set_reverse(bit_set.begin(),
				    bit_set.end());
  std::reverse(bit_set_reverse.begin(),
	       bit_set_reverse.end());
  
  std::string to_return;
  for (const char bit_elem : bit_set_reverse) {

    std::string to_add =
      (bit_elem & x) ? "1" : "0";
    
    to_return = to_return + to_add;
  }
  return to_return;
}

int main () {
  std::cout << GetBinaryDigitsFromChar('a') << std::endl;
  return 0;
}
