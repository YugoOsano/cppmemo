// https://onihusube.hatenablog.com/entry/2019/07/23/183851
#include <regex>
#include <iostream>
#include <string>
#include <sstream>
#include <cctype>

int main()
{
  {
    // R"(...)" represents raw string literal,
    // where escape sequence is ignored within ().
    //see https://cpprefjp.github.io/lang/cpp11/raw_string_literals.html
    std::string str = "1421, 34353, 7685, 12765, 976754";
    std::regex pattern{R"(\d+)"};
    std::smatch match{};
  
    while (std::regex_search(str, match, pattern)) {
      std::cout << match[0].str() << std::endl;
      const std::string& pre  = match.prefix();
      const std::string& last = match.suffix();
      str = pre + last;
      std::cout << "str: " << str << std::endl;
    }
  }
  {
    const std::string str = "1421, 34353, 7685, 12765, 976754";
    std::regex pattern{","};
    std::stringstream ss;
    ss << std::regex_replace(str, pattern, ".");
    std::cout << ss.str() << std::endl;
  }
  {
    const std::string coloring = "abcdefmf^[[30;00molkh";
    std::cout << coloring << std::endl;
    std::regex pattern_color{R"(\^\[\[(\d+))"};
    std::stringstream ss;
    ss << std::regex_replace(coloring, pattern_color, "");
    std::cout << ss.str() << std::endl;
  }
  {//-- simple parse by istringstream
    std::cout << "--------(isdigit)--" << std::endl;
    const std::string text("1 b 2 c 3 hello 23");
    std::istringstream iss(text);
    std::string tmp;
    while (iss >> tmp) {
      const char* ptr = tmp.c_str();
      std::cout << tmp << "\t(" << std::isdigit(*ptr) << ")" << std::endl;
    }
  }
  return 0;
}
// list of characters which need escape (by \)
// \ * + . ? {} () [] ^ $ - | /
// see https://qiita.com/katsukii/items/1c1550f064b4686c04d4
