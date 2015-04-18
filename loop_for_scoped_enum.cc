// http://stackoverflow.com/questions/8498300/allow-for-range-based-for-with-enum-classes
// compile: g++ -std=c++11 loop_for_scoped_enum

#include <type_traits>
#include <iostream>

enum class COLOR
{
    Blue,
    Red,
    Green,
    Purple,
    First=Blue,
    Last=Purple
};

COLOR operator++(COLOR& x) { 
  return x = (COLOR)(std::underlying_type<COLOR>::type(x) + 1); 
}
COLOR operator*(COLOR c) {return c;} 
COLOR begin(COLOR r) {return COLOR::First;}
COLOR end(COLOR r)   {return COLOR::Last;}

int main(){

  for(const auto& c : COLOR()){
    std::cout << static_cast<int>(c) << std::endl;
  }

  //-- same output as the above --
  for(auto c = begin(COLOR()); 
      c     != end(COLOR());
      ++c) {
    std::cout << static_cast<int>(c) << std::endl;
  }
  return 0;
}
