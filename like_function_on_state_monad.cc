#include <utility>

struct A{};

A ReturnA(A& a) {
  //--- some manipulation on a

  return std::move(a);//-- 0x7ffffffee3a7
}
//-- output of (p &a) in gdb
int main() {
  A a;  //-- 0x7ffffffee3a7
  a = ReturnA(a);

  return 0; //-- 0x7ffffffee3a7
}


