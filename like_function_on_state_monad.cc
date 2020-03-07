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

/*
GDB memo
Reload source file (to switch debugging target): simple use of
directory command
https://stackoverflow.com/questions/4118207/how-to-reload-source-files-in-gdb

variable printing in GDB:
info variables : static/global
info locals
info args
https://stackoverflow.com/questions/6261392/printing-all-global-variables-local-variables

info frame : to show stack frame
https://stackoverflow.com/questions/7848771/how-can-one-see-content-of-stack-with-gdb

info proc mappings : to show mapped memory
https://stackoverflow.com/questions/5691193/gdb-listing-all-mapped-memory-regions-for-a-crashed-process
 */

