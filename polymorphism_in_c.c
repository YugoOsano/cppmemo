#include <stdio.h>

typedef struct _classlike {
  void (*ptr_func_)();
} Classlike;

void Hello(){
  printf("Hello\n");
}
void Bye(){
  printf("Bye\n");
}

int main () {
  Classlike obj;
  obj.ptr_func_ = Hello;
  (*obj.ptr_func_)();
  obj.ptr_func_ = Bye;
  (*obj.ptr_func_)();
  return 0;
}
