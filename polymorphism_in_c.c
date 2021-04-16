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
  Classlike obj={Hello};
  (*obj.ptr_func_)();
  Classlike bye={Bye};
  (*bye.ptr_func_)();
  return 0;
}
