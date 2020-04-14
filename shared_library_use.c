// compile
// gcc -I./ -L./ shared_library_use.c -lsharedlib

// before run a.out:
// export LD_LIBRARY_PATH=./:$LD_LIBRARY_PATH

#include <stdio.h>

int main() {
  printf("add(10, 20): %d\n", add(10,20));
  return 0;
}
