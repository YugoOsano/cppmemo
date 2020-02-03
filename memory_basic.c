#include <stdlib.h>

int main() {
  void* p = malloc(1000);
  free(p);
  return 0;
}
