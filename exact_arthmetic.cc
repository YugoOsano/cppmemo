#include <iostream>

int main() {

  double a_pre = 10.0 / 3.0;
  double* a = &a_pre;

  //int i = static_cast<int>(a);
  long int* i = reinterpret_cast<long int *>(a);

  std::cout << "*i: " << *i << std::endl;

  a = reinterpret_cast<double *>(i);

  std::cout << "*a: " << *a << std::endl;

  return 0;
}
