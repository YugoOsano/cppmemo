#include "separate_template.h"

int main() {

  int a = 5;
  double b = 55.5;

  func(a);
  func(b);

  ClassA<int>    insA;
  ClassA<double> insB;

  insA.get();
  insB.get();

  return 0;
}
