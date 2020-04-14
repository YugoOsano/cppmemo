// CRTP practice transcribed from
// https://qiita.com/Riyaaaa_a/items/a9af401520f238f45b80

#include <iostream>

template <class T>
class Interface {
public:
  void function() {static_cast<T&>(this)->function();}
};

class Derived1 : public Interface<Derived1> {
public:
  void function() {std::cout << "Derived1" << std::endl;}
};

class Derived2 : public Interface<Derived2> {
public:
};

int main () {
  Derived1 a;
  Derived2 b;
  a.function();
  //b.function();//error about invalid static_cast
  return 0;
}

