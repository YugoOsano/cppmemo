// Compiled with:
// g++ -std=c++11 smart_pointer.cc 

// Reference:
// http://kaworu.jpn.org/cpp/std::unique_ptr

#include <memory>
#include <iostream>
#include <utility>

class Base{
public:
  Base() = default;
  Base(Base&& b) {
    std::cout << "move constructor of Base." << std::endl;
  }
  virtual ~Base(){}
};

class Derived : public Base {
public:
  Derived(Base&& b) : Base(std::move(b)) {
    std::cout << "move constructor of Derived." << std::endl;
  }
};

int main()
{
  const int size = 100;

  std::unique_ptr<int[]> p(new int[size]);

  p[10] = 7;
  std::cout << p[10] << std::endl;

  //--- move to unique pointer of derived
  std::unique_ptr<Base> ptr_base
    (std::make_unique<Base>());

  std::unique_ptr<Base> ptr_2
    = std::move(ptr_base);

  //  std::unique_ptr<Derived> ptr_derived
  //  = std::move(ptr_2);
  
  return 0;
}
