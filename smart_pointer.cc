// Compiled with:
// g++ -std=c++11 smart_pointer.cc 

// Reference:
// http://kaworu.jpn.org/cpp/std::unique_ptr

#include <memory>
#include <iostream>
#include <utility>
#include <vector>

class Base{
public:
  Base() : value_(123.456){}
  Base(Base&& b) : Base() {
    std::cout << "move constructor of Base." << std::endl;
  }
  virtual ~Base(){}

  void SetValue(const double value) {
    value_ = value;
  }
  double GetValue() const {return value_;}
protected:
  double value_;
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

  ptr_2->SetValue(98.76);
  //  std::unique_ptr<Derived> ptr_derived
  //  = std::move(ptr_2);

  std::unique_ptr<const Base> ptr_const
    = std::make_unique<const Base>();

  //ptr_const->SetValue(321.098);//-- compile error
  std::vector<decltype(ptr_const)> vec;
  vec.push_back(std::move(ptr_const));

  std::cout << vec.at(0)->GetValue() << std::endl;
  return 0;
}
