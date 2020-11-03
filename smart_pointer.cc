// Compiled with:
// g++ -std=c++11 smart_pointer.cc 

// Reference:
// http://kaworu.jpn.org/cpp/std::unique_ptr

#include <memory>
#include <iostream>
#include <utility>
#include <vector>
#include <map>

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
//-- capture a referenced object as a pointer member
class Capturing {
public:
  Capturing(const Base& base) :
    ptr_base_(&base) {}
protected:
  const std::unique_ptr<const Base> ptr_base_;
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

  //-- take reference
  std::unique_ptr<Base>& ref_ptr =  ptr_2;
  Base& ref_base = *ptr_2;
  Capturing capturing(ref_base);
  {
    std::unique_ptr<const Base> ptr_const
      = std::make_unique<const Base>();

    //ptr_const->SetValue(321.098);//-- compile error
    std::vector<decltype(ptr_const)> vec;
    vec.push_back(std::move(ptr_const));

    std::cout << vec.at(0)->GetValue() << std::endl;

    //-- take reference in a range-based for
    for(const std::unique_ptr<const Base>& ptr : vec) {
    }
  }
  {
    std::unique_ptr<const Base> ptr_const
      = std::make_unique<const Base>();
    std::map<size_t, std::unique_ptr<const Base>> map;
    map.emplace(std::make_pair(0, std::move(ptr_const)));
    for (const auto& pair : map) {
    }
  }
  return 0;
}
