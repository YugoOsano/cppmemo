// polymorphism by reference
// factory pattern without memory new allocation (pointer)

#include <iostream>

class Base {
public:
  virtual ~Base()=default;
  virtual void Print()const =0;
};

class DerivedA : public Base {
public:
  void Print() const override {
    std::cout << "DerivedA" << std::endl;
  }
};

class DerivedB : public Base {
public:
  void Print() const override {
    std::cout << "DerivedB" << std::endl;
  }
};

class Factory {
public:
  static Base& CreateReference(const bool is_a) {
    if(is_a)
      return static_cast<Base&>(Factory::derived_a_);

    return  static_cast<Base&>(Factory::derived_b_);
  }
protected:
  static DerivedA derived_a_;
  static DerivedB derived_b_;
};

DerivedA Factory::derived_a_ = DerivedA();
DerivedB Factory::derived_b_ = DerivedB();

    
int main () {
  const Base& foo = Factory::CreateReference(true);
  foo.Print();

  const Base& bar = Factory::CreateReference(false);
  bar.Print();

  return 0;
}
