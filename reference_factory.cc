// polymorphism by reference
// factory pattern without memory new allocation (pointer)

#include <iostream>
#include <vector>

class Base {
public:
  virtual ~Base()=default;
  virtual void Print()const =0;

  //-- accessor to vector of different types
};

class DerivedA : public Base {
public:
  void Print() const override {
    std::cout << "DerivedA" << std::endl;
  }
protected:
  std::vector<int> int_data_; 
};

class DerivedB : public Base {
public:
  void Print() const override {
    std::cout << "DerivedB" << std::endl;
  }
protected:
  std::vector<double> double_data_;
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

//------
struct DataHolder {
  DataHolder () : ref_a_(data_a_),
		  ref_b_(data_b_){}
  Base& ref_a_;
  Base& ref_b_;
  
  DerivedA data_a_;
  DerivedB data_b_;
};
//-----
void ActionOnBase(Base& base) {
  base.Print();
}
//-----
template <typename T>
Base& CastToBase(T& any){return static_cast<Base&>(any);}

int main () {
  const Base& foo = Factory::CreateReference(true);
  foo.Print();

  const Base& bar = Factory::CreateReference(false);
  bar.Print();

  //-----
  DataHolder data_holder;
  ActionOnBase(data_holder.ref_a_);
  ActionOnBase(data_holder.ref_b_);

  ActionOnBase(static_cast<Base&>(data_holder.data_a_));
  ActionOnBase(CastToBase(data_holder.data_a_));

  return 0;
}
