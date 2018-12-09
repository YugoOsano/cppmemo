// polymorphism by reference
// factory pattern without memory new allocation (pointer)

#include <iostream>
#include <vector>
//--------
struct VectorWrapBase {
};
template <typename T>
struct VectorWrap : public VectorWrapBase {

  T& operator[](size_t i) {return *(vector_.at(i));}
  std::vector<T*> vector_;
};

//---------
class Base {
public:
  virtual ~Base()=default;
  virtual void Print()const =0;
};

template <typename T=int>
class DerivedA : public Base {
public:
  void Print() const override {
    std::cout << "DerivedA" << std::endl;
  }
};

template <typename T=int>
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
  static DerivedA<> derived_a_;
  static DerivedB<> derived_b_;
};

DerivedA<> Factory::derived_a_ = DerivedA<>();
DerivedB<> Factory::derived_b_ = DerivedB<>();

//------
struct DataHolder {
  DataHolder () : ref_a_(data_a_),
		  ref_b_(data_b_){}
  Base& ref_a_;
  Base& ref_b_;
  
  DerivedA<> data_a_;
  DerivedB<> data_b_;

  VectorWrap<int>    int_data_;
  VectorWrap<double> double_data_;
};
//-----
void ActionOnBase(Base& base) {
  base.Print();
}
template <typename T>
Base& CastToBase(T& any){return static_cast<Base&>(any);}
//----
int ActionOnIntVector(VectorWrapBase& vector_base, size_t index) {
  return static_cast<VectorWrap<int>&>(vector_base)[index];
}


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
