// http://cpplover.blogspot.jp/2009/11/rvalue-reference_23.html
// https://qiita.com/go_astrayer/items/5d85565e992487daa618
#include <iostream>
#include <algorithm>
#include <utility>
#include <atomic>
#include <memory>

struct X{
  X() : member_(100){}
  int member_;
};
struct Y{
  Y() : member_(100){}
  Y(const Y&)=delete;
  int member_;
};

class Xclass
{
private:
  char *ptr;
public:
  int  memint_;

  Xclass() : memint_(23)
  {
    ptr = new char[1000];
    std::cout << "constructor" << std::endl;
  }
  Xclass( Xclass const & r ) : memint_(45)
  {
    ptr = new char[1000];
    std::copy( &ptr[0], &ptr[1000], &r.ptr[0] );
    std::cout << "copy constructor" << std::endl;
  }
  Xclass( Xclass && r )  : memint_(67)
  {
    ptr   = r.ptr ;
    r.ptr = nullptr ;
    std::cout << "move constructor" << std::endl;
  }
  ~Xclass()
  {
    delete [] ptr;
    std::cout << "destructor" << std::endl;
  }
};

void f(X &) {}
void g(X const &) {}

template <typename T>
void template_f(T && t)
{
  X x(std::forward<T>(t));
}

int h_two_times(const int&& x){
  return x * 2;
}

X ReturnX() {
  X xinstance;
  // std::move is equivalent to a cast to T&&
  // https://stackoverflow.com/questions/52104649/c11-rvalue-reference-vs-const-reference
  //return std::move(xinstance);
  return static_cast<X&&>(xinstance);
}

// Discussion on ownership in C++ and Rust
//https://softwareengineering.stackexchange.com/questions/318078/possible-to-achieve-rusts-ownership-model-with-a-generic-c-wrapper
//-- factory function to pass a unique pointer with instantiation
std::unique_ptr<X> CreateX() {
  std::unique_ptr<X> ptr
    = std::make_unique<X>();
  return std::move(ptr);
}
// function to avoid returning values by an argument
Y&& UpdateMember(Y&& y) {
  y.member_ += 1;
  return std::move(y);
}
void MainForY() {
  std::unique_ptr<Y> ptry(std::make_unique<Y>());
  Y&& refy1(UpdateMember(std::move(*ptry)));
  Y&& refy2(UpdateMember(std::move(refy1)));
  std::cout << "Y::member is " << refy2.member_ << std::endl;
}
int main ()
{
  const X&& rref_x = ReturnX();
  
  X x;

  f(x);
  //f(X());// error
  g(X());

  // lvalue
  X & lr1 = x;
  //X & lr2 = X(); //error

  // rvalue
  //X && rr1 = x; //error
  X && rr1 = X();
  
  int azero = 0;
  auto&& y = azero; // no error

  template_f(x);
  template_f(X()); //no error

  Xclass x2;
  Xclass x3 = x2;

  std::cout << "&x2: " << &x2 << std::endl;

  Xclass x4 = std::move(x2);
  //Xclass x5 = x2;// Segmentation fault

  Xclass&& rref_xclass = Xclass();
  
  std::cout << "&x2: " << &x2 << std::endl;
  std::cout << "&x4: " << &x4 << std::endl;

  int&& a = 10;

  std::cout << "a: " << a << std::endl; 
  std::cout << "h_two_times(a): " << h_two_times(static_cast<int&&>(a)) << std::endl; 
  std::cout << "h_two_times(a): " << h_two_times(std::move(a)) << std::endl; 

  std::cout << "rref_xclass.memint_: " << rref_xclass.memint_ << std::endl;

  //Xclass x6(rref_xclass);  // -- this will call copy constructor
  Xclass x6(static_cast<Xclass&&>(rref_xclass));

  std::cout << "rref_xclass.memint_: " << rref_xclass.memint_ << std::endl;
  
  // -- atomic --
  std::atomic<int> ai0{ 0 } ;
  ai0 = 1;

  MainForY();
  return 0;
}
