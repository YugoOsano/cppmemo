// http://cpplover.blogspot.jp/2009/11/rvalue-reference_23.html
#include <iostream>
#include <algorithm>
#include <utility>
#include <atomic>

struct X{};

class Xclass
{
private:
  char *ptr;

public:
  Xclass()
  {
    ptr = new char[1000];
    std::cout << "constructor" << std::endl;
  }
  Xclass( Xclass const & r )
  {
    ptr = new char[1000];
    std::copy( &ptr[0], &ptr[1000], &r.ptr[0] );
    std::cout << "copy constructor" << std::endl;
  }
  Xclass( Xclass && r )
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

int h(int&& x){
  return x * 2;
}

int main ()
{
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

  std::cout << "&x2: " << &x2 << std::endl;
  std::cout << "&x4: " << &x4 << std::endl;

  int&& a = 10;

  std::cout << "a: " << a << std::endl; 
  std::cout << "h(a): " << h(static_cast<int&&>(a)) << std::endl; 
  std::cout << "h(a): " << h(std::move(a)) << std::endl; 

  // -- atomic --
  std::atomic<int> ai0{ 0 } ;

  ai0 = 1;

  return 0;
}
