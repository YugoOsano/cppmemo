// transcribed from stackoverflow 44388110

#include <iostream>
#include <vector>
//#include <any>
#include <boost/any.hpp>
#include <stdio.h>

struct A
{
  int a;
  explicit operator int() const { return a; }
};

struct B
{
  double b;
  explicit operator double() const { return b; }
};

int main()
{
  A a{ 5 };
  B b{ 6.};

  //std::vector<std::any> v;
  std::vector<boost::any> v;
  v.push_back(3 );
  v.push_back(4.);
  v.push_back(a );
  v.push_back(b );

  for (auto const e : v)
    {
      if (e.type() == typeid(double))
	std::cout << boost::any_cast<double>(e) << std::endl;

      if (e.type() == typeid(B))
	std::cout << (double)boost::any_cast<B>(e) << std::endl;
    }
  {
    double x = 234.567;
    void* ptr = &x;
    printf("%p\n", ptr);
    unsigned long i = *static_cast<unsigned long*>(ptr);
    printf("%ld\n", i);
    void* ptr_i = &i;
    printf("%p\n", ptr_i);
    double x_back = *static_cast<double*>(ptr_i);
    printf("%f\n", x_back);
  }
}
