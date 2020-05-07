// transcribed from stackoverflow 44388110

#include <iostream>
#include <vector>
//#include <any>
#include <boost/any.hpp>

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
}
