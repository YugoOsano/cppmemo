#include <iostream>
#include <boost/format.hpp>

template <int N>
struct Factorial
{
  enum { value = N * Factorial<N - 1>::value };
};

template <>
struct Factorial<0>
{
  enum { value = 1 };
};

//-- const practice --

const int N1 = 3;
int const N2 = 4;
//const int N3; <- initialization needed (error)

int const& Ptr = N1;

struct X 
{
  int n;
};

//--- option -std=c++0x needed 
// constexpr X xmember = {10};
// int a[xmember.n] = {1};

int main(int argc, char *argv[])
{
  std::cout << "Hello, World!" << std::endl;
  std::cout << boost::format("%s\n") % "Hello, Boost!";
  
  int x = Factorial<4>::value;
  int y = Factorial<0>::value;

  std::cout << boost::format("factorial(4): %s\tfactorial(0): %s\n") %
    x % y ;

  std::cout << "N1 = "  << N1  << std::endl;
  std::cout << "&Ptr = " << &Ptr << std::endl;

  return 0;
}
