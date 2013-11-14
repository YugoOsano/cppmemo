#include <iostream>
#include <boost/format.hpp>
//-- boost/format is a library for formatting of outputs -- 

//-- A tutorial for matrix handling by boost is:
// http://www.page.sannet.ne.jp/d_takahashi/boost/ublas/
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/io.hpp>
#include <complex>

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

  //--- handling complex number --
  std::complex<double> c1(0.0, 1.0);
  std::cout << "c1 = " << c1 << std::endl;

  //-------------------
  //  boost matrix
  //-------------------
  boost::numeric::ublas::matrix<double> matd(3,4), mat2(3,4); //lines, rows
  boost::numeric::ublas::matrix<std::complex<double> > matc(2,2);
  
  for(int i=0; i<3; i++)
    {
      for(int j=0; j<4; j++)
	{
	  matd(i,j) = 1.0;
	  mat2(i,j) = double(i) + 2.0*double(j);
	}
    }
  for(int i=0; i<2; i++)
    {
      for(int j=0; j<2; j++)
	{
	  //-- A complex element can't be directly substituted like this:
	  //   matc(i,j) = (1.0, -1.0);
	  matc(i,j) = c1;
	}
    }

  std::cout << "matd = " << matd << std::endl;
  std::cout << "matd + mat2 = " << matd + mat2 << std::endl;

  std::cout << "matc = " << matc << std::endl;
  return 0;
}
