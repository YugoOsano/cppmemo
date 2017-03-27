#include <iostream>
#include <stdlib.h>
#include <boost/format.hpp>
//-- boost/format is a library for formatting of outputs -- 

//-- A tutorial for matrix handling by boost is:
// http://www.page.sannet.ne.jp/d_takahashi/boost/ublas/

// For Ubuntu, boost library will be installed with:
// sudo apt-get install 'libboost*-dev'
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

//-- example for variadic template (C++11) --
// http://cpplover.blogspot.jp/2010/03/variadic-templates.html
template <typename T>
T min( T const & a, T const & b )
{
  return a < b ? a : b ;
}
template <typename T, typename ... Types >
T min( T const & head, Types ... tail)
{
  return min(head, min( tail ...) );
}
// variadic template
//http://yuhsylphy.hateblo.jp/entry/20130609/1370716034
struct StructA {
  int ReturnHundred() { return 100; }
}; 

int InnerFunc(int a) {
  return a;
}
int InnerFunc(int a, StructA struct_a) {
  return a * struct_a.ReturnHundred();
}

template <class Type1, class... TypeSet>
int VariadicArgFunc(Type1 arg1, TypeSet... argset) {

  return arg1 * InnerFunc(argset...);
}

// variadic template in a class 
class OuterClass {
protected:
  int InnerFunc(int a) {
    return a;
  }
  int InnerFunc(int a, StructA struct_a) {
    std::cout << "member value: " << member_ << std::endl;
    return a * struct_a.ReturnHundred();
  }
  double member_;
public:
  OuterClass() : member_(2.345){}

  template <class Type1, class... TypeSet>
  int VariadicArgFunc(Type1 arg1, TypeSet... argset) {
    
    return arg1 * InnerFunc(argset...);
  }
};

//--- gain the size of an array ---
// http://ssa.techarts.co.jp/index.php?MPL%E3%81%A8%E3%81%AF

template <typename T, size_t N>
size_t CountOf(const T(&)[N]){
  return N;
}

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

  //-- variadic template --
  if(argc >= 2)
    {
      double v1 = 3131.5;
      double v2 = atof(argv[1]); //-- stdlib.h is for this --
      std::cout << min(v1, v2, 3135.0) << std::endl;
    }
  //-- array size --
  int aarray[1000];
  std::cout << "size of an array: " << CountOf(aarray) << std::endl;;  

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
  
  //--- variadic template ---
  StructA instanceA;

  std::cout << "Variadic: " << VariadicArgFunc(3, 5, instanceA)
	    << std::endl;

  OuterClass instance_outer;
  std::cout << "Variadic func in class: " 
	    << instance_outer.VariadicArgFunc(3, 5, instanceA)
	    << std::endl;

  return 0;
}
