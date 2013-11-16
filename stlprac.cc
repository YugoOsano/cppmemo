#include <iostream>
#include <vector>

//-- prepare a class to use as a member of SampleClass --
// default constructor and copy constructor are needed to
// put an original class into std::vector
// http://d.hatena.ne.jp/satosystems/20110506/1304688718
class SubClass
{
public:
  int member;
  // initialize the member by colon(:)
  //http://d.hatena.ne.jp/higepon/20051107/1131335521
  SubClass() : member(0)
  {
    std::cout << "default" << std::endl;
  }
  //-- copy constructor --
  SubClass(const SubClass &rhs) : member(rhs.member)
  {
    std::cout << "copy constructor" << std::endl;
  }
  ~SubClass()
  {
    std::cout << "destructor" << std::endl;
  }
};

//--- use std::vector as a member variable of a class ---
// http://stackoverflow.com/questions/8553464/vector-as-a-class-member
class SampleClass
{
public:
  std::vector<double> samplevec;

  //-- nested class member --
  std::vector<class SubClass> classvec;

public:
  //-- constructors --
  SampleClass(){};
  SampleClass(int n_vecdim);
};

SampleClass::SampleClass(int n_vecdim)
{
  samplevec.assign(n_vecdim, 0.0);
  //  classvec.push_back(SubClass());
  classvec.assign(n_vecdim, SubClass());
}

//--- number of dimension --
const int NDIM  = 3;
const int NDIMX = 2;
const int NDIMY = 3;

//--- main ---
int main(int argc, char *argv[])
{
  std::cout << "argc: " << argc <<
    "\nargv[0]: " << argv[0] << std::endl;

  //-- compare a std::vector with a native array --
  std::vector<double> x;
  x.assign(NDIM, 0.0);
  // above 2 lines are equivalent to 
  // std::vector<double> x(10, 0.0);

  double y[NDIM];

  x[0] = 1.5;
  y[0] = 2.5;

  for(int i=0; i<NDIM; i++)
    {
      std::cout << "x[" << i << "]: " << x[i] 
		<< "\t" << &x[i] << "\t" 
		<< "y[" << i << "]: " << y[i] 
		<< "\t" << &y[i] << std::endl;
    }

  //-- 2d vector --
  // http://stackoverflow.com/questions/2665936/is-there-a-way-to-specify-the-dimensions-of-a-nested-stl-vector-c
  std::vector<std::vector<double> > t;//(2, std::vector<double>(3));
  t.assign(NDIMX, std::vector<double>(NDIMY, 1.23));

  t[0][0] = 10.3;

  for(int i=0; i<NDIMX; i++)
    {
      for(int j=0; j<NDIMY; j++)
	{
	  std::cout << "t[" << i << "][" << j << "]: " 
		    << t[i][j] << "\t" << &t[i][j] << std::endl;
	}
    }

  std::cout << std::endl;
  //-- create an instance of the class --
  class SampleClass Instance1(NDIM);

  std::cout << "Pointer of the instance: " << &Instance1 << std::endl;
  for(int i=0; i<NDIM; i++)
    {
      std::cout << "Instance1.samplevec[" << i << "]: " 
		<< Instance1.samplevec[i] 
		<< "\t" << &Instance1.samplevec[i] << std::endl;
      std::cout << "Instance1.classvec[" << i << "].member: "
		<< Instance1.classvec[i].member
		<< "\t" << &Instance1.classvec[i] << std::endl;
    }

  return 0;
}
