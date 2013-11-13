#include <iostream>
#include <vector>

//--- use std::vector as a member variable of a class ---
class SampleClass
{
public:
  std::vector<double> samplevec;

public:
  //-- constructors --
  SampleClass(){};
  SampleClass(int n_vecdim);
};

SampleClass::SampleClass(int n_vecdim)
{
  samplevec.assign(n_vecdim, 0.0);
}

//--- main ---
int main(int argc, char *argv[])
{
  std::cout << "argc: " << argc <<
    "\nargv[0]: " << argv[0] << std::endl;

  //-- compare a std::vector with a native array --
  std::vector<double> x;
  x.assign(10, 0.0);
  // above 2 lines are equivalent to 
  // std::vector<double> x(10, 0.0);

  double y[10];

  x[0] = 1.5;
  y[0] = 2.5;

  for(int i=0; i<10; i++)
    {
      std::cout << "x[" << i << "]: " << x[i] 
		<< "\t" << &x[i] << "\t" 
		<< "y[" << i << "]: " << y[i] 
		<< "\t" << &y[i] << std::endl;
    }

  //-- 2d vector --
  // http://stackoverflow.com/questions/2665936/is-there-a-way-to-specify-the-dimensions-of-a-nested-stl-vector-c
  std::vector<std::vector<double> > t;//(2, std::vector<double>(3));
  t.assign(2, std::vector<double>(3, 1.23));

  t[0][0] = 10.3;

  for(int i=0; i<2; i++)
    {
      for(int j=0; j<3; j++)
	{
	  std::cout << "t[" << i << "][" << j << "]: " 
		    << t[i][j] << "\t" << &t[i][j] << std::endl;
	}
    }

  std::cout << std::endl;
  //-- create an instance of the class --
  class SampleClass Instance1(6);

  std::cout << "Pointer of the instance: " << &Instance1 << std::endl;
  for(int i=0; i<6; i++)
    {
      std::cout << "Instance1.samplevec[" << i << "]: " 
		<< Instance1.samplevec[i] 
		<< "\t" << &Instance1.samplevec[i] << std::endl;
    }

  return 0;
}
