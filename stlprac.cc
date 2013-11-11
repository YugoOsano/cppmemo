#include <iostream>
#include <vector>

int main(int argc, char *argv[])
{
  std::cout << "argc: " << argc <<
    "\nargv[0]: " << argv[0] << std::endl;

  //-- compare a std::vector with a native array --
  std::vector<double> x(10);
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
  std::vector<std::vector<double> > t(2, std::vector<double>(3));

  t[0][0] = 10.3;

  for(int i=0; i<2; i++)
    {
      for(int j=0; j<3; j++)
	{
	  std::cout << "t[" << i << "][" << j << "]: " 
		    << t[i][j] << "\t" << &t[i][j] << std::endl;
	}
    }

  return 0;
}
