//-- examine to create a function that returns
//   multiple values
//   
//   example is demonstrated by transformation from
//   cartatian coordinate to polar

#include<math.h>
#include<iostream>

struct pos_cartesian
{
  double x;
  double y;
};
struct pos_polar
{
  double r;
  double theta;
};

struct pos_polar cartesian2polar(struct pos_cartesian p)
{
  struct pos_polar ppol;
  ppol.r     = sqrt(p.x*p.x + p.y*p.y);
  ppol.theta = atan(p.y/p.x);
  return ppol;
}

int main()
{
  struct pos_cartesian xy;
  xy.x = 1.0;
  xy.y = 1.0;
  struct pos_polar rtheta;
  rtheta = cartesian2polar(xy);
  std::cout << "rtheta.r: " << rtheta.r 
	    << "\trtheta.theta: " << rtheta.theta << std::endl;
  return 0;
}
