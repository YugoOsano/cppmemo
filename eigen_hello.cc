// compile:
// g++ eigen_hello.cc -I[home]/software/eigen

// simple first program

#include <Eigen/Core>
#include <Eigen/Dense>
#include <iostream>

int main() {
  Eigen::MatrixXd m(2,2);
  m(0,0) = 3;
  m(1,0) = 2.5;
  m(0,1) = -1;
  m(1,1) = m(1,0) + m(0,1);

  std::cout << m << "\n" << std::endl;

  Eigen::Matrix<double,2,2> mattmp;
  Eigen::Matrix<double,2,1>   vectmp;
  
  mattmp << 3,2.5,-1,1.5;
  std::cout << mattmp << "\n" << std::endl;

  vectmp << 10,20;
  std::cout << vectmp << "\n" << std::endl;

  Eigen::Matrix<double,2,1>   vecproduct
    = mattmp * vectmp;
  std::cout << vecproduct << "\n" << std::endl;

  Eigen::Matrix<double,2,1> x_solution = mattmp.colPivHouseholderQr().solve(vecproduct);
  
  std::cout << x_solution << "\n" << std::endl;
  
  return 0;
}
