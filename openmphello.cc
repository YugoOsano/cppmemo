// basic of OpenMP
// http://codezine.jp/article/detail/4693
// Compile:
// g++ -fopenmp openmphello.cc

#include <stdio.h>
#include <iostream>
#include <omp.h>

int main()
{
  std::cout << "The number of processors is: " 
	    << omp_get_num_procs() << std::endl;

  omp_set_dynamic(0);
  std::cout << "omp_get_dynamic() = "
	    << omp_get_dynamic() << std::endl;

  //omp_set_num_threads(4);
#pragma omp parallel num_threads(4)
  {
    #pragma omp single
    {
      std::cout << "omp_get_num_threads() = "
		<< omp_get_num_threads() << std::endl;
      std::cout << "omp_get_max_threads() = "
		<< omp_get_max_threads() << std::endl;
    }
    printf("Hello, OpenMP! %d\n", omp_get_thread_num());
    printf("How about this code? %d\n", omp_get_thread_num());
  }
#pragma omp parallel for
  for(int i = 0; i < 5; i++)
    printf("index: %d, thread: %d\n", i, omp_get_thread_num());

  return 0;
}
