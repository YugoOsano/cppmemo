// basic of OpenMP
// http://codezine.jp/article/detail/4693
// Compile:
// g++ -fopenmp -std=c++11 openmphello.cc

#include <stdio.h>
#include <iostream>
#include <omp.h>
#include <vector>
#include <thread>
#include <map>

struct MapValue {
  MapValue(const int x) : x_(x){}
  int x_;
};

int main()
{
  auto n_core
    = std::thread::hardware_concurrency;
  std::cout << "The number of cores: " << n_core << std::endl;
  
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
  int sum;
#pragma omp parallel reduction(-:sum)
  {
#pragma omp parallel for
    for(int i = 0; i < 5; i++) {
      printf("index: %d, thread: %d\n", i, omp_get_thread_num());
      sum = sum + i;
      printf("sum: %d\n", sum);
    }
     printf("final sum: %d\n", sum);    
  }
  std::vector<int> vec{1,2,3,4};

  // omp for range based for <- error
  //#pragma omp parallel for
  for (int& elem : vec)
    printf("value: %d, thread: %d\n", elem, omp_get_thread_num());


  //#pragma omp parallel num_threads(8)
  {
    int num_threads =  omp_get_num_threads();
    printf("num_threads: %d\n", num_threads);

#pragma omp parallel for
    for (int i_all = 0; i_all < 8; i_all++) {
      printf("barrier test: thread: %d\n", omp_get_thread_num());
      if (omp_get_thread_num() == 0) {
	#pragma omp critical
	printf("critical section entered\n");
      }
    }
  }
  //--- parallelize loop over iterator ---
  {
    const std::map<size_t, MapValue> map_to_loop {
      {1, MapValue(5)},
      {10, MapValue(50)},
      {100, MapValue(500)},
      {1000, MapValue(5000)},
      {10000, MapValue(50000)},
      {100000, MapValue(500000)}};
    std::vector<decltype(map_to_loop)::const_iterator>
      iter_list;
    for (auto iter = map_to_loop.cbegin();
	 iter     != map_to_loop.cend(); iter++)
      iter_list.emplace_back(iter);
    const size_t listsize = iter_list.size();

#pragma omp parallel for
    for (size_t i = 0; i < listsize; i++) {
      printf("map value: %d, thread: %d\n",
	     iter_list[i]->second.x_,
	     omp_get_thread_num());
    }
  }
  return 0;
}
