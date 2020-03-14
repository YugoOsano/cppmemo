// https://riptutorial.com/cplusplus/example/26420/openmp--parallel-gathering---reduction
// Map Reduce practice with OpenMP
#include <vector>

int main () {
  //    The Master vector
  //    We want a vector of results gathered from slave threads
  std::vector<int> Master;    

  //    Hint the compiler to parallelize this { } of code
  //    with all available threads (usually the same as logical processor qty)
#pragma omp parallel
  {
    //    In this area, you can write any code you want for each
    //    slave thread, in this case a vector to hold each of their results
    //    We don't have to worry about how many threads were spawn or if we need
    //    to repeat this declaration or not.
    std::vector<int> Slave;

    //    Tell the compiler to use all threads allocated for this parallel region
    //    to perform this loop in parts. Actual load appx = 1000000 / Thread Qty
    //    The nowait keyword tells the compiler that the slave threads don't
    //    have to wait for all other slaves to finish this for loop job
    #pragma omp for nowait
    for (size_t i = 0; i < 1000000; ++i) {
      /* Do something */
      Slave.push_back(0);
    }
    //    Slaves that finished their part of the job
    //    will perform this thread by thread one at a time
    //    critical section ensures that only 0 or 1 thread performs
    //    the { } at any time
    #pragma omp critical
    {
      //    Merge slave into master
      //    use move iterators instead, avoid copy unless
      //    you want to use it for something else after this section
      Master.insert(Master.end(), 
		    std::make_move_iterator(Slave.begin()), 
		    std::make_move_iterator(Slave.end()));
    }
  }
  //    Have fun with Master vector
  return 0;
}
