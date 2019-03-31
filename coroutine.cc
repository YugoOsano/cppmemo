//https://theboostcpplibraries.com/boost.coroutine

// to run on Ubuntu:
// sudo apt-get install libboost-all-dev
// is needed;
// to compile this:
// g++ -std=c++11 -lboost_system

// to find boost path:
// $ whereis boost


#include <boost/coroutine2/all.hpp>
#include <boost/version.hpp>
#include <iostream>

// Boost.Coroutine still has an issue on version
// the platform needs fcontext or ucontext properties
// void cooperative(boost::coroutines2::coroutine<void>::push_type& sink) {
//   std::cout << "Hello";
//   sink();
//   std::cout << "World";
// }

int main () {

  std::cout << BOOST_VERSION << std::endl;
  
  return 0;
}
