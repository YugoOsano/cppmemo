// practice of flat_map
// compiler with -std=c++11

#include <boost/container/flat_map.hpp>
#include <utility>
#include <iostream>

int main () {

  boost::container::flat_map<int, int> map_sample;

  map_sample.emplace(std::make_pair(2,4));
  map_sample.emplace(std::make_pair(20,40));
  map_sample.emplace(std::make_pair(200,400));

  for(const auto& pair : map_sample)
    std::cout << pair.first << "\t"
	      << pair.second << std::endl;

  std::cout << map_sample.at(20) << std::endl;
  
  return 0;
}

