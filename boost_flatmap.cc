// practice of flat_map
// compiler with -std=c++11

// bimap added

#include <boost/container/flat_map.hpp>
#include <boost/bimap/bimap.hpp>
#include <iostream>
#include <map>
#include <string>

int main () {

  boost::container::flat_map<int, int> map_sample;

  map_sample.emplace(std::make_pair(2,4));
  map_sample.emplace(std::make_pair(20,40));
  map_sample.emplace(std::make_pair(200,400));

  for(const auto& pair : map_sample)
    std::cout << pair.first << "\t"
	      << pair.second << std::endl;

  std::cout << map_sample.at(20) << std::endl;

  {//-- copy from std::map (stackoverflow 12178067)
    const std::map<std::string, int> name_to_id {
	 {"Hello", 10},
	 {"World", 23}
    };
    boost::bimaps::bimap<std::string, int> bm;
    bm.left.insert(name_to_id.cbegin(),
		   name_to_id.cend());
    std::cout << bm.left.at("World") << ","
	      << bm.right.at(10)     << std::endl;
  }
  return 0;
}
