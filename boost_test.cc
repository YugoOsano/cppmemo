#include <vector>
#include <iostream>
#include <boost/range/algorithm.hpp>
#include <boost/range/adaptors.hpp>

#include <map>
#include <set>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/algorithm/copy.hpp>

void output(const int a) {
  std::cout << a << std::endl;
}
struct Data{
  Data(const int v) : value_(v) {}
  int value_;
};
int main () {
  std::vector<int> alist{2,3,4};
  // second argument is an unary function
  // (single argument function)
  boost::for_each(alist, output);

  //-- lambda
  auto outputlambda = [](const int a) {
    std::cout << a << std::endl;
  };
  boost::for_each(alist, outputlambda);

  // extract maps keys to store to std::set
  // https://stackoverflow.com/questions/110157/how-to-retrieve-all-keys-or-values-from-a-stdmap-and-put-them-into-a-vector
  {
    const std::map<int, int> mapint{{0,1},{2,3},{4,5}};
    std::set<int> keys;
    boost::copy(mapint | boost::adaptors::map_keys,
		std::inserter(keys, keys.begin()));
    for (const int key : keys)
      std::cout << key << " ";
    std::cout << std::endl;
  }
  //-- filter map elements by keys stored in a set
  //https://theboostcpplibraries.com/boost.range-adaptors
  {
    const std::map<int, int> mapint{{0,1},{2,3},{4,5}};
    const std::set<int>      keys{2,4};
    std::map<int,int> filtered;
    boost::copy(boost::adaptors::filter(
	          mapint,
		  [&keys](const std::pair<int,int>& pair){
		    return (keys.find(pair.first) != keys.cend()); }),
		std::inserter(filtered, filtered.begin()));
    for (const auto& pair : filtered)
      std::cout << pair.first << " " << pair.second << std::endl;
    std::cout << "-- filtered end --" << std::endl;
  }
  // boost range transform
  // https://greek0.net/boost-range/boost-range-transform.html
  // convert map<key, Data> to map<key, value (of Data)>
  {
    const std::map<int, Data> mapData{
      {0,Data(5)},{2,Data(10)},{4,Data(5)}};
    std::map<int,int> converted;
    boost::transform(mapData,
		     std::inserter(converted, converted.begin()),
		     [](const std::pair<int, Data>& pair) {
		       return std::make_pair(pair.first,
					     pair.second.value_);});
    for (const auto& pair : converted)
      std::cout << pair.first << " " << pair.second << std::endl;
  }
  return 0;
}
