#include <vector>
#include <iostream>
#include <boost/range/algorithm.hpp>

#include <map>
#include <set>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/algorithm/copy.hpp>

void output(const int a) {
  std::cout << a << std::endl;
}
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
  return 0;
}
