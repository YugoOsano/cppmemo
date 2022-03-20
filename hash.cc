#include <algorithm>
#include <string>
#include <iostream>
#include <vector>
#include <numeric>
#include <boost/crc.hpp>

int main() {
  {
    std::hash<std::string> hash_str;
    const size_t hashed = hash_str("hello");

    std::cout << hashed << std::endl;
  }
  {
    std::vector<int> intv(10000);
    std::iota(intv.begin(),
	      intv.end(), 1);
    std::hash<decltype(intv)*> hash_v;
    const size_t hashed_v = hash_v(&intv);
    std::cout << hashed_v << std::endl;
  }
  {
    std::vector<int> intv(10000);
    std::iota(intv.begin(),
	      intv.end(), 1);
    //intv[10000] = 10;// to see sum change with the range on process_bytes
    boost::crc_32_type crc32;
    crc32.process_bytes(intv.data(),
			sizeof(int) * (intv.size() /*+1*/));
    const size_t sum = crc32.checksum();
    std::cout << "crc32.checksum: " << sum << std::endl;//1886789874
  }
  return 0;
}
