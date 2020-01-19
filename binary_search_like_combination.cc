#include <vector>
#include <iostream>
#include <string>
#include <set>
#include <map>
#include <cmath>
#include <boost/dynamic_bitset.hpp>

//--
using Type = std::vector<std::string>;
std::set<size_t> DumpCombination(
         const Type&          input_vector,
	 const Type::iterator iter,
	 Type&                temp_vector,
	 std::map<Type::iterator, std::set<size_t>>& sum_string_length) {
  //-- all length set to return
  std::set<size_t> to_return;
  
  //-- dump when iter reaches the end of input
  if (iter == input_vector.cend()) {
    for (const std::string& elem : temp_vector)
      std::cout << elem << ", ";
    std::cout << std::endl;
    return to_return;
  }
  auto new_iter = iter + 1;
  //-- without appending the current iter's target
  std::vector<std::string> new_without(temp_vector);
  new_without.push_back("_");
  const std::set<size_t>& to_add0 = DumpCombination(input_vector,
						    new_iter,
						    new_without,
						    sum_string_length);
  to_return.insert(to_add0.cbegin(),
		   to_add0.cend());
  //-- appending the current iter's target
  std::vector<std::string> new_with(temp_vector);
  new_with.push_back(*iter);
  const std::set<size_t>& to_add1 = DumpCombination(input_vector,
						    new_iter,
						    new_with,
						    sum_string_length);
  to_return.insert(to_add1.cbegin(),
		   to_add1.cend());

  std::set<size_t> to_add2;
  for (const size_t elem : to_return)
    to_add2.insert(elem + (*iter).length());
  to_return.insert(to_add2.cbegin(),
		   to_add2.cend());
  return to_return;
}

//-- bitset version
void DumpCombinationWithBitset(const Type& string_list) {
  const size_t length = string_list.size();

  for (size_t i = 0; i < std::pow(2, length); i++) {
    const boost::dynamic_bitset<> bit_list(length, i);
    for (size_t i_bit = 0; i_bit < length; i_bit++) {
      if (bit_list[i_bit])
	std::cout << string_list.at(i_bit) << ", ";
      else
	std::cout << "_, ";
    }
    std::cout << std::endl;
  }
}

int main() {
  std::vector<std::string> phonetic {"xray", "alpha", "romeo", "juliet"};
  std::vector<std::string> empty_vec{};
  std::map<Type::iterator, std::set<size_t>> dp_recorder; 
  
  DumpCombination(phonetic,
		  phonetic.begin(),
		  empty_vec,
		  dp_recorder);
  DumpCombinationWithBitset(phonetic);
  return 0;
}
