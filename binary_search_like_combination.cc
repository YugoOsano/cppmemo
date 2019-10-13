#include <vector>
#include <iostream>
#include <string>

void DumpCombination(
         const std::vector<std::string>&          input_vector,
	 const std::vector<std::string>::iterator iter,
	 std::vector<std::string>&                temp_vector) {
  //-- dump when iter reaches the end of input
  if (iter == input_vector.cend()) {
    for (const std::string& elem : temp_vector)
      std::cout << elem << ", ";
    std::cout << std::endl;
    return;
  }
  auto new_iter = iter + 1;
  //-- without appending the current iter's target
  std::vector<std::string> new_without(temp_vector);
  new_without.push_back("_");
  DumpCombination(input_vector,
		  new_iter,
		  new_without);
  //-- appending the current iter's target
  std::vector<std::string> new_with(temp_vector);
  new_with.push_back(*iter);
  DumpCombination(input_vector,
		  new_iter,
		  new_with);
}

int main() {
  std::vector<std::string> phonetic {"xray", "alpha", "romeo", "juliet"};
  std::vector<std::string> empty_vec{};
  DumpCombination(phonetic,
		  phonetic.begin(),
		  empty_vec);
  return 0;
}
