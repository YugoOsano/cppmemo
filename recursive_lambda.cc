//-- need -std=c++14
// https://stackoverflow.com/questions/2067988/recursive-lambda-functions-in-c11

// Haskell-convertible substitute of for loop:

// int sum = 0;
// std::vector<int> v;
// for (int i=0; i<n; i++) {
//   sum += i;
//   v.push_back(i);
// }

#include <vector>
#include <tuple>
#include <iostream>

//--------  i    n   sum  v
std::tuple<int, int, int, std::vector<int>>
ForLoopSubstitute(const int i,
		  const int n,
		  const int sum,
		  const std::vector<int>& v) {
  if (i >= n) return
		std::make_tuple(i,n,sum,v);
  //-- the inside of scope {} of a for loop
  const int         new_sum = sum + i;
  std::vector<int>  new_v(v);
  new_v.push_back(i);
  return ForLoopSubstitute(i+1,n,new_sum,new_v);
}

int main () {

  auto f = [](const unsigned int n) {return n;};
  
  auto recursive_f = [](auto& f, const unsigned int n)->unsigned int {
    return f(f, n-1) + n;
  };

  //recursive_f(f, 10);

  std::vector<int> v;
  const std::tuple<int, int, int, std::vector<int>>&
    result = ForLoopSubstitute(0, 10, 0, v);
  std::cout << "sum is: "
	    << std::get<2>(result) << std::endl;
  return 0;
}
