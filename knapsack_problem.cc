/*
  For given 
  weights [w0, w1, w2, w3, ... w(n-1)]
  values  [v0, v1, v2, v3, ... v(n-1)]
  max_weight,
  the goal is to maximize the value sum of indices list.
  
  create a (temporary) step list which starts from [1]
  with monitoring status: weight_sum, cursor_to_make_branch,              
  [1] (pick 0th item)
   if the weight_sum < max_weight, append index 
  [1,1] (pick 0th, 1st items)
  [1,1,1]
  ....     (append until weight_sum > max_weight or index comes over the size)
  if [1,1,1,1,1,1]'s weight_sum > max_weight,
  record [1,1,1,1,1]'s value as the max_value;
                  *
  then put a cursor at *'s position,
  then increment the number of step at the cursor position:
  [1,1,1,1,2] 
  [1,1,1,1,2,1]
  .... 
 */
#include <vector>
#include <tuple>
#include <cassert>

std::tuple <double, double, std::vector<size_t>>
  GetWeightValueIndicesList(const std::vector<double>& weights,
			    const std::vector<double>& values,
			    const double               max_weight) {
  assert (weights.size() == values.size());
  assert (weights.size() > 0);

  const size_t list_size = weights.size();
  
  std::vector<size_t> step_list{1};

  double tmp_weight_sum = 0.0;
  double tmp_value_sum  = 0.0;
  double max_value_sum  = 0.0;
  size_t index_sum      = 0;
    
  while (step_list[0] <= list_size) {
    
    
  }
  
  return std::make_tuple(0.0, 0.0, step_list);
}


int main () {

  return 0;
}
