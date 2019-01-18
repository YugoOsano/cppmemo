/*
  For given 
  weights [w0, w1, w2, w3, ... w(n-1)]  ex: {2,1,3,2}
  values  [v0, v1, v2, v3, ... v(n-1)]  ex: {3,2,4,2}
  max_weight,
  the goal is to maximize the value sum of indices list.
  
  create a (temporary) boolean list which starts from [T,T,T,...] (True)
  with monitoring statuses: weight_sum, cursor_to_search   
  [] (start from void)
   if the weight_sum < max_weight, progress the cursor to the next index
  [w0] 
  [w0,w1] 
  [w0,w1,w2]
  ....     (progress until weight_sum > max_weight or index comes over the size)
           here, weight_sum includes w_i where the corresponding boolean is T.
  if [w0,w1,w2,w3,w4,w5]'s weight_sum > max_weight,
  record [w0,w1,w2,w3,w4]'s value as the max_value;
                      *
  then put back the cursor to the first True boolean from w4 position;
  then 
  [T,  T, T, T, F, T] 
  [w0,w1,w2,w3,__,w5,] is calculated.
  .... 
 */
#include <vector>
#include <tuple>
#include <cassert>

std::tuple <double, double, std::vector<bool>>
  GetWeightValueIndicesList(const std::vector<double>& weights,
			    const std::vector<double>& values,
			    const double               max_weight) {
  assert (weights.size() == values.size());
  assert (weights.size() > 0);

  const size_t size = weights.size();
  
  std::vector<bool> boolean_list(size, true);
  std::vector<double> tmp_weight_list(0);

  auto CalcList = [](const std::vector<double>& list,
		     const std::vector<bool>&   boolean_list,
		     const size_t               tmp_size) {
    double to_return = 0.0;
    for (size_t i = 0; i < tmp_size; i++) {
      if(boolean_list[i]) to_return += list[i];
    }
    return to_return;
  };
  
  double max_value_sum  = 0.0;
  size_t cursor_to_search = 0;
    
  while (boolean_list[0] == true) {
    const double tmp_weight_sum = CalcList(tmp_weight_list,
					   boolean_list,
					   tmp_weight_list.size());
    if ((tmp_weight_sum <= max_weight) &&
	(cursor_to_search < size)) {
      tmp_weight_list.push_back(weights[cursor_to_search]);
      cursor_to_search++;
    } else {
      tmp_weight_list.pop_back();

      const double tmp_value_sum = CalcList(values,
					    boolean_list,
					    cursor_to_search);
      //-- register if it is maximum
      if (tmp_value_sum > max_value_sum)
	max_value_sum = tmp_value_sum;
      
      do {
	cursor_to_search--;
      } while(boolean_list[cursor_to_search]==false);
      
      boolean_list[cursor_to_search]=false;

      for (size_t i = cursor_to_search + 1; i < size; i++)
	boolean_list[i]=true;
    }    
  }  
  return std::make_tuple(max_value_sum, 0.0, boolean_list);
}


int main () {
  GetWeightValueIndicesList(std::vector<double>{2,1,3,2},
			    std::vector<double>{3,2,4,2},
			    10000);
  return 0;
}
