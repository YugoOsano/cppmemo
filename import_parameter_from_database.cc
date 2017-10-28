// prototype of module to fetch data from a database
// to store the data into a parameter storage

#include <iostream>
#include <string>
#include <map>
#include <boost/variant.hpp>

std::map <std::string, std::string> Database{
  {"integer", "1000"},
  {"double", "1234.5678"},
  {"string", "Hello"}};

struct ParameterStorage {
  int    i;
  double x;
  std::string str;
};

int main () {

  ParameterStorage param_storage;

  // std::map<int, std::string> param_ptr_to_string
  // {{ param_storage.i, "integer"}};

  //----
  using VarType =
    boost::variant<int*, double*, std::string*>;
  
  std::map<VarType, std::string> map_from_param_var_to_string
  {{ &param_storage.i, "integer"},
   { &param_storage.x, "double"},
   { &param_storage.str, "string"}};

  for (auto& param_to_string : map_from_param_var_to_string) {
    if (param_to_string.first.type() == typeid(int*)) {
      int* ptr = 
	boost::get<int*>(param_to_string.first);
      std::cout << ptr << std::endl;
      
      *ptr =
	std::stoi(Database.at(param_to_string.second));
    } else if (param_to_string.first.type() == typeid(double*)) {
      double* ptr = 
	boost::get<double*>(param_to_string.first);
      std::cout << ptr << std::endl;
      
      *ptr =
	std::stof(Database.at(param_to_string.second));
    } else if (param_to_string.first.type() == typeid(std::string*)) {
      std::string* ptr = 
	boost::get<std::string*>(param_to_string.first);
      std::cout << ptr << std::endl;
      
      *ptr = Database.at(param_to_string.second);
    } 
  }
  
  std::cout << "param int: "
	    << param_storage.i << std::endl;
  std::cout << "param double: "
	    << param_storage.x << std::endl;
  std::cout << "param string: "
	    << param_storage.str << std::endl;

  
  return 0;
}
