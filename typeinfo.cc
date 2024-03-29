#include <iostream>
#include <typeinfo>
#include <typeindex>
#include <type_traits>
#include <tuple>
#include <vector>
#include <map>
#include <memory>

class Aclass{};
class Bclass : public Aclass {};
class Cclass : public Aclass {};

// Reference; C++ template technique p-93
template <class T>
struct identity {
  typedef T type;
};

template <class T>
struct add_pointer {
  typedef T* type;
};

int main(){

  int a, b;
  long int c;

  Aclass amember;
  identity<int>::type    value1;
  add_pointer<int>::type value2;

  std::cout << typeid(a).name() << "\n"
	    << typeid(c).name() << "\n"
	    << typeid(amember).name() << "\n"
	    << std::endl;
  //auto idofa = typeid(a);
  if(typeid(a) == typeid(b))
    std::cout << "Hello!" << std::endl; 

  std::cout << "value1's type: "
	    << typeid(value1).name() 
	    << "\nvalue2's type: "
	    << typeid(value2).name() << std::endl;

  //---- dump type ----
  typedef std::tuple<bool, int> TupleBoolInt;
  using TupleIntBool = std::tuple<int, bool>;

  std::cout << "typedef dump: " << typeid(TupleBoolInt).name() << std::endl;
  std::cout << "using dump: " << typeid(TupleIntBool).name() << std::endl;

  auto tname = typeid(TupleBoolInt).name();
  std::cout << "name dump: " << typeid(tname).name() << std::endl;

  //----STL's value_type
  {
    std::vector<int>::value_type x;
    static_assert(std::is_same<decltype(x), int>::value,
		  "x is int"); // message is shown on error

    std::map<int, Aclass>::value_type map_value;
    static_assert(std::is_same<decltype(map_value),
		  std::pair<int const, Aclass>>::value,
		  "map_value is std::pair<int, Aclass>");
    std::cout << "map_value type: " << typeid(map_value).name()
	    << std::endl;
  }
  { // recognize switch of type
    // https://stackoverflow.com/questions/53467813/comparing-two-type-info-from-typeid-operator
    std::unique_ptr<Aclass> ptr_A(std::make_unique<Bclass>());
    const auto& ref_before_switch   = typeid(*ptr_A);
    const auto  index_before_switch = std::type_index(ref_before_switch);
    [&index_before_switch](){}();// p in gdb results: {_M_target = 0x555555558c48 <typeinfo for Aclass>}
  }
  return 0;
}
