#include <iostream>
#include <typeinfo>

class Aclass{
};

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

  return 0;
}
