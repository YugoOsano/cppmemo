//http://kaworu.jpn.org/cpp/std::mem_fn

#include <iostream>
#include <functional>
#include <typeinfo>

class Foo {
private:
public:
  int     data;
  Foo () :data(7) {}
  void display_hello () {
    std::cout << "Hello" << std::endl;
  }
  void display_number(int i) {
    std::cout << "Number: " << i << std::endl;
  }
};

template<class FunctionType, typename ... Ts>
//void RepeatFunc(std::function<void(T&)> func,
void RepeatFunc(FunctionType func, 
		Ts ...       instance) {
  for(int i = 0; i < 5; i++)
    func(instance ...);
} 

int main(int argc, char const* argv[])
{
  Foo     f;
 
  //https://stackoverflow.com/questions/31604893/c11-type-name-for-stdmem-fn
  std::function<void(Foo&)> hello = 
    std::mem_fn(&Foo::display_hello);

  RepeatFunc<std::function<void(Foo&)>, Foo>(hello, f);

  std::function<void(Foo&, int)> number = 
    std::mem_fn(&Foo::display_number);

  RepeatFunc<std::function<void(Foo&, int)>, 
	     Foo, int>(number, f, 987);

  auto data       = std::mem_fn(&Foo::data);
  auto bind_hello = std::bind(&Foo::display_hello, f);

  hello(f);
  number(f,123);
  std::cout << data(f) << std::endl;
  std::cout << typeid(hello).name() << std::endl;
  std::cout << typeid(bind_hello).name() << std::endl;
  
  return 0;
}
