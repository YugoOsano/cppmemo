//http://kaworu.jpn.org/cpp/std::mem_fn

#include <iostream>
#include <functional>
#include <typeinfo>
#include <map>
#include <string>
#include <set>

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

//-- act on iterators and predicate like algorithm library
template<typename Iter, typename Pred>
void ApplyAlgorithmByRef(Iter iter_begin,
			 Iter iter_end,
			 Pred& pred) {
  for (auto iter = iter_begin;
       iter     != iter_end; iter++)
    pred(*iter);
}
template<typename Iter, typename Pred>
void ApplyAlgorithm(Iter iter_begin,
		    Iter iter_end,
		    Pred pred) {
  for (auto iter = iter_begin;
       iter     != iter_end; iter++)
    pred(*iter);
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
  {
    std::map<size_t, std::string> id_string{{2, "lkjh"}};
    //-- functor to apply
    struct Printer {
      void operator()(const std::pair<size_t, std::string>& pair) {
	std::cout << pair.first << ", " << pair.second << "\n";
      }
    };
    Printer printer;
    ApplyAlgorithmByRef(id_string.cbegin(),
			id_string.cend(),
			printer
		   // [](const std::pair<size_t, std::string>& pair) {
		   //   std::cout << pair.first << ", " << pair.second << "\n";
		   // }
		   );
    struct Collector {
      void operator()(const std::pair<size_t, std::string>& pair) {
	idset_.emplace(pair.first);
      }
      std::set<size_t> idset_;
    };
    Collector collector;
    ApplyAlgorithmByRef(id_string.cbegin(),
			id_string.cend(),
			collector);
    if (collector.idset_.size() > 0)
      std::cout << "succeeded to collect: "
		<< *collector.idset_.cbegin() << std::endl;
    //-- explicit instantiation by reference type
    Collector collector2;
    ApplyAlgorithm<decltype(id_string)::const_iterator, Collector&>(
	      id_string.cbegin(),
	      id_string.cend(),
	      collector2);
    if (collector2.idset_.size() > 0)
      std::cout << "succeeded to collect: "
		<< *collector2.idset_.cbegin() << std::endl;
  }
  return 0;
}
