//-- this is from C++ Template Technique p-105
//   needs C++11 or later

#include <utility>
#include <tuple>

template <class T>
T* add_pointer(T);

//-- type list; decomposing a given list to
//   its beginning and the remaining
template <class Head, class... Tail>
struct g {
  using head = Head;
  using tail = std::tuple<Tail...>;
};
template <class... List>
struct f {
  using head = typename g<List...>::head;
  using tail = typename g<List...>::tail;
};

int main () {
  //-- declval accepts T to return T&&, and has to be used with decltype 
  typedef decltype(add_pointer(std::declval<int>())) T_as_result;
  T_as_result x;//int*

  using TAsResult = decltype(add_pointer(std::declval<int>()));
  TAsResult y;//int*

  //-- type list
  using list = f<int, char, double>;
  return 0;
}
