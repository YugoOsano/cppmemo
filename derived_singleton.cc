#include <iostream>

class Base{
public:
  Base() : m_(12345) {}
  int m_;
};
class Derived : public Base {
public:
  static Derived& Get() {
    static Derived derived;
    return         derived;
  }
private:
  Derived()=default;// to use in Get
  Derived(const Derived&)=delete;
  Derived(Derived&&)=delete;
};
int main() {
  Base b;
  //Derived d; <- error
  std::cout << Derived::Get().m_ << std::endl;;
  return 0;
}
