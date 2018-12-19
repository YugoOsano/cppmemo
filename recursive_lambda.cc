//-- need -std=c++14
// https://stackoverflow.com/questions/2067988/recursive-lambda-functions-in-c11

int main () {

  auto f = [](const unsigned int n) {return n;};
  
  auto recursive_f = [](auto& f, const unsigned int n)->unsigned int {
    return f(f, n-1) + n;
  };

  //recursive_f(f, 10);
  
  return 0;
}
