#include <iostream>

class Inner{
public:
  /* getter & setter */
  int     m() { return m_; }
  void setm(int p_m) { m_ = p_m; }

protected:
  int m_;
};

class Outer{
public:
  /* getter of inner; 
     reference(&) should be returned */
  Inner& inner(){ return inner_; }
  //Inner inner(){ return inner_; } // wrong

protected:
  Inner inner_;
};

int main(){
  Outer outer;
  
  outer.inner().setm(1234);
  std::cout << "outer.inner().m(): " 
	    <<  outer.inner().m() << std::endl;

  return 0;
}
