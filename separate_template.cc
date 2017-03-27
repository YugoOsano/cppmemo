//http://stackoverflow.com/questions/1724036/splitting-templated-c-classes-into-hpp-cpp-files-is-it-possible
#include <iostream>

#include "separate_template.h"

template <class T>
T func(T& in) {
  std::cout << "in: " << in << std::endl;
  return in;
}

template double func(double& in);
template int    func(int&    in);

template <class T>
T& ClassA<T>::get(){ 
  std::cout << "member: " << member_ << std::endl;
  return member_; 
}

template class ClassA<int>;
template class ClassA<double>;
