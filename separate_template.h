// template function and class' implementation 
// is separated

template <class T>
T func(T& in);

template <class T>
class ClassA {
public:
  T& get();

protected:
  T member_;
};
