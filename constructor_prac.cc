#include <vector>

struct SampleStruct {

  SampleStruct(const int x, const double y, const long z) :
    x_(x), y_(y), z_(z) {}

  int x_;
  double y_;
  long z_;
};

int main () {
  SampleStruct instance(1, 123.456, 5000000000000);
  SampleStruct instance_init{1, 123.456, 5000000000000};

  /* this is a test for use of std_initializer for an arbitrary struct */ 
  std::vector<SampleStruct> vec {
    {1, 123.456, 5000000000000},
      {1, 123.456, 5000000000000},
	{1, 123.456, 5000000000000}
  };
  return 0;
}
