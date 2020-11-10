#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <unordered_map>
#include <random>
#include <algorithm>
#include <string>

//-- prepare a class to use as a member of SampleClass --
// default constructor and copy constructor are needed to
// put an original class into std::vector
// http://d.hatena.ne.jp/satosystems/20110506/1304688718
class SubClass
{
private:
  int member;

public:
  // initialize the member by colon(:)
  //http://d.hatena.ne.jp/higepon/20051107/1131335521
  SubClass() : member(0)
  {
    std::cout << "default" << std::endl;
  }
  //-- copy constructor --
  SubClass(const SubClass &rhs) : member(rhs.member)
  {
    std::cout << "copy constructor" << std::endl;
  }
  ~SubClass()
  {
    std::cout << "destructor" << std::endl;
  }
  int outmember()
  {
    return member;
  }
};

//--- use std::vector as a member variable of a class ---
// http://stackoverflow.com/questions/8553464/vector-as-a-class-member
class SampleClass
{
public:
  std::vector<double> samplevec;

  //-- nested class member --
  std::vector<class SubClass> classvec;

public:
  //-- constructors --
  SampleClass(){};
  SampleClass(int n_vecdim);
};

SampleClass::SampleClass(int n_vecdim)
{
  samplevec.assign(n_vecdim, 0.0);
  //  classvec.push_back(SubClass());
  classvec.assign(n_vecdim, SubClass());
}

//--- print every element of set --
// http://d.hatena.ne.jp/minus9d/20120611/1339422594
template <typename Tset>
void printSet(Tset &s)
{
  std::cout << "set: " ;
  for(std::set<double>::iterator it = s.begin();
      it != s.end(); ++it)
    {
      std::cout << *it << ",";
    }
  std::cout << std::endl;
}

//--- number of dimension --
const int NDIM  = 3;
const int NDIMX = 2;
const int NDIMY = 3;

//--- main ---
int main(int argc, char *argv[])
{
  std::cout << "argc: " << argc <<
    "\nargv[0]: " << argv[0] << std::endl;

  //-- compare a std::vector with a native array --
  std::vector<double> x;
  x.assign(NDIM, 0.0);
  // above 2 lines are equivalent to 
  // std::vector<double> x(NDIM, 0.0);

  double y[NDIM];

  x[0] = 1.5; x[1] = 1.5;
  y[0] = 2.5;

  for(int i=0; i<NDIM; i++)
    {
      std::cout << "x[" << i << "]: " << x[i] 
		<< "\t" << &x[i] << "\t" 
		<< "y[" << i << "]: " << y[i] 
		<< "\t" << &y[i] << std::endl;
    }
  std::cout << "x.size: " << x.size() << std::endl;

  //--- iterator --
  std::vector<double>::iterator it;
  it = x.begin();
  while(it != x.end())
    {
      std::cout << "reference of iterator it: " << *it << std::endl;
      ++it;
    }
  //-- vector -> set --
  std::set<double> s(x.begin(), x.end());
  std::multiset<double> smulti(x.begin(), x.end());
  printSet(s);// 0, 1.5
  printSet(smulti);
  const size_t n_erased1 = s.erase(7.0);
  std::cout << "s.erase(7.0) returns: " << n_erased1 << std::endl;//0
  const size_t n_erased2 = s.erase(1.5);
  std::cout << "s.erase(1.5) returns: " << n_erased2 << std::endl;//1

  //-- 2d vector --
  // http://stackoverflow.com/questions/2665936/is-there-a-way-to-specify-the-dimensions-of-a-nested-stl-vector-c
  std::vector<std::vector<double> > t;//(2, std::vector<double>(3));
  t.assign(NDIMX, std::vector<double>(NDIMY, 1.23));

  t[0][0] = 10.3;

  for(int i=0; i<NDIMX; i++)
    {
      for(int j=0; j<NDIMY; j++)
	{
	  std::cout << "t[" << i << "][" << j << "]: " 
		    << t[i][j] << "\t" << &t[i][j] << std::endl;
	}
    }

  std::cout << std::endl;
  //-- create an instance of the class --
  class SampleClass Instance1(NDIM);

  std::cout << "Pointer of the instance: " << &Instance1 << std::endl;
  
  for(int i=0; i<NDIM; i++)
    {
      int tmp = Instance1.classvec[i].outmember();
      std::cout << "Instance1.samplevec[" << i << "]: " 
		<< Instance1.samplevec[i] 
		<< "\t" << &Instance1.samplevec[i] << std::endl;
      std::cout << "Instance1.classvec[" << i << "].member: "
		<< tmp << "\t" << &Instance1.classvec[i] << std::endl;
    }

  //--- member function pointer --
  // http://www.geocities.jp/ky_webid/cpp/language/034.html
  //--- conditional branching with function pointer
  // http://www.c-lang.org/pointer_function.html

  int (SubClass::*pfunc)() = &SubClass::outmember;
  class SubClass Instance2;
  std::cout << (Instance2.*pfunc)() << std::endl;

  //--- map ---
  std::map<std::string, int> mapstringHeight;
  mapstringHeight["Fuji"] = 3776;

  std::cout << "The height of Fuji is: " 
	    << mapstringHeight["Fuji"] << std::endl;

  //--- random number ---
  // random seed different in every run is generated by
  std::random_device seed;
  std::mt19937 engine(seed());
  //std::uniform_real_distribution<> dist1(-1.0, 1.0);
  std::uniform_int_distribution<> dist_int(0, 1000);

  for (int i=0; i<20; i++)
    std::cout << dist_int(engine) << ", ";
  std::cout << std::endl;

  //-- Replacing part of std::vector by smaller std::vector
  // https://stackoverflow.com/questions/28013793/replacing-part-of-stdvector-by-smaller-stdvector
  {
    std::vector<int> input = { 0, 0, 1, 1, 2, 22, 3, 33, 99 };
    std::vector<int> a = { 1, 2, 3 };
    std::vector<int> b = { 4, 5, 6, 7, 8 };

    std::copy(std::begin(a), std::end(a), std::begin(input));
    std::copy(std::begin(b), std::end(b), std::begin(input) + a.size());
  }
  //--- practice string manipulation
  {
    //https://cpprefjp.github.io/reference/algorithm/remove.html
    std::string yymmdd("2040-01-01");
    std::string::iterator removed = std::remove(yymmdd.begin(), yymmdd.end(), '-');

    yymmdd.erase(removed, yymmdd.end());
    std::cout << "yymmdd: " << yymmdd << std::endl;
  }
  //-- unordered_map::bucket
  {
    const std::unordered_map<std::string, int> um = {
    {"1st", 1},
    {"2nd", 2},
    {"3rd", 3},
    {"3rd2", 33},
    {"4th", 4},
    {"4th2", 44},
    {"5th", 5}
    };
    const size_t bucketcount = um.bucket_count();
    std::cout << "bucket count is " << bucketcount << std::endl;
    std::cout << "(bucket No, size)" << std::endl;
#pragma omp parallel for
    for (size_t i=0; i<bucketcount;i++) {
      std::cout << "(" << i << "," << um.bucket_size(i) << "): elements are: ";
      for (decltype(um)::const_local_iterator liter = um.cbegin(i);
	   liter                                   != um.cend(i); liter++)
	std::cout << "[" << liter->first
		  << "," << liter->second << "]";
      std::cout << std::endl;
    }
    std::cout << std::endl;
  }
  return 0;
}
