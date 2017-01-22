// http://stackoverflow.com/questions/1505675/power-of-an-integer-in-c

#include <iostream>
#include <stdlib.h>
#include <cmath>
#include <bitset>

//template<class T>
long long myPow(long long x, long long p) {
  if (p == 0) return 1;
  if (p == 1) return x;
  return x * myPow(x, p-1);
}

const long long Two_31stpower     = myPow(2, 31);
const long long Two_31stminus_one = myPow(2, 31) - 1; 
const long long Two_62ndpower     = myPow(2, 62);

class DoubleLongLong{
public:
  long long lower31bit_;
  long long upper31bit_;
  
  long long result_lower31bit_;
  long long result_upper31bit_;

public:
  virtual ~DoubleLongLong(){};
  // getter
  long long lower31bit(){ return lower31bit_;}
  long long upper31bit(){ return upper31bit_;}

  // Conversion from/to long long 
  void FromLongLong(const long long input) {
    if (input >= Two_62ndpower) {
      std::cerr << "input: " << input << " is larger than: "
		<< Two_62ndpower
		<< std::endl;
      exit(1);
    }
    //this->lower31bit_ = input % Two_31stpower;
    //this->upper31bit_ = input / Two_31stpower;
    this->lower31bit_ = input & Two_31stminus_one;
    this->upper31bit_ = input >> 31;

    std::bitset<31> outlower31(lower31bit_);
    std::bitset<31> outupper31(upper31bit_);
    std::cout << "upper: " << upper31bit_ << "\t"
	      << outupper31 << std::endl;
    std::cout << "lower: " << lower31bit_ << "\t"
	      << outlower31 << std::endl;
  }
  long long ToLongLong() {
    return upper31bit_ * Two_31stpower + lower31bit_;
  }

  DoubleLongLong Multiply(DoubleLongLong& input) {
    
    DoubleLongLong ret;

    /* get result */
    long long result_lower_times_lower
      = lower31bit_ * input.lower31bit();
    long long result_lower_times_upper
      = lower31bit_ * input.upper31bit();
    long long result_upper_times_lower
      = upper31bit_ * input.lower31bit();
    long long result_upper_times_upper
      = upper31bit_ * input.upper31bit();
    
    ret.lower31bit_ = result_lower_times_lower & Two_31stminus_one;

    ret.upper31bit_ = (result_lower_times_lower >> 31)
      +               (result_lower_times_upper & Two_31stminus_one)
      +               (result_upper_times_lower & Two_31stminus_one);

    ret.result_lower31bit_ =
      (                result_lower_times_upper >> 31)
      +               (result_upper_times_lower >> 31)
      +               (result_upper_times_upper & Two_31stminus_one);

    ret.result_upper31bit_ =
      result_upper_times_upper >> 31;

    return ret;
  }
};
