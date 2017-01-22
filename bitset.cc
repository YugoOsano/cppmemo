//https://cpprefjp.github.io/reference/bitset.html
// compile with:
// g++ bitset.cc -std=c++11

#include <iostream>
#include <bitset>
#include <limits>

#include "double_long_long.hpp"

int main()
{
  // 整数から8ビットのビット集合を構築
  std::bitset<8> bs1(131uL); // 10000011

  // 文字列から8ビットのビット集合を構築
  std::bitset<8> bs2("10000011");

  // 1ビット目が1かを判定
  if (bs1[1]) {
    std::cout << "1st bit is 1" << std::endl;
  }

  // 2ビット目を1にする
  bs1.set(2);
  std::cout << "2nd bit to 1 : " << bs1 << std::endl;

  // 2ビット目を0に戻す
  bs1.reset(2);

  // いずれかのビットが1かを判定
  if (bs1.any()) {
    std::cout << "some bits are 1" << std::endl;
  }

  // 論理演算
  std::bitset<8> and_bits = bs1 & std::bitset<8>("10000001"); // 論理積
  std::bitset<8> or_bits  = bs1 | std::bitset<8>("00010100"); // 論理和
  std::bitset<8> xor_bits = bs1 ^ std::bitset<8>("00100011"); // 排他的論理和

  std::cout << "and : " << and_bits << std::endl;
  std::cout << "or  : " << or_bits << std::endl;
  std::cout << "xor : " << xor_bits << std::endl;

  // assign bitset from long
  const long la = 65536;
  constexpr long two_to_five = 2*2*2*2*2 * 2*2*2*2*2 * 2*2*2*2*2;

  std::bitset<17> bits_la(la);

  std::cout << "bits_la: " << bits_la << std::endl;

  long carry = la / two_to_five;

  std::cout << "carry: " << carry 
	    << "\ttwo_to_five: " << two_to_five << std::endl;

  std::cout << "max of long: "  
	    << std::numeric_limits<long>::max() << std::endl;

  long long maxoflonglong = std::numeric_limits<long long>::max();

  std::cout << "max of long long: "  
	    << std::numeric_limits<long long>::max() << std::endl;

  std::bitset<64> bits_max_longlong(maxoflonglong);

  std::cout << "bits_max_longlong: " << bits_max_longlong << std::endl;  
  DoubleLongLong instance;
  instance.FromLongLong((maxoflonglong - 1) / 2);
  long long ret = instance.ToLongLong();

  std::cout << "After conversion: " << ret << std::endl;

  std::cout << "Two_62ndpower: " << Two_62ndpower << std::endl;

  // -- bit operation --
  long long bit_operation = 15; // 1111
  long long result = bit_operation >> 2;
  std::cout << "bit operation's result: " << result 
	    << std::endl;
}
