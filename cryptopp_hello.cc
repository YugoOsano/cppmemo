// https://www.cryptopp.com/wiki/Linux
// compiled by:
// g++ cryptopp_hello.cc -l:libcryptopp.a

#include <iostream>
#include <string>
#include <cryptopp/cryptlib.h>
#include <cryptopp/integer.h>
#include <cryptopp/sha.h>
#include <cryptopp/hex.h>

//-- Modern C++ Challenge Q.91
std::string GetHash(const std::string& password) {
  CryptoPP::SHA512 sha;
  byte digest[CryptoPP::SHA512::DIGESTSIZE];

  sha.CalculateDigest(
      digest,                                         //output buffer for hashed string
      reinterpret_cast<byte const*>(password.c_str()),//input string
      password.length());                             // string length

  //-- encode converts byte array to readable characters
  CryptoPP::HexEncoder encoder;
  std::string result;
  encoder.Attach(new CryptoPP::StringSink(result));
  encoder.Put(digest,sizeof(digest));
  encoder.MessageEnd();
  return result;
}
int main () {
  CryptoPP::Integer i;
  std::cout << "i: " << i << std::endl;
  std::cout << GetHash("pwstring") << std::endl;
  return 0;
}
