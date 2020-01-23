// asio was downloaded by:
// wget https://sourceforge.net/projects/asio/files/asio/1.12.2%20%28Stable%29/asio-1.12.2.tar.gz
// tar zxvf [the file]
// configure can be skipped unless tests or examples are wanted.

// tutorial is:
// http://think-async.com/Asio/asio-1.13.0/doc/asio/tutorial/tuttimer1.html

// compiled by:
// g++-9 -I../software/asio-1.12.2/include/ asio_native.cc -pthread

#include <iostream>
#include <asio.hpp>
#include <vector>
#include <string>

// Modern C++ Challenge Q95
std::vector<std::string> GetIPAddress(std::string const& hostname) {
  std::vector<std::string> ips;

  try {
    asio::io_context context;
    asio::ip::tcp::resolver resolver(context);
    auto endpoints = resolver.resolve(asio::ip::tcp::v4(),
				      hostname.c_str(), "");
    for (auto const& e : endpoints)
      ips.push_back(e.endpoint().address().to_string());

  } catch (std::exception const& e) {
    std::cerr << "exception: " << e.what() << std::endl;
  }
  return ips;
}
void HelloAsio() {
  asio::io_context io;
  asio::steady_timer t(io, asio::chrono::seconds(5));

  t.wait();
  std::cout << "Hello, world!" << std::endl;
}
int main () {
  //HelloAsio();
  const std::vector<std::string>& ips
    = GetIPAddress("github.com");
  for (auto const& ip : ips)
    std::cout << ip << std::endl;
  return 0;
}
