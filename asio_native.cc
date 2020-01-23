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

int main () {
  asio::io_context io;
  asio::steady_timer t(io, asio::chrono::seconds(5));

  t.wait();
  std::cout << "Hello, world!" << std::endl;
  return 0;
}
