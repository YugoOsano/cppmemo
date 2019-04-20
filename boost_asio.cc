// this is from Modern C++ Challenge p-258 - 260
// compile with -lpthread

#include <boost/asio.hpp>
#include <string>
#include <vector>
#include <iostream>

std::vector<std::string> get_ip_address(const std::string& hostname) {

  std::vector<std::string> ips;
  try {
    boost::asio::io_context        context;
    boost::asio::ip::tcp::resolver resolver(context);
    auto endpoints = resolver.resolve(boost::asio::ip::tcp::v4(),
				      hostname.c_str(), "");

    for (const auto& e : endpoints)
      ips.push_back(e.endpoint().address().to_string());
  }
  catch (std::exception const & e) {
    std::cerr << "exception: " << e.what() << std::endl;
  }
  return ips;
}

int main () {

  auto ips = get_ip_address("localhost");

  for (const auto& ip : ips)
    std::cout << ip << std::endl;
  
  return 0;
}
