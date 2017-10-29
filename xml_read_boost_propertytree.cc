// XML reading
// https://boostjp.github.io/tips/xml.html

// direct use of a string as an XML input
// http://katze.hatenablog.jp/entry/2013/06/26/194513

// compile with -std=c++11 

// target xml is:
/*
<?xml version="1.0" encoding="utf-8"?>
<root>
    <str>Hello</str>
    <values>
        <value>1</value>
        <value>2</value>
        <value>3</value>
    </values>
</root>"
*/

#include <iostream>
#include <sstream>
#include <string>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
//#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>

int main () {
  std::stringstream ss;
  ss << "<?xml version=\"1.0\" encoding=\"utf-8\"?><root><str>Hello</str><values><value>1</value> <value>2</value> <value>3</value> </values></root>";

  boost::property_tree::ptree pt;
  boost::property_tree::xml_parser::read_xml(ss, pt);
  /* this is switched to file reading just by replacing ss with a file name */
  //    read_xml("data.xml", pt);

  if (boost::optional<std::string> str =
      pt.get_optional<std::string>("root.str")) {
    std::cout << str.get() << std::endl;
  }
  else {
    std::cout << "root.str is nothing" << std::endl;
  }

  for (const boost::property_tree::ptree::value_type& child :
	 pt.get_child("root.values")) {
    const int value = boost::lexical_cast<int>(child.second.data());
    std::cout << value << std::endl;
  }
  return 0;
}

