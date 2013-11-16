//--- 
//   write a json file with
//   boost::property_tree
//  https://sites.google.com/site/boostjp/tips/json

//  However, boost::property_tree is not functional for
//  all of JSON system
//  http://d.hatena.ne.jp/thinca/20111006/1317832338

//  An article on boost::optional is
//  http://d.hatena.ne.jp/gintenlabo/20100606/1275854791

#include <iostream>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/optional.hpp>

int main(int argc, char *argv[])
{
  boost::property_tree::ptree pt;

  pt.put("Data1", 3.456);
  pt.put("Data2.value", 3);
  pt.put("Data2.str", "Hello");

  boost::property_tree::write_json("data.json", pt);

  //--- read ---
  // Data1
  if (boost::optional<double> x = pt.get_optional<double>("Data1"))
    {
      std::cout << "Data1: " << x.get() << std::endl;
    }
  else
    {
      std::cout << "Data1 is nothing" << std::endl;
    }

  // Data2.str
  if (boost::optional<std::string> str = pt.get_optional<std::string>("Data2.str")) 
    {
      std::cout << "Data2.str : " << str.get() << std::endl;
    }
  else 
    {
      std::cout << "Data2.str is nothing" << std::endl;
    }

  return 0;
}
