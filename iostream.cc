/*
 * choose a file handler in constructor from ofstream or null 
 */
// http://stackoverflow.com/questions/8243743/is-there-a-null-stdostream-implementation-in-c-or-libraries

#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/null.hpp>
#include <fstream>

class Output{

public:
  Output(bool is_open_file){
    if(is_open_file)
      fp = new std::ofstream("foo.dat");
    else
      fp = new boost::iostreams::stream< boost::iostreams::null_sink >
	(boost::iostreams::null_sink());
  }
  virtual ~Output(){
    delete fp;
  }
  void writefoo(){
    *fp << "foo\n";
  }
protected:
  std::ostream* fp;
};

int main(){
  Output output(false);// true or false
  output.writefoo();

  return 0;
}
