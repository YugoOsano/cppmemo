// h5c++ hdf5_practice.cc
// h5c++ -show (to see the equivalent g++ option)

#include <H5Cpp.h>
#include <string>

int main () {
  const std::string fname("h5sample.h5");
  hid_t fid = H5Fcreate(fname.c_str(),H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);

  return 0;
}
