// translation of a symbolic perturbation FORTRAN sample
// written by Prof. Kokichi Sugihara

#include <iostream>
#include <vector>
#include <string>

void SortBN (int&                      nofp,
	     std::vector<int>&         ilist,
	     std::vector<int>&         name,
	     std::vector<int>&         jlist,
	     int&                      ichang) {
  for (int i=1; i <= nofp; i++) {
    jlist[i] = ilist[i];
  }
  ichang = 1;
  for (int ii = nofp - 1; ii >= 1; ii--) {
    for (int jj = 1; jj <= ii; jj++) {
      if (name[jlist[jj]] > name[jlist[jj+1]]) {
	int jtemp = jlist[jj];
	jlist[jj] = jlist[jj + 1];
	jlist[jj + 1] = jtemp;
	ichang = - ichang;
      }
    }
  }
}

int Isd1s(std::vector<int>& ia) {
  if (ia[2] > ia[1]) {
    return 1;
  } else if (ia[1] > ia[2]) {
    return -1;
  } 
  return 0;
}

// int ItLeft (int& iv, int& jv, int& kv, 
// 	    int& ix, int& iy, std::vector<int>& name) {
//   std::vector<int> ilist, jlist, ia1, ia2;

//   ilist.reserve(3);
//   ilist[0] = iv;
//   ilist[1] = jv;
//   ilist[2] = kv;

//   int ichang;
//   SortBN (3,
// 	  ilist,
// 	  name,
// 	  jlist,
// 	  ichang);

//   return 0;
// }
  

int main() {
  return 0;
}

  
