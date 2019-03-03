#include <algorithm>
#include <stdio.h>
#include <execinfo.h>

//-- ref about execinfo:
//  http://man7.org/linux/man-pages/man3/backtrace.3.html

const int MAX_N = 10000;
const int BT_BUF_SIZE = 100;

int n = 4, W = 5;
int w[MAX_N] = {2,1,3,2};
int v[MAX_N] = {3,2,4,2};

//-- see stack by bt command in gdb
//-- i th item
int rec(int i, int remaining_weight_capacity) {

  void *buffer[BT_BUF_SIZE];
  int nptrs = backtrace(buffer, BT_BUF_SIZE);
  printf("%d th item picked; remaining capacity: %d, bt: %d\n",
	 i, remaining_weight_capacity, nptrs);

  if (i == n) {
    //-- no remaining
    return 0;
  } else if (remaining_weight_capacity < w[i]) {
    // if the weight of ith item is over capacity, pick the next
    return rec(i + 1, remaining_weight_capacity);
  }
  return
    std::max(
	     // weight of ith item is over capacity
	     rec(i + 1, remaining_weight_capacity),
	     // take ith item in
	     rec(i + 1, remaining_weight_capacity - w[i]) + v[i]
	     );
}

void solve() {
  printf("%d\n", rec(0, W));
}

int main() {
  solve();
  return 0;
}
