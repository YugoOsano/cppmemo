#include <algorithm>
#include <stdio.h>
#include <execinfo.h>
#include <cstring> // memset

//-- ref about execinfo:
//  http://man7.org/linux/man-pages/man3/backtrace.3.html

const int MAX_N = 10000;
const int BT_BUF_SIZE = 100;

const int n = 4, W = 5;
int w[MAX_N] = {2,1,3,2};
int v[MAX_N] = {3,2,4,2};
int MemoArray[n+1][W+1];// n+1, W+1

//-- see stack by bt command in gdb
//-- i th item
int rec(int i, int remaining_weight_capacity) {
  if (MemoArray[i][remaining_weight_capacity] >= 0) {
    printf("%d th item; remaining capacity: %d was memoized.\n",
	   i, remaining_weight_capacity);
    return MemoArray[i][remaining_weight_capacity];
  }
  //--- this region for monitoring stack state (execinfo)
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
  const int to_return =
    std::max(
	     // this is for skipping ith item
	     rec(i + 1, remaining_weight_capacity),
	     // take ith item in
	     // (remaining weight reduced, value added)
	     rec(i + 1, remaining_weight_capacity - w[i]) + v[i]
	     );
  MemoArray[i][remaining_weight_capacity] = to_return;
  return to_return;
}

void solve() {
  printf("size of MemoArray: %ld\n", sizeof(MemoArray));
  std::memset(MemoArray, -1, sizeof(MemoArray));
  printf("%d\n", rec(0, W));
}

//-- create the following search tree (naive version) --
/*
(start: -1)
|         \     \   \ \
0          1     2   3 E
| \  \ \   |\\   |\  |
1  2  3 E  2 3E  3 E E
|\\ \\ \   |\ \  |
2 3E 3E E  3 E E E
|\ \  \    |
3 E E  E   E
|
(End: 4)
 */
void Search(const int index,
	    const int w_sum,
	    const int v_sum,
	    const int n_of_item) {
  if (index == n_of_item) {
    printf("index: %d, sum of weight: %d, sum of value: %d\n",
	   index, w_sum, v_sum);
    if (w_sum <= 5)
      printf("weight OK\n");
    return;
  }
  //-- loop over index after self
  for (int i = index + 1; i <= n_of_item; i++) {
    Search(i,
	   w_sum + w[index],
	   v_sum + v[index],
	   n_of_item);
  }
}

int main() {
  solve();
  //Search (-1, 0, 0, n);
  return 0;
}
