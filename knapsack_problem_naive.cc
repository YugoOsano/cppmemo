#include <algorithm>
#include <stdio.h>

const int MAX_N = 10000;

int n = 4, W = 5;
int w[MAX_N] = {2,1,3,2};
int v[MAX_N] = {3,2,4,2};

//-- i th item
int rec(int i, int weight_limit) {
  int res;
  if (i == n) {
    //-- no remaining
    res = 0;
  } else if (weight_limit < w[i]) {
    // weight of ith item is over capacity
    res = rec(i + 1, weight_limit);
  } else {
    res = std::max(rec(i + 1, weight_limit), // weight of ith item is over capacity
		   rec(i + 1, weight_limit - w[i]) + v[i]);// take ith item in
  }
  return res;
}

void solve() {
  printf("%d\n", rec(0, W));
}

int main() {
  solve();
  return 0;
}
