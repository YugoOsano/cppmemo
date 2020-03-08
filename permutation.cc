#include <algorithm>
#include <string>
#include <iostream>

//-- transcribed from
// https://en.cppreference.com/w/cpp/algorithm/next_permutation
template<class BidirIt>
bool next_permutation_trans(BidirIt first, BidirIt last)
{
    if (first == last) return false;
    BidirIt i = last;
    if (first == --i) return false;//-- i is the last elem
 
    while (true) {
        BidirIt i1, i2;

	//-- at the initial loop, i1 is the last elem
        i1 = i;
	//-- i steps back (then compared);
	//   this if statement means search of
	//   initial 'ordered' pairs of elements like 'ab'
	//   in the reverse direction;
	//   In "abcd" for example, "cd" is the initial ordered pair
	//   from the last of the string.
        if (*--i < *i1) {
            i2 = last;
            while (!(*i < *--i2))
                ;
            std::iter_swap(i, i2);
            std::reverse(i1, last);
            return true;
        }
        if (i == first) {
            std::reverse(first, last);
            return false;
        }
    }
}

int main () {
  std::string s("ABBC");//("aba");
  std::sort(s.begin(), s.end());
  do {
    std::cout << s << '\n';
  } while (next_permutation_trans(s.begin(), s.end()));
  return 0;
}
/* string "ABBC"'s sequence
ABBC
ABCB
ACBB
BABC
BACB
BBAC
BBCA
BCAB
BCBA
CABB
CBAB
CBBA
*/
