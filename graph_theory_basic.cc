// Ref: Basic of Graph & Network Algorithm
//      authored by Takao Asano

// A directed graph is expressed by
// a set of functions (edge -> vertex)

// E           1  2  3  4  5  6  7  8  9
// round d+(e) v1 v1 v6 v6 v4 v5 v3 v2 v4 (tail)
// round d-(e) v2 v5 v2 v5 v1 v4 v6 v3 v3 (head)

// where the direction is [tail -> head]

// complete data structure of a directed graph:
// a set of inverse functions
// delta+(v)/delta-(v) -> list of edges

#include <vector>

int main () {

  //-- zero index is unused in this book
  std::vector<unsigned> tail{0,1,1,6,6,4,5,3,2,4};
  std::vector<unsigned> head{0,2,5,2,5,1,4,6,3,3};
  
  return 0;
}

