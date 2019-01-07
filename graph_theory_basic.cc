/* Ref: Basic of Graph & Network Algorithm
      authored by Takao Asano

 A directed graph is expressed by
 a set of functions (edge -> vertex)

 ---------------------------------------------
 E(edge)     e1 e2 e3 e4 e5 e6 e7 e8 e9
 round d+(e) v1 v1 v6 v6 v4 v5 v3 v2 v4 (tail)
 round d-(e) v2 v5 v2 v5 v1 v4 v6 v3 v3 (head)
 ---------------------------------------------

 where the direction is [tail -> head]

 complete data structure of a directed graph:
 a set of inverse functions
 delta+(v)/delta-(v) -> list of edges

 edgefirst/edgenext are taken to be functions: 
 [v1-v6] in tail row to E 

 -------------------------------------
 V(vertex)  v1 v2 v3 v4 v5 v6
 edgefirst  e1 e8 e7 e5 e6 e3 
 (<- here come the smallest number of e among v (tail))

 E(edge)    e1 e2 e3 e4 e5 e6 e7 e8 e9
 edgenext   e2  0 e4  0 e9  0  0  0  0 
 (<- this plays role of linked list which starts from edgefirst)
 ------------------------------------- 
*/

#include <vector>
#include <algorithm>
#include <iostream>

std::vector<unsigned> GetEdgeFirst(const std::vector<unsigned>& tail) {

  const unsigned n_vertices = tail.size() - 1;
  const unsigned n_edges = *(std::max_element(tail.cbegin(),
					      tail.cend()));
  std::vector<unsigned> to_return(n_edges + 1, 0);
  for (unsigned i_vertex = 1; i_vertex < n_vertices + 1; i_vertex++) {

    const unsigned index_to_update = tail[i_vertex];

    //-- update only for the first time (to assign the smallest)
    if (to_return[index_to_update] == 0)
      to_return[index_to_update] = i_vertex;
  }
  return to_return;
}

int main () {

  //-- zero index is unused in this book
  std::vector<unsigned> tail{0,1,1,6,6,4,5,3,2,4};
  std::vector<unsigned> head{0,2,5,2,5,1,4,6,3,3};

  const std::vector<unsigned>& edgefirst
    = GetEdgeFirst(tail);

  for (const auto edge : edgefirst)
    std::cout << edge << std::endl;
  
  return 0;
}

