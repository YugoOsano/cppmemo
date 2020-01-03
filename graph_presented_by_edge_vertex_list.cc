//-- revised expression of a graph
//   as one in Dr. Asano's text is hard to deal with
//   (see graph_theory_basic.cc)
#include <iostream>
#include <vector>

// vector<Edge> is initially defined as a graph,
//  where individual vector index presents the edge number; 
// vector<Vertex> is then created to make complete structure.
struct Edge {
  explicit Edge (const int from,
		 const int to) :
    from_(from), to_(to) {}
  const int from_;
  const int to_;
};

//-- see StackOverFlow (C++ derive from native type) + use of tag type
//https://stackoverflow.com/questions/1183554/c-derive-from-a-native-type
template <class T, class Tag = T>
class TypeWrapper {
public:
  explicit TypeWrapper(const T value) :
    value_(value) {}
  virtual ~TypeWrapper()=default;
  operator T() const {return value_;}
  void Set(const T& value) {value_ = value;}
  void operator= (const TypeWrapper<T,Tag>& x) {value_ = x.value_;}
private:
  T value_;
};
//-- tag types
struct TagA{};struct TagB{};

using VertexToFirstEdgeNo = TypeWrapper<int, TagA>;
using EdgeToNextEdgeNo    = TypeWrapper<int, TagB>;

//-- dummy function to confirm type incompatibility
void F() {
  VertexToFirstEdgeNo a(5),b(10);
  a=b;
  EdgeToNextEdgeNo c(20);
  //c=a;//<- this will be errors about type (successfully)
}

int main () {
  std::vector<Edge> edge_list {
    Edge(0,1), Edge(0,4), Edge(0,5),
    Edge(1,3), Edge(1,4),
    Edge(2,1), 
    Edge(3,2), Edge(3,4)};
  const int vertex_maximum = 5;
  
  std::vector<VertexToFirstEdgeNo>
    vertex_to_first_edge_number(vertex_maximum + 1,
				VertexToFirstEdgeNo(-1));
  for (int i_edge = 0;
       i_edge < edge_list.size(); i_edge++) {
    const int from = edge_list.at(i_edge).from_;
    if (vertex_to_first_edge_number.at(from) == -1) {
      vertex_to_first_edge_number.at(from).Set(i_edge);
    }
  }
  
  VertexToFirstEdgeNo a(123);
  std::cout << a << std::endl;
  a.Set(246);
  std::cout << a << std::endl;
  return 0;
}

  
