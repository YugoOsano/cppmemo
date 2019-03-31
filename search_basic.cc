// a graph is modelled by a vector of Node intances in this sample
// compile with -std=c++11

#include <vector>
#include <map>
#include <iostream>

class Node {
public:
  Node(const int               label,
       const std::vector<int>& labels) :
    label_(label),
    labels_connected_node_(labels),
    is_visited_(false) {}
  const int               label_;
  const std::vector<int>  labels_connected_node_;
  bool                    is_visited_;
};

bool DepthFirstSearch(const int                target_label,
		      Node&                    node,
		      std::vector<Node>&       graph) {
  // turn is_visited flag
  node.is_visited_ = true;
  //-- scan nonnected node --
  for (const int label_connected_node : node.labels_connected_node_) {

    Node& node_to_scan = graph.at(label_connected_node);
    //-- skip scanning if the concerned node is already visited
    if (node_to_scan.is_visited_) continue;

    std::cout << "label: " << node_to_scan.label_ << std::endl;
    //-- label check --
    if (node_to_scan.label_ == target_label) {
      std::cout << "target: " << target_label
		<< " exists." << std::endl;
      return true;
    }
    const bool is_target_exist =
      DepthFirstSearch(target_label,
		       node_to_scan,
		       graph);						  
    if(is_target_exist) {
      return true;
    }
  }
  return false;
}
 
int main () {
  // DFS will go by: 0->1->3->2->4->5
  std::vector<Node> graph {
      Node(0, std::vector<int>{1,4,5}),//0->1, 0->4, 0->5
      Node(1, std::vector<int>{3,4}),//1->3, 1->4
      Node(2, std::vector<int>{1}),//2->1 
      Node(3, std::vector<int>{2,4}),//3->2, 3->4
      Node(4, std::vector<int>{}),//4
      Node(5, std::vector<int>{})//5
  };
  DepthFirstSearch(6, graph.at(0), graph);
  
  return 0;
}
