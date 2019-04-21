// a graph is modelled by a vector of Node intances in this sample
// compile with -std=c++11

#include <vector>
#include <map>
#include <iostream>
#include <deque>

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

//bool DepthFirstSearchWithStack(const int                target_label,
//			       const int                start_label,
//			       const std::vector<Node>& graph) {
  //-- deep copy of the given graph
//  std::vector<Node> 
  
//  std::deque<int> label_queue;


bool BreadthFirstSearch(const int                target_label,
			Node&                    node,
			std::vector<Node>&       graph) {
  std::deque<int> label_queue;
  label_queue.push_back(node.label_);

  while (label_queue.size() > 0) {
    const int queue_size = label_queue.size();

    std::vector<int> labels_to_add;
    //-- loop about node remaining in the queue
    for (const int label : label_queue) {
      Node& ref_node = graph.at(label);
      //-- loop about connected nodes --
      for (const int label_connected_node : ref_node.labels_connected_node_) {
	if (!graph.at(label_connected_node).is_visited_) {
	  graph.at(label_connected_node).is_visited_ = true;
	  labels_to_add.push_back(label_connected_node);
	  std::cout << label_connected_node << " added to the queue." << std::endl;
	  if (label_connected_node == target_label) {
	    std::cout << "target: " << target_label << " found." << std::endl;
	    return true;
	  }
	}
      }
    }
    //-- add labels
    for (const int label_to_add : labels_to_add)
      label_queue.push_back(label_to_add);

    //-- remove scanned labels
    for (int i=0; i<queue_size; i++) {
      std::cout << label_queue.front() << " will be popped." << std::endl;
      label_queue.pop_front();
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
  //DepthFirstSearch(6, graph.at(0), graph);
  BreadthFirstSearch(6, graph.at(0), graph);
  
  return 0;
}
