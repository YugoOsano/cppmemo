// this is from the official boost website
// https://boostjp.github.io/tips/graph.html

#include <iostream>
#include <vector>
#include <deque>
#include <string>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/assign/list_of.hpp>

typedef boost::adjacency_list<boost::listS, boost::vecS, boost::directedS,
    boost::no_property, boost::property<boost::edge_weight_t, int> > Graph;
typedef std::pair<int, int>                             Edge;
typedef boost::graph_traits<Graph>::vertex_descriptor   Vertex;

enum { S, A, B, C, D, E, F, Z, N };
const std::string Names = "SABCDEFZ";

// グラフを作る
Graph make_graph()
{
    const std::vector<Edge> edges = boost::assign::list_of<Edge>
        (S, A)
        (A, B)
        (B, C)
        (B, D)
        (C, E)
        (C, F)
        (D, F)
        (E, D)
        (F, E)
        (E, Z)
        (F, Z)
    ;

    const std::vector<int> weights = boost::assign::list_of
        (3)
        (1)
        (2)
        (3)
        (7)
        (12)
        (2)
        (11)
        (3)
        (2)
        (2)
    ;

    return Graph(edges.begin(), edges.end(), weights.begin(), N);
}
// practice about boost::graph
// https://www.boost.org/doc/libs/1_67_0/libs/graph/doc/adjacency_list.html
void HelloGraph(const Graph& g) {
  auto iter = boost::vertices(g);
  // type of first is:
  // class boost::range_detail::integer_iterator<unsigned long>
  auto first  = iter.first;
  std::cout << *first << std::endl;//0
  auto second = iter.second;
  std::cout << *second << std::endl;//8
}

int main()
{
    const Graph g = make_graph();
    HelloGraph(g);
    const Vertex from = S; // start point
    const Vertex to = Z; // goal

    // shortest path
    std::vector<Vertex> parents(boost::num_vertices(g));
    boost::dijkstra_shortest_paths(g, from,
                boost::predecessor_map(&parents[0]));

    // 経路なし
    if (parents[to] == to) {
        std::cout << "no path" << std::endl;
        return 1;
    }

    // 最短経路の頂点リストを作成
    std::deque<Vertex> route;
    for (Vertex v = to; v != from; v = parents[v]) {
        route.push_front(v);
    }
    route.push_front(from);

    // 最短経路を出力
    for (const Vertex v : route) {
        std::cout << Names[v] << std::endl;
    }
}
