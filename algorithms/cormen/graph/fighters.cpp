/*
** This is 22.2.7 task
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>

struct Graph {
  std::unordered_map<int, std::unordered_set<int>> adj;
};

std::ostream &operator<<(std::ostream &out, const Graph &g) {
  for (auto &[vertex, neigh_set]: g.adj) {
    for (auto &neigh : neigh_set) {
      out << vertex << " -> " << neigh << std::endl;
    }
  }

  return out;
}

std::istream &operator>>(std::istream &in, Graph &g) {
  int start_vertex;
  int end_vertex;
  int n;

  in >> n;

  while (--n >= 0) {
    in >> start_vertex >> end_vertex;

    if (auto it = g.adj.find(start_vertex); it != g.adj.end()) {
      it->second.
    }
  }
}

int main(int argc, char *argv[]) {
  
