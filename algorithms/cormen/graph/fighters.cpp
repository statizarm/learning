/*
** Copyright 2021 statizarm
** This is 22.2.7 task
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <queue>

struct Graph {
  std::unordered_map<int, std::unordered_set<int>> adj;
};

std::ostream &operator<<(std::ostream &out, const Graph &g) {
  for (auto &[vertex, neigh_set] : g.adj) {
    for (auto &neigh : neigh_set) {
      out << vertex << " -- " << neigh << std::endl;
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
      it->second.insert(end_vertex);
    } else {
      g.adj.insert(
          std::make_pair(start_vertex, std::unordered_set<int> {end_vertex}));
    }

    if (auto it = g.adj.find(end_vertex); it != g.adj.end()) {
      it->second.insert(start_vertex);
    } else {
      g.adj.insert(
          std::make_pair(end_vertex, std::unordered_set<int> {start_vertex}));
    }
  }

  return in;
}

int main(int argc, char *argv[]) {
  Graph g;

  std::cin >> g;

  std::unordered_set<int> visited;
  std::unordered_set<int> teams[2];
  std::queue<std::pair<int, int>> vertex_queue;

  std::cout << "Start team dividing" << std::endl;
  std::cout << "Graph:" << std::endl << g;
  for (auto p : g.adj) {
    if (visited.find(p.first) != visited.end()) {
      continue;
    }

    vertex_queue.push({p.first, 0});
    visited.insert(p.first);
    teams[0].insert(p.first);

    for (auto [v, team_id] = vertex_queue.front(); !vertex_queue.empty();) {
      for (auto u : g.adj[v]) {
        if (visited.find(u) == visited.end()) {
          vertex_queue.push({u, team_id ^ 1});
          visited.insert(u);
          teams[team_id ^ 1].insert(u);
        } else if (teams[team_id].find(u) != teams[team_id].end()) {
          std::cout << "Unable to divide fighters" << std::endl;
          exit(-1);
        }
      }

      vertex_queue.pop();
      std::tie(v, team_id) = vertex_queue.front();
    }
  }

  std::cout << "First team:" << std::endl;
  for (auto v : teams[0]) {
    std::cout << v << " ";
  }
  std::cout << std::endl << "Second team:" << std::endl;
  for (auto v : teams[1]) {
    std::cout << v << " ";
  }
  std::cout << std::endl << "Graph:" << std::endl << g;
  return 0;
}

