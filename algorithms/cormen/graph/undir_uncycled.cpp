/*
** Copyright 03.08.2021 statizarm
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <queue>

struct Graph {
  std::unordered_map<int, std::unordered_set<int>> adj;
};

std::istream &operator>>(std::istream &in, Graph &g) {
  int n;
  in >> n;
  while (--n >= 0) {
    int start, end;

    in >> start >> end;

    if (auto it = g.adj.find(start); it != g.adj.end()) {
      it->second.insert(end);
    } else {
      g.adj.insert({start, std::unordered_set<int> {end}});
    }

    if (auto it = g.adj.find(end); it != g.adj.end()) {
      it->second.insert(start);
    } else {
      g.adj.insert({end, std::unordered_set<int> {start}});
    }
  }

  return in;
}

int main() {
  Graph g;

  std::cin >> g;

  std::queue<std::pair<int, int>> queue;
  std::unordered_set<int> visited;

  for (auto p : g.adj) {
    if (visited.find(p.first) != visited.end()) {
      continue;
    }

    queue.emplace(0, p.first);
    visited.insert(p.first);
    while (!queue.empty()) {
      auto [p, u] = queue.front();

      for (auto v : g.adj[u]) {
        if (v == p) {
          continue;
        }
        if (visited.find(v) == visited.end()) {
          queue.emplace(u, v);
          visited.insert(v);
        } else {
          std::cout << "Graph has simple cycle" << std::endl;
          return 0;
        }
      }
      queue.pop();
    }
  }
  return 0;
}

