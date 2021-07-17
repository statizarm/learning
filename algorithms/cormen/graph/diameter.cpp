/*
** Copyright 2021 statizarm
** this is the 2.2.8 task
*/
#include <unordered_map>
#include <unordered_set>
#include <iostream>
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

    if (auto it = g.adj.find(start); it == g.adj.end()) {
      g.adj.insert(std::make_pair(start, std::unordered_set<int> {end}));
    } else {
      it->second.insert(end);
    }

    if (auto it = g.adj.find(end); it == g.adj.end()) {
      g.adj.insert(std::make_pair(end, std::unordered_set<int> {start}));
    } else {
      it->second.insert(start);
    }
  }

  return in;
}

int main() {
  Graph g;

  std::cin >> g;

  std::queue<int> node_queue;
  std::unordered_set<int> visited;
  node_queue.push(g.adj.begin()->first);

  std::cout << "Starting..." << std::endl;
  int last;
  for (auto v = node_queue.front(); !node_queue.empty();) {
    visited.insert(v);
    last = v;

    for (auto u : g.adj[v]) {
      if (visited.find(u) == visited.end()) {
        node_queue.push(u);
      }
    }

    node_queue.pop();
    v = node_queue.front();
  }

  visited.clear();
  node_queue.push(last);
  int length = 0;
  std::cout << "Searching the farest leaf..." << std::endl;
  while (!node_queue.empty()) {
    ++length;

    auto size = node_queue.size();
    while (size-- > 0) {
      auto v = node_queue.front();
      visited.insert(v);

      for (auto u : g.adj[v]) {
        if (visited.find(u) == visited.end()) {
          node_queue.push(u);
        }
      }

      node_queue.pop();
    }
  }

  std::cout << length << std::endl;
}
