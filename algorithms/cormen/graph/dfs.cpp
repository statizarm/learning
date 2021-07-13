/*
** Copyright 13.07.2021 statizarm
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <stack>

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
      g.adj.insert({start, std::unordered_set<int> {end}});
    } else {
      it->second.insert(end);
    }

    if (auto it = g.adj.find(end); it == g.adj.end()) {
      g.adj.insert({end, std::unordered_set<int> {start}});
    } else {
      it->second.insert(start);
    }
  }

  return in;
}

int main() {
  Graph g;

  std::cin >> g;

  std::unordered_set<int> visited;
  std::stack<int> stack;
  stack.push(g.adj.begin()->first);

  for (auto v = stack.top(); !stack.empty(); v = stack.top()) {
    if (visited.find(v) != visited.end() || g.adj[v].empty()) {
      std::cout << v << std::endl;
      stack.pop();
      continue;
    }

    visited.insert(v);

    for (auto u : g.adj[v]) {
      if (visited.find(u) == visited.end()) {
        stack.push(u);
      }
    }
  }

  return 0;
}


