/*
** Copyright 13.07.2021 statizarm
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <stack>

struct Graph {
  using table_type = std::unordered_map<int, std::unordered_set<int>>;
  using child_iterator = table_type::mapped_type::iterator;
  table_type adj;
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
  int start;

  std::cin >> g;
  std::cin >> start;

  std::unordered_set<int> visited;
  std::stack<std::pair<int, typename Graph::child_iterator>> stack;
  stack.emplace(start, g.adj[start].begin());

  while (!stack.empty()) {
    auto &[v, it] = stack.top();

    visited.insert(v);

    while (it != g.adj[v].end() && visited.find(*it) != visited.end()) {
      ++it;
    }

    if (it == g.adj[v].end()) {
      stack.pop();
      std::cout << v << std::endl;
      continue;
    }

    stack.emplace(*it, g.adj[*it].begin());
 }

  return 0;
}


