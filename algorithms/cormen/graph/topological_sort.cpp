/*
** Copyright 03.08.2021 statizarm
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <stack>
#include <list>

struct Graph {
  using adj_type = std::unordered_map<int, std::unordered_set<int>>;
  using neighbour_iterator = adj_type::mapped_type::iterator;

  adj_type adj;
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
      g.adj.insert({start, Graph::adj_type::mapped_type {end}});
    }
  }

  return in;
}

bool is_acycled(Graph g) {
  std::stack<std::pair<int, Graph::neighbour_iterator>> stack;
  std::unordered_map<int, int> visited_time;
  std::unordered_map<int, int> handled_time;
  int time = 1;

  for (auto p : g.adj) {
    if (visited_time.find(p.first) != visited_time.end()) {
      continue;
    }

    stack.emplace(p.first, p.second.begin());
    visited_time[p.first] = time++;

    while (!stack.empty()) {
      auto &[u, n] = stack.top();
      if (n == g.adj[u].end()) {
        handled_time[u] = time++;
        stack.pop();
      } else if (auto v = *n++; visited_time[v] == 0) {
        visited_time[v] = time++;
        stack.emplace(v, g.adj[v].begin());
      } else if (handled_time[v] == 0) {
        return false;
      }
    }
  }

  return true;
}

std::list<int> topological_sort(Graph g) {
  std::unordered_set<int> visited;
  std::stack<std::pair<int, Graph::neighbour_iterator>> stack;
  std::list<int> sorted;

  for (auto p : g.adj) {
    if (visited.find(p.first) != visited.end()) {
      continue;
    }

    stack.emplace(p.first, p.second.begin());
    visited.insert(p.first);
    while (!stack.empty()) {
      auto &[u, n] = stack.top();

      while (n != g.adj[u].end() && visited.find(*n) != visited.end()) {
        ++n;
      }

      if (n == g.adj[u].end()) {
        sorted.push_front(u);
        stack.pop();
      } else {
        stack.emplace(*n, g.adj[*n].begin());
        visited.insert(*n);
      }
    }
  }

  return sorted;
}

int main() {
  Graph g;

  std::cin >> g;

  if (!is_acycled(g)) {
    std::cout << "Graph has simple cycle" << std::endl;
  } else {
    for (auto v : topological_sort(g)) {
      std::cout << v << " ";
    }

    std::cout << std::endl;
  }

  return 0;
}

