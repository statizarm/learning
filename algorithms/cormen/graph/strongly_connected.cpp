/*
** Copyright 15.08.2021 statizarm
*/
#include <iostream>
#include <unordered_set>
#include <unordered_map>
#include <stack>
#include <list>
#include <queue>

using adj_type = std::unordered_map<int, std::list<int>>;

int main() {
  adj_type adj;
  adj_type transp;

  int n;
  std::cin >> n;
  while (--n >= 0) {
    int start, end;
    std::cin >> start >> end;
    if (auto it = adj.find(start); it != adj.end()) {
      it->second.push_back(end);
    } else {
      adj.insert({start, std::list<int> {end}});
    }

    if (auto it = transp.find(end); it != adj.end()) {
      it->second.push_back(start);
    } else {
      transp.insert({end, std::list<int> {start}});
    }
  }

  std::unordered_set<int> visited;
  std::stack<std::pair<int, adj_type::mapped_type::iterator>> stack;
  std::stack<int> order;

  for (auto &p : adj) {
    if (visited.find(p.first) != visited.end()) {
      continue;
    }

    stack.emplace(p.first, p.second.begin());

    while (!stack.empty()) {
      auto &[u, neigh] = stack.top();
      visited.insert(u);

      while (neigh != adj[u].end() && visited.find(*neigh) != visited.end()) {
        ++neigh;
      }

      if (neigh == adj[u].end()) {
        stack.pop();
        order.push(u);
      } else {
        stack.emplace(*neigh, adj[*neigh].begin());
      }
    }
  }

  std::cout << "------------------------------------------" << std::endl;
  std::cout << "order size: " << order.size() << std::endl;

  visited.clear();
  for (; !order.empty(); order.pop()) {
    auto u = order.top();
    if (visited.find(u) != visited.end()) {
      std::cout << "visited: " << u << std::endl;
      continue;
    }

    stack.emplace(u, transp[u].begin());
    while (!stack.empty()) {
      auto &[u, neigh] = stack.top();
      visited.insert(u);

      while (neigh != transp[u].end() &&
          visited.find(*neigh) != visited.end()) {
        neigh++;
      }

      if (neigh != transp[u].end()) {
        stack.emplace(*neigh, transp[*neigh].begin());
      } else {
        stack.pop();
        std::cout << u << " ";
      }
    }

    std::cout << std::endl;
  }

  return 0;
}
