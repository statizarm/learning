/*
** Copyright 18.08.2022 statizarm
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <stack>

using adj_type = std::unordered_map<int, std::unordered_set<int>>;

adj_type &insert_edge(adj_type *adj, int start, int end) {
  if (auto it = adj->find(start); it != adj->end()) {
    it->second.insert(end);
  } else {
    adj->insert({start, adj_type::mapped_type {end}});
  }

  return *adj;
}

std::istream &operator>>(std::istream &in, adj_type &adj) {
  int n;
  in >> n;
  while (--n >= 0) {
    int start, end;
    in >> start >> end;
    insert_edge(&adj, start, end);
  }
  return in;
}

std::ostream &operator<<(std::ostream &out, const adj_type &adj) {
  for (const auto &p : adj) {
    for (auto v : p.second) {
      out << p.first << " -> " << v << std::endl;
    }
  }
  return out;
}

std::stack<adj_type::mapped_type::value_type>
topological_sort(adj_type adj) {
  std::stack<adj_type::mapped_type::value_type> order;

  std::stack<
      std::pair<adj_type::key_type, adj_type::mapped_type::iterator>
  > stack;
  std::unordered_set<adj_type::key_type> visited;

  for (auto &p : adj) {
    if (visited.find(p.first) != visited.end()) {
      continue;
    }

    stack.emplace(p.first, p.second.begin());
    while (!stack.empty()) {
      auto &[u, neighs] = stack.top();
      visited.insert(u);

      while (neighs != adj[u].end() && visited.find(*neighs) != visited.end()) {
        ++neighs;
      }

      if (neighs == adj[u].end()) {
        order.push(u);
        stack.pop();
      } else {
        stack.emplace(*neighs, adj[*neighs].begin());
      }
    }
  }

  return order;
}

adj_type transpose(const adj_type &adj) {
  adj_type transposed;

  for (const auto &p : adj) {
    for (auto v : p.second) {
      if (auto it = transposed.find(v); it != transposed.end()) {
        it->second.insert(p.first);
      } else {
        transposed.insert({v, adj_type::mapped_type {p.first}});
      }
    }
  }

  return transposed;
}


adj_type component_graph(adj_type adj) {
  auto order = topological_sort(adj);
  auto transposed = transpose(adj);

  adj_type::key_type component_counter {};
  std::unordered_map<adj_type::key_type, adj_type::key_type> component_table;
  std::stack<
      std::pair<adj_type::key_type, adj_type::mapped_type::iterator>> stack;
  adj_type component_graph;
  for (; !order.empty(); order.pop()) {
    auto u = order.top();
    if (component_table.find(u) != component_table.end()) {
      continue;
    }

    stack.emplace(u, transposed[u].begin());
    while (!stack.empty()) {
      auto &[u, neighs] = stack.top();
      component_table[u] = component_counter;

      while (neighs != transposed[u].end()) {
        auto it = component_table.find(*neighs);
        if (it == component_table.end()) {
          break;
        } else if (it->second != component_counter) {
          insert_edge(&component_graph, it->second, component_table[u]);
        }
        ++neighs;
      }

      if (neighs == transposed[u].end()) {
        stack.pop();
      } else {
        stack.emplace(*neighs, transposed[*neighs].begin());
      }
    }

    ++component_counter;
  }

  return component_graph;
}

bool is_semiconnected(const adj_type &adj) {
  adj_type component_adj = component_graph(adj);
  auto order = topological_sort(component_adj);
  if (order.size() == 1) {
    return true;
  }

  int prev = order.top();
  order.pop();
  while (!order.empty()) {
    int curr = order.top();
    if (component_adj[prev].find(curr) == component_adj[prev].end()) {
      return false;
    }
    prev = curr;
    order.pop();
  }

  return true;
}

int main() {
  adj_type adj;

  std::cin >> adj;

  auto component_adj = component_graph(adj);

  std::cout << component_adj << std::endl;

  if (is_semiconnected(adj)) {
    std::cout << "graph is semiconnected" << std::endl;
  } else {
    std::cout << "graph isn't semiconnected" << std::endl;
  }

  return 0;
}
