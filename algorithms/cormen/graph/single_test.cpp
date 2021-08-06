/*
** Copyright statizarm 02.08.2021
*/
#include <iostream>
#include <unordered_set>
#include <unordered_map>
#include <stack>

struct Graph {
  using row_type = std::unordered_set<int>;
  using neigh_iterator = row_type::iterator;

  std::unordered_map<int, row_type> adj;
};

struct Timestamp {
  static constexpr int not_visited_value = 0;
  static constexpr int begin_value = not_visited_value + 1;

  Timestamp() : d(not_visited_value), f(not_visited_value) { }

  int d;
  int f;
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
      g.adj.insert({start, Graph::row_type {end}});
    }
  }
  return in;
}

bool contains_cross_forward(Graph *g, int u) {
  std::stack<std::pair<int, Graph::neigh_iterator>> stack;
  std::unordered_map<int, Timestamp> times;

  int t = Timestamp::begin_value;
  stack.emplace(u, g->adj[u].begin());
  times[u].d = t++;
  while (!stack.empty()) {
    auto &[u, it] = stack.top();
    if (it == g->adj[u].end()) {
      times[u].f = t++;
      stack.pop();
    } else {
      auto v = *it++;
      if (times[v].d == Timestamp::not_visited_value) {
        times[v].d = t++;
        stack.emplace(v, g->adj[v].begin());
      } else if (times[u].d < times[v].d ||
                 times[v].f != Timestamp::not_visited_value &&
                 times[u].d > times[v].f) {
        return true;
      }
    }
  }

  return false;
}

int main() {
  Graph g;

  std::cin >> g;

  for (auto &[u, n] : g.adj) {
    if (contains_cross_forward(&g, u)) {
      std::cout << "Not single connected" << std::endl;
      return 0;
    }
  }
  std::cout << "Single connected" << std::endl;
  return 0;
}

