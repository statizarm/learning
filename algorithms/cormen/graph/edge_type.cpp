/*
** Copyright statizarm 30.07.2021
** This is 22.3.10 exercise
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <stack>

struct Graph {
  using row_type = std::unordered_set<int>;
  using neigh_iterator = row_type::iterator;
  std::unordered_map<int, row_type> adj;
};

struct Timestamp {
  static constexpr int unvisited_value = 0;
  static constexpr int begin_value = unvisited_value + 1;
  Timestamp() : d(unvisited_value), f(unvisited_value) { }

  int d;
  int f;
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
  }

  return in;
}

int main() {
  Graph g;

  std::cin >> g;

  std::stack<std::pair<int, Graph::neigh_iterator>> stack;
  std::unordered_map<int, Timestamp> times;
  int t = Timestamp::begin_value;

  for (auto &p : g.adj) {
    if (times[p.first].d != Timestamp::unvisited_value) {
      continue;
    }

    stack.emplace(p.first, p.second.begin());
    times[p.first].d = t++;
    while (!stack.empty()) {
      auto &[u, it] = stack.top();
      auto &stamp = times[u];

      if (it == g.adj[u].end()) {
        stamp.f = t++;
        stack.pop();
      } else {
        int v = *it++;
        if (times[v].d == Timestamp::unvisited_value) {
          std::cout << u << " " << v << " - tree edge" << std::endl;
          stack.emplace(v, g.adj[v].begin());
          times[v].d = t++;
        } else if (stamp.d < times[v].f && stamp.d > times[v].d) {
          std::cout << u << " " << v << " - back edge" << std::endl;
        } else if (stamp.d < times[v].d) {
          std::cout << u << " " << v << " - forward edge" << std::endl;
        } else if (stamp.d > times[v].f) {
          std::cout << u << " " << v << " - cross edge" << std::endl;
        }
      }
    }
  }

  for (auto [u, t] : times) {
    std::cout << u << '\t' << t.d << "/" << t.f << std::endl;
  }

  return 0;
}
