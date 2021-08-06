/*
** Copyright 05.08.2021 statizarm
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <queue>

int main () {
  std::unordered_map<int, std::unordered_set<int>> adj;
  std::unordered_map<int, int> input_degree;
  std::queue<int> zero_queue;
  int n;

  std::cin >> n;

  while (--n >= 0) {
    int start, end;

    std::cin >> start >> end;
    input_degree[start] = input_degree[start];
    ++input_degree[end];

    if (auto it = adj.find(start); it != adj.end()) {
      it->second.insert(end);
    } else {
      adj.insert({start, std::unordered_set<int> {end}});
    }
  }

  for (auto [u, d] : input_degree) {
    if (d == 0) {
      zero_queue.push(u);
    }
  }

  while (!zero_queue.empty()) {
    auto u = zero_queue.front();

    std::cout << u << " ";

    for (auto v : adj[u]) {
      if (--input_degree[v] == 0) {
        zero_queue.push(v);
      }
    }

    zero_queue.pop();
  }

  std::cout << std::endl;

  return 0;
}
