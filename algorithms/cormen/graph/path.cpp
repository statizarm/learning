/*
** Copyright 12.07.2021 statizarm
** This is the 22.2.9 exercise
*/
#include <iostream>
#include <unordered_map>
#include <unordered_set>

struct Graph {
  std::unordered_map<int, std::unordered_set<int>> adj;
};

class Color {
 public:
  enum color_id {WHITE, GRAY, BLACK};
  Color() : id_(Color::WHITE) { }
  explicit Color(color_id id) : id_(id) { }

  Color(const Color &other) = default;
  Color(Color &&other) = default;

  Color &operator=(const Color &other) = default;
  Color &operator=(Color &&other) = default;

  bool operator==(Color other) const {return this->id_ == other.id_;}
  bool operator==(color_id id) const {return this->id_ == id;}
 private:
  color_id id_;
};

template<class T>
bool operator!=(const Color c, T&& v) {
  return !(c == v);
}

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

void path(const Graph &g, int node, std::unordered_map<int, Color> *visited) {
  std::cout << node;
  if ((*visited)[node] == Color::BLACK) {
    return;
  }

  (*visited)[node] = Color(Color::GRAY);

  for (auto v : g.adj.find(node)->second) {
    if ((*visited)[v] != Color::GRAY) {
      path(g, v, visited);
      std::cout << node;
    }
  }

  (*visited)[node] = Color(Color::BLACK);
}

int main() {
  Graph g;

  std::cin >> g;

  std::unordered_map<int, Color> visited;
  int start = g.adj.begin()->first;

  path(g, start, &visited);

  return 0;
}
