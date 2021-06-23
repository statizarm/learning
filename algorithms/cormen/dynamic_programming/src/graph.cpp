#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>
#include <queue>
#include <tuple>

struct Edge {
	Edge() = default;
	Edge(int vertex, int weight) : vertex(vertex), weight(weight) {}

	int vertex;
	int weight;
};

auto find_longest_path(std::vector<std::vector<Edge>> &adj, int start, int end)
{
	std::vector<Edge> weights;

	weights.resize(adj.size());

	for (auto &edge : weights) {
		edge.weight = INT_MIN;
	}

	weights[start].vertex = start;
	weights[start].weight = 0;


	std::queue<int> vertex_queue;
	vertex_queue.push(start);
	while (vertex_queue.size() > 0) {
		int curr = vertex_queue.front();
		vertex_queue.pop();
		for (const auto &edge : adj[curr]) {
			vertex_queue.push(edge.vertex);
			int w = edge.weight + weights[curr].weight;
			if (w > weights[edge.vertex].weight) {
				weights[edge.vertex].weight = w;
				weights[edge.vertex].vertex = curr;
			}
		}
	}

	std::vector<int> path;
	for (int vertex = end; vertex != start; vertex = weights[vertex].vertex) {
		path.push_back(vertex);
	}
	path.push_back(start);

	std::reverse(path.begin(), path.end());

	return std::make_tuple(path, weights[end].weight);
}
	

int main(int argc, char *argv[])
{
	int n_vertexes = 0;

	int n_links = 0;

	int vertex = 0;
	int weight = 0;

	std::cout << "Enter number of vertexes: ";
	std::cin >> n_vertexes;

	std::vector<std::vector<Edge>> adj;
	adj.resize(n_vertexes);
	for (int i = 0; i < n_vertexes; ++i) {
		std::cout << "Enter number of links: ";

		std::cin >> n_links;

		for (int j = 0; j < n_links; ++j) {
			std::cout << "Enter link: (format - <vertex> <weight>): ";

			std::cin >> vertex >> weight;
			adj[i].emplace_back(vertex, weight);
		}
	}

	int start = 0;
	int end = 0;

	std::cout << "Enter start: ";
	std::cin >> start;

	std::cout << "Enter end: ";
	std::cin >> end;

	auto [path, total_w] = find_longest_path(adj, start, end);

	auto it = path.begin();
	std::cout << *it;
	while (++it != path.end()) {
		std::cout << " -> " << *it;
	}

	std::cout << std::endl;

	std::cout << "Total weight: " << total_w << std::endl;

	return 0;
}
	
