#include <iostream>
#include <vector>

std::vector<std::vector<int>>
matrix_chain_order(std::vector<int> &seq) {
	std::vector<std::vector<int>> min_costs;
	min_costs.resize(seq.size() - 1);
	int n = min_costs.size();

	for (int i = 0; i < n; ++i) {
		min_costs[i].resize(n);
		min_costs[i][i] = 0;
	}

	for (int l = 2; l <= n; ++l) {
		for (int i = 0; i <= n - l; ++i) {
			int j = i + l - 1;
			min_costs[i][j] = min_costs[i + 1][j] +
				seq[i] * seq[i + 1] * seq[j + 1];

			for (int k = i; k < j; ++k) {
				int q = min_costs[i][k] + min_costs[k + 1][j] +
					seq[i] * seq[k + 1] * seq[j + 1];

				if (min_costs[i][j] > q) {
					min_costs[i][j] = q;
				}
			}
		}
	}

	return min_costs;
}

int main()
{
	std::vector<int> seq;
	int n;

	std::cout << "Enter number of matrices: ";

	std::cin >> n;

	seq.resize(n);

	std::cout << "Enter sequence:" << std::endl;

	for (int i = 0; i < n; ++i) {
		std::cin >> seq[i];
	}

	std::vector<std::vector<int>> min_costs = matrix_chain_order(seq);

	int i;
	int j;

	std::cout << "Enter start and end indexes of sequence: ";
	while (std::cin >> i >> j) {
		std::cout << min_costs[i][j] << std::endl;
		std::cout << "Enter start and end indexes of sequence: ";
	}

	std::cout << "exiting..." << std::endl;

	return 0;
}

