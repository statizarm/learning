#include <iostream>
#include <vector>
#include <string>

int lcs_length(const std::string &x, const std::string &y)
{
	std::vector<std::vector<int>> max_length;

	int x_len = x.size();
	int y_len = y.size();

	max_length.resize(x_len + 1);

	for (auto &row : max_length) {
		row.resize(y_len + 1);
		row[0] = 0;
	}
	for (auto &elem : max_length[0]) {
		elem = 0;
	}

	const char *x_raw = x.c_str() - 1;
	const char *y_raw = y.c_str() - 1;
	for (int i = 1; i <= x_len; ++i) {
		for (int j = 1; j <= y_len; ++j) {
			int max_val;

			if (x_raw[i] == y_raw[j]) {
				max_val = max_length[i-1][j-1] + 1;
			} else if ((max_val = max_length[i][j-1]) <
				max_length[i-1][j]) {

				max_val = max_length[i-1][j];
			}

			max_length[i][j] = max_val;
		}
	}

	return max_length[x_len][y_len];
}

int main(int argc, char *argv[])
{
	std::string x;
	std::string y;

	std::cout << "LSC of x and y length program" << std::endl;

	std::cout << "Enter x: ";
	std::cin >> x;

	std::cout << "Enter y: ";
	std::cin >> y;

	std::cout << "LSC length: " << lcs_length(x, y) << std::endl;
	return 0;
}
