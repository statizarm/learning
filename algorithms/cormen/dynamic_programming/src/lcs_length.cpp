#include <iostream>
#include <vector>
#include <string>

int lcs_length(const std::string &x, const std::string &y)
{
	std::vector<int> max_length;

	int x_len = x.size();
	int y_len = y.size();

	max_length.resize(y_len + 1);

	for (auto &elem : max_length) {
		elem = 0;
	}

	const char *x_raw = x.c_str() - 1;
	const char *y_raw = y.c_str() - 1;

	for (int i = 1; i <= x_len; ++i) {
		int prev = max_length[0];
		for (int j = 1; j <= y_len; ++j) {
			int max_val;

			if (x_raw[i] == y_raw[j]) {
				max_val = prev + 1;
			} else {
				max_val = max_length[j];
				int alt_val = max_length[j - 1];

				if (max_val < alt_val) {
					max_val = alt_val;
				}	
			}

			prev = max_length[j];
			max_length[j] = max_val;
		}
	}

	return max_length[y_len];
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
