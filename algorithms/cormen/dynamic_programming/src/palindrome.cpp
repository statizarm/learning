#include <iostream>

// 1 2 4 3 2 1
//

auto palindrome(const std::vector<int> &seq)
{
	int seq_len = seq.size() + 1;
	std::vector<std::vector<int>> lengths(seq_len);

	for (auto &row : lengths) {
		row.resize(seq_len);
	}
	--seq_len;

	int i = 1;
	int j = seq_len;
	for (; i < seq_len - lenghts[i][j]; ++i) {
		for (j = seq_len - 1; j > i; --j) {
			if (seq[i - 1] == seq[j]) {
				lengths[i][j] = lengths[i - 1][j + 1] + 2;
			} else {
				lengths[i][j] = std::max(lengths[i - 1][j], lengths[i][j + 1]);
			}
		}
		// Что-то надо с этим сделать
		lengths[i][j] = lengths[i][j + 1] + 1;
	}

	std::vector<int> mps(lengths[i][j]);
	while (i > 0 && j < seq_len) {
		if (lengths[i][j] == lengths[i - 1][j]) {
			--i;
		} else if (lengths[i][j] == lengths[i][j + 1]) {
			++j;
		} else {
			mps[i] = seq[i];
			mps[j] = seq[j];
			--i;
			++j;
		}
	}

	return mps;
}

int main(void)
{
	int n_elems;

	std::cout << "Enter number of elements in sequence: ";
	std::cin >> n_elems;

	std::vector<int> seq(n_elems);
	std::cout << "Enter elemets separeted by space:" << std::endl;
	for (auto &elem : seq) {
		std::cin >> elem;
	}

	pal = palindrome(seq);

	std::cout << "Length of maximum palindromic subsequence is " << pal.size() << std::endl;
	std::cout << "Maximum palindromic subsequence:";
	for (const auto &elem: pal) {
		std::cout << " " << elem;
	}
	std::cout << std::endl;
	return 0;
}
