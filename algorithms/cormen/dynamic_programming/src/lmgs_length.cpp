#include <iostream>
#include <vector>
#include <memory>
#include <algorithm>

struct sequence {
	sequence(std::shared_ptr<struct sequence> pref, int val) :
		m_pref(pref), m_curr(val)
	{}

	std::shared_ptr<struct sequence> m_pref;
	int m_curr;
};

bool operator<(int val, std::shared_ptr<sequence> &seq)
{
	return val < seq->m_curr;
}

std::shared_ptr<sequence> lgms_length(std::vector<int> &seq)
{
	std::vector<std::shared_ptr<sequence>> max_len {nullptr};
	
	max_len.reserve(seq.size());

	auto end = max_len.end();
	auto begin = max_len.begin() + 1;
	for (auto val : seq) {
		auto it = std::upper_bound(begin, end, val);

		if (it == end) {
			max_len.resize(max_len.size() + 1);
			++end;
		}

		*it = std::make_shared<sequence>(*(it - 1), val);
	}
	
	return max_len.back();
}

std::ostream &operator<<(std::ostream &out, const std::shared_ptr<sequence> &seq)
{
	if (seq->m_pref == nullptr) {
		out << seq->m_curr;
	} else {
		out << seq->m_pref << " " << seq->m_curr;
	}

	return out;
}

int main()
{
	std::vector<int> seq;
	int n;

	std::cout << "Enter number of elements in sequence: ";
	std::cin >> n;

	std::cout << "Enter " << n << " element(s): ";

	seq.resize(n);

	for (auto &elem : seq) {
		std::cin >> elem;
	}

	std::cout << "lgms is: " << lgms_length(seq) << std::endl;

	return 0;
}
