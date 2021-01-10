#include <iostream>
#include <vector>
#include <fstream>
#include <map>

double rod_cut(const std::map<double, double> &price_list, double len)
{
	std::map<double,double> max_prices;

	for (auto it = price_list.begin(); it != price_list.end(); ++it) {
		if ((*it).first > len) {
			break;
		}

		double max = (*it).second;

		for (auto jt = price_list.begin(); jt != it; ++jt) {
			double price = (*jt).second;
			double delta = (*it).first - (*jt).first;

			if (max_prices.find(delta) != max_prices.end()) {
				price += max_prices[delta];
			}
			
			if (max < price) {
				max = price;
			}
		}

		max_prices[(*it).first] = max;
	}

	double res_price = 0;
	for (auto it = max_prices.begin(); it != max_prices.end(); ++it) {
		double price = (*it).second;
		double delta = len - (*it).first;
		if (max_prices.find(delta) != max_prices.end()) {
			price += max_prices[delta];
		}

		if (res_price < price) {
			res_price = price;
		}
	}

	return res_price;
}

int main (int argc, char *argv[])
{
	int n;
	std::vector<double> lens;
	std::map<double, double> price_list;

	std::cin >> n;

	lens.reserve(n);

	for (int i = 0; i < n; ++i) {
		std::cin >> lens[i];
	}

	for (int i = 0; i < n; ++i) {
		double price;
		std::cin >> price;
		price_list.insert(std::make_pair(lens[i], price));
	}

	std::cout << "enter length: ";

	double len;
	std::cin >> len;

	std::cout << "max price is " << rod_cut(price_list, len)
	          << std::endl;

	return 0;
}
