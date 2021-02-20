#include <iostream>

int fib(int n)
{
	if (n < 0) {
		return -1;
	}

	int res[2] = {0, 1};

	while (n-- > 0) {
		int tmp = res[1];
		res[1] += res[0];
		res[0] = tmp;
	}

	return res[0];
}

int main(void)
{
	int n;

	while (1) {
		std::cout << "enter n: ";

		if (!(std::cin >> n)) {
			break;
		}

		std::cout << "fib number is: " << fib(n) << std::endl;
	}

	return 0;
}
