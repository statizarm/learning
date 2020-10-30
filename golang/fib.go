package fib

func RecursiveFib (n int) int {
	if (n == 1) {
		return 0
	}
	if (n == 2) {
		return 1
	}

	return RecursiveFib (n - 1) + RecursiveFib (n - 2)
}

func DynFib (n int) int {
	n -= 1
	var f = [...]int {0, 1}

	for n > 1 {
		f[0] = f[0] + f[1]
		f[1] = f[0] + f[1]
		n -= 2
	}

	return f[n]
}
