#include <stdio.h>
#define MAX(a, b) (a > b) ? a : b
#define MIN(a, b) (a < b) ? a : b

int main (void)
{
	int array[] = {1, 56, 19, -12, 123, 55, 191, -132};
	int *first = array;
	int *last = &array[sizeof (array) / sizeof (int) - 1];
	int min, max;

	if (*first < *last) {
		min = *first;
		max = *last;
	} else {
		min = *last;
		max = *first;
	}

	while (++first < --last) {
		if (*first < *last) {
			min = MIN (*first, min);
			max = MAX (*last, max);
		} else {
			min = MIN (*last, min);
			max = MAX (*first, max);
		}
	}

	printf ("min - %d, max - %d\n", min, max);

	return 0;
}

