#include <stdio.h>
#define _inline __attribute__ ((always_inline))
static int *find_min (int *array, int *stop);

void selection_sort (int *array, int length) {
	for (int *end = array + length; array < end; ++array) {
		int *min = find_min (array, end);

		int tmp = *min;
		*min = *array;
		*array = tmp;
	}
}

static int *find_min (int *array, int *stop)
{
	int *min = array;

	while (++array < stop) {
	    if (*array < *min)
		 	min = array;
	}

	return min;
}

