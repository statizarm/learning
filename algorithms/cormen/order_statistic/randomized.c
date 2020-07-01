#include <stdlib.h>
#include <stdio.h>
#include <time.h>

void swap (int *array, int p, int r)
{
	int tmp = array[p];
	array[p] = array[r];
	array[r] = tmp;
}

int randomized_partition (int *array, int p, int r)
{
	int index = rand () % (r - p) + p;
	int elem = array[index];
	
	array[index] = array[p];
	array[p] = elem;

	int j = p;
	for (int i = p + 1; i < r; ++i) {
		if (array[i] < elem) {
			swap (array, i, ++j);
		}
	}

	swap (array, j, p);
	return j;
}

int randomized_select (int *array, int length, int i)
{
	srand (time (NULL));

	int p = 0;
	int r = length;
	int statistic;

	while ((statistic = randomized_partition (array, p, r)) != i) {
		if (statistic < i) {
			p = statistic + 1;
		} else {
			r = statistic;
		}
	}

	return array[statistic];
}



int main (void)
{
	int array[] = {11, 2, 93, 74, 59, -6, -70};

	for (int i = 6; i >= 0; --i) {
		printf ("%d\n", randomized_select (array, 7, i));
	}

	return 0;
}
