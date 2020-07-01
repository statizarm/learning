#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

void print_n (int *array, int n)
{
	for (int i = 0; i < n - 1; ++i) {
		printf ("%d, ", array[i]);
	}

	printf ("%d\n", array[n - 1]);
}


int is_sorted (int *array, int p, int r)
{
	while (++p < r) {
		if (array[p] < array[p - 1]) {
			return 0;
		}
	}

	return 1;
}

void merge (int *array, int p, int q, int r)
{
	int n_left  = q - p;
	int n_right = r - q;

	int *left  = (int *) malloc (n_left * sizeof (int));
	int *right = (int *) malloc (n_right * sizeof (int));

	int *sources[2] = {left, right};
	int *sources_ends[2] = {left + n_left, right + n_right};
	int source_id = 0;

	memcpy (left, array + p, n_left * sizeof (int));
	memcpy (right, array + q, n_right *sizeof (int));

	array = array + p;
	while (sources[source_id] < sources_ends[source_id]) {
		if (*sources[source_id] > *sources[(source_id + 1) & 0x1]) {
			source_id = (source_id + 1) & 0x1;
		}
		*array++ = *sources[source_id]++;
	}
	
	source_id = (source_id + 1) & 0x1;

	while (sources[source_id] < sources_ends[source_id]) {
		*array++ = *sources[source_id]++;
	}

	free (left);
	free (right);
}
	
void merge_sort (int *array, int p, int r)
{
	if (!is_sorted (array, p, r)) {
		int mid = (p + r) / 2;
		
		merge_sort (array, p, mid);
		merge_sort (array, mid, r);

		merge (array, p, mid, r);
	}
}

void print_first_n_by_sort (const int *array, int length, int n)
{
	int *tmp_array = (int *) malloc (length * sizeof (int));

	memcpy (tmp_array, array, length * sizeof (int));

	merge_sort (tmp_array, 0, length);
	
	print_n (tmp_array, n);

	free (tmp_array);
}

void swap (int *array, int l, int r) 
{
	int tmp = array[l];
	array[l] = array[r];
	array[r] = tmp;
}

int randomized_partition (int *array, int p, int r)
{
	int elem_id = rand () % (r - p) + p;
	int elem = array[elem_id];
	int j = p;
	
	array[elem_id] = array[p];
	array[p] = elem;

	for (int i = p + 1; i < r; ++i) {
		if (array[i] < elem) {
			swap (array, i, ++j);
		}
	}

	array[p] = array[j];
	array[j] = elem;

	return j;
}

int randomized_select (int *array, int p, int r, int n)
{
	int statistic;

	srand (time (NULL));

	while ((statistic = randomized_partition (array, p, r)) != n) {
		if (statistic > n) {
			r = statistic;
		} else {
			p = statistic + 1;
		}
	}

	return array[n];
}

void insertion_sort (int *array, int p, int r)
{
	int j, elem;
	for (int i = p + 1; i < r; ++i) {
		elem = array[i];
		for (j = i - 1; j >= p && array[j] > elem; --j) {
			array[j + 1] = array[j];
		}

		array[j + 1]  = elem;
	}
}
		

void sort_first_n (int *array, int p, int r, int n)
{
	randomized_select (array, p, r, n);
	print_n (array + p, r - p);

	insertion_sort (array, p, n);
}

void print_first_n_by_sort_first_n (int *array, int length, int n)
{
	int *tmp_array = (int *) malloc (length * sizeof (int));

	memcpy (tmp_array, array, length * sizeof (int));

	sort_first_n (tmp_array, 0, length, n);

	print_n (tmp_array, n);

	free (tmp_array);
}

void min_heapify (int *array, int length, int i)
{
	int left = i * 2 + 1;
	int right = (i + 1) * 2;
	int min_child_id;

	if (left >= length) {
		return;
	}

	if (right >= length) {
		min_child_id = left;
	} else {
		min_child_id = (array[left] < array[right]) ? left : right;
	}

	if (array[min_child_id] < array[i]) {
		swap (array, min_child_id, i);
		min_heapify (array, length, min_child_id);
	}
}

void build_min_heap (int *array, int length)
{
	for (int i = length / 2 - 1; i >= 0; --i) {
		min_heapify (array, length, i);
	}
}

int extract_min (int *array, int *length)
{
	int min = array[0];
	array[0] = array[--*length];
	min_heapify (array, *length, 0);
	return min;
}

void print_first_n_by_extract (const int *array, int length, int n)
{
	int *tmp_array = (int *) malloc (length * sizeof (int));

	memcpy (tmp_array, array, length * sizeof (int));

	build_min_heap (tmp_array, length);

	for (int i = 0; i < n - 1; ++i) {
		printf ("%d, ", extract_min (tmp_array, &length));
	}

	printf ("%d\n", extract_min (tmp_array, &length));

	free (tmp_array);
}

int main (void)
{

	int i = 3;
	int array[] = {123, 132, 2, 45, 313, -12, 35, 90, -921};
	int length = sizeof (array) / sizeof (int);

	print_n (array, length);
	print_first_n_by_sort (array, length, i);
	printf ("Gratz\n");

	print_first_n_by_sort_first_n (array, length, i);
	printf ("Gratz\n");

	print_first_n_by_extract (array, length, i);
	printf ("Gratz\n");

	return 0;
}
