#include <time.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {
	int start;
	int end;
} range_t; // range type: [start, end)

int is_equal (range_t smt, range_t to)
{
	return smt.start < to.end && smt.end > to.start;
}

int is_subrange (range_t smt, range_t to)
{
	return smt.start >= to.start && smt.end <= to.end;
}

int comp_range (range_t l, range_t r)
{
	if (is_equal (l, r))
		return 0;
	return r.end - l.start;
}

void swap (range_t *arr, int l, int r)
{
	range_t tmp = arr[l];
	arr[l] = arr[r];
	arr[r] = tmp;
}

void print_range (range_t r)
{
	printf ("[%d, %d)", r.start, r.end);
}

void print_range_array (range_t *arr, int length)
{
	int i;
	putchar ('[');
	for (i = 0; i < length - 1; ++i) {
		print_range (arr[i]);
		printf (", ");
	}
	print_range (arr[i]);
	puts ("]\n");
}

int min_range (range_t *arr, int p, int r)
{
	int min = arr[p].end - arr[p].start;
	int min_i = p;
	int range;

	for (int i = min_i + 1; i < r; ++i) {
		if ((range = arr[i].end - arr[i].start) < min) {
			min = range;
			min_i = i;
		}
	}

	return min_i;
}

int hoar_partition (range_t *arr, int p, int r)
{
	int q = min_range (arr, p, r);
	int i = p;
	int j = r - 2;

	swap (arr, q, --r);

	while (1) {
		while (arr[i].start < arr[r].start)
			++i;
		while (arr[j].start >= arr[r].start)
			--j;
		if (i < j) {
			swap (arr, i, j);
		} else {
			swap (arr, i, r);
			return i;
		}
	}
}
range_t fuzzy_partition (range_t *arr, int p, int r)
{
	int q = hoar_partition (arr, p, r); // just for experiment
	range_t res = {
		.start = q,
		.end = q + 1
	};

	
	print_range_array (&arr[p], r - p);
	for (int i = res.end; i < r; ++i) {
		if (is_equal (arr[i], arr[q])) {
			swap (arr, i, res.end++);
		}
	}
	for (int i = res.start - 1; i >= p; --i) {
		if (is_equal (arr[i], arr[q])) {
			swap (arr, i, --res.start);
		}
	}

	print_range_array (&arr[p], r - p);
	return res;
}

void fuzzy_quick_sort (range_t *arr, int p, int r) {
	while (p < r - 1) {
		range_t q = fuzzy_partition (arr, p, r);
		fuzzy_quick_sort (arr, p, q.start);
		p = q.end;
	}
}

void fuzzy_sort (range_t *arr, int length) {
	fuzzy_quick_sort (arr, 0, length);
}

int check_sort (range_t *arr, int length)
{
	for (int i = 1; i < length; ++i)
		if (comp_range (arr[i - 1], arr[i]) < 0)
			return -1;
	
	return 0;
}

void gen_range_arr (range_t *arr, int length)
{
	int tmp;
	for (int i = 0; i < length; ++i) {
		arr[i].start = rand () % 128;
		arr[i].end   = rand () % 128;
		if (arr[i].start > arr[i].end) {
			tmp = arr[i].start;
			arr[i].start = arr[i].end;
			arr[i].end = tmp;
		} else if (arr[i].start == arr[i].end) {
			--i;
		}
	}
}


int main (void)
{
	srand (time (NULL));
	range_t range_arr[16];
	gen_range_arr (range_arr, 16);
	print_range_array (range_arr, 16);
	fuzzy_sort (range_arr, 16);
	print_range_array (range_arr, 16);
	if (check_sort (range_arr, 16))
		printf ("Fail :(\n");
	else
		printf ("Excelent :)\n");
	return 0;
}
