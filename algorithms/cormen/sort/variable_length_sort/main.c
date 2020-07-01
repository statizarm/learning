#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void strrev (char *str)
{
	int length = strlen (str);
	char tmp;

	for (int i = 0; i < --length; i++) {
		tmp = str[i];
		str[i] = str[length];
		str[length] = tmp;
	}
}

int int_counting_sort (int *array, int p, int r, int divisor) {
	int indexes [10] = {0};
	int *sorted_array = (int *) malloc ((r - p) * sizeof (int));
	int q = p;
	int divided;

	for (int i = p; i < r; ++i) {
		divided = array[i] / divisor;
		if (divided == 0) {
			++q;
		}
		++indexes[divided % 10];
	}

	for (int i = 1; i < 10; ++i) {
		indexes[i] += indexes [i - 1];
	}

	for (int i = r - 1; i >= p; --i) {
		divided = array[i] / divisor;
		sorted_array[--indexes[divided % 10]] = array[i];
	}

	for (int i = p; i < r; ++i) {
		array[i] = sorted_array[i - p];
	}

	return q;
}

	
	
void int_sort (int *array, int length)
{
	int q = 0;
	int divisor = 1;
	while ((q = int_counting_sort (array, q, length, divisor)) < length) {
		divisor *= 10;
	}
}

int _string_counting_basket_sort (char **array, int p, int r, int char_index)
{
	if (p < r - 1) {
		char **sorted_array = (char **) malloc ((r - p) * sizeof (char *));
		int indexes[128] = {0};
		int basket_indexes[128];

		for (int i = p; i < r; ++i) {
			++indexes[array[i][char_index]];
		}

		for (int i = 1; i < 128; ++i) {
			indexes[i] += indexes[i - 1];
		}

		memcpy (basket_indexes, indexes, 128 * sizeof (int));

		for (int i = r - 1; i >= p; --i) {
			sorted_array[--indexes[array[i][char_index]]] = array[i];
		}

		for (int i = p; i < r; ++i) {
			array[i] = sorted_array[i - p];
		}

		free (sorted_array);

		++char_index;
		for (int i = 0; i < 127; ++i) {
			_string_counting_basket_sort (array, basket_indexes[i],
			                              basket_indexes[i + 1], char_index);
		}
	}
}



void string_sort (char **array, int length)
{
	_string_counting_basket_sort (array, 0, length, 0);
}

int is_sorted (void **array, int length, int (*cmp)(void *, void *))
{
	int res;
	for (int i = 1; i < length; ++i) {
		res = cmp (array[i], array[i - 1]);
		if (res < 0) {
			return 0;
		}
	}

	return 1;
}

int int_cmp (int l, int r) {
	return l - r;
}

int is_int_array_sorted (int *array, int length)
{
	for (int i = 1; i < length; ++i) {
		if (array[i] < array[i - 1]) {
			return 0;
		}
	}
	
	return 1;
}
char s1[] = "Hello";
char s2[] = "Hastalavista";
char s3[] = "hmm";
char s4[] = "Whatdoyouthinkaboutthis?";
char s5[] = "Idon'treallyknow,whatiwriting";

int main (void)
{
	int int_array[] = {123, 321, 222, 34, 312, 353, 1, 33, 2, 10, 935};
	char *string_array[]= {
		s1,
		s2,
		s3,
		s4,
		s5
	};

	int_sort (int_array, sizeof (int_array) / sizeof (int));

	printf ("kukusiki\n");
	if (!is_int_array_sorted (int_array, sizeof (int_array) / sizeof (int))) {
		printf ("int sort FAILED :(\n");
		for (int i = 0; i < sizeof (int_array) / sizeof (int); ++i) {
			printf ("%d ", int_array[i]);
		}
		printf ("\n");
	} else {
		printf ("int sort SUCCESS :)\n");
	}

	string_sort (string_array, sizeof (string_array) /  sizeof (char *));
	printf ("kukusiki\n");
	if (!is_sorted ((void **) string_array,
	    sizeof (string_array) / sizeof (char *),
		(int (*) (void *, void *)) strcmp)) {
		printf ("string sort FAILED :(\n");
		for (int i = 0; i < 5; ++i) {
			printf ("%s\n", string_array[i]);
		}
	} else {
		printf ("string sort SUCCESS :)\n");
	}
	return 0;
}
