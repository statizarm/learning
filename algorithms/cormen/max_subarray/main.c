#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define MAIN_ARRAY_SIZE 100
#define MAX_RAND 1000
#define MIN_RAND -10

struct _sub_array {
	int32_t *begin;
	int32_t *end;
	int64_t sum;
};

void __find_max_cross_sub_array (int32_t *array, uint32_t p, uint32_t q,
                                        uint32_t r, struct _sub_array *sub) {
	
	int32_t left_sum  = 0;
	int32_t right_sum = 0;

	int32_t tmp_sum = 0;

	for (int32_t i = q - 1; i >= (int32_t) p; --i) {
		tmp_sum += array [i];

		if (tmp_sum > left_sum)
		{
		  left_sum = tmp_sum;
		  sub->begin = &array[i];
		}
	}

	tmp_sum = 0;

	for (uint32_t i = q; i < r; ++i) {
		tmp_sum +=array[i];

		if (tmp_sum > right_sum)
		{
			right_sum = tmp_sum;
			sub->end = &array[i + 1];
		}
	}

	sub->sum = left_sum + right_sum;
}

void _find_max_sub_array (int32_t *array, uint32_t p, uint32_t r,
                            struct _sub_array *sub)
{
	if (p == r - 1) {
		sub->begin = &array[p];
		sub->end   = &array[r];
		sub->sum   = array[p];
	} else {
		uint32_t mid = (p + r) >> 1;

		struct _sub_array _left_sub;
		struct _sub_array _right_sub;

		_find_max_sub_array (array, p, mid, &_left_sub);
		_find_max_sub_array (array, mid, r, &_right_sub);
		__find_max_cross_sub_array (array, p, mid, r, sub);

		if (_left_sub.sum >= _right_sub.sum &&
		    _left_sub.sum >= sub->sum) {

			sub->begin = _left_sub.begin;
			sub->end   = _left_sub.end;
			sub->sum   = _left_sub.sum;
		} else if (_right_sub.sum >= _left_sub.sum &&
		           _right_sub.sum >= sub->sum) {

			sub->begin = _right_sub.begin;
			sub->end   = _right_sub.end;
			sub->sum   = _right_sub.sum;
		}
	}
}

void find_max_sub_array (int32_t *array, uint32_t len, struct _sub_array *sub) {
	int32_t *diff_array = (int32_t *) malloc ((len - 1) << 2);

	for (uint32_t i = 0; i < len - 1; ++i)
		diff_array[i] = array[i + 1] - array[i];

	sub->begin = sub->end = diff_array;

	_find_max_sub_array (diff_array, 0, len, sub);

	if (sub->begin != sub->end) {
		sub->begin = array + (sub->begin - diff_array);
		sub->end   = array + (sub->end - diff_array + 1);
	}

	free (diff_array);
}

void print_sub_array (struct _sub_array *sub) {
	printf ("Maximum profit equals: %ld\n", sub->sum);
	puts ("Elements of maximum subarray:\n{");
	for (int32_t *i = sub->begin; i < sub->end; ++i) {
		printf ("%d%s", *i, (i + 1 == sub->end) ? "" : ", ");
	}
	puts ("}\n");
}

int main () {
	int *array = (int *) malloc (MAIN_ARRAY_SIZE << 2);

	srand (time (NULL));

	for (int i = 0; i < MAIN_ARRAY_SIZE; ++i)
		array[i] = rand () % (MAX_RAND - 1) + 1;

	struct _sub_array max_sub_array;

	find_max_sub_array (array, MAIN_ARRAY_SIZE, &max_sub_array);

	puts ("searching complete\n");

	print_sub_array (&max_sub_array);

	free (array);

	return 0;
}
