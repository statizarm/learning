#include <stdlib.h>
static void _merge_sort (int *array, int p, int r);
static void __merge (int *array, int p, int q, int r);

void merge_sort (int *array, int length)
{
	_merge_sort (array, 0, length);
}

static void _merge_sort (int *array, int p, int r) {
	if (p < r - 1) {
		int q = (p + r) / 2;
		_merge_sort (array, p, q);
		_merge_sort (array, q, r);

		__merge (array, p, q, r);
	}
}

struct _iterator {
	int *cur;
	int *end;
};

static void __merge (int *array, int p, int q, int r)
{
	int nl = q - p;
	int nr = r - q;

	int *left = (int *) malloc (nl << 2);
	int *right = (int *) malloc (nr << 2);
	for (int i = 0; i < nl; ++i)
		left[i] = array[p + i];
  
	for (int i = 0; i < nr; ++i)
		right[i] = array[q + i];
	
	struct _iterator _l_it = {
		.cur = left,
		.end = left + nl
	};

	struct _iterator _r_it = {
		.cur = right,
		.end = right + nr
	};

	struct _iterator *f = &_l_it;
	struct _iterator *s = &_r_it;

	int *arr_it = array + p;
	
	while (f->cur < f->end) {
		if (*f->cur > *s->cur) {
			struct _iterator *tmp = f;
			f = s;
			s = tmp;
		}

		*arr_it++ = *f->cur++;
	}

	while (s->cur < s->end)
		*arr_it++ = *s->cur++;
	
	free (left);
	free (right);
}
