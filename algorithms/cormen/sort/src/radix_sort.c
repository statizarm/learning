#include <stdlib.h>
#define DIGIT(a, b) ((a % b) / (b / 10))

static void _counting_sort_on_digit(int *arr, int length, int rad) {
    int indexes[19];
    int *sorted_arr = (int *) malloc(sizeof(int) * length);
    if (!sorted_arr) {
        return;
    }
    for (int i = 0; i < 19; i++) {
        indexes[i] = 0;
    }
    for (int i = 0; i < length; i++) {
        ++indexes[DIGIT(arr[i], rad) + 9];
    }
    for (int i = 1; i < 19; i++) {
        indexes[i] += indexes[i - 1];
    }
    for (int i = length - 1; i >= 0; i--) {
        int digit = DIGIT(arr[i], rad) + 9;
        sorted_arr[indexes[digit] - 1] = arr[i];
        --indexes[digit];
    }
    for (int i = 0; i < length; i++) {
        arr[i] = sorted_arr[i];
    }
    free(sorted_arr);
}

void swap (int *arr, int l, int r)
{
	int tmp = arr[l];
	arr[l] = arr[r];
	arr[r] = tmp;
}

int __bit_partition (int *arr, int length, int n_bit) {
	unsigned int mask = 0x01 << n_bit;
	int j = 0;

	for (int i = 0; i < j; ++i) {
		if (!(arr[i] & mask)) {
			swap (arr, i, j++);
		}
	}
	return j;
}

void _sort_by_bit (int *arr, int length, int n_bit) {
	int q = __bit_partition (arr, length, n_bit);

	for (int i = q, j = length - 1; i < j; ++i, --j) {
		swap (arr, i, j);
	}
}

void radix_sort(int *arr, int length) {
    int max = arr[0];
    for (int i = 1; i < length; i++) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    for (int i = 1; max / i > 0;) {
        i *= 10;
        _counting_sort_on_digit(arr, length, i);
    }

	/*for (int i = 0; i < 32; ++i) {
		_sort_by_bit (arr, length, i);
	}*/
}

