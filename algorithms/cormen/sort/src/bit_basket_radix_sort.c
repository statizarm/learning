#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void bit_basket_radix_sort (int *arr, int length)
{
	register int elem;
	int *sorted_arr = malloc (length * sizeof (int));
	int elems_in_basket [8][16] = {0};
	int indexes_in_baskets [16] = {0};
	unsigned int mask = 0;
	int *tmp_ptr;
	unsigned int offset;
	unsigned int shift;

	for (int i = 0; i < length; ++i) {
		elem = arr[i];
		++elems_in_basket[0][elem & 0x0000000f];
		++elems_in_basket[1][(elem & 0x000000f0) >> 4];
		++elems_in_basket[2][(elem & 0x00000f00) >> 8];
		++elems_in_basket[3][(elem & 0x0000f000) >> 12];
		++elems_in_basket[4][(elem & 0x000f0000) >> 16];
		++elems_in_basket[5][(elem & 0x00f00000) >> 20];
		++elems_in_basket[6][(elem & 0x0f000000) >> 24];
		++elems_in_basket[7][(elem & 0xf0000000) >> 28];
	}

	for (int i = 0; i < 8; ++i) {
		shift = i << 2;
		mask = 0x0f << shift;
		indexes_in_baskets[0] = 0;

		for (int j = 0; j < 15; ++j) {
			indexes_in_baskets[j + 1] = indexes_in_baskets[j] + elems_in_basket[i][j];
		}

		for (int j = 0; j < length; ++j) {
			offset = (arr[j] & mask) >> shift;
			sorted_arr[indexes_in_baskets[offset]++] = arr[j];
		}

		tmp_ptr = arr;
		arr = sorted_arr;
		sorted_arr = tmp_ptr;
	}

	free (sorted_arr);
}
