#include <stdio.h>
#include <stdlib.h>

extern void _counting_sort(int *arr, int length, int min, int max) {
    int size = max - min + 1;
    int *indexes = (int *) malloc(sizeof(int) * size);
    int *ranges = (int *) malloc(sizeof(int) * size);
	int elem, id;
    if (!indexes) {
        return;
    }
    for (int i = 0; i < size; i++) {
        indexes[i] = 0;
    }
    for (int i = 0; i < length; i++) {
        ++indexes[arr[i] - min];
    }
    for (int i = 1; i < size; i++) {
        indexes[i] += indexes[i - 1];
		ranges[i] = indexes[i];
    }
	ranges[0] = indexes[0];
    for (int i = 0; i < length; i++) {
		elem = arr[i];
		id = elem - min;
		if (ranges[id - 1] > i || i >= ranges[id]) {
			for (int j = --indexes[id]; j != i; j = --indexes[id]) {
				arr[i] = arr[j];
				arr[j] = elem;
				elem = arr[i];
				id = elem - min;
			}
		}
    }
	free (ranges);
    free(indexes);
}

void counting_sort(int *arr, int length) {
    int min = ~0 ^ ((int) 1 << ((sizeof(int) << 3) - 1));
    int max = min + 1;
    for (int i = 0; i < length; i ++) {
        if (arr[i] < min) {
            min = arr[i];
        } else if (arr[i] > max) {
            max = arr[i];
        }
    }
    _counting_sort(arr, length, min, max);
}
