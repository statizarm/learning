#include <stdlib.h>

extern void _counting_sort(int *arr, int length, int min, int max) {
    int size = max - min + 1;
    int *indexes = (int *) malloc(sizeof(int) * size);
    if (!indexes) {
        return;
    }
    int *sorted_arr = (int *) malloc(sizeof(int) * length);
    if (!sorted_arr) {
        free(indexes);
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
    }
    for (int i = 0; i < length; i++) {
        sorted_arr[indexes[arr[i] - min] - 1] = arr[i];
        indexes[arr[i] - min] -= 1;
    }
    for (int i = 0; i < length; i++) {
        arr[i] = sorted_arr[i];
    }
    free(sorted_arr);
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