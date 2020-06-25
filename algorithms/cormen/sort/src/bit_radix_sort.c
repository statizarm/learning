#include <stdlib.h>
#define SWAP(a, b) {int tmp = a; a = b; b = tmp;}

static void _bit_counting_sort(int *arr, int length, int r, int shift) {
    int size = 1 << r;
    int mask = size - 1;
    int indexes[size];
    int *sorted_arr = (int *) malloc(sizeof(int) * length);
    if (!sorted_arr) {
        return;
    }

    for (int i = 0; i < size; i++) {
        indexes[i] = 0;
    }
    for (int i = 0; i < length; i++) {
        ++indexes[(arr[i] >> shift) & mask];
    }
    for (int i = 1; i < size; i++) {
        indexes[i] += indexes[i - 1];
    }
    for (int i = length - 1; i >= 0; i--) {
        int tmp = (arr[i] >> shift) & mask;
        sorted_arr[indexes[tmp] - 1] = arr[i];
        --indexes[tmp];
    }
    for (int i = 0; i < length; i++) {
        arr[i] = sorted_arr[i];
    }
    free(sorted_arr);
}

static int _high_bit_partition(int *arr, int l, int r) {
    int i = l;
    int mask = 1 << ((sizeof(int) << 3) - 1);
    for (int j = l; j <= r; j++) {
        if (arr[j] & mask) {
            SWAP(arr[j], arr[i]);
            i++;
        }
    }
    return i;
}

void bit_radix_sort(int *arr, int length) {
    int r = 4;
    int size = sizeof(int) << 3;
    int i = 0;
    int j = _high_bit_partition(arr, 0, length - 1);

    if (j > 1) {
        for (int i = 0; i < size; i += r) {
            _bit_counting_sort(arr, j, r, i);
        }
    }

    if ((length -= j) > 1) {
        for (int i = 0; i < size; i += r) {
            _bit_counting_sort(&arr[j], length, r, i);
        }
    }
}
