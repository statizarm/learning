#include <stdio.h>
#define SWAP(a, b) {int tmp = a; a = b; b = tmp;}

/*
**
**  Не мешало бы оптимизировать, т к в ней нет смысла при текущем уровне производительности
**
*/

static int _bit_partition(int *arr, int l, int r, int shift) {
    int i = l - 1;
    int mask = 1 << shift;
    for (int j = l; j <= r; j++) {
        if (!(arr[j] & mask)) {
            i++;
            SWAP(arr[i], arr[j]);
        }
    }
    return i + 1;
}

static void _bit_radix_quick_sort(int *arr, int l, int r, int shift) {
    if (--shift >= 0 && l < r) {
        int q = _bit_partition(arr, l, r, shift);
        _bit_radix_quick_sort(arr, l, q - 1, shift);
        _bit_radix_quick_sort(arr, q, r, shift);
    }
}

void bit_radix_quick_sort(int *arr, int length) {
    int max = arr[0];
    for (int i = 1; i < length; i++) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    int shift = 1;
    while (max >> shift) {
        shift++;
    }
    _bit_radix_quick_sort(arr, 0, length - 1, shift);
}
