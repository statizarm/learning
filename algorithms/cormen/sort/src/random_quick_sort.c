#include <stdlib.h>
#define SWAP(a, b) {                                                        \
    int tmp = a;                                                            \
    a = b;                                                                  \
    b = tmp;                                                                \
}

static int partition(int *arr, int l, int r) {
    int i = l - 1;
    int x = arr[r];
    for (int j = l; j < r; j++) {
        if (arr[j] < x) {
            i++;
            SWAP(arr[j], arr[i]);
        } 
    }
    SWAP(arr[i + 1], arr[r]);
    return i + 1;
}

static int random_partition(int *arr, int l, int r) {
    int i = rand() % (l - r) + l;
    SWAP(arr[i], arr[r]);
    return partition(arr, l, r);
}

static void _quick_sort(int *arr, int l, int r) {
    if(l < r) {
        int q = partition(arr, l, r);
        _quick_sort(arr, l, q - 1);
        _quick_sort(arr, q + 1, r);
    }
}

void random_quick_sort(int *arr, int length) {
    _quick_sort(arr, 0, length - 1);
}
