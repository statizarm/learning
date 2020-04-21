#include "sort.h"
#define NBASKETS 1 << 4
#define MIN_ELEMENTS 1 << 6

/*
**
** Не робит :(
**
*/


static void _basket_sort(int *arr, int length, int min, int max) {
    int baskets[NBASKETS];
    for (int i = 0; i < NBASKETS; i++) {
        baskets[i] = i;
    }

}

void basket_sort(int *arr, int length) {
    if (length > MIN_ELEMENTS) {
        int max;
        int min;
        if (length % 2) {
            max = min = arr[length >> 1];
        } else {
            max = min = arr[0];
        }
        for (int i = 0, j = length - 1; i < j; i++, j--) {
            if (arr[i] > arr[j]) {
                if (arr[i] > max) {
                    max = arr[i];
                }
                if (arr[j] < min) {
                    min = arr[j];
                }
            } else {
                if (arr[j] > max) {
                    max = arr[j];
                }
                if (arr[i] < min) {
                    min = arr[i];
                }
            }
        }
        _basket_sort(arr, length, min, max);
    } else {
        insert_sort(arr, length);
    }
}