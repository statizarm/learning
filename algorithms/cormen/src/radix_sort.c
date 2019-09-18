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

void radix_sort(int *arr, int length) {
    int max = arr[0];
    for (int i = 0; i < length; i++) {
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    for (int i = 1; max / i > 0;) {
        i *= 10;
        _counting_sort_on_digit(arr, length, i);
    }
}