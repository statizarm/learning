#include "test.h"
#include <time.h>

#ifdef __DEBUG
#include <stdio.h>

void print_array(int *arr, int length) {
    for (int i = 0; i < length; i++) {
        printf((i == length - 1) ? "%d\n\n" : "%d ", arr[i]);
    }
}

static void print_series(int *arr, int length) {
    int serie = 0;
    for (int i = 1; i < length; i++) {
        printf((!serie) ? "\x1b[32m%d\x1b[0m " : "\x1b[31m%d\x1b[0m ", arr[i - 1]);
        if (arr[i - 1] > arr[i]) {
            serie = !serie;
        }
    }
    printf((!serie) ? "\x1b[32m%d\x1b[0m\n\n" : "\x1b[31m%d\x1b[0m\n\n", arr[length - 1]);
}

#endif // __DEBUG

static int get_control_sum(const int *arr, const int length) {
    int sum = 0;

    for (int i = 0; i < length; i++) {
        sum += arr[i];
    }
    return sum;
}

static int get_series_count(const int *arr, const int length) {
    int series_count = 1;

    for (int i = 1; i < length; i++) {
        if (arr[i-1] > arr[i]) {
            series_count++;
        }
    }
    return series_count;
}

double sort_test(sort_func_t func, int *arr, const int length) {
    int sum_before_sort = get_control_sum(arr, length);
    
    clock_t start = clock();
    func(arr, length);
    clock_t end = clock();

#ifdef __DEBUG
    int sum_after_sort;
#endif // __DEBUG

    if (sum_before_sort != (
#ifdef __DEBUG    
    sum_after_sort =
#endif // __DEBUG
    get_control_sum(arr, length)) || get_series_count(arr, length) != 1) {

#ifdef __DEBUG
       if (sum_after_sort != sum_before_sort) { 
        printf( "****DEBUG MODE****\n"
                "sum before sort: %d\n"
                "sum after sort: %d\n"
                , sum_before_sort, sum_after_sort);
        }
        printf( "****DEBUG MODE****\n"
                "Series in array:\n");
        print_series(arr, length);   
#endif // __DEBUG
        return WRONG_SORT;
    } else {
        return (double) end - (double) start;
    }
}
