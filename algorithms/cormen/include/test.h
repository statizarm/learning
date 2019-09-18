#ifndef TEST_H_
#define TEST_H_

enum ret_value {WRONG_SORT};

typedef void (*sort_func_t)(int *arr, int length);

double sort_test(sort_func_t func, int *arr, int length);

#ifdef __DEBUG
    void print_array(int *arr, int length);
#endif

typedef struct _sort_t {
    sort_func_t func;
    char *name;
} sort_t;

#endif // TEST_H_
