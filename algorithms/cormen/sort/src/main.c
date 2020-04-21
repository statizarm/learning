#include "test.h"
#include "utils.h"
#include "foo_list.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define DEFAULT_MAX_NUMBER (1 << 16)
#define DEFAULT_MIN_NUMBER -(1 << 16)
#define DEFAULT_LIB_DIR "lib/"
#define DEFAULT_LIB_NAME "libsort.so"

#ifdef __DEBUG
#define DEFAULT_ARRAY_SIZE 1 << 4
#else
#define DEFAULT_ARRAY_SIZE 1 << 24
#endif // __DEBUG

func_list_t *make_default_list(func_list_t *head, char *lib_path, char *lib_name) {
    char *default_names[] = {
        "pyramid_sort",
        "quick_sort",
        "random_quick_sort",
        "counting_sort",
        "radix_sort",
        "bit_radix_sort",
//      "insert_sort",
//		"selection_sort",
		"merge_sort"
//        "bit_radix_quick_sort"
    };

    for (int i = 0; i < sizeof(default_names) / sizeof(char *); i++) {
        head = insert(head, default_names[i], lib_path, lib_name);
    }

    return head;
}

void copy_array(int *arr1, int *arr2, int length) {
    for (int i = 0; i < length; i++) {
        arr1[i] = arr2[i];
    }
}

int main(int argc, char *argv[]) {
    settings_t settings = {
        .lib_path = DEFAULT_LIB_DIR,
        .lib_name = DEFAULT_LIB_NAME,
        .max_number = DEFAULT_MAX_NUMBER,
        .min_number = DEFAULT_MIN_NUMBER,
        .arr_size = DEFAULT_ARRAY_SIZE
    };
    func_list_t *func_list = parse_args(argc, argv, &settings);

    if (!func_list) {
        func_list = make_default_list(func_list, settings.lib_path, settings.lib_name);
    }
#ifdef __DEBUG
    for (func_list_t *tmp_list = func_list; tmp_list; tmp_list = tmp_list->next) {
        printf( "function name - %s\n"
                "lib name - %s\n", 
                tmp_list->func_name, tmp_list->lib);
    }

    settings.arr_size = DEFAULT_ARRAY_SIZE;
#endif // __DEBUG

    int *arr = (int *) malloc(sizeof(int) * settings.arr_size);
    int *sorted_arr = (int *) malloc(sizeof(int) * settings.arr_size); 
    
    arr = fill_arr(arr, &settings);

    void *handler = NULL;
    void *prev_lib = NULL;

    for (func_list_t *tmp_list = func_list; tmp_list; tmp_list = tmp_list->next) {
        printf("\x1b[32m%s\x1b[0m\n", tmp_list->func_name);
        copy_array(sorted_arr, arr, settings.arr_size);

        if (prev_lib != tmp_list->lib) {
            if (handler) {
                dlclose(handler);
            }
            handler = dlopen(tmp_list->lib, RTLD_LAZY);

            if (!handler) {
                fprintf(stderr, "\x1b[31mERROR:\x1b[0m %s\n", dlerror());
                continue;
            }
        }

        sort_func_t func = dlsym(handler, tmp_list->func_name);

        char *error = NULL;
        if ((error = dlerror())) {
            fprintf(stderr, "\x1b[31mERROR:\x1b[0m %s\n", error);
            continue;
        }

#ifdef __DEBUG
        printf( "****DEBUG MODE****\n"
                "Before sort:\n");
        print_array(arr, settings.arr_size);
#endif // __DEBUG
        double result = 0.0;
        if ((result = sort_test(func, sorted_arr, settings.arr_size))) {
            printf("время выполнения - %lf\n", result / CLOCKS_PER_SEC);
        } else {
            printf("некоректный алгоритм\n");
        }

#ifdef __DEBUG
        printf( "****DEBUG MODE****\n"
                "After sort:\n");
        print_array(sorted_arr, settings.arr_size);
#endif // __DEBUG
    }

    if (handler) {
        dlclose(handler);
    }
    free_list(func_list);
    free(arr);
    free(sorted_arr);
    return 0;
}
