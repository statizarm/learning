#include "utils.h"
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdio.h>

enum flags {SORTED_ARR = 0x001, RANDOM_ARR = 0x002, REVERSE_SORTED_ARR = 0x004, PARTIALLY_SORTED_ARR = 0x008};
typedef void (*arg_func)(settings_t *);

struct _arg_func {
    char *arg_name;
    arg_func func;
};

static void set_sorted(settings_t *settings) {
    set_flag(settings->flags, SORTED_ARR);
}

static void set_reverse_sorted(settings_t *settings) {
    set_flag(settings->flags, REVERSE_SORTED_ARR);
}

static void set_random(settings_t *settings) {
    set_flag(settings->flags, RANDOM_ARR);
}

static struct _arg_func args[] = {
    {
        .arg_name = "sorted",
        .func = set_sorted
    },
    {
        .arg_name = "random",
        .func = set_random
    },
    {
        .arg_name = "reverse_sorted",
        .func = set_reverse_sorted  
    }
};

static arg_func arg_search(char *name) {
    for (int i = 0; i < sizeof(args) / sizeof(struct _arg_func); i++) {
        if (!strcmp(args[i].arg_name, name)) {
            return args[i].func;
        }
    }
    return NULL;
}

func_list_t *parse_args(int argc, char *argv[], settings_t *settings) {
    func_list_t *func_list = NULL;
    while (--argc > 0) {
        if (*(++argv)[0] == '-') {
            switch (*++argv[0])
            {
                case 'l':
                    if (*++argv[0]) {
                        settings->lib_name = argv[0];
                    } else {
                        settings->lib_name = (++argv)[0];
                        argc--;
                    }
                    break;
                case 'p':
                    if (*++argv[0]) {
                        settings->lib_path = argv[0];
                    } else {
                        settings->lib_path = (++argv)[0];
                        argc--;
                    }
                    break;
                case 'M':
                    if (*++argv[0]) {
                        settings->max_number = atoi(argv[0]);
                    } else {
                        settings->max_number = atoi((++argv)[0]);
                        argc--;
                    }
                    break;
                case 'm':
                    if (*++argv[0]) {
                        settings->min_number = atoi(argv[0]);
                    } else {
                        settings->min_number = atoi((++argv)[0]);
                        argc--;
                    }
                    break;
                case 'n':
                    if (*++argv[0]) {
                        settings->arr_size = atoi(argv[0]);
                    } else {
                        settings->arr_size = atoi((++argv)[0]);
                        argc--;
                    }
                    break;
                case '-': {
                    arg_func func = arg_search(++argv[0]);
                    if (func) {
                        func(settings);
                    } else {
                        printf("unknown arg - %s\n", argv[0]);
                    }
                }
                default:
                    break;
            }
        } else {
            func_list = insert(func_list, argv[0], settings->lib_path, settings->lib_name);
        }
    }
    return func_list;
}

static int *reverse_arr(int *arr, int length) {
    for (int i = 0, j = length - 1; i < j; i++, j--) {
        int tmp = arr[i];
        arr[i] = arr[j];
        arr[j] = tmp;
    }
    return arr;
}

int *fill_arr(int *arr, const settings_t *settings) {
    void bit_radix_sort(int *, int);

    srand(time(NULL));
    for (int i = 0; i < settings->arr_size; i++) {
        arr[i] = rand() % (settings->max_number - settings->min_number) + settings->min_number;
    }
    if (check_flag(settings->flags, SORTED_ARR | REVERSE_SORTED_ARR)) {
        bit_radix_sort(arr, settings->arr_size);

#ifdef __DEBUG
        printf( "****DEBUG MODE****\n"
                "sorted\n");
#endif // __DEBUG
    }
    if (check_flag(settings->flags, REVERSE_SORTED_ARR)) {
        arr = reverse_arr(arr, settings->arr_size);

#ifdef __DEBUG
        printf( "****DEBUG MODE****\n"
                "reversed\n");
#endif // __DEBUG
    }
    return arr;
}
