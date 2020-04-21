#ifndef UTILS_H_
#define UTILS_H_

#include "foo_list.h"
#include <stddef.h>

#define set_flag(a, flag) ((a) |= (flag))
#define unset_flag(a, flag) ((a) &= ~(flag))
#define check_flag(a, flag) ((a) & (flag))

typedef struct _settings
{
    char *lib_path;
    char *lib_name;
    int max_number;
    int min_number;
    size_t arr_size;
    int flags;
} settings_t;

func_list_t *parse_args(int argc, char *argv[], settings_t *settings);
int *fill_arr(int *arr, const settings_t *settings);

#endif // UTILS_H_
