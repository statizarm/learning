#ifndef FOO_LIST_H_
#define FOO_LIST_H_

typedef struct _func_list {
    char *func_name;
    char *lib;
    struct _func_list *next;
} func_list_t;

func_list_t *insert(func_list_t *head, char *func_name, char *lib_path, char *lib_name);
void free_list(func_list_t *head);

#endif // FOO_LIST_H_