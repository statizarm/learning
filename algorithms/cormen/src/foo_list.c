#include "foo_list.h"
#include <stdlib.h>
#include <string.h>

func_list_t *insert(func_list_t *head, char *func_name, char *lib_path, char *lib_name) {
    if (!func_name || !lib_name || !lib_path) {
        return NULL;
    }
    func_list_t *new = (func_list_t *) malloc(sizeof(func_list_t));
    new->lib = (char *) malloc(strlen(lib_path) + strlen(lib_name) + 1);
    new->func_name = (char *) malloc(strlen(func_name) + 1);

    strcat(strcpy(new->lib, lib_path), lib_name);
    strcpy(new->func_name, func_name);

    new->next = head;
    return new;
}

void free_list(func_list_t *head) {
    if (head) {
        free_list(head->next);
        free(head->lib);
        free(head->func_name);
        free(head);
    }
}
