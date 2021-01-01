#include <stdio.h>
#include <string.h>
#include "red_black.h"
#include "tree_print.h"

int main (int argc, char **argv) {
	int32_t data;
	char buf[128];
	struct rb_tree_node *head = NULL;

	printf("> ");
	while (scanf("%s", buf) > 0) {
		if (!strcmp(buf, "PRINT")) {
			tree_print(head, (void (*)(void*, char*)) rb_node_toa);

			puts("OK");
		} else if (!strcmp(buf, "INSERT")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_tree_insert(&head, data);

			puts("OK");
		} else if (!strcmp(buf, "DELETE")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_tree_delete(&head, data);

			puts("OK");
		} else if (!strcmp(buf, "SEARCH")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_node_toa(rb_tree_search(head, data), buf);

			puts("OK");
			puts(buf);
		} else if (!strcmp(buf, "EXIT")) {
			puts("EXITING...");
			break;
		} else {
			puts("UNKNOWN command!");
		}

		printf("> ");
	}

	rb_tree_free(&head);
	return 0;

expected_int32_data:
	puts("EXPECTED an int32 data\n");
	return 1;
}
