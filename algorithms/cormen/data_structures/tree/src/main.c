#include <stdio.h>
#include "red_black.h"
#include "tree_print.h"

int main (int argc, char **argv) {
	int32_t data;
	char buf[128];
	struct rb_tree_node *head = NULL;

	puts("> ");
	while (scanf("%s", cmd) > 0) {
		if (!strcmp (cmd, "PRINT")) {
			tree_print (head, rb_node_toa);

			puts("OK\n");
		} else if (!strcmp(cmd, "INSERT")) {
			if (scanf ("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_tree_insert (&head, data);

			puts("OK\n");
		} else if (!strcmp(cmd, "DELETE")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_tree_delete(&head, data, rb_tree_search(head, data));

			puts("OK\n");
		} else if (!strcmp(cmd, "SEARCH")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_node_toa (rb_tree_search (head, data), buf);

			printf("%s\n", buf);

			puts("OK\n");
		} else {
			puts("UNKNOWN command!\n");
		}

		puts("> ");
	}
	return 0;

expected_int32_data:
	puts("EXPECTED an int32 data\n");
	return 1;
}
