#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

#include "red_black.h"
#include "tree_print.h"

void insert_random_n(struct rb_tree_node **head, int32_t n);

int main (int argc, char **argv) {
	int32_t data;
	char buf[128];
	struct rb_tree_node *head = NULL;

	printf("> ");
	while (scanf("%s", buf) > 0) {
		if (!strcmp(buf, "PRINT")) {
			puts("PRINTING...");

			tree_print(head, (void (*)(void*, char*)) rb_node_toa);

			puts("OK");
		} else if (!strcmp(buf, "INSERT")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_tree_insert(&head, data);

			printf("INSERT %d OK\n", data);
		} else if (!strcmp(buf, "DELETE")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_tree_delete(&head, data);

			printf("DELETE %d OK\n", data);
		} else if (!strcmp(buf, "SEARCH")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_node_toa(rb_tree_search(head, data), buf);

			printf("SEARCH %d OK\n", data);
			puts(buf);
		} else if (!strcmp(buf, "EXIT")) {
			puts("EXITING...");
			break;
		} else if (!strcmp(buf, "LEFT_ROTATE")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_left_rotate(rb_tree_search(head, data));
			puts("OK");
		} else if (!strcmp(buf, "RIGHT_ROTATE")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			rb_right_rotate(rb_tree_search(head, data));
			puts("OK");
		} else if (!strcmp(buf, "RANDOM")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			insert_random_n(&head, data);
			puts("OK");
		} else {
			puts("UNKNOWN command!");
		}

		printf("> ");
	}

	rb_tree_free(&head);
	return 0;

expected_int32_data:
	puts("EXPECTED an int32 data\n");
	rb_tree_free(&head);
	return 1;
}

void insert_random_n(struct rb_tree_node **head, int32_t n)
{
	srand(time(NULL));
	for (int32_t i = 0; i < n; ++i) {
		rb_tree_insert(head, rand() % (n << 1));
	}
}
