#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

#ifdef RED_BLACK
#include "red_black.h"

typedef struct rb_tree_node tree_node;

#define insert(x, y) rb_tree_insert(x, y)
#define delete(x, y) rb_tree_delete(x, y)

#define search(x, y) rb_tree_search(x, y)

#define tree_free(x) rb_tree_free(x)

#define left_rotate(x) rb_left_rotate(x)
#define right_rotate(x) rb_right_rotate(x)

#define node_toa rb_node_toa
#define node_tota(x,y) rb_node_toa(x, y)

#endif // RED_BLACK

#ifdef AVL
#include "avl.h"

typedef struct avl_tree_node tree_node;

#define insert(x, y) avl_tree_insert(x, y)
#define delete(x, y) avl_tree_delete(x, y)

#define search(x, y) avl_tree_search(x, y)

#define tree_free(x) avl_tree_free(x)

#define left_rotate(x) avl_left_rotate(x)
#define right_rotate(x) avl_right_rotate(x)

#define node_toa avl_node_toa
#define node_tota(x,y) avl_node_toa(x, y)

#endif // AVL

#include "tree_print.h"

void insert_random_n(tree_node **head, int32_t n);

int main (int argc, char **argv) {
	int32_t data;
	char buf[128];
	tree_node *head = NULL;

	printf("> ");
	while (scanf("%s", buf) > 0) {
		if (!strcmp(buf, "PRINT")) {
			puts("PRINTING...");

			tree_print(head, (void (*)(void*, char*)) node_toa);

			puts("OK");
		} else if (!strcmp(buf, "INSERT")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			insert(&head, data);

			printf("INSERT %d OK\n", data);
		} else if (!strcmp(buf, "DELETE")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			delete(&head, data);

			printf("DELETE %d OK\n", data);
		} else if (!strcmp(buf, "SEARCH")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			node_toa(search(head, data), buf);

			printf("SEARCH %d OK\n", data);
			puts(buf);
		} else if (!strcmp(buf, "EXIT")) {
			puts("EXITING...");
			break;
		} else if (!strcmp(buf, "LEFT_ROTATE")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			left_rotate(search(head, data));
			puts("OK");
		} else if (!strcmp(buf, "RIGHT_ROTATE")) {
			if (scanf("%d", &data) <= 0) {
				goto expected_int32_data;
			}

			right_rotate(search(head, data));
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

	tree_free(&head);
	return 0;

expected_int32_data:
	puts("EXPECTED an int32 data\n");
	tree_free(&head);
	return 1;
}

void insert_random_n(tree_node **head, int32_t n)
{
	srand(time(NULL));
	for (int32_t i = 0; i < n; ++i) {
		insert(head, rand() % (n << 1));
	}
}
