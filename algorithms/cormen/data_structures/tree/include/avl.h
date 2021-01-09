#ifndef TREE_AVL_H_
#define TREE_AVL_H_

#include <stdint.h>

struct avl_tree_node {
	struct avl_tree_node *left;
	struct avl_tree_node *right;
	int64_t data;
	int8_t height; // Posible values: -1, 0, 1
};

void avl_tree_insert(struct avl_tree_node **head, int64_t data);
void avl_tree_delete(struct avl_tree_node **head, int64_t data);

struct avl_tree_node *avl_tree_search(struct avl_tree_node *head, int64_t data);

void avl_tree_free(struct avl_tree_node **head);

void avl_left_rotate(struct avl_tree_node *head);
void avl_right_rotate(struct avl_tree_node *head);

void avl_node_toa(struct avl_tree_node *node, char *buf);

#endif // TREE_AVL_H_
