#ifndef TREE_RED_BLACK_H_
#define TREE_RED_BLACK_H_

#define BLACK	0
#define RED	1

#include <stdint.h>

struct rb_tree_node {
	struct rb_tree_node *left;
	struct rb_tree_node *right;
	int64_t data;
	uint8_t color; // 0 - black, 1 - red
};


void rb_tree_insert(struct rb_tree_node **head, int64_t data);
void rb_tree_delete(struct rb_tree_node **head, int64_t data);
struct rb_tree_node *rb_tree_search(struct rb_tree_node *head, int64_t data);
void rb_tree_free(struct rb_tree_node **head);
void rb_left_rotate(struct rb_tree_node *head);
void rb_right_rotate(struct rb_tree_node *head);

void rb_node_toa (struct rb_tree_node *node, char *buf);


#endif // TREE_RED_BLACK_H_
