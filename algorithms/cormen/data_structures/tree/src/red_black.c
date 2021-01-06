#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "red_black.h"


static struct rb_tree_node _nil = {
	.left = &_nil,
	.right = &_nil,
	.color = BLACK
};

struct rb_tree_node *nil = &_nil;

static void _rb_tree_left_rotate(struct rb_tree_node **head);
static void _rb_tree_right_rotate(struct rb_tree_node **head);

static void _rb_tree_insert(struct rb_tree_node **head, int64_t data);

void rb_tree_insert(struct rb_tree_node **head, int64_t data) {
	if (*head == NULL) {
		*head = malloc(sizeof(struct rb_tree_node));
		(*head)->left = (*head)->right = nil;
		(*head)->data = data;
		(*head)->color = BLACK;
	} else {
		_rb_tree_insert (head, data);
	}
}

static void _rb_tree_insert(struct rb_tree_node **head, int64_t data)
{
	struct rb_tree_node **parent_stack[32];
	struct rb_tree_node **curr = head;
	uint8_t stack_top = 1;
	parent_stack[0] = &nil;
	// insert
	while (*curr != nil) {
		parent_stack[stack_top++] = curr;
		if (data < (*curr)->data) {
			curr = &(*curr)->left;
		} else if (data > (*curr)->data) {
			curr = &(*curr)->right;
		} else {
			goto exit;
		}
	}

	*curr = malloc(sizeof(struct rb_tree_node));
	(*curr)->data = data;
	(*curr)->left = (*curr)->right = nil;
	(*curr)->color = RED;
	// fixup
	while ((*(curr = parent_stack[--stack_top]))->color == RED) {
		curr = parent_stack[--stack_top];
		if ((*curr)->right->color == BLACK) {
			if ((*curr)->left->left->color == BLACK) {
				_rb_tree_left_rotate(&(*curr)->left);
			}
			_rb_tree_right_rotate(curr);
			(*curr)->color = BLACK;
			(*curr)->left->color = (*curr)->right->color = RED;
			break;
		} else if ((*curr)->left->color == BLACK) {
			if ((*curr)->right->right->color == BLACK) {
				_rb_tree_right_rotate(&(*curr)->right);
			}
			_rb_tree_left_rotate(curr);
			(*curr)->color = BLACK;
			(*curr)->left->color = (*curr)->right->color = RED;
			break;
		} else {
			(*curr)->left->color = BLACK;
			(*curr)->right->color = BLACK;
			(*curr)->color = RED;
		}
	}

exit:
	(*head)->color = BLACK;
}

static void _rb_tree_right_rotate(struct rb_tree_node **head)
{
	struct rb_tree_node *tmp = (*head)->left->right;
	(*head)->left->right = *head;
	*head = (*head)->left;
	(*head)->right->left = tmp;
}

static void _rb_tree_left_rotate(struct rb_tree_node **head)
{
	struct rb_tree_node *tmp = (*head)->right->left;
	(*head)->right->left = *head;
	*head = (*head)->right;
	(*head)->left->right = tmp;
}

static void _delete_fixup(struct rb_tree_node ***stack, uint8_t stack_top);

void rb_tree_delete(struct rb_tree_node **head, int64_t data)
{
	struct rb_tree_node **parent_stack[32];
	struct rb_tree_node **curr = head;
	struct rb_tree_node *tmp;
	uint8_t stack_top = 0;
	uint8_t node_stack_id, color;

	while (*curr != nil) {
		parent_stack[stack_top++] = curr;
		if (data < (*curr)->data) {
			curr = &(*curr)->left;
		} else if (data > (*curr)->data) {
			curr = &(*curr)->right;
		} else {
			goto node_found;
		}
	}
	return;
node_found:
	tmp = *curr;
	if ((*curr)->right != nil) {
		node_stack_id = stack_top;

		curr = &(*curr)->right;
		while (1) {
			parent_stack[stack_top++] = curr;
			if ((*curr)->left == nil) {
				break;
			}
			curr = &(*curr)->left;
		}
		color = (*curr)->color;

		*parent_stack[node_stack_id - 1] = *curr;
		(*parent_stack[node_stack_id - 1])->color = tmp->color;
		parent_stack[node_stack_id] = &(*curr)->right;

		(*curr)->left = tmp->left;
		*curr = (*curr)->right;

		*parent_stack[node_stack_id] = tmp->right;
	} else {
		color == (*curr)->color;
		*curr = (*curr)->left;
	}
	if (color == BLACK) {
		_delete_fixup(parent_stack, stack_top);
	}
	free(tmp);
}

static void _delete_fixup(struct rb_tree_node ***stack, uint8_t stack_top)
{
	struct rb_tree_node **curr = stack[--stack_top];
	struct rb_tree_node **parent;
	while (stack_top > 0 && (*curr)->color == BLACK) {
		parent = stack[stack_top - 1];
		if (*curr == (*parent)->left) {
			if ((*parent)->right->color == RED) {
				(*parent)->right->color = BLACK;
				(*parent)->color = RED;
				_rb_tree_right_rotate(parent);
				stack[stack_top++] = parent = &(*parent)->left;
			}
			if ((*parent)->right->right->color == BLACK
				&& (*parent)->right->left->color == BLACK) {
				(*parent)->right->color = RED;
				curr = stack[--stack_top];
			} else if ((*parent)->right->right->color == BLACK) {
				(*parent)->right->left->color = BLACK;
				(*parent)->right->color = RED;
				_rb_tree_right_rotate(&(*parent)->right);

				(*parent)->right->color = (*parent)->color;
				(*parent)->color = BLACK;
				(*parent)->right->right->color = BLACK;
				break;
			}
		} else {
			if ((*parent)->left->color == RED) {
				(*parent)->left->color = BLACK;
				(*parent)->color = RED;
				_rb_tree_left_rotate(parent);
				stack[stack_top++] = parent = &(*parent)->right;
			}
			if ((*parent)->left->left->color == BLACK
				&& (*parent)->left->right->color == BLACK) {
				(*parent)->left->color = RED;
				curr = stack[--stack_top];
			} else if ((*parent)->left->left->color == BLACK) {
				(*parent)->left->right->color = BLACK;
				(*parent)->left->color = RED;
				_rb_tree_left_rotate(&(*parent)->left);

				(*parent)->left->color = (*parent)->color;
				(*parent)->color = BLACK;
				(*parent)->left->left->color = BLACK;
				break;
			}
		}
	}

	(*curr)->color = BLACK;
}

struct rb_tree_node *rb_tree_search(struct rb_tree_node *head, int64_t data)
{
	if (head == NULL) {
		return NULL;
	} else if (data > head->data) {
		return rb_tree_search (head->right, data);
	} else if (data < head->data) {
		return rb_tree_search (head->left, data);
	} else {
		return head;
	}
}

void rb_node_toa(struct rb_tree_node *head, char *buf)
{
	if (head == NULL) {
		sprintf (buf, "NIL");
	} else if (head->color == RED) {
		sprintf (buf, "\e[0;31m%d\e[m", head->data);
	} else {
		sprintf (buf, "\e[0;34m%d\e[m", head->data);
	}
}

static void _rb_tree_free(struct rb_tree_node **head);

void rb_tree_free(struct rb_tree_node **head)
{
	if (*head != NULL) {
		_rb_tree_free(head);
	}

	*head = NULL;
}
static void _rb_tree_free (struct rb_tree_node **head)
{
	if (*head != nil) {
		rb_tree_free(&(*head)->right);
		rb_tree_free(&(*head)->left);

		free (*head);
	}
}

void rb_left_rotate(struct rb_tree_node *head)
{
	struct rb_tree_node tmp;
	if (head->right) {
		tmp.data = head->data;
		tmp.color = head->color;
		tmp.left = head->left;
		tmp.right = head->right;

		head->data = head->right->data;
		head->color = head->right->color;
		head->right = head->right->right;
		
		tmp.right->data = tmp.data;
		tmp.right->color = tmp.color;
		tmp.right->right = tmp.right->left;
		tmp.right->left = head->left;
		head->left = tmp.right;
	}
}

void rb_right_rotate(struct rb_tree_node *head)
{
	struct rb_tree_node tmp;
	if (head->left) {
		tmp = *head;

		head->data = head->left->data;
		head->color = head->left->color;
		head->left = head->left->left;

		tmp.left->data = tmp.data;
		tmp.left->color = tmp.color;
		tmp.left->left = tmp.left->right;
		tmp.left->right = head->right;
		head->right = tmp.left;
	}
}

