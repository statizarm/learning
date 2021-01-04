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

void rb_tree_delete(struct rb_tree_node **head, int64_t data)
{
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

