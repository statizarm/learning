#include <stdlib.h>
#include <stdio.h>

#include "avl.h"

extern struct avl_tree_node *nil;

struct avl_tree_node *_new_node(int64_t data);

void _avl_tree_insert(struct avl_tree_node **head, int64_t data);

void avl_tree_insert(struct avl_tree_node **head, int64_t data)
{
	if (*head == NULL) {
		*head = _new_node(data);
	} else {
		_avl_tree_insert(head, data);
	}
}

struct avl_tree_node *_new_node(int64_t data)
{
	struct avl_tree_node *node = malloc(sizeof(struct avl_tree_node));
	
	if (node != NULL) {
		node->data = data;
		node->left = node->right = nil;
		node->height = 0;
	}

	return node;
}

struct _path {
	struct avl_tree_node **node;
	int8_t delta;
};

static void _avl_tree_big_right_rotate(struct avl_tree_node **head);
static void _avl_tree_big_left_rotate(struct avl_tree_node **head);
static void _avl_tree_right_rotate(struct avl_tree_node **head);
static void _avl_tree_left_rotate(struct avl_tree_node **head);

void _avl_tree_insert(struct avl_tree_node **head, int64_t data)
{
	struct _path stack[32];
	uint8_t top = 0;
	int8_t height;

	while (*head != nil) {
		stack[top].node = head;
		if (data < (*head)->data) {
			stack[top++].delta = -1;
			head = &(*head)->left;
		} else if (data > (*head)->data) {
			stack[top++].delta = 1;
			head = &(*head)->right;
		} else {
			goto exit;
		}
	}

	*head = _new_node(data);

	while (top > 0) {
		head = stack[--top].node;
		height = (*head)->height + stack[top].delta;
		if (height < -1) {
			if ((*head)->left->height > 0) {
				_avl_tree_big_right_rotate(head);
			} else {
				_avl_tree_right_rotate(head);
			}
			break;
		} else if (height > 1) {
			if ((*head)->right->height < 0) {
				_avl_tree_big_left_rotate(head);
			} else {
				_avl_tree_left_rotate(head);
			}
			break;
		} else {
			(*head)->height = height;

			if (height == 0) {
				break;
			}
		}
	}
exit:
	return;
}

static void _avl_tree_big_right_rotate(struct avl_tree_node **head)
{
	struct avl_tree_node *tmp = (*head)->left->right;
	int8_t h;

	// moving left node
	(*head)->left->right = tmp->left;
	tmp->left = (*head)->left;

	// moving right node
	(*head)->left = tmp->right;
	tmp->right = *head;

	// moving dst node to root
	*head = tmp;

	// calculate h:
	// if height == +1 -> h = 0 else height == -1 -> h = 1
	h = (int8_t) ((uint8_t) (*head)->height >> 7);

	(*head)->right->height = h;
	(*head)->left->height = h - 1;
	(*head)->height = 0;
	
}

static void _avl_tree_big_left_rotate(struct avl_tree_node **head)
{
	struct avl_tree_node *tmp = (*head)->right->left;
	int8_t h;

	// moving right node
	(*head)->right->left = tmp->right;
	tmp->right = (*head)->right;

	// moving left node
	(*head)->right = tmp->left;
	tmp->left = *head;

	// moving dst node to root
	*head = tmp;

	// calc h:
	// see _avl_tree_big_right_rotate
	h = (int8_t) ((uint8_t) (*head)->height >> 7);

	tmp->right->height = h;
	tmp->left->height = h - 1;
	tmp->height = 0;
}

static void _avl_tree_right_rotate(struct avl_tree_node **head)
{
	struct avl_tree_node *tmp = (*head)->left;

	(*head)->left = tmp->right;
	tmp->right = *head;

	*head = tmp;

	tmp->left->height = 0;
	tmp->height = 0;
}

static void _avl_tree_left_rotate(struct avl_tree_node **head)
{
	struct avl_tree_node *tmp = (*head)->right;

	(*head)->right = tmp->left;
	tmp->left = *head;

	*head = tmp;

	tmp->height = 0;
	tmp->left->height = 0;
}

void avl_tree_delete(struct avl_tree_node **head, int64_t data)
{

}

struct avl_tree_node *avl_tree_search(struct avl_tree_node *head, int64_t data)
{
	if (head == NULL) {
		goto not_found;
	}

	while (head != nil) {
		if (data < head->data) {
			head = head->left;
		} else if (data > head->data) {
			head = head->right;
		} else {
			goto found;
		}
	}

not_found:
	return NULL;

found:
	return head;

}

static void _avl_tree_free(struct avl_tree_node *head);

void avl_tree_free(struct avl_tree_node **head) {
	if (*head != NULL) {
		_avl_tree_free(*head);
	}

	*head = NULL;
}

static void _avl_tree_free(struct avl_tree_node *head)
{
	if (head != nil) {
		_avl_tree_free(head->left);
		_avl_tree_free(head->right);

		free (head);
	}
}

void avl_left_rotate(struct avl_tree_node *head)
{
}

void avl_right_rotate(struct avl_tree_node *head)
{
}

void avl_node_toa(struct avl_tree_node *node, char *buf)
{
	if (node == NULL) {
		sprintf(buf, "NIL");
	} else {
		sprintf(buf, "%d", node->data);
	}
}

