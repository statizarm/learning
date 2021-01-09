#include <stdlib.h>
#include <stdio.h>

#include "avl.h"

static const uint8_t _htab[] = {
	1, 0, 0,
	0, 0, -1,
	0, 0, 0,
	0, -1,
	0, 1,
	1, 0,
	1, 0
};

#define big_rotate_htab_left_ptr (&(_htab[4]))
#define big_rotate_htab_right_ptr (&(_htab[1]))
#define big_rotate_htab_root_ptr (&(_htab[7]))
#define left_rotate_htab_left_ptr (&(_htab[13]))
#define left_rotate_htab_root_ptr (&(_htab[15]))
#define right_rotate_htab_right_ptr (&(_htab[10]))
#define right_rotate_htab_root_ptr (&(_htab[12]))

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
			stack[top].delta = -1;
			head = &(*head)->left;
		} else if (data > (*head)->data) {
			stack[top].delta = 1;
			head = &(*head)->right;
		} else {
			goto exit;
		}

		++top;
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
	tmp->left->height = big_rotate_htab_left_ptr[tmp->height];
	tmp->right->height = big_rotate_htab_right_ptr[tmp->height];
	tmp->height = big_rotate_htab_root_ptr[tmp->height];
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

	// calculate h:
	tmp->left->height = big_rotate_htab_left_ptr[tmp->height];
	tmp->right->height = big_rotate_htab_right_ptr[tmp->height];
	tmp->height = big_rotate_htab_root_ptr[tmp->height];
}

static void _avl_tree_right_rotate(struct avl_tree_node **head)
{
	struct avl_tree_node *tmp = (*head)->left;

	(*head)->left = tmp->right;
	tmp->right = *head;

	*head = tmp;

	tmp->right->height = right_rotate_htab_right_ptr[tmp->height];
	tmp->height = right_rotate_htab_root_ptr[tmp->height];
}

static void _avl_tree_left_rotate(struct avl_tree_node **head)
{
	struct avl_tree_node *tmp = (*head)->right;

	(*head)->right = tmp->left;
	tmp->left = *head;

	*head = tmp;

	tmp->left->height = left_rotate_htab_left_ptr[tmp->height];
	tmp->height = left_rotate_htab_root_ptr[tmp->height];
}

static void _avl_tree_delete_fixup(struct _path *stack, uint8_t top);

void avl_tree_delete(struct avl_tree_node **head, int64_t data)
{
	struct _path stack[32];
	struct avl_tree_node *tmp;
	uint8_t top = 0;
	uint8_t node_id;

	while (*head != nil) {
		if (data < (*head)->data) {
			stack[top].node = head;
			stack[top].delta = 1;
			head = &(*head)->left;
		} else if (data > (*head)->data) {
			stack[top].node = head;
			stack[top].delta = -1;
			head = &(*head)->right;
		} else {
			goto node_founded;
		}

		++top;
	}
	goto exit;
node_founded:
	tmp = *head;
	if (tmp->right != nil) {
		node_id = top;
		stack[top].node = head;
		stack[top++].delta = -1;

		head = &tmp->right;
		while ((*head)->left != nil) {
			stack[top].node = head;
			stack[top].delta = 1;

			head = &(*head)->left;

			++top;
		}

		*(stack[node_id++].node) = *head;
		(*head)->left = tmp->left;

		stack[node_id].node = &(*head)->right;
		*head = (*head)->right;
		*(stack[node_id].node) = tmp->right;

		free(tmp);
		
	} else {
		*head = tmp->left;
		free(tmp);
	}

	_avl_tree_delete_fixup(stack, top);
exit:
	return;
}

static void _avl_tree_delete_fixup(struct _path *stack, uint8_t top) {
	struct avl_tree_node **head;
	int8_t height;
	while (top > 0) {
		head = stack[--top].node;
		height = (*head)->height + stack[top].delta;
		if (height < -1) {
			if ((*head)->left->height > 0) {
				_avl_tree_big_right_rotate(head);
			} else {
				_avl_tree_right_rotate(head);
			}
		} else if (height > 1) {
			if ((*head)->right->height < 0) {
				_avl_tree_big_left_rotate(head);
			} else {
				_avl_tree_left_rotate(head);
			}
		} else {
			if ((*head)->height == 0) {
				(*head)->height = height;
				break;
			}

			(*head)->height = height;
		}
	}
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
		sprintf(buf, "%d", node->height);
	}
}

