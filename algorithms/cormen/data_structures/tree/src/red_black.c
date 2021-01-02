#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "red_black.h"

void rb_tree_insert(struct rb_tree_node **head, int64_t data)
{
	if (*head == NULL) {
		*head = malloc(sizeof(struct rb_tree_node));

		(*head)->left = NULL;
		(*head)->right = NULL;

		(*head)->data = data;

		(*head)->color = RED;
	} else if (data > (*head)->data) {
		rb_tree_insert (&(*head)->right, data);
	} else if (data < (*head)->data) {
		rb_tree_insert (&(*head)->left, data);
	}
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
		sprintf (buf, "\e[0;30m%d\e[m", head->data);
	}
}

void rb_tree_free (struct rb_tree_node **head)
{
	if (*head != NULL) {
		rb_tree_free(&(*head)->right);
		rb_tree_free(&(*head)->left);

		free (*head);
		*head = NULL;
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

