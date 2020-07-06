#include <stdlib.h>
#include <stdio.h>

typedef struct l_node {
	struct l_node *next;
	int data;
} l_node_t;

void init_head (l_node_t *head)
{
	head->next = head;
	head->data = 0;
}

void insert (l_node_t *head, int value)
{
	l_node_t *node = (l_node_t *) malloc (sizeof (l_node_t));

	node->data = value;
	node->next = head->next;
	head->next = node;
}

void free_list (l_node_t *head)
{
	l_node_t *node = head->next;
	l_node_t *tmp_node;

	while (node != head) {
		tmp_node = node;
		node = node->next;
		free (tmp_node);
	}
}

void print_list (l_node_t *head)
{
	l_node_t *node = head->next;

	while (node->next != head) {
		printf ("%d, ", node->data);
		node = node->next;
	}

	printf ("%d\n", node->data);
}

void reverse_list (l_node_t *head)
{
	l_node_t *node = head->next;
	l_node_t *next_node;
	l_node_t *prev_node = head;

	while (node != head) {
		next_node = node->next;
		node->next = prev_node;
		prev_node = node;
		node = next_node;
	}

	node->next = prev_node;
}

int main (void)
{
	l_node_t head;

	init_head (&head);

	for (int i = 0; i < 10; ++i) {
		insert (&head, i);
	}

	print_list (&head);
	reverse_list (&head);
	print_list (&head);

	free_list (&head);
	return 0;
}
