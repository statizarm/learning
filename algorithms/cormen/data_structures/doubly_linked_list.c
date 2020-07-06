#include <stdlib.h>
#include <stdio.h>

struct linked_list {
	struct linked_list *next;
	struct linked_list *prev;
	int data;
};

void init_head (struct linked_list *head)
{
	head->next = head;
	head->prev = head;
	head->data = 0;
}

void insert (struct linked_list *head, int value)
{
	struct linked_list *node = malloc (sizeof (struct linked_list));

	head->next->prev = node;
	node->next = head->next;
	head->next = node;
	node->prev = head;

	node->data = value;
}

void delete (struct linked_list *head, struct linked_list *node)
{
	node->prev->next = node->next;
	node->next->prev = node->prev;

	free (node);
}

struct linked_list *search (struct linked_list *head, int value)
{
	struct linked_list *node = head->next;
	head->data = value;

	while (node->data != value) {
		node = node->next;
	}

	return (node == head) ? NULL : node;
}

void free_list (struct linked_list *head)
{
	struct linked_list *node = head->next;
	struct linked_list *tmp_node;
	
	while (node != head) {
		tmp_node = node;
		node = node->next;
		free (tmp_node);
	}
}

int is_empty (struct linked_list *head)
{
	return head->next == head->prev;
}

int main (void)
{
	struct linked_list head;
	struct linked_list *node;

	init_head (&head);

	for (int i = 0; i < 10; ++i) {
		printf ("Inserted value: %d\n", i);

		insert (&head, i);
	}

	for (int i = 0; i < 20; ++i) {
		printf ("Serched value: %d\n", i);

		node = search (&head, i);
		if (!node) {
			printf ("Not found\n");
		} else {
			printf ("Found and deleted: %d\n", node->data);

			delete (&head, node);
		}
	}

	free_list (&head);

	if (is_empty (&head)) {
		printf ("SUCCESS\n");
	} else {
		printf ("FAILED\n");
	}
	return 0;
}

