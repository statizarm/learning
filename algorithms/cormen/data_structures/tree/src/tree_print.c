#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "tree_print.h"

#define FRAME_TAB_SIZE 127
#define NODE_QUEUE_SIZE 32

struct _tree_node {
	struct _tree_node *left;
	struct _tree_node *right;
	int64_t data;
};

struct _node_frame {
	uint32_t start;
	uint32_t end;
};

struct __list {
	struct __list *next;
	struct _node_frame frame;
	void *key;
};

struct _hash_tab {
	struct __list **mem;
	uint64_t size;
};

struct _queue {
	struct _tree_node **mem;
	uint64_t size;
	uint32_t first;
	uint32_t last;
};

struct _hash_tab *_hash_tab_create(uint64_t size);
void _hash_tab_free(struct _hash_tab *tab);

void _hash_tab_insert(struct _hash_tab *tab, struct _tree_node *key,
	struct _node_frame frame);
struct _node_frame _hash_tab_delete(struct _hash_tab *tab,
	struct _tree_node *key);


struct _queue *_queue_create(uint64_t size);
void _queue_free(struct _queue *q);

struct _tree_node *_dequeue(struct _queue *nqueue);
void _enqueue(struct _queue *nq, struct _tree_node *node);
uint64_t _queue_len(struct _queue *nqueue);

void _calc_frames(struct _tree_node *head, struct _hash_tab *ftab);

void _node_print(struct _tree_node *node, struct _node_frame frame,
	void (*ntoa)(void *, char *));

void tree_print (void *head, void (*ntoa) (void *, char *))
{
	if (head == NULL) {
		return;
	}
	
	struct _tree_node *node;
	struct _node_frame frame;
	uint64_t printed;
	uint64_t qlen;

	// In general case FRAME_TAB_SIZE and NODE_QUEUE_SIZE must be replaced
	// with value, calculated from tree height
	struct _hash_tab *frame_tab = _hash_tab_create(FRAME_TAB_SIZE);
	struct _queue *nodes_q = _queue_create(NODE_QUEUE_SIZE);
	_calc_frames((struct _tree_node *) head, frame_tab);

	_enqueue(nodes_q, (struct _tree_node *) head);

	while ((qlen = _queue_len(nodes_q)) > 0) {
		printed = 0;
		for (int i = 0; i < qlen; ++i) {
			node = _dequeue(nodes_q);

			_enqueue(nodes_q, node->left);
			_enqueue(nodes_q, node->right);

			frame = _hash_tab_delete(frame_tab, node);

			while (printed < frame.start) {
				putchar(' ');
				++printed;
			}

			_node_print(node, frame, ntoa);

			printed = frame.end;
		}

		puts("\n\n");
	}

	_hash_tab_free(frame_tab);
	_queue_free(nodes_q);
}

struct _hash_tab *_hash_tab_create(uint64_t size)
{
	struct _hash_tab *tab = malloc(sizeof(struct _hash_tab));
	tab->mem = calloc(size, sizeof(struct __list *));
	tab->size = size;
	return tab;
}

void __list_free(struct __list *head);

void _hash_tab_free(struct _hash_tab *tab)
{
	struct __list **mem = tab->mem;
	struct __list **mem_end = mem + tab->size;
	while (mem < mem_end) {
		__list_free(*mem);
		++mem;
	}

	free(tab);
}

uint64_t __hash(struct _tree_node *key, uint64_t mod);

void __list_insert(struct __list **head, struct _tree_node *key,
	struct _node_frame frame);
struct _node_frame __list_delete(struct __list **head, struct _tree_node *key);

void _hash_tab_insert(struct _hash_tab *tab, struct _tree_node *key,
	struct _node_frame frame)
{
	__list_insert(&tab->mem[__hash(key, tab->size)], key, frame);	
}

struct _node_frame _hash_tab_delete(struct _hash_tab *tab,
	struct _tree_node *key)
{
	return __list_delete(&tab->mem[__hash(key, tab->size)], key);
}

void __list_free(struct __list *head)
{
	if (head != NULL) {
		__list_free (head->next);
		free (head);
	}
}

uint64_t __hash(struct _tree_node *key, uint64_t mod)
{
	uint64_t res = (uint64_t) key;
	return res << 2 % mod;
}

void __list_insert(struct __list **head, struct _tree_node *key,
	struct _node_frame frame)
{
	struct __list *new_node = malloc(sizeof(struct __list));
	new_node->next = *head;
	new_node->frame = frame;

	*head = new_node;
}

struct _node_frame __list_delete(struct __list **head, struct _tree_node *key)
{
	struct __list *prev = (struct __list *) head;
	struct __list *curr = *head;
	struct _node_frame frame;

	while (curr != NULL) {
		if (curr->key == key) {
			prev->next = curr->next;
			frame = curr->frame;

			free(curr);

			break;
		}
	}

	return frame;
}

struct _queue *_queue_create(uint64_t size)
{
	struct _queue *q = malloc(sizeof(struct _queue));

	q->size = size;
	q->last = q->first = 0;
	q->mem = calloc(size, sizeof(struct _tree_node *));

	return q;
}

void _queue_free(struct _queue *q)
{
	free(q->mem);
	free(q);
}

void _enqueue(struct _queue *q, struct _tree_node *node)
{
	q->mem[q->last] = node;
	q->last = (q->last + 1) % q->size;
}

struct _tree_node *_dequeue(struct _queue *q)
{
	struct _tree_node *node = q->mem[q->first];
	q->first = (q->first - 1) % q->size;
	return node;
}

uint64_t _queue_len(struct _queue *q)
{
	if (q->first > q->last) {
		return q->size - q->first + q->last;
	} else {
		return q->last - q->first;
	}
}

struct _node_frame __calc_frames(struct _tree_node *head,
	struct _hash_tab *ftab, uint32_t offset);

void _calc_frames(struct _tree_node *head, struct _hash_tab *ftab)
{
	struct _node_frame frame = __calc_frames(head, ftab, 0);
	_hash_tab_insert(ftab, head, frame);
}

struct _node_frame __calc_frames(struct _tree_node *head,
	struct _hash_tab *ftab, uint32_t offset)
{
	uint32_t min_width = (uint32_t) log10(head->data) + 1;
	struct _node_frame l_frame = {
		.start = offset,
		.end = offset + min_width
	};
	struct _node_frame r_frame = {
		.start = l_frame.end,
		.end = l_frame.end + min_width
	};

	struct _node_frame frame;

	if (head->left != NULL) {
		l_frame = __calc_frames (head->left, ftab, offset);

		if (l_frame.end - l_frame.start < min_width) {
			l_frame.end = l_frame.start + min_width;
		}

		_hash_tab_insert(ftab, head->left, l_frame);
	}
	

	if (head->right != NULL) {
		r_frame = __calc_frames (head->right, ftab, l_frame.end);
		_hash_tab_insert(ftab, head->right, r_frame);
	}

	frame.start = l_frame.start;
	frame.end = r_frame.end;
}

void _node_print(struct _tree_node *node, struct _node_frame frame,
	void (*ntoa)(void *, char *))
{
	char buf[32];
	uint32_t len;
	ntoa(node, buf);

	len = strlen (buf);

	len = printf("%.*s", (frame.end - frame.start - len / 2), buf);

	for (int64_t i = frame.end - frame.start; i > len; --i) {
		putchar(' ');
	}
}

