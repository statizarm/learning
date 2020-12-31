#include <stdio.h>
#include "tree_print.h"

struct _tree_node {
	struct _tree_node *left;
	struct _tree_node *right;
	void *data;
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
	struct __list **nodes;
};

struct _queue {
	void **mem;
};

struct _hash_tab *_hash_tab_create(uint32_t size);
void _hash_tab_free(struct _hast_tab *tab);

void _hash_tab_insert(void *key, struct _node_frame frame);
struct _node_frame *_hash_tab_search(void *key);

struct _queue *_queue_create(uint32_t size);
void _queue_free(struct _queue *q);

struct _tree_node *_dequeue(struct _queue *nqueue);
void _enqueue(struct _queue *nq, struct _tree_node *node);
uint32_t _queue_len(struct _queue *nqueue);

void _calc_frames(struct _tree_node *head, struct _hash_tab *ftab);

void _node_print(struct _tree_node *node, struct _node_frame *frame,
	void (*ntoa)(void *, char *));

void tree_print (void *head, void (*ntoa) (void *, char *))
{
	if (head == NULL) {
		return;
	}
	
	struct _tree_node *node;
	struct _node_frame *frame;
	uint64_t printed;

	struct _hash_tab *frame_tab = _hash_tab_create(127);
	struct _queue *nodes_q = _queue_create(32);
	_calc_frames(head, widths);

	while ((qlen = _queue_len(nodes_q)) > 0) {
		printed = 0;
		for (int i = 0; i < qlen; ++i) {
			node = _dequeue(nodes_q);

			_enqueue(nodes_q, node->left);
			_enqueue(nodes_q, node->right);

			frame = _hash_tab_search(frame_tab, node);

			_node_print(node, frame, ntoa);

			printed = frame->end;
		}

		puts("\n\n");
	}
}

