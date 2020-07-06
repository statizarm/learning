#include <stdio.h>
#define QUEUE_SIZE 8 

struct queue {
	int first;
	int last;
	int queue_mem[QUEUE_SIZE];
};

void init (struct queue *q)
{
	q->first = 0;
	q->last = 1;
}

void enqueue (struct queue *q, int value)
{
	if (q->last == q->first) {
		printf ("Queue overflow\n");
	} else {
		q->queue_mem[q->last] = value;
		q->last = (q->last + 1) % QUEUE_SIZE;
	}
}

int dequeue (struct queue *q)
{
	int new_first = (q->first + 1) % QUEUE_SIZE;
	if (new_first == q->last) {
		printf ("Queue underflow\n");
		return -1;
	} else {
		return q->queue_mem[(q->first = new_first)];
	}
}

int main (void)
{
	struct queue queue;

	init (&queue);

	for (int i = 0; i < QUEUE_SIZE; ++i) {
		printf ("enqueued value: %d\n", i);
		enqueue (&queue, i);
	}

	for (int i = 0; i < QUEUE_SIZE; ++i) {
		printf ("dequeued value: %d\n", dequeue (&queue));
	}

	return 0;
}

