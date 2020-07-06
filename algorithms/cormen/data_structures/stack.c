#include <stdio.h>
#define STACK_SIZE 16

struct stack {
	int *top;
	int stack_mem[STACK_SIZE];
};

void init_stack (struct stack *stack)
{
	stack->top = stack->stack_mem;
}

void push (struct stack *stack, int value)
{
	if (stack->top == stack->stack_mem + STACK_SIZE) {
		printf ("Stack overflow\n");
	} else {
		*stack->top++ = value;
	}
}

int pop (struct stack *stack)
{
	if (stack->top == stack->stack_mem) {
		printf ("Stack underflow\n");
		return -1;
	} else {
		return *--stack->top;
	}
}

int main (void)
{
	struct stack stack;
	init_stack (&stack);

	for (int i = 0; i < STACK_SIZE + 1; ++i) {
		printf ("pushed value to stack: %d\n", i);
		push (&stack, i);
	}

	for (int i = 0; i < STACK_SIZE + 1; ++i) {
		printf ("poped value from stack: %d\n", pop (&stack));
	}
	return 0;
}
