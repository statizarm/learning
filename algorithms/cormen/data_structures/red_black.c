#include <stdio.h>
#include <stdint.h>

typedef struct __rb_tree_node {
	struct __rb_tree_node *left;
	struct __rb_tree_node *right;

	uint64_t value;

	uint8_t color; // 0 - black, !0 - red
} rb_tree_node_t;
	
void rb_insert_value (rb_tree_node_t **root, uint64_t value)
{
	rb_tree_node_t *node = (rb_tree_node_t *) malloc (sizeof (rb_tree_node_t));

	node->value = value;
	node->color = !0; // red - default color

	_rb_insert_node (root, node);

	_rb_fixup (root);
}

void _rb_insert_node (rb_tree_node_t **root, rb_tree_node_t *node)
{
	rb_tree_node_t **branch = root;

	while (*branch != NULL) {
		if (node->value < (*branch)->value) {
			branch = &(*branch)->left;
		} else if (node->value > (*branch)->value) {
			branch = &(*branch)->right;
		} else {
			return;
		}
	}

	*branch = node;
}

void _rb_fixup (rb_tree_node_t **root)
{
	// do some stuff
}

void rb_pretty_print (rb_tree_node_t *root, uint32_t offset)
{
	if (root->right) {
		rb_pretty_print (root->right, offset + 2);
		
		for (int i = 0; i < offset + 1; i++) {
			putchar (' ');
		}
		putchar ('/');
		

	
}

int main ()
{
	return 0;
}
