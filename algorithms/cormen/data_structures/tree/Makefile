FLAGS 		= 	-Iinclude/\
			-lm\
			-fanalyzer
SRC		=\
			src/red_black.c\
			src/main.c\
			src/tree_print.c\
			src/avl.c

.PHONY: avl rb

avl:
	gcc $(FLAGS) $(SRC) -o tree -D AVL

rb:
	gcc $(FLAGS) $(SRC) -o tree -D RED_BLACK
