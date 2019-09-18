CC			=	gcc
TARGET		=	test
SRC_DIR		=	src/
INCL_DIR	=	include/

FLAGS		=	\
				-I$(INCL_DIR)

DEPS		=	\
				$(SRC_DIR)main.o\
				$(SRC_DIR)euclid_alg.o

$(SRC_DIR)%.o : $(SRC_DIR)%.c
	$(CC) -c $(FLAGS) $^ -o $@

$(TARGET) : $(DEPS)
	$(CC) $^ -o $@

.PHONY : clean rebuild delete

clean :
	rm -f $(SRC_DIR)*.o

delete :
	rm -f $(TARGET) && make clean

rebuild :
	make delete && make
