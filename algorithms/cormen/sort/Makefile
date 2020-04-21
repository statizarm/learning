SRC_DIR		=	src/
LIB_DIR		=	lib/
INCL_DIR	=	include/
ifdef __DEBUG
FLAGS		=	\
				-I$(INCL_DIR)\
				-D__DEBUG\
				-rdynamic
else
FLAGS		=	\
				-I$(INCL_DIR)\
				-rdynamic
endif

DEPS		=	\
				$(SRC_DIR)main.o\
				$(SRC_DIR)test.o\
				$(SRC_DIR)foo_list.o\
				$(SRC_DIR)utils.o\
				$(SRC_DIR)bit_radix_sort.o

LIB_DEPS	=	\
				$(LIB_DIR)pyramid_sort.o\
				$(LIB_DIR)quick_sort.o\
				$(LIB_DIR)random_quick_sort.o\
				$(LIB_DIR)counting_sort.o\
				$(LIB_DIR)radix_sort.o\
				$(LIB_DIR)bit_radix_sort.o\
				$(LIB_DIR)insert_sort.o\
				$(LIB_DIR)bit_radix_quick_sort.o\
				$(LIB_DIR)selection_sort.o\
				$(LIB_DIR)merge_sort.o

TARGET		=	test
LIB			=	$(LIB_DIR)libsort.so

$(SRC_DIR)%.o : $(SRC_DIR)%.c
	$(CC) -c $^ -o $@ $(FLAGS)

$(LIB_DIR)%.o : $(SRC_DIR)%.c
	$(CC) -c -o $@ -fPIC $^

$(TARGET) : $(DEPS)
	$(CC) $^ -o $@ -ldl

$(LIB) : $(LIB_DEPS)
	$(CC) -shared -o $@ $^
	rm -f $(LIB_DIR)*.o

.PHONY : clean rebuild delete debug lib

clean :
	rm -f $(SRC_DIR)*.o

delete :
	make clean && rm -f $(TARGET)

rebuild :
	make clean && make

debug :
	make clean && make __DEBUG=1

lib:
	make $(LIB)
