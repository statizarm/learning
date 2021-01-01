#!/bin/bash

BIN_NAME='a.out'
TEST_DIR='tests/'

make $BIN_NAME

for i in `ls $TEST_DIR`
do
	echo "run TEST №$i"
	cat $TEST_DIR$i | ./$BIN_NAME
done
