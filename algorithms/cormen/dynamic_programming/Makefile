FLAGS	=\
	-Iinclude/\
	-std=c++17

.PHONY: cut_rod fib

cut_rod: src/cut_rod.cpp
	g++ $^ -o $@ $(FLAGS)

fib: src/fib.cpp
	g++ $^ -o $@ $(FLAGS)
