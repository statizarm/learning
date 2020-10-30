package main

import "fmt"

func findMinMax (args ...int) (int, int) {
	var min = args[0]
	var max = min

	for _, arg := range args[1:] {
		if arg > max {
			max = arg
		} else if arg < min {
			min = arg
		}
	}
	return min, max
}

func sum (args ...int) (sum int) {
	for _, arg := range args {
		sum += arg
	}

	return
}

func printWithCallBack (msg string, foo func ()) {
	defer foo ()
	fmt.Println (msg)
}

func main () {
	defer func () {
		what := recover ()
		fmt.Println (what)
	} ()
	var s = []int {
		48,96,86,68,
		57,82,63,70,
		37,34,83,27,
		19,97, 9,17,
	}

	min, max := findMinMax (s...)

	fmt.Println ("Min: ", min)
	fmt.Println ("Max: ", max)

	fmt.Println ("Sum: ", sum (48,96,86,68,57,82,63,70,37,34,83,27,19,97,9,17,))

	panic ("hmmm")
	printWithCallBack ("hello World!!!!", func () {
		fmt.Println ("i'm a callback")
	})

}
