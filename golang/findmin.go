package main

import "fmt"

func main () {
	var s = []int {
		48,96,86,68,
		57,82,63,70,
		37,34,83,27,
		19,97, 9,17,
	}

	fmt.Println (findMin (s))
}

func findMin (s []int) int {
	var min = s[0]

	for _, val := range s[1:] {
		if val < min {
			min = val
		}
	}

	return min
}
