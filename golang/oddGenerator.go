package main

import "fmt"

func main () {
	oddGenerator := makeOddGenerator ();

	fmt.Println (oddGenerator (), oddGenerator (), oddGenerator ())
}

func makeOddGenerator () func () int {
	seed := 1

	return func () int {
		tmp := seed
		seed += 2
		return tmp
	}
}
