package main

import "fmt"

func main () {
	for i := 0; i < 100; i += 1 {
		if i % 3 == 0 {
			if i % 5 == 0 {
				fmt.Println ("FizzBuzz")
			}
			fmt.Println ("Fizz")
		} else if i % 5 == 0 {
			fmt.Println ("Buzz")
		} else {
			fmt.Println (i)
		}
	}
}
