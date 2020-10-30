package main

import (
	"fmt"
	"math"
)

type rect struct {
	h int
	w int
}

type cube struct {
	h int
	w int
	d int
}

type circle struct {
	r int
}

func (r rect) String () string {
	return "height: " + fmt.Sprint (r.h) +
	       ", width: " + fmt.Sprint (r.w)
}

func (c cube) String () string {
	return "height: " + fmt.Sprint (c.h) +
	       ", width: " + fmt.Sprint (c.w) +
	       ", Depth: " + fmt.Sprint (c.d)
}

func (c circle) String () string {
	return "radius: " + fmt.Sprint (c.r)
}

func (c cube) area () int {
	return c.w * c.h * 2 + c.w * c.d * 2 + c.d * c.h * 2
}

func (c cube) volume () int {
	return c.h * c.w * c.d
}

func (r rect) area () int {
	return r.h * r.w
}

func (c circle) area () int {
	return int (math.Pi * float64 (c.r) * float64 (c.r))
}

type shape2d interface {
	area () int
}

type shape3d interface {
	shape2d
	volume () int
}

func printArea (shapes ...shape2d) {
	for _, shape := range shapes {
		fmt.Println (shape.area ())
	}
}

func printVolumeAndArea (shapes ...shape3d) {
	for _, shape := range shapes {
		printArea (shape)
		fmt.Println (shape.volume ())
	}
}

func main () {
	pr := new (rect)
	pr.h = 10
	pr.w = 7

	var c = cube {h: 5, w:7, d:3}

	var pc = new (circle)
	pc.r = 3

	fmt.Println (*pr, c.String (), *pc)

	printArea (*pr, c, *pc)
	printVolumeAndArea (c)
}
