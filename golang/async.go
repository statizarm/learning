package main

import (
	"fmt"
	"time"
)

func main () {
	var c = make (chan string, 10)

	go func () {
		for i := 0; i < 10; i++ {
			c <- "working hour №" + fmt.Sprint (i)
			time.Sleep (time.Second)
		}
	} ()

	go func () {
		for {
			  select {
			  case s := <- c:
				  fmt.Println (s)
				  time.Sleep (time.Second * 3)
			  case <-time.After (time.Second * 2):
				  break
			  }
		}
	} ()

	<-time.After (time.Second * 30)
}
