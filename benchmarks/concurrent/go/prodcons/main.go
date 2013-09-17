package main

import (
	"flag"
	"fmt"
	"strconv"
	"sync"
)

func main() {
	flag.Parse()
	args := flag.Args()
	num_iters, _ := strconv.Atoi(args[0])
	num_workers, _ := strconv.Atoi(args[1])
	mutex := &sync.Mutex{}

	cond := sync.NewCond(mutex)
	x := make([]int, 0)

	done := make(chan bool)

	for i := 0; i < num_workers; i++ {
		go func() {
			for j := 0; j < num_iters; j++ {
				mutex.Lock()
				for len(x) == 0 {
					cond.Wait()
				}

				x = x[1:len(x)]
				cond.Signal()

				mutex.Unlock()
			}
			done <- true
		}()
	}

	for i := 0; i < num_workers; i++ {

		go func() {
			for j := 0; j < num_iters; j++ {
				mutex.Lock()

				x = append(x, j)
				cond.Signal()

				mutex.Unlock()
			}
			done <- true
		}()
	}

	for i := 0; i < 2*num_workers; i++ {
		<-done
	}

	fmt.Printf("%d\n", len(x))
}
