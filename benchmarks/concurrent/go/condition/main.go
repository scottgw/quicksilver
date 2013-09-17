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
	x := 0

	done := make(chan bool)

	for i := 0; i < 2*num_workers; i++ {
		
		go func(sign int) {
			for j := 0; j < num_iters; j++ {
				mutex.Lock()
				for x % 2 != sign {
					cond.Signal()
					cond.Wait()
				}
				x++
				cond.Signal()
				mutex.Unlock()
			}
			done <- true
		}(i % 2)
	}

	for i := 0; i < 2*num_workers; i++ {
		<-done
	}

	fmt.Printf("%d\n", x)
}
