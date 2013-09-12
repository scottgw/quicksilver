/*
 * randmat: random number generation
 *
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   martix: a nrows x ncols integer matrix
 *
 */
package main

import (
	"flag"
	"fmt"
	"runtime"
	"strconv"
)

type ByteMatrix struct {
	Rows, Cols int
	array      []byte
}

func NewByteMatrix(r, c int) *ByteMatrix {
	return &ByteMatrix{r, c, make([]byte, r*c)}
}

func (m *ByteMatrix) Row(i int) []byte {
	return m.array[i*m.Cols : (i+1)*m.Cols]
}

const (
	LCG_A = 1664525
	LCG_C = 1013904223
)

var (
	is_bench = flag.Bool("is_bench", false, "")
)

func randmat(n int, s uint32) *ByteMatrix {
	matrix := NewByteMatrix(n, n)

	work := make(chan int)

	go func() {
		for i := 0; i < n; i++ {
			work <- i
		}
		close(work)
	}()

	done := make(chan bool)
	NP := runtime.GOMAXPROCS(0)

	for i := 0; i < NP; i++ {
		go func() {
			for i := range work {
				seed := s + uint32(i)
				row := matrix.Row(i)
				for j := range row {
					seed = LCG_A*seed + LCG_C
					row[j] = byte(seed%100) % 100
				}
			}
			done <- true
		}()
	}

	for i := 0; i < NP; i++ {
		<-done
	}

	return matrix
}

func main() {
	flag.Parse()

	var args = flag.Args()

	n, _ := strconv.ParseInt(args[0], 0, 0)
	seed, _ := strconv.ParseInt(args[1], 0, 32)

	matrix := randmat(int(n), uint32(seed))

	if !*is_bench {
		for i := 0; i < int(n); i++ {
			row := matrix.Row(i)
			for j := range row {
				fmt.Printf("%d ", row[j])
			}
			fmt.Printf("\n")
		}
		fmt.Printf("\n")
	}
}
