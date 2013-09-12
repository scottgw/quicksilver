/*
 * thresh: histogram thresholding
 *
 * input:
 *   matrix: the integer matrix to be thresholded
 *   nrows, ncols: the number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   mask: a boolean matrix filled with true for cells that are kept
 *
 */
package main

import (
	"flag"
	"fmt"
	"runtime"
	"strconv"
)

var is_bench = flag.Bool("is_bench", false, "")

type ByteMatrix struct {
	Size  uint32
	array []byte
}

func NewByteMatrix(n uint32) *ByteMatrix {
	return &ByteMatrix{n, make([]byte, n*n)}
}

func WrapBytes(n uint32, bytes []byte) *ByteMatrix {
	return &ByteMatrix{n, bytes}
}

func (m *ByteMatrix) Row(i uint32) []byte {
	return m.array[i*m.Size : (i+1)*m.Size]
}

func (m *ByteMatrix) Bytes() []byte {
	return m.array[0 : m.Size*m.Size]
}

var mask [][]bool

func thresh(m *ByteMatrix, n, percent uint32) {
	NP := runtime.GOMAXPROCS(0)

	hist_work := make(chan uint32)
	hist_parts := make(chan []int)
	go func() {
		for i := uint32(0); i < n; i++ {
			hist_work <- i
		}
		close(hist_work)
	}()

	for i := 0; i < NP; i++ {
		go func() {
			my_hist := make([]int, 100)
			for i := range hist_work {
				row := m.Row(i)
				for j := range row {
					my_hist[row[j]]++
				}
			}
			hist_parts <- my_hist
		}()
	}

	var hist [100]int

	for i := 0; i < NP; i++ {
		my_hist := <-hist_parts
		for j := range my_hist {
			hist[j] += my_hist[j]
		}
	}

	count := (n * n * percent) / 100
	prefixsum := 0
	threshold := 99

	for ; threshold > 0; threshold-- {
		prefixsum += hist[threshold]
		if prefixsum > int(count) {
			break
		}
	}

	mask_work := make(chan uint32)

	go func() {
		for i := uint32(0); i < n; i++ {
			mask_work <- i
		}
		close(mask_work)
	}()

	mask_done := make(chan bool)
	for i := 0; i < NP; i++ {
		go func() {
			for i := range mask_work {
				row := m.Row(i)
				for j := range row {
					mask[i][j] = row[j] >= byte(threshold)
				}
			}
			mask_done <- true
		}()
	}

	for i := 0; i < NP; i++ {
		<-mask_done
	}
}

func main() {
	flag.Parse()
	args := flag.Args()

	n, _ := strconv.ParseInt(args[0], 0, 0)
	percent, _ := strconv.ParseInt(args[1], 0, 32)

	mask = make([][]bool, n)
	for i := range mask {
		mask[i] = make([]bool, n)
	}

	m := WrapBytes(uint32(n), make([]byte, n*n))

	if !*is_bench {
		for i := uint32(0); i < uint32(n); i++ {
			row := m.Row(i)
			for j := range row {
				fmt.Scanf("%d", &row[j])
			}
		}
	}

	thresh(m, uint32(n), uint32(percent))

	if !*is_bench {
		for i := uint32(0); i < uint32(n); i++ {
			for j := uint32(0); j < uint32(n); j++ {
				if mask[i][j] {
					fmt.Printf("1 ")
				} else {
					fmt.Printf("0 ")
				}
			}
			fmt.Printf("\n")
		}
		fmt.Printf("\n")
	}
}
