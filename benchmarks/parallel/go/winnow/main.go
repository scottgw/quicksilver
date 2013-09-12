/*
 * winnow: weighted point selection
 *
 * input:
 *   matrix: an integer matrix, whose values are used as masses
 *   mask: a boolean matrix showing which points are eligible for
 *     consideration
 *   nrows, ncols: the number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   points: a vector of (x, y) points
 *
 */
package main

import (
	"flag"
	"fmt"
	"runtime"
	"sort"
	"strconv"
)

type ByteMatrix struct {
	Rows, Cols int
	array      []byte
}

func WrapBytes(n int, bytes []byte) *ByteMatrix {
	return &ByteMatrix{n, n, bytes}
}

func NewByteMatrix(n int) *ByteMatrix {
	return &ByteMatrix{n, n, make([]byte, n*n)}
}

func (m *ByteMatrix) Row(i int) []byte {
	return m.array[i*m.Cols : (i+1)*m.Cols]
}

func (m *ByteMatrix) Bytes() []byte {
	return m.array[0 : m.Rows*m.Cols]
}

var is_bench = flag.Bool("is_bench", false, "")
var matrix []byte
var mask [][]bool
var points []int

type WinnowPoints struct {
	m *ByteMatrix
	e []int // indexes into the ByteMatrix 'm'
}

func (p *WinnowPoints) Len() int {
	return len(p.e)
}

func (p *WinnowPoints) Swap(i, j int) {
	p.e[i], p.e[j] = p.e[j], p.e[i]
}

func (p *WinnowPoints) Less(i, j int) bool {
	return ArrayLess(p.m.array, p.e[i], p.e[j])
}

func ArrayLess(array []byte, x, y int) bool {
	if array[x] != array[y] {
		return array[x] < array[y]
	}
	return x < y
}

func WinnowMerge(points chan WinnowPoints) {
	var merged WinnowPoints
	x := <-points
	y := <-points

	new_size := len(x.e) + len(y.e)

	merged.m = x.m
	merged.e = make([]int, new_size)

	j := 0
	k := 0
	for i := 0; i < new_size; i++ {
		if j < len(x.e) && k < len(y.e) {
			if ArrayLess(merged.m.array, x.e[j], y.e[k]) {
				merged.e[i] = x.e[j]
				j++
			} else {
				merged.e[i] = y.e[k]
				k++
			}
		} else if j < len(x.e) {
			merged.e[i] = x.e[j]
			j++
		} else if k < len(y.e) {
			merged.e[i] = y.e[k]
			k++
		}
	}
	points <- merged
}

func Winnow(m *ByteMatrix, n, winnow_nelts int) {
	NP := runtime.GOMAXPROCS(0)
	var values WinnowPoints
	values.m = m

	values_work := make(chan int, 1024)
	values_done := make(chan WinnowPoints, NP)
	values_done <- WinnowPoints{m, make([]int, 0)}

	go func() {
		for i := 0; i < n; i++ {
			values_work <- i
		}
		close(values_work)
	}()

	merged := make(chan bool, NP)

	for i := 0; i < NP; i++ {
		go func() {
			WinnowMerge(values_done)
			merged <- true
		}()
	}

	for i := 0; i < NP; i++ {
		go func() {
			var local_indexes []int
			for i := range values_work {
				for j := 0; j < n; j++ {
					idx := i*n + j
					if *is_bench {
						mask[i][j] = ((i * j) % (n + 1)) == 1
					}
					if mask[i][j] {
						local_indexes = append(local_indexes, idx)
					}
				}
			}
			var local_values WinnowPoints
			local_values.m = m
			local_values.e = local_indexes

			sort.Sort(&local_values)
			values_done <- local_values
		}()
	}

	for i := 0; i < NP; i++ {
		<-merged
	}

	values = <-values_done

	chunk := values.Len() / winnow_nelts

	points = make([]int, winnow_nelts)
	point_work := make(chan int, 1024)
	point_done := make(chan bool)
	go func() {
		for i := 0; i < winnow_nelts; i++ {
			point_work <- i
		}
		close(point_work)
	}()

	for i := 0; i < NP; i++ {
		go func() {
			for i := range point_work {
				points[i] = values.e[i*chunk]
			}
			point_done <- true
		}()
	}

	for i := 0; i < NP; i++ {
		<-point_done
	}
}

func read_integer() int {
	var value int
	for true {
		var read, _ = fmt.Scanf("%d", &value)
		if read == 1 {
			break
		}
	}
	return value
}

func read_matrix(n int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			matrix[i*n+j] = byte(read_integer())
		}
	}
}

func read_mask(n int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			mask[i][j] = (read_integer() == 1)
		}
	}
}

func main() {
	flag.Parse()
	args := flag.Args()

	n, _ := strconv.ParseInt(args[0], 0, 0)
	nelts, _ := strconv.ParseInt(args[1], 0, 32)

	m := NewByteMatrix(int(n))

	matrix = m.array

	mask = make([][]bool, n)
	for i := range mask {
		mask[i] = make([]bool, n)
	}
	if !*is_bench {
		read_matrix(int(n))
		read_mask(int(n))
	}

	points = make([]int, nelts)

	Winnow(m, int(n), int(nelts))

	if !*is_bench {
		fmt.Printf("%d\n", nelts)
		for i := 0; i < int(nelts); i++ {
			fmt.Printf("%d %d\n", points[i]/int(n), points[i]%int(n))
		}
		fmt.Printf("\n")
	}
}
