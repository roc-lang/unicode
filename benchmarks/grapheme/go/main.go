package main

import (
	"fmt"
	"io"
	"os"
	"strconv"

	"github.com/clipperhouse/uax29/v2/graphemes"
)

func main() {
	input, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	if len(input) < 9 || input[8] != '\n' {
		panic("expected 8-digit repeat count, newline, and UTF-8 corpus")
	}
	repeats, err := strconv.ParseUint(string(input[:8]), 10, 64)
	if err != nil {
		panic("invalid repeat count")
	}
	source := string(input[9:])

	if repeats == 0 {
		var count uint64
		var sumEnds uint64
		var weightedEnds uint64
		iterator := graphemes.FromString(source)
		for iterator.Next() {
			count++
			end := uint64(iterator.End())
			sumEnds += end
			weightedEnds += count * end
		}
		fmt.Printf("%d %d %d\n", count, sumEnds, weightedEnds)
		return
	}

	var total uint64
	for i := uint64(0); i < repeats; i++ {
		iterator := graphemes.FromString(source)
		for iterator.Next() {
			total++
		}
	}
	fmt.Println(total)
}
