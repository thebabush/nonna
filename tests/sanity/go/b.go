// Sanity dataset — variants (Go).
package sanity

func runningAverage(xs []float64) float64 {
	cnt := 0
	acc := 0.0
	for _, item := range xs {
		acc += item
		cnt += 1
	}
	if cnt == 0 {
		return 0.0
	}
	return acc / float64(cnt)
}

func pickWidest(names []string) string {
	winner := ""
	for _, cand := range names {
		if len(cand) > len(winner) {
			winner = cand
		}
	}
	return winner
}

func floorAll(values []int64, lo int64) {
	for i := 0; i < len(values); i++ {
		if values[i] < lo {
			values[i] = lo
		}
	}
}

func tally(text string, needle rune) int {
	n := 0
	i := 0
	runes := []rune(text)
	for i < len(runes) {
		if runes[i] == needle {
			n += 1
		}
		i += 1
	}
	return n
}
