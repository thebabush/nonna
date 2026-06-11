// Sanity dataset — originals + distractors (Go).
package sanity

func mean(values []float64) float64 {
	total := 0.0
	count := 0
	for _, v := range values {
		total += v
		count += 1
	}
	if count == 0 {
		return 0.0
	}
	return total / float64(count)
}

func longest(words []string) string {
	best := ""
	for _, w := range words {
		if len(w) > len(best) {
			best = w
		}
	}
	return best
}

func countChar(text string, needle rune) int {
	n := 0
	for _, c := range text {
		if c == needle {
			n += 1
		}
	}
	return n
}

func clampAll(values []int64, lo int64, hi int64) {
	for i := 0; i < len(values); i++ {
		if values[i] < lo {
			values[i] = lo
		} else if values[i] > hi {
			values[i] = hi
		}
	}
}

// distractor
func joinWith(parts []string, sep string) string {
	out := ""
	first := true
	for _, p := range parts {
		if !first {
			out += sep
		}
		out += p
		first = false
	}
	return out
}

// distractor
func fib(n uint64) uint64 {
	a := uint64(0)
	b := uint64(1)
	i := uint64(0)
	for i < n {
		t := a + b
		a = b
		b = t
		i += 1
	}
	return a
}
