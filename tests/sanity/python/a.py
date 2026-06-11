# Sanity dataset — originals + distractors (Python).


def mean(values):
    total = 0.0
    count = 0
    for v in values:
        total += v
        count += 1
    if count == 0:
        return 0.0
    return total / count


def longest(words):
    best = None
    for w in words:
        if best is None or len(w) > len(best):
            best = w
    return best


def count_char(text, needle):
    n = 0
    for c in text:
        if c == needle:
            n += 1
    return n


def clamp_all(values, lo, hi):
    for i in range(len(values)):
        if values[i] < lo:
            values[i] = lo
        elif values[i] > hi:
            values[i] = hi


def join_with(parts, sep):
    out = ""
    first = True
    for p in parts:
        if not first:
            out += sep
        out += p
        first = False
    return out


def fib(n):
    a = 0
    b = 1
    i = 0
    while i < n:
        t = a + b
        a = b
        b = t
        i += 1
    return a
