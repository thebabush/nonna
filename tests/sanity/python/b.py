# Sanity dataset — variants (Python).


def running_average(xs):
    cnt = 0
    acc = 0.0
    for item in xs:
        acc += item
        cnt += 1
    if cnt == 0:
        return 0.0
    return acc / cnt


def pick_widest(names):
    winner = None
    for cand in names:
        if winner is None or len(cand) > len(winner):
            winner = cand
    return winner


def floor_all(values, lo):
    for i in range(len(values)):
        if values[i] < lo:
            values[i] = lo


def tally(text, needle):
    n = 0
    i = 0
    while i < len(text):
        if text[i] == needle:
            n += 1
        i += 1
    return n
