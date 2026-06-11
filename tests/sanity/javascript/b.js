// Sanity dataset — variants (JavaScript).

function runningAverage(xs) {
  let cnt = 0;
  let acc = 0.0;
  for (const item of xs) {
    acc += item;
    cnt += 1;
  }
  if (cnt === 0) {
    return 0.0;
  }
  return acc / cnt;
}

function pickWidest(names) {
  let winner = null;
  for (const cand of names) {
    if (winner === null || cand.length > winner.length) {
      winner = cand;
    }
  }
  return winner;
}

function floorAll(values, lo) {
  for (let i = 0; i < values.length; i++) {
    if (values[i] < lo) {
      values[i] = lo;
    }
  }
}

function tally(text, needle) {
  let n = 0;
  let i = 0;
  while (i < text.length) {
    if (text[i] === needle) {
      n += 1;
    }
    i += 1;
  }
  return n;
}
