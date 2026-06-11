// Sanity dataset — originals + distractors (JavaScript).

function mean(values) {
  let total = 0.0;
  let count = 0;
  for (const v of values) {
    total += v;
    count += 1;
  }
  if (count === 0) {
    return 0.0;
  }
  return total / count;
}

function longest(words) {
  let best = null;
  for (const w of words) {
    if (best === null || w.length > best.length) {
      best = w;
    }
  }
  return best;
}

function countChar(text, needle) {
  let n = 0;
  for (const c of text) {
    if (c === needle) {
      n += 1;
    }
  }
  return n;
}

function clampAll(values, lo, hi) {
  for (let i = 0; i < values.length; i++) {
    if (values[i] < lo) {
      values[i] = lo;
    } else if (values[i] > hi) {
      values[i] = hi;
    }
  }
}

// distractor
function joinWith(parts, sep) {
  let out = "";
  let first = true;
  for (const p of parts) {
    if (!first) {
      out += sep;
    }
    out += p;
    first = false;
  }
  return out;
}

// distractor
function fib(n) {
  let a = 0;
  let b = 1;
  let i = 0;
  while (i < n) {
    const t = a + b;
    a = b;
    b = t;
    i += 1;
  }
  return a;
}
