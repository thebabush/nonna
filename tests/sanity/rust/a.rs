// Sanity dataset — originals + distractors (Rust).
// Groups: mean, longest, count_char, clamp_all. Distractors: join_with, fib.

pub fn mean(values: &[f64]) -> f64 {
    let mut total = 0.0;
    let mut count = 0usize;
    for v in values {
        total += v;
        count += 1;
    }
    if count == 0 {
        0.0
    } else {
        total / count as f64
    }
}

pub fn longest(words: &[String]) -> Option<&String> {
    let mut best: Option<&String> = None;
    for w in words {
        match best {
            Some(b) if b.len() >= w.len() => {}
            _ => best = Some(w),
        }
    }
    best
}

pub fn count_char(text: &str, needle: char) -> usize {
    let mut n = 0;
    for c in text.chars() {
        if c == needle {
            n += 1;
        }
    }
    n
}

pub fn clamp_all(values: &mut Vec<i64>, lo: i64, hi: i64) {
    for v in values.iter_mut() {
        if *v < lo {
            *v = lo;
        } else if *v > hi {
            *v = hi;
        }
    }
}

// distractor: string building, not numeric accumulation
pub fn join_with(parts: &[String], sep: &str) -> String {
    let mut out = String::new();
    for (i, p) in parts.iter().enumerate() {
        if i > 0 {
            out.push_str(sep);
        }
        out.push_str(p);
    }
    out
}

// distractor: arithmetic loop but different dataflow shape
pub fn fib(n: u64) -> u64 {
    let mut a = 0u64;
    let mut b = 1u64;
    let mut i = 0u64;
    while i < n {
        let t = a + b;
        a = b;
        b = t;
        i += 1;
    }
    a
}
