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

pub fn parse_port(s: &str) -> Option<u16> {
    let n: u32 = s.trim().parse().ok()?;
    if n > 0 && n < 65536 {
        Some(n as u16)
    } else {
        None
    }
}

pub fn max_by_len(words: &[String]) -> Option<&String> {
    let mut best: Option<&String> = None;
    for w in words {
        match best {
            Some(b) if b.len() >= w.len() => {}
            _ => best = Some(w),
        }
    }
    best
}

pub fn count_lines(text: &str) -> usize {
    let mut n = 0;
    for c in text.chars() {
        if c == '\n' {
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
