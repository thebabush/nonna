// Sanity dataset — variants (Rust).
// running_average: renamed + decl-reordered mean        -> MUST match mean
// pick_widest:     renamed longest                      -> MUST match longest
// floor_all:       subset of clamp_all (lo half only)   -> high CONTAINMENT
// tally:           count_char as while+index            -> measured, not asserted

fn running_average(xs: &[f64]) -> f64 {
    let mut cnt = 0usize;
    let mut acc = 0.0;
    for item in xs {
        acc += item;
        cnt += 1;
    }
    if cnt == 0 {
        0.0
    } else {
        acc / cnt as f64
    }
}

fn pick_widest(names: &[String]) -> Option<&String> {
    let mut winner: Option<&String> = None;
    for cand in names {
        match winner {
            Some(w) if w.len() >= cand.len() => {}
            _ => winner = Some(cand),
        }
    }
    winner
}

fn floor_all(values: &mut Vec<i64>, lo: i64) {
    for v in values.iter_mut() {
        if *v < lo {
            *v = lo;
        }
    }
}

fn tally(text: &str, needle: char) -> usize {
    let chars: Vec<char> = text.chars().collect();
    let mut n = 0;
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == needle {
            n += 1;
        }
        i += 1;
    }
    n
}
