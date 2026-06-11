// "Agent draft": a renamed, decl-reordered re-implementation of corpus mean().
fn avg(xs: &[f64]) -> f64 {
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

// Subset draft: does only the lower-bound half of corpus clamp_all().
// Should score high CONTAINMENT against clamp_all (it covers everything
// this does, plus more) even though jaccard is mediocre.
fn floor_all(values: &mut Vec<i64>, lo: i64) {
    for v in values.iter_mut() {
        if *v < lo {
            *v = lo;
        }
    }
}

// Unrelated control: should NOT match anything in the corpus strongly.
fn join_with(parts: &[String], sep: &str) -> String {
    let mut out = String::new();
    for (i, p) in parts.iter().enumerate() {
        if i > 0 {
            out.push_str(sep);
        }
        out.push_str(p);
    }
    out
}
