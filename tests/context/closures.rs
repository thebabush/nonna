// Call-graph context (D17), parent side: the same closure inside two
// dissimilar parents. Its context holds the parent's features, so with
// family.context > 0 the two closures stop being identical.

pub fn ids_csv(xs: &[u64], limit: usize) -> String {
    let parts: Vec<String> = xs.iter().take(limit).map(|e| e.to_string() + ",").collect();
    if parts.is_empty() {
        return String::from("none");
    }
    parts.concat()
}

pub fn scaled_labels(xs: &[u64], factor: u64) -> Vec<String> {
    let mut out = Vec::new();
    let mut total = 0;
    for x in xs {
        total += x * factor;
        if total > 1000 {
            break;
        }
    }
    out.push(total.to_string());
    out.extend(xs.iter().map(|e| e.to_string() + ","));
    out
}
