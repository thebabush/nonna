// Call-graph context (D17): thin wrappers whose bodies differ only in the
// function they call. With family.context > 0, wrappers over dissimilar
// callees drift apart; wrappers over similar callees stay together.

pub struct Item {
    pub data: Vec<u64>,
    pub seed: u64,
}

// dissimilar callees

pub fn checksum(data: &[u64], seed: u64) -> u64 {
    let mut acc = seed;
    for b in data {
        acc = acc.wrapping_mul(31).wrapping_add(*b);
        acc ^= acc >> 7;
    }
    acc
}

pub fn label_width(data: &[u64], seed: u64) -> u64 {
    let text = format!("{}:{}", data.len(), seed);
    if text.starts_with('0') {
        return 0;
    }
    match text.find(':') {
        Some(i) => (i as u64) * 2,
        None => text.len() as u64,
    }
}

// similar callees (renamed copies)

pub fn total_weight(data: &[u64], seed: u64) -> u64 {
    let mut total = 0;
    for w in data.iter() {
        if *w > seed {
            total += w * w;
        }
    }
    total
}

pub fn total_cost(items: &[u64], floor: u64) -> u64 {
    let mut sum = 0;
    for c in items.iter() {
        if *c > floor {
            sum += c * c;
        }
    }
    sum
}

// the wrappers: one body, four callees

pub fn wrap_checksum(item: &Item) -> u64 {
    checksum(&item.data, item.seed + 1)
}

pub fn wrap_label(item: &Item) -> u64 {
    label_width(&item.data, item.seed + 1)
}

pub fn wrap_weight(item: &Item) -> u64 {
    total_weight(&item.data, item.seed + 1)
}

pub fn wrap_cost(item: &Item) -> u64 {
    total_cost(&item.data, item.seed + 1)
}
