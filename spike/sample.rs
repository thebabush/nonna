fn sum_positive(items: &[i32]) -> i32 {
    let mut total = 0;
    for &x in items {
        if x > 0 {
            total += x;
        }
    }
    total
}

fn classify(n: i32) -> &'static str {
    match n {
        0 => "zero",
        n if n < 0 => "neg",
        _ => "pos",
    }
}

fn first_even(items: &[i32]) -> Option<i32> {
    let found = items.iter().find(|&&x| x % 2 == 0)?;
    Some(*found)
}

fn shout(name: &str) -> String {
    format!("HELLO {}", name.to_uppercase())
}
