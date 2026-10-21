// Clone classes (D18): a closure inside a class member folds into it.
// `outer` is mostly its closure `f`, and both match `plain`, so the class
// is {outer, f, plain}; f's lines are outer's, so neither the class's
// duplicated SLOC nor the scanned total counts them twice.

pub fn outer(xs: &[i64]) -> i64 {
    let f = |v: &[i64]| -> i64 {
        let mut acc = 0;
        for x in v {
            if *x > 10 {
                acc += x * 2;
            } else {
                acc -= x;
            }
        }
        acc
    };
    f(xs)
}

pub fn plain(v: &[i64]) -> i64 {
    let mut acc = 0;
    for x in v {
        if *x > 10 {
            acc += x * 2;
        } else {
            acc -= x;
        }
    }
    acc
}
