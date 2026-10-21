// Macro-argument reparse fixture (tests/run.sh). Opengrep keeps a macro's
// token tree opaque unless every argument is a single token or path; nonna
// reparses `()`/`[]` token trees as expressions.

fn token_cases() {
    assert_eq!(linux_file_token("Ubuntu 24.04", None), "ubuntu24.04");
    assert_eq!(linux_file_token("Amazon Linux 2", None), "amazonlinux2");
    assert!(is_ok(&linux_file_token("x", Some("y"))), "token={}", 1);
}

// Same body as token_cases up to renaming: must still match.
fn token_cases_again() {
    assert_eq!(linux_file_token("Ubuntu 24.04", None), "ubuntu24.04");
    assert_eq!(linux_file_token("Amazon Linux 2", None), "amazonlinux2");
    assert!(is_ok(&linux_file_token("x", Some("y"))), "token={}", 1);
}

// Same assertion shape, different callee and literals: must not collapse
// onto token_cases.
fn path_cases() {
    assert_eq!(codeview_path(b"ntdll.pdb\0\0\0\0"), "ntdll.pdb");
    assert_eq!(codeview_path(b""), "");
    assert!(is_empty(&codeview_path(b"\0junk")), "path={}", 2);
}

// Token trees that are not expressions keep their bag (and must not crash).
fn unparseable(v: u32) {
    tracing::info!(target: "nonna", count = v, "seen {}", v);
    let j = json!({ "a": v });
    let w = vec![f(v); 3];
    log(j, w);
}
