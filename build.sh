#!/usr/bin/env bash
# Build the nonna WASM demo.
#
# Prerequisites:
#   cargo install wasm-pack
#   rustup target add wasm32-unknown-unknown
#   brew install wasi-libc        # provides wasm32-wasi headers for tree-sitter C code
#   (optional) cargo install wasm-opt
#
# Why the CFLAGS are needed:
#   tree-sitter and tree-sitter-python compile C code when targeting
#   wasm32-unknown-unknown.  The C compiler needs WASI headers (stdio.h etc.)
#   and the __wasi__ define to suppress the dup() call in tree-sitter's
#   tree.c (see tree-sitter/lib/src/tree.c, the #elif !defined(__wasi__) guard).
#   RUSTC_WRAPPER="" disables sccache (if installed) which would otherwise
#   strip the custom sysroot flags.
#
# Required env vars (set automatically below):
#   RUSTC_WRAPPER=""
#   CFLAGS_wasm32_unknown_unknown="-I<wasi-sysroot>/include/wasm32-wasi -D__wasi__"
#
# Usage:
#   ./wasm-demo/build.sh                        # build WASM only
#   ./wasm-demo/build.sh --corpus               # also rebuild corpus.bin from bench/pyenv
#   ./wasm-demo/build.sh --corpus --serve       # build everything + serve locally

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEMO_DIR="$SCRIPT_DIR"

BUILD_CORPUS=0
SERVE=0

for arg in "$@"; do
  case $arg in
    --corpus) BUILD_CORPUS=1 ;;
    --serve)  SERVE=1 ;;
  esac
done

# ─── Locate wasi-libc sysroot ────────────────────────────────────────────────
# Try Homebrew location first, then a few common fallbacks.
WASI_SYSROOT=""
for candidate in \
    "$(brew --prefix wasi-libc 2>/dev/null)/share/wasi-sysroot" \
    "/opt/homebrew/share/wasi-sysroot" \
    "/usr/local/share/wasi-sysroot"; do
  if [ -f "$candidate/include/wasm32-wasi/stdio.h" ]; then
    WASI_SYSROOT="$candidate"
    break
  fi
done

if [ -z "$WASI_SYSROOT" ]; then
  echo "ERROR: wasi-libc sysroot not found." >&2
  echo "  Install with: brew install wasi-libc" >&2
  echo "  Or set WASI_SYSROOT manually and re-run." >&2
  exit 1
fi

# ─── 1. Build WASM ──────────────────────────────────────────────────────────
echo "Building nonna-wasm with wasm-pack..."
echo "  WASI sysroot: $WASI_SYSROOT"
cd "$REPO_ROOT"
RUSTC_WRAPPER="" \
CFLAGS_wasm32_unknown_unknown="-I${WASI_SYSROOT}/include/wasm32-wasi -D__wasi__" \
wasm-pack build crates/nonna-wasm \
  --target web \
  --out-dir "$DEMO_DIR/pkg" \
  --release 2>&1

# Patch the generated JS: replace bare "env" module imports with an inline
# allocator stub (browsers can't resolve a bare "env" specifier).
node "$DEMO_DIR/postprocess.mjs"

echo "WASM build complete: $(du -sh "$DEMO_DIR/pkg/nonna_wasm_bg.wasm" | cut -f1) wasm"

# ─── 2. Build corpus (optional) ─────────────────────────────────────────────
if [ $BUILD_CORPUS -eq 1 ]; then
  SITE_PACKAGES=$(ls -d "$REPO_ROOT"/bench/pyenv/.venv/lib/python*/site-packages 2>/dev/null | head -1)
  if [ -z "$SITE_PACKAGES" ]; then
    echo "ERROR: bench/pyenv/.venv not found. Run 'uv sync' in bench/pyenv first." >&2
    exit 1
  fi
  echo "Indexing corpus from $SITE_PACKAGES..."
  cargo run -p nonna-corpus --release --bin build-corpus -- \
    "$SITE_PACKAGES" \
    --output "$DEMO_DIR/corpus.bin" \
    --max-functions 5000 \
    --threshold 0.3
  echo "Corpus: $(du -sh "$DEMO_DIR/corpus.bin" | cut -f1)"
fi

# ─── 3. Check corpus exists ─────────────────────────────────────────────────
if [ ! -f "$DEMO_DIR/corpus.bin" ]; then
  echo ""
  echo "WARNING: corpus.bin not found in wasm-demo/."
  echo "Run with --corpus flag to build it, or copy an existing one."
fi

# ─── 4. Serve (optional) ────────────────────────────────────────────────────
if [ $SERVE -eq 1 ]; then
  echo ""
  echo "Serving at http://localhost:8080"
  echo "(Ctrl+C to stop)"
  cd "$DEMO_DIR"
  # Try python3 first, fall back to npx serve
  if command -v python3 &>/dev/null; then
    python3 -m http.server 8080
  elif command -v npx &>/dev/null; then
    npx serve -l 8080 .
  else
    echo "No HTTP server found. Install python3 or npx." >&2
    exit 1
  fi
fi
