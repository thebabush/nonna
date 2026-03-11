#!/usr/bin/env node
// Patches the wasm-pack generated nonna_wasm.js to remove bare "env" module
// imports (which browsers can't resolve) and replaces them with an inline
// allocator stub.  This must be run after every wasm-pack build.
//
// Usage: node wasm-demo/postprocess.mjs

import { readFileSync, writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const pkgFile = join(__dirname, 'pkg', 'nonna_wasm.js');

let src = readFileSync(pkgFile, 'utf8');

// ── 1. Remove all  `import * as importN from "env"` lines ──────────────────
src = src.replace(/^import \* as import\d+ from "env"\n/gm, '');

// ── 2. Replace every `"env": importN,` entry in __wbg_get_imports with a
//       single reference to our inline stub (we'll inject it next) ──────────
// The object literal has duplicate "env" keys; only the last one matters at
// runtime, but all of them cause ES module resolution errors.  Replace the
// whole block of "env": importN lines with a single "env": __wasm_env_stub.
src = src.replace(/(\s+"env": import\d+,?\n)+/g,
    '        "env": __wasm_env_stub,\n');

// ── 3. Inject the inline stub just before __wbg_get_imports ─────────────────
const STUB = `
// ---------------------------------------------------------------------------
// Inline libc stub for tree-sitter C imports.
// malloc/realloc/free allocate in a bump arena placed well above Rust's heap.
// ---------------------------------------------------------------------------
const __wasm_env_stub = (() => {
    const PAGE = 65536;
    let _wasm = null;   // set after instantiation via __wasm_env_stub.__setWasm
    let arenaBase = 0, arenaOff = 0;
    const sizes = new Map();

    function _growTo(end) {
        const cur = _wasm.memory.buffer.byteLength;
        if (end > cur) _wasm.memory.grow(Math.ceil((end - cur) / PAGE));
    }
    function _ensureArena() {
        if (arenaBase) return;
        // Poke Rust allocator so it initialises, then place our arena 32 MB above.
        const p = _wasm.__wbindgen_malloc(8, 1);
        _wasm.__wbindgen_free(p, 8, 1);
        arenaBase = arenaOff = _wasm.memory.buffer.byteLength + 32 * 1024 * 1024;
        _growTo(arenaBase + PAGE);
    }
    function malloc(sz) {
        if (!_wasm) throw new Error('env.malloc called before WASM init');
        if (sz <= 0) sz = 1;
        _ensureArena();
        arenaOff = (arenaOff + 7) & ~7;
        const p = arenaOff; arenaOff += sz;
        _growTo(arenaOff + 64);
        sizes.set(p, sz);
        return p;
    }
    function realloc(p, newSz) {
        if (!p) return malloc(newSz);
        if (newSz <= 0) { sizes.delete(p); return 0; }
        const old = sizes.get(p) || 0;
        if (newSz <= old) { sizes.set(p, newSz); return p; }
        const np = malloc(newSz);
        if (old > 0) new Uint8Array(_wasm.memory.buffer).copyWithin(np, p, p + Math.min(old, newSz));
        sizes.delete(p);
        return np;
    }
    function free(p) { if (p) sizes.delete(p); }
    function calloc(n, sz) {
        const p = malloc(n * sz);
        new Uint8Array(_wasm.memory.buffer).fill(0, p, p + n * sz);
        return p;
    }
    function strncmp(a, b, n) {
        if (!_wasm || !a || !b) return 0;
        const m = new Uint8Array(_wasm.memory.buffer);
        for (let i = 0; i < n; i++) {
            const d = (m[a+i]||0) - (m[b+i]||0);
            if (d !== 0) return d;
            if (!m[a+i]) break;
        }
        return 0;
    }
    const stub = {
        malloc, realloc, free, calloc, strncmp,
        fprintf: ()=>0, fwrite:(_,_2,c)=>c, fputc:c=>c, fclose:()=>0,
        snprintf:()=>0, vsnprintf:()=>0, clock_gettime:()=>0,
        abort() { throw new Error('wasm tree-sitter abort'); },
        __assert_fail(_c,_f,line) { throw new Error('wasm assert line '+line); },
        __setWasm(inst) { _wasm = inst; },
    };
    return stub;
})();

`;

src = src.replace('function __wbg_get_imports()', STUB + 'function __wbg_get_imports()');

// ── 4. Inject __setWasm call right after the WASM instance is finalised ─────
// __wbg_finalize_init receives the instance and assigns `wasm`; patch it.
src = src.replace(
    /function __wbg_finalize_init\(instance, module\) \{/,
    'function __wbg_finalize_init(instance, module) {\n    __wasm_env_stub.__setWasm(instance.exports);'
);

writeFileSync(pkgFile, src, 'utf8');
console.log('postprocess: patched', pkgFile);
