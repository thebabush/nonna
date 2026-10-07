// End-to-end regressions for live indexes, editor positions and dependency caches.
// Run after dune build nonna: node tests/regressions.js
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawn, execFileSync } = require('node:child_process');
const { pathToFileURL } = require('node:url');
const net = require('node:net');
const bin = path.resolve('_build/default/nonna/cli/main.exe');
const root = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), 'nonna-regressions-')));
const children = [];
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));
const code = name => `fn ${name}(xs: &[i32]) -> i32 {
    let mut total = 0;
    for x in xs {
        total += x;
    }
    total
}
`;
function workspace(name) {
  const dir = path.join(root, name);
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(path.join(dir, '.nonna'), '');
  return dir;
}
function client(args, lsp = false, env = process.env) {
  const proc = spawn(bin, args, { env });
  children.push(proc);
  let buffer = Buffer.alloc(0), nextId = 0, stderr = '';
  const messages = [];
  proc.stderr.on('data', chunk => { stderr += chunk; });
  proc.stdout.on('data', chunk => {
    buffer = Buffer.concat([buffer, chunk]);
    for (;;) {
      let body;
      if (lsp) {
        const end = buffer.indexOf('\r\n\r\n');
        if (end < 0) break;
        const length = Number(/Content-Length: (\d+)/i.exec(buffer.subarray(0, end).toString())[1]);
        if (buffer.length < end + 4 + length) break;
        body = buffer.subarray(end + 4, end + 4 + length);
        buffer = buffer.subarray(end + 4 + length);
      } else {
        const end = buffer.indexOf('\n');
        if (end < 0) break;
        body = buffer.subarray(0, end);
        buffer = buffer.subarray(end + 1);
      }
      messages.push(JSON.parse(body.toString()));
    }
  });
  async function wait(predicate, after = 0) {
    for (let i = 0; i < 1500; i++) {
      const found = messages.slice(after).find(predicate);
      if (found) return found;
      if (proc.exitCode !== null) throw new Error(`server exited: ${stderr}`);
      await delay(10);
    }
    throw new Error(`server timeout: ${stderr}\n${JSON.stringify(messages.slice(-5))}`);
  }
  function send(method, params, id) {
    const body = JSON.stringify({ jsonrpc: '2.0', method, params, ...(id === undefined ? {} : { id }) });
    proc.stdin.write(lsp ? `Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}` : `${body}\n`);
  }
  async function request(method, params) {
    const id = ++nextId;
    send(method, params, id);
    const response = await wait(m => m.id === id);
    assert.equal(response.error, undefined);
    return response.result;
  }
  return { proc, messages, wait, send, request,
    call: (name, args = {}) => request('tools/call', { name, arguments: args }) };
}
const text = result => result.content.map(c => c.text || '').join('\n');
async function ready(c) {
  for (let i = 0; i < 300; i++) {
    if (text(await c.call('status')).includes('indexing: done')) return;
    await delay(20);
  }
  throw new Error('indexing timeout');
}
async function mcpRefresh() {
  const dir = workspace('mcp');
  const file = path.join(dir, 'old.rs');
  fs.writeFileSync(file, code('old'));
  const c = client(['mcp', dir]);
  await ready(c);
  const query = () => c.call('find_similar', { code: code('draft'), threshold: 0.99 });
  assert.match(text(await query()), /## `old`/);
  fs.unlinkSync(file);
  fs.writeFileSync(path.join(dir, 'new.rs'), code('new'));
  const result = text(await query());
  assert.match(result, /## `new`/);
  assert.doesNotMatch(result, /## `old`/);
  // Same-size edits with the original mtime must also invalidate the snapshot.
  const newFile = path.join(dir, 'new.rs'), stat = fs.statSync(newFile);
  fs.writeFileSync(newFile, code('now'));
  fs.utimesSync(newFile, stat.atime, stat.mtime);
  assert.match(text(await query()), /## `now`/);
  c.proc.stdin.end();
  console.log('ok MCP detects additions, deletions and same-size edits');
}
async function lspPositions() {
  const dir = workspace('lsp');
  const a = path.join(dir, 'a.rs'), b = path.join(dir, 'b.rs');
  fs.writeFileSync(a, code('first'));
  fs.writeFileSync(b, code('second'));
  const au = pathToFileURL(a).href, bu = pathToFileURL(b).href;
  const c = client(['lsp'], true);
  const init = await c.request('initialize', { rootUri: pathToFileURL(dir).href, capabilities: {} });
  assert.equal(init.capabilities.textDocumentSync.change, 1);
  await c.wait(m => m.params?.message?.includes('indexed '));
  for (const [uri, name] of [[au, 'first'], [bu, 'second']]) {
    c.send('textDocument/didOpen', { textDocument: { uri, languageId: 'rust', version: 1, text: code(name) } });
  }
  const diag = (uri, version) => m => m.method === 'textDocument/publishDiagnostics' && m.params.uri === uri && m.params.version === version;
  await c.wait(diag(bu, 1));
  let start = c.messages.length;
  const moved = '\n\n\n\n' + code('first');
  c.send('textDocument/didChange', { textDocument: { uri: au, version: 2 }, contentChanges: [{ text: moved }] });
  const ad = (await c.wait(diag(au, 2), start)).params;
  assert.equal(ad.diagnostics[0].range.start.line, 4);
  const bd = (await c.wait(diag(bu, 1), start)).params;
  assert.equal(bd.diagnostics[0].relatedInformation.find(r => r.location.uri === au).location.range.start.line, 4);
  const fn = await c.request('nonna/functionText', { textDocument: { uri: au }, position: { line: 6, character: 0 } });
  assert.match(fn.text, /total \+= x/);
  // Stale document versions cannot restore old locations.
  c.send('textDocument/didChange', { textDocument: { uri: au, version: 1 }, contentChanges: [{ text: '' }] });
  const found = await c.request('nonna/findSimilar', { textDocument: { uri: au }, position: { line: 6, character: 0 } });
  assert.equal(found.query, 'first');
  // Reindex + save + close must preserve the saved position after the scan commits.
  // Keep the scan busy after a.rs is read so the edit reaches a live scan.
  const padding = Array.from({ length: 100 }, (_, i) => `fn pad_${i}(x:i32)->i32 { x ^ (x << 2) }`).join('\n');
  for (let i = 0; i < 40; i++) fs.writeFileSync(path.join(dir, `z${i}.rs`), padding);
  start = c.messages.length;
  await c.request('nonna/reindex', {});
  const saved = '\n\n' + moved;
  fs.writeFileSync(a, saved);
  c.send('textDocument/didChange', { textDocument: { uri: au, version: 3 }, contentChanges: [{ text: saved }] });
  c.send('textDocument/didSave', { textDocument: { uri: au } });
  c.send('textDocument/didClose', { textDocument: { uri: au } });
  await c.request('nonna/findSimilar', { textDocument: { uri: bu }, position: { line: 2, character: 0 } });
  assert.ok(!c.messages.slice(start).some(m => m.params?.message?.includes('indexed ')), 'edit must overlap the background scan');
  await c.wait(m => m.params?.message?.includes('indexed '), start);
  const latest = await c.request('nonna/findSimilar', { textDocument: { uri: bu }, position: { line: 2, character: 0 } });
  assert.equal(latest.hits.find(h => h.name === 'first').line_start, 7);
  const clear = c.messages.slice(start).filter(m => m.method === 'textDocument/publishDiagnostics' && m.params.uri === au).at(-1);
  assert.deepEqual(clear.params.diagnostics, []);
  // Deleting the remaining buffer clears its diagnostics immediately.
  start = c.messages.length;
  c.send('textDocument/didChange', { textDocument: { uri: bu, version: 2 }, contentChanges: [{ text: '' }] });
  assert.deepEqual((await c.wait(diag(bu, 2), start)).params.diagnostics, []);
  c.send('exit', {});
  console.log('ok LSP tracks unsaved positions, related locations, versions, reindex and close');
}
function topK() {
  const output = execFileSync(bin, ['query', 'tests/fixtures', '--', path.resolve('tests/fixtures/draft.rs'), '-t', '0.7', '-k', '1'], { encoding: 'utf8' });
  assert.match(output.split('── floor_all')[1].split('── join_with')[0], /clamp_all/);
  console.log('ok self-exclusion happens before top-k');
}
async function dependencyCaches() {
  const dir = workspace('cargo'), tools = workspace('tools');
  fs.writeFileSync(path.join(dir, 'Cargo.toml'), '[package]\nname="app"\nversion="0.1.0"\n');
  const metadata = path.join(root, 'metadata.json');
  fs.writeFileSync(path.join(tools, 'cargo'), '#!/bin/sh\ncat "$NONNA_TEST_METADATA"\n', { mode: 0o755 });
  fs.writeFileSync(path.join(tools, 'rustc'), '#!/bin/sh\nexit 1\n', { mode: 0o755 });
  const cache = path.join(root, 'cache');
  const env = { ...process.env, PATH: `${tools}:${process.env.PATH}`, XDG_CACHE_HOME: cache, NONNA_TEST_METADATA: metadata };
  const dep = workspace('dependency');
  const source = path.join(dep, 'lib.rs');
  function setMetadata(origin) {
    fs.writeFileSync(metadata, JSON.stringify({ workspace_members: ['app'], packages: [
      { id: 'app', name: 'app', version: '0.1.0', source: null, manifest_path: path.join(dir, 'Cargo.toml') },
      { id: 'dep', name: 'dep', version: '0.1.0', source: origin, manifest_path: path.join(dep, 'Cargo.toml') }
    ] }));
  }
  const query = c => c.call('find_similar', { code: code('draft'), threshold: 0.99 });
  for (const revision of ['one', 'two']) {
    setMetadata(`git+https://example.invalid/dep#${revision}`);
    fs.writeFileSync(source, code(revision));
    const c = client(['mcp', dir], false, env);
    await ready(c);
    assert.match(text(await query(c)), new RegExp('## `' + revision + '`'));
    c.proc.stdin.end();
  }
  const files = fs.readdirSync(path.join(cache, 'nonna/sigdb'));
  assert.equal(files.filter(f => f.endsWith('.bin')).length, 2);
  // source:null does not mean workspace membership. Local deps stay mutable.
  setMetadata(null);
  fs.writeFileSync(source, code('local'));
  const corpus = execFileSync(bin, ['corpus', dir], { env, encoding: 'utf8' });
  assert.match(corpus, /dep-0\.1\.0: 1 fns/);
  const c = client(['mcp', dir], false, env);
  await ready(c);
  assert.match(text(await query(c)), /## `local`/);
  fs.writeFileSync(source, code('updated'));
  assert.match(text(await query(c)), /## `updated`/);
  assert.equal(fs.readdirSync(path.join(cache, 'nonna/sigdb')).filter(f => f.endsWith('.bin')).length, 2);
  c.proc.stdin.end();
  console.log('ok cache separates Git revisions; external path dependencies refresh without caching');
}
async function httpRefresh() {
  const dir = workspace('http');
  fs.writeFileSync(path.join(dir, 'a.rs'), code('first'));
  fs.writeFileSync(path.join(dir, 'b.rs'), code('second'));
  const listener = net.createServer();
  await new Promise(resolve => listener.listen(0, '127.0.0.1', resolve));
  const port = listener.address().port;
  await new Promise(resolve => listener.close(resolve));
  const c = client(['serve', dir, '-p', String(port)]);
  const url = `http://127.0.0.1:${port}`;
  let result;
  for (let i = 0; i < 300; i++) {
    try { result = await (await fetch(url + '/api/pairs')).json(); if (!result.indexing) break; } catch {}
    await delay(20);
  }
  assert.ok(result.pairs.length > 0);
  fs.unlinkSync(path.join(dir, 'b.rs'));
  result = await (await fetch(url + '/api/pairs')).json();
  assert.deepEqual(result.pairs, []);
  c.proc.kill();
  console.log('ok HTTP explorer invalidates cached pairs after deletion');
}
(async () => {
  try {
    topK();
    await mcpRefresh();
    await lspPositions();
    await dependencyCaches();
    await httpRefresh();
  } finally {
    for (const p of children) p.kill();
    fs.rmSync(root, { recursive: true, force: true });
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
