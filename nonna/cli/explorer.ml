(* Duplication explorer: a self-contained web UI served by `nonna serve`.
   GET /            -> the page (embedded below; no external assets)
   GET /api/pairs   -> all dupe pairs above a floor threshold (cached once
                       indexing is done; the UI filters client-side)
   GET /api/fn      -> one function's source (only files that are indexed) *)

module J = Yojson.Safe
module Engine = Nonna_index.Engine

let pairs_floor = 0.3
let pairs_cap = 5000
let cached_pairs : J.t option ref = ref None

let meta_json (m : Engine.meta) : J.t =
  `Assoc
    [
      ("name", `String m.Engine.name);
      ("file", `String m.Engine.file);
      ("start", `Int m.Engine.line_start);
      ("end", `Int m.Engine.line_end);
      ("lines", `Int m.Engine.code_lines);
    ]

let pairs_json (eng : Engine.t) ~(ready : bool) : J.t =
  match (!cached_pairs, ready) with
  | Some j, true -> j
  | _, false ->
      (* the indexing thread is still mutating the engine: iterating its
         postings here races (Dynarray detects it and raises). Report
         progress only; the UI polls until ready. *)
      `Assoc
        [
          ("pairs", `List []);
          ("indexing", `Bool true);
          ("count", `Int (Engine.size eng));
        ]
  | None, true ->
      let ps = Engine.duplicates_full eng ~threshold:pairs_floor in
      let truncated = List.length ps > pairs_cap in
      let ps = List.filteri (fun i _ -> i < pairs_cap) ps in
      let j =
        `Assoc
          [
            ( "pairs",
              `List
                (List.map
                   (fun (p : Engine.pair) ->
                     `Assoc
                       [
                         ("a", meta_json p.Engine.a);
                         ("b", meta_json p.Engine.b);
                         ("af", `Int p.Engine.a_features);
                         ("bf", `Int p.Engine.b_features);
                         ("j", `Float p.Engine.j);
                         ("c", `Float p.Engine.c);
                       ])
                   ps) );
            ("truncated", `Bool truncated);
            ("floor", `Float pairs_floor);
            ("indexing", `Bool false);
            ("count", `Int (Engine.size eng));
          ]
      in
      cached_pairs := Some j;
      j

(* Serve source only for (file, start) pairs that exist in the index — the
   endpoint must not be a generic file reader. *)
let fn_json (eng : Engine.t) ~(file : string) ~(start : int) ~(stop : int) :
    J.t =
  let known = ref false in
  for fid = 0 to Engine.size eng - 1 do
    let m = Engine.get_meta eng fid in
    if m.Engine.file = file && m.Engine.line_start = start then known := true
  done;
  if not !known then `Assoc [ ("error", `String "unknown function") ]
  else
    `Assoc
      [ ("text", `String (String.concat "\n" (Units.file_slice file start stop))) ]

let html : string =
  {|<!doctype html>
<html><head><meta charset="utf-8"><title>nonna — duplication explorer</title>
<style>
:root { --bg:#1e1f22; --panel:#26282c; --fg:#d7d9dd; --dim:#8a8f98; --acc:#7aa2f7;
        --del:#46242a; --add:#1f3a28; --sel:#33415e; }
* { box-sizing:border-box; }
body { margin:0; background:var(--bg); color:var(--fg);
       font:13px/1.45 -apple-system, "Segoe UI", sans-serif; height:100vh;
       display:flex; flex-direction:column; }
header { display:flex; gap:14px; align-items:center; padding:8px 14px;
         background:var(--panel); border-bottom:1px solid #000; flex-wrap:wrap; }
header b { font-size:15px; }
header label { color:var(--dim); display:flex; gap:6px; align-items:center; }
input { background:var(--bg); color:var(--fg); border:1px solid #3a3d44;
        border-radius:4px; padding:4px 7px; }
input[type=range] { width:130px; }
#count { color:var(--dim); margin-left:auto; }
main { flex:1; display:flex; min-height:0; }
#list { width:430px; min-width:300px; overflow-y:auto; margin:0; padding:0;
        list-style:none; border-right:1px solid #000; background:var(--panel); }
#list li { padding:7px 12px; border-bottom:1px solid #1a1b1e; cursor:pointer; }
#list li:hover { background:#2d3036; }
#list li.sel { background:var(--sel); }
#list small { color:var(--dim); }
#list b { color:var(--acc); }
#diff { flex:1; display:flex; flex-direction:column; min-width:0; }
#dhead { padding:8px 14px; border-bottom:1px solid #000; color:var(--dim); }
#dhead b { color:var(--fg); }
#panes { flex:1; display:flex; overflow:auto; }
#panes pre { flex:1; margin:0; padding:10px 0; min-width:0; overflow-x:auto;
             font:12px/1.5 "SF Mono", Menlo, monospace; }
#panes pre:first-child { border-right:1px solid #000; }
.ln { padding:0 10px; white-space:pre; }
.del { background:var(--del); }
.add { background:var(--add); }
.gap { color:#3a3d44; background:#222327; }
#empty { color:var(--dim); padding:40px; text-align:center; }
</style></head><body>
<header>
  <b>👵🏻 nonna</b>
  <label>threshold <input id=th type=range min=30 max=100 value=70>
    <span id=thv>0.70</span></label>
  <input id=fname placeholder="function name…" size=14>
  <input id=ffile placeholder="file…" size=14>
  <label>min code lines <input id=fmin type=number value=0 min=0 style="width:60px"></label>
  <label>min features <input id=ffeat type=number value=0 min=0 style="width:60px"></label>
  <span id=count></span>
</header>
<main>
  <ul id=list></ul>
  <section id=diff>
    <div id=dhead>pick a pair — ↑/↓ to navigate</div>
    <div id=panes><pre id=lpane></pre><pre id=rpane></pre></div>
  </section>
</main>
<script>
const $ = id => document.getElementById(id);
let PAIRS = [], rows = [], sel = -1, srcCache = {};

async function load() {
  let d;
  try {
    $('count').textContent = PAIRS.length ? $('count').textContent : 'scanning…';
    d = await (await fetch('/api/pairs')).json();
  } catch (e) { setTimeout(load, 2000); return; }
  PAIRS = d.pairs || [];
  render();
  if (d.indexing) {
    $('count').textContent = `indexing… ${d.count || 0} functions so far`;
    setTimeout(load, 2000);
  }
}

function filt() {
  const t = +$('th').value / 100,
        fn = $('fname').value.toLowerCase(),
        ff = $('ffile').value.toLowerCase(),
        mn = +$('fmin').value || 0,
        mf = +$('ffeat').value || 0;
  return PAIRS.map((p, i) => ({p, i})).filter(({p}) =>
    Math.max(p.j, p.c) >= t
    && (!fn || p.a.name.toLowerCase().includes(fn) || p.b.name.toLowerCase().includes(fn))
    && (!ff || p.a.file.toLowerCase().includes(ff) || p.b.file.toLowerCase().includes(ff))
    && Math.min(p.a.lines, p.b.lines) >= mn
    && Math.min(p.af, p.bf) >= mf);
}

const short = f => f.split('/').slice(-2).join('/');
const esc = s => { const d = document.createElement('span'); d.textContent = s; return d.innerHTML; };

function render() {
  rows = filt();
  $('count').textContent = rows.length + ' pairs';
  const ul = $('list');
  ul.innerHTML = '';
  rows.forEach(({p, i}) => {
    const li = document.createElement('li');
    if (i === sel) li.className = 'sel';
    li.innerHTML = `<b>${p.j.toFixed(2)}</b> <span style="color:var(--dim)">j</span>
      · ${p.c.toFixed(2)} <span style="color:var(--dim)">c</span>
      &nbsp; ${esc(p.a.name)} ↔ ${esc(p.b.name)}<br>
      <small>${esc(short(p.a.file))}:${p.a.start} ↔ ${esc(short(p.b.file))}:${p.b.start}</small>`;
    li.onclick = () => select(i);
    ul.appendChild(li);
  });
}

async function srcOf(m) {
  const k = m.file + ':' + m.start;
  if (!(k in srcCache))
    srcCache[k] = (await (await fetch(
      `/api/fn?file=${encodeURIComponent(m.file)}&start=${m.start}&end=${m.end}`
    )).json()).text || '';
  return srcCache[k];
}

async function select(i) {
  sel = i;
  render();
  const li = document.querySelector('#list li.sel');
  if (li) li.scrollIntoView({block: 'nearest'});
  const p = PAIRS[i];
  $('dhead').innerHTML =
    `<b>${esc(p.a.name)}</b> ${esc(p.a.file)}:${p.a.start}-${p.a.end}
     &nbsp;↔&nbsp; <b>${esc(p.b.name)}</b> ${esc(p.b.file)}:${p.b.start}-${p.b.end}
     &nbsp;·&nbsp; jaccard ${p.j.toFixed(3)} · containment ${p.c.toFixed(3)}`;
  const [a, b] = await Promise.all([srcOf(p.a), srcOf(p.b)]);
  diff(a.split('\n'), b.split('\n'));
}

function diff(A, B) {
  // line LCS
  const n = A.length, m = B.length;
  const dp = Array.from({length: n + 1}, () => new Uint16Array(m + 1));
  for (let i = n - 1; i >= 0; i--)
    for (let j = m - 1; j >= 0; j--)
      dp[i][j] = A[i] === B[j] ? dp[i+1][j+1] + 1 : Math.max(dp[i+1][j], dp[i][j+1]);
  const L = [], R = [];
  let i = 0, j = 0;
  const push = (arr, txt, cls) => {
    const div = document.createElement('div');
    div.className = 'ln ' + cls;
    div.textContent = txt === '' && cls === 'gap' ? ' ' : (txt || ' ');
    arr.push(div);
  };
  while (i < n && j < m) {
    if (A[i] === B[j]) { push(L, A[i], ''); push(R, B[j], ''); i++; j++; }
    else if (dp[i+1][j] >= dp[i][j+1]) { push(L, A[i], 'del'); push(R, '', 'gap'); i++; }
    else { push(L, '', 'gap'); push(R, B[j], 'add'); j++; }
  }
  while (i < n) { push(L, A[i++], 'del'); push(R, '', 'gap'); }
  while (j < m) { push(L, '', 'gap'); push(R, B[j++], 'add'); }
  $('lpane').replaceChildren(...L);
  $('rpane').replaceChildren(...R);
}

window.addEventListener('keydown', e => {
  if (e.target.tagName === 'INPUT' && e.target.type !== 'range') return;
  const pos = rows.findIndex(r => r.i === sel);
  if (e.key === 'ArrowDown' || e.key === 'j') {
    e.preventDefault();
    if (pos < rows.length - 1) select(rows[pos + 1].i);
  } else if (e.key === 'ArrowUp' || e.key === 'k') {
    e.preventDefault();
    if (pos > 0) select(rows[pos - 1].i);
  }
});

$('th').oninput = () => { $('thv').textContent = (+$('th').value / 100).toFixed(2); render(); };
for (const id of ['fname', 'ffile', 'fmin', 'ffeat']) $(id).oninput = render;
load();
</script></body></html>
|}
