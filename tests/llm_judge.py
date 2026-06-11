#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# ///
"""LLM judge for code-pair similarity — the 'RPG stat sheet' eval.

Samples pairs from the mined benchmark, shows both functions to Claude
(headless `claude -p`), and collects per-axis 0-1 scores:

    purpose    same task/goal?
    algorithm  same algorithmic approach?
    structure  same control-flow shape?
    dataflow   same value flow through operations?
    api        same external calls/types?
    naming     identifier overlap (engine should be INVARIANT to this —
               a control axis, not a target)
    overall    1 = same up to variable names / 0 = different purpose,
               behavior and structure

Joined with engine scores, this answers: which axis does our jaccard
actually track, and what do the disagreement pairs look like?

Usage:
    uv run tests/llm_judge.py --pairs bench-out/pairs.tsv \
        --scores bench-out/scores.tsv [--sample 48] [--model haiku] \
        [-o bench-out/judged.tsv]
"""

import argparse
import json
import random
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, fields
from pathlib import Path

AXES = ['purpose', 'algorithm', 'structure', 'dataflow', 'api', 'naming', 'overall']

RUBRIC = """You are scoring the similarity of two code functions for a \
code-clone-detection benchmark. Output ONLY a JSON object, no prose, no fences.

Score each axis from 0.0 to 1.0:
- "purpose": do they accomplish the same task? (1 = same goal, 0 = unrelated)
- "algorithm": same algorithmic approach/steps, even if expressed differently?
- "structure": same control-flow shape (loops, branches, early returns)?
- "dataflow": do values flow through the same operations in the same way?
- "api": same external functions/methods/types used?
- "naming": how much do identifier names overlap? (variables, not keywords)
- "overall": holistic; anchors: 1.0 = identical up to variable names and \
formatting; 0.7-0.9 = same algorithm, minor edits; 0.4-0.6 = same purpose, \
substantially different implementation; 0.1-0.3 = superficial resemblance \
only; 0.0 = different purpose, behavior and structure.
Also include "note": one short sentence on the main difference.

JSON keys: purpose, algorithm, structure, dataflow, api, naming, overall, note.
"""


@dataclass
class Pair:
    label: str
    kind: str
    fa: str
    la: int
    lae: int
    na: str
    fb: str
    lb: int
    lbe: int
    nb: str
    jaccard: float = -1.0
    containment: float = -1.0


def read_pairs(pairs_path: Path, scores_path: Path | None) -> list[Pair]:
    scores: dict[tuple[str, int, str, int], tuple[float, float]] = {}
    if scores_path and scores_path.exists():
        for line in scores_path.read_text().splitlines():
            fa, la, fb, lb, j, c = line.split('\t')
            scores[(fa, int(la), fb, int(lb))] = (float(j), float(c))
    pairs = []
    for line in pairs_path.read_text().splitlines():
        cols = line.split('\t')
        if len(cols) != 10:
            continue
        p = Pair(
            label=cols[0], kind=cols[1],
            fa=cols[2], la=int(cols[3]), lae=int(cols[4]), na=cols[5],
            fb=cols[6], lb=int(cols[7]), lbe=int(cols[8]), nb=cols[9],
        )
        if (key := (p.fa, p.la, p.fb, p.lb)) in scores:
            p.jaccard, p.containment = scores[key]
        pairs.append(p)
    return pairs


def stratified_sample(pairs: list[Pair], n_total: int) -> list[Pair]:
    """Half per kind sampled randomly, half 'most surprising' for the engine
    (positives with the lowest jaccard, negatives with the highest)."""
    rng = random.Random(42)
    by_kind: dict[str, list[Pair]] = {}
    for p in pairs:
        if p.jaccard >= 0:
            by_kind.setdefault(p.kind, []).append(p)
    quota = {
        'exact': 4, 'renamed': 10, 'evolved': 10, 'evolved_major': 8,
        'samefile': 10, 'random': 6,
    }
    scale = n_total / sum(quota.values())
    picked: list[Pair] = []
    for kind, group in by_kind.items():
        k = max(2, round(quota.get(kind, 4) * scale))
        k = min(k, len(group))
        surprising = sorted(
            group,
            key=lambda p: p.jaccard if p.label == 'pos' else -p.jaccard,
        )[: k // 2]
        rest = [p for p in group if p not in surprising]
        picked += surprising + rng.sample(rest, min(k - len(surprising), len(rest)))
    return picked


def snippet(path: str, start: int, end: int, max_lines: int = 100) -> str:
    lines = Path(path).read_text(errors='replace').splitlines()
    body = lines[max(0, start - 1) : end]
    if len(body) > max_lines:
        body = body[:max_lines] + ['/* ... truncated ... */']
    return '\n'.join(body)


def judge_one(p: Pair, model: str) -> dict | None:
    prompt = (
        f'{RUBRIC}\n=== FUNCTION A ({p.na}) ===\n{snippet(p.fa, p.la, p.lae)}\n'
        f'\n=== FUNCTION B ({p.nb}) ===\n{snippet(p.fb, p.lb, p.lbe)}\n'
    )
    try:
        out = subprocess.run(
            ['claude', '-p', prompt, '--output-format', 'json', '--model', model],
            capture_output=True, text=True, timeout=180, stdin=subprocess.DEVNULL,
        )
        result = json.loads(out.stdout)['result']
        # strip fences / surrounding prose, grab the outermost JSON object
        m = re.search(r'\{.*\}', result, re.DOTALL)
        if not m:
            return None
        d = json.loads(m.group(0))
        return d if all(a in d for a in AXES) else None
    except Exception as e:
        print(f'  judge failed for {p.na}<->{p.nb}: {e}', file=sys.stderr)
        return None


def spearman(xs: list[float], ys: list[float]) -> float:
    def ranks(v: list[float]) -> list[float]:
        order = sorted(range(len(v)), key=lambda i: v[i])
        r = [0.0] * len(v)
        i = 0
        while i < len(order):
            j = i
            while j + 1 < len(order) and v[order[j + 1]] == v[order[i]]:
                j += 1
            avg = (i + j) / 2 + 1
            for k in range(i, j + 1):
                r[order[k]] = avg
            i = j + 1
        return r

    rx, ry = ranks(xs), ranks(ys)
    n = len(xs)
    mx, my = sum(rx) / n, sum(ry) / n
    cov = sum((a - mx) * (b - my) for a, b in zip(rx, ry))
    vx = sum((a - mx) ** 2 for a in rx) ** 0.5
    vy = sum((b - my) ** 2 for b in ry) ** 0.5
    return cov / (vx * vy) if vx and vy else float('nan')


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument('--pairs', type=Path, required=True)
    ap.add_argument('--scores', type=Path, default=None)
    ap.add_argument('--sample', type=int, default=48)
    ap.add_argument('--model', default='haiku')
    ap.add_argument('--jobs', type=int, default=4)
    ap.add_argument('-o', '--out', type=Path, default=Path('bench-out/judged.tsv'))
    args = ap.parse_args()

    pairs = read_pairs(args.pairs, args.scores)
    sample = stratified_sample(pairs, args.sample)
    print(f'judging {len(sample)} pairs with {args.model} ({args.jobs} parallel)...')

    with ThreadPoolExecutor(max_workers=args.jobs) as ex:
        verdicts = list(ex.map(lambda p: judge_one(p, args.model), sample))

    rows = [(p, v) for p, v in zip(sample, verdicts) if v is not None]
    print(f'{len(rows)}/{len(sample)} judged successfully\n')

    args.out.parent.mkdir(parents=True, exist_ok=True)
    with args.out.open('w') as f:
        f.write('label\tkind\tjaccard\tcontainment\t' + '\t'.join(AXES)
                + '\tnameA\tnameB\tlocA\tlocB\tnote\n')
        for p, v in rows:
            f.write(
                f'{p.label}\t{p.kind}\t{p.jaccard:.3f}\t{p.containment:.3f}\t'
                + '\t'.join(f'{float(v[a]):.2f}' for a in AXES)
                + f'\t{p.na}\t{p.nb}\t{p.fa}:{p.la}\t{p.fb}:{p.lb}\t'
                + str(v.get('note', ''))[:160].replace('\t', ' ') + '\n'
            )
    print(f'judged pairs -> {args.out}\n')

    # which axis does engine jaccard track?
    js = [p.jaccard for p, _ in rows]
    print('spearman(engine jaccard, axis):')
    for a in AXES:
        vs = [float(v[a]) for _, v in rows]
        print(f'  {a:10s} {spearman(js, vs):+.3f}')

    # disagreements: engine low but judge says same algorithm (missed reuse),
    # and engine high but judge says different purpose (false positive risk)
    print('\nengine LOW (<0.4) but judge algorithm >= 0.7 (missed similarity):')
    for p, v in rows:
        if p.jaccard < 0.4 and float(v['algorithm']) >= 0.7:
            print(f'  j={p.jaccard:.2f} alg={v["algorithm"]} [{p.kind}] '
                  f'{p.na}<->{p.nb}: {str(v.get("note", ""))[:100]}')
    print('\nengine HIGH (>=0.5) but judge purpose < 0.4 (false-positive risk):')
    for p, v in rows:
        if p.jaccard >= 0.5 and float(v['purpose']) < 0.4:
            print(f'  j={p.jaccard:.2f} pur={v["purpose"]} [{p.kind}] '
                  f'{p.na}<->{p.nb}: {str(v.get("note", ""))[:100]}')


if __name__ == '__main__':
    main()
