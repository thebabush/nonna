#!/usr/bin/env bash
# Python benchmark: download sdists of popular pure-Python packages at two
# versions each (old/new — same multi-version trick as the cargo registry),
# extract, mine ground-truth pairs, and evaluate:
#   bash tests/mine-pypi.sh [outdir]
set -eu
BIN=_build/default/nonna/cli/main.exe
OUT=${1:-bench-py}
mkdir -p "$OUT/sdists" "$OUT/src"

# package==old ==new ; pure-Python only (no native-code-heavy packages)
PKGS="
requests==2.25.1 requests==2.32.3
urllib3==1.26.18 urllib3==2.2.3
flask==1.1.4 flask==3.0.3
click==7.1.2 click==8.1.7
jinja2==2.11.3 jinja2==3.1.4
werkzeug==1.0.1 werkzeug==3.0.4
attrs==21.4.0 attrs==24.2.0
rich==10.16.2 rich==13.9.4
pygments==2.10.0 pygments==2.18.0
python-dateutil==2.8.2 python-dateutil==2.9.0.post0
packaging==21.3 packaging==24.1
itsdangerous==1.1.0 itsdangerous==2.2.0
charset-normalizer==2.0.12 charset-normalizer==3.4.0
beautifulsoup4==4.9.3 beautifulsoup4==4.12.3
"

# fetch sdist tarballs straight from the PyPI JSON API (no pip: old pips
# try to BUILD sdists for metadata and choke on modern build backends)
for spec in $PKGS; do
  pkg=${spec%%==*}; ver=${spec##*==}
  url=$(curl -s "https://pypi.org/pypi/$pkg/$ver/json" | python3 -c \
    'import json,sys; d=json.load(sys.stdin); print(next(u["url"] for u in d["urls"] if u["packagetype"]=="sdist"))')
  echo "  $pkg $ver"
  curl -sL "$url" -o "$OUT/sdists/$pkg-$ver.tar.gz"
done

for t in "$OUT"/sdists/*.tar.gz; do
  tar xzf "$t" -C "$OUT/src"
done
ls "$OUT/src" > "$OUT/pkgs.txt"
ls -d "$OUT"/src/*/ > "$OUT/paths.txt"
echo "extracted $(wc -l < "$OUT/pkgs.txt" | tr -d ' ') package versions"

echo "mining..."
$BIN mine $(tr '\n' ' ' < "$OUT/paths.txt") -o "$OUT/pairs.tsv"
$BIN eval "$OUT/pairs.tsv" -o "$OUT/scores.tsv" | tee "$OUT/report.txt" | head -12
