#!/usr/bin/env python3
# ============================================================================
# relabel_exhibits.py — rename exhibit FILES to match their rendered order of
# appearance in paper/main.tex (the §59.66 nomenclature: tab_NN_slug /
# fig_NN[panel]_slug, appendix prefixes a/b). Built 2026-08-10 (§59.79) during
# MM's restructure; safe to re-run after ANY reordering.
#
# What it does:  simulate LaTeX counters over main.tex (tables = \input{tables/..};
# figures = figure envs, counted once each; \appendix switches to a/b prefixes;
# a file input twice keeps its FIRST-appearance number), derive old->new stems
# preserving slug and panel letters, then TWO-PHASE rename (via temp stems, so
# swaps/permutations are safe) across: output/tables, output/figures,
# paper/figures, paper/main.tex, and every code/R/*.R generator.
#
# Usage:  python3 code/relabel_exhibits.py          (dry run: prints the map)
#         python3 code/relabel_exhibits.py --apply  (performs the renames)
# Run from the repo root. ALWAYS recompile + check undef refs afterwards.
# ============================================================================
import io, os, re, glob, sys

APPLY = "--apply" in sys.argv
t = io.open('paper/main.tex', encoding='utf-8').read()
app = t.find('\\appendix')
events = []
# tables enter EITHER via \input{tables/..} OR the \appfloat{tables/..} macro
# (\appfloat = \input + \clearpage); both must be counted, in document order.
for m in re.finditer(r'\\(?:input|appfloat)\{tables/([^}]+)\}', t):
    events.append((m.start(), 'tab', (m.group(1),)))
for m in re.finditer(r'\\begin\{figure\}.*?\\end\{figure\}', t, re.S):
    files = tuple(re.findall(r'\\includegraphics\[[^\]]*\]\{([^}]+)\}', m.group(0)))
    if files: events.append((m.start(), 'fig', files))
events.sort()

STEM = re.compile(r'^(tab|fig)_(a|b)?(\d{2})([a-z]?)_(.+?)(\.(png|pdf|tex|jpg))?$')
tn = fn = atn = afn = 0
seen = set(); mapping = []
for pos, kind, files in events:
    inapp = pos > app
    if kind == 'tab':
        num = f"a{atn+1:02d}" if inapp else f"{tn+1:02d}"
        atn, tn = atn + inapp, tn + (not inapp)
    else:
        num = f"b{afn+1:02d}" if inapp else f"{fn+1:02d}"
        afn, fn = afn + inapp, fn + (not inapp)
    if files in seen: continue          # duplicate render: keep first-appearance name
    seen.add(files)
    for f in files:
        m = STEM.match(os.path.basename(f))
        if not m:
            print(f"  !! cannot parse stem, skipped: {f}"); continue
        kind2, _, _, panel, slug = m.group(1), m.group(2), m.group(3), m.group(4), m.group(5)
        old = f"{m.group(1)}_{(m.group(2) or '')}{m.group(3)}{panel}_{slug}"
        new = f"{kind2}_{num}{panel}_{slug}"
        if old != new: mapping.append((old, new))

print(f"counts: {tn} main tables, {fn} main figures, {atn} appendix tables, {afn} appendix figures")
if not mapping:
    print("all first-appearance files already match their rendered numbers — nothing to do")
    sys.exit(0)
print("rename map:")
for o, n in mapping: print(f"  {o}  ->  {n}")
if not APPLY:
    print("\n(dry run — pass --apply to perform)"); sys.exit(0)

targets = glob.glob('code/R/*.R') + ['paper/main.tex']
DIRS = ('output/tables', 'output/figures', 'paper/figures')
EXTS = ('.tex', '.png', '.pdf', '.jpg')
for k, (o, n) in enumerate(mapping):          # phase 1: old -> temp
    tmp = f"XRLBX{k:03d}X"
    for fp in targets:
        s = io.open(fp, encoding='utf-8').read()
        if o in s: io.open(fp, 'w', encoding='utf-8').write(s.replace(o, tmp))
    for d in DIRS:
        for e in EXTS:
            p = f"{d}/{o}{e}"
            if os.path.exists(p): os.rename(p, f"{d}/{tmp}{e}")
for k, (o, n) in enumerate(mapping):          # phase 2: temp -> new
    tmp = f"XRLBX{k:03d}X"
    for fp in targets:
        s = io.open(fp, encoding='utf-8').read()
        if tmp in s: io.open(fp, 'w', encoding='utf-8').write(s.replace(tmp, n))
    for d in DIRS:
        for e in EXTS:
            p = f"{d}/{tmp}{e}"
            if os.path.exists(p): os.rename(p, f"{d}/{n}{e}")
print(f"applied {len(mapping)} renames. NOW: recompile main.tex twice and check undefined refs.")
