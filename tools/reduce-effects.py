#!/usr/bin/env python3
"""reduce-effects.py — close the loop from census to a reviewable DATA diff.

Usage:
  python3 tools/reduce-effects.py <probe-dir> <census.tsv>

Two stages:
  1. bare-call-lexicon: bare names -> domain (classify-effects.sh), emitting
     pure-names and bare-name-effect additions.
  2. receiver-type levers (return-type-projection, unbound-binding-resolution,
     self-attr-typing, call-result, attribute-alias-typing): unresolved method
     calls -> (module, domain) via classify-receivers.sh, emitting
     effect-rule lines for the module-tag DATA.

The key is NOT touched here; both classifiers read DEEPSEEK_API_KEY/CLASSIFY_KEY
from the environment.
"""
import json, os, subprocess, sys
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
CLASSIFY = os.path.join(HERE, "classify-effects.sh")
CLASSIFY_RECV = os.path.join(HERE, "classify-receivers.sh")
CLASSIFY_FN = os.path.join(HERE, "classify-functions.sh")
RECV_LEVERS = {
    "return-type-projection", "unbound-binding-resolution", "self-attr-typing",
    "call-result", "attribute-alias-typing", "parameter-dataflow",
}


def read_tsv(tsv):
    import csv
    rows = []
    with open(tsv, encoding="utf-8") as f:
        for r in csv.DictReader(f, delimiter="\t"):
            rows.append(r)
    return rows


def run_classifier(script, items):
    res = subprocess.run([script], input=json.dumps(items),
                         capture_output=True, text=True, timeout=300)
    if res.returncode != 0:
        sys.stderr.write(res.stderr)
        raise SystemExit(f"classifier failed ({script} exit {res.returncode})")
    return json.loads(res.stdout)


def stage_bare(rows):
    names = {}
    for row in rows:
        if row["lever"] != "bare-call-lexicon":
            continue
        op = row["op"]
        if not op or op == "?":
            continue
        names.setdefault(op, {"sites": 0, "comps": set()})
        names[op]["sites"] += 1
        names[op]["comps"].add(row["component"])
    if not names:
        return None
    items = [{"name": n, "import": "", "sites": i["sites"]}
             for n, i in sorted(names.items(), key=lambda kv: -kv[1]["sites"])]
    return run_classifier(CLASSIFY, items)


def stage_functions(rows):
    """Function-level declared effects: the residual the engine cannot resolve."""
    funcs = {}
    for row in rows:
        comp = row["component"]
        fn = row["function"]
        if not fn or not row["site"]:
            continue
        key = (comp, fn)
        funcs.setdefault(key, {"name": f"{comp}::{fn}", "sites": [], "sites_n": 0})
        if row["site"] not in funcs[key]["sites"]:
            funcs[key]["sites"].append(row["site"])
        funcs[key]["sites_n"] += 1
    if not funcs:
        return None
    items = [v for v in funcs.values()]
    return run_classifier(CLASSIFY_FN, items)


def stage_receivers(rows):
    sites = {}
    for row in rows:
        if row["lever"] not in RECV_LEVERS:
            continue
        if not row["op"] or row["op"] == "?":
            continue
        key = (row["site"], row["function"])
        sites.setdefault(key, {
            "site": row["site"], "receiver": row["receiver"],
            "method": row["op"], "function": row["function"],
            "shape": row["shape"], "sites": 0,
        })
        sites[key]["sites"] += 1
    if not sites:
        return None
    items = list(sites.values())
    return run_classifier(CLASSIFY_RECV, items)


def main():
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    probe_dir, tsv = sys.argv[1], sys.argv[2]
    rows = read_tsv(tsv)

    bare = stage_bare(rows)
    recv = stage_receivers(rows)
    fnfx = stage_functions(rows)

    print("== proposed DATA diff ==\n")

    if bare:
        pure = sorted({it["name"] for it in bare
                       if it.get("domain") == "pure"})
        fx = {}
        for it in bare:
            if it.get("domain") in ("io", "network", "db", "time", "process"):
                fx[it["name"]] = it["domain"]
        print(f"-- bare names ({len(bare)}) --")
        if pure:
            print('  pure-names:  "' + '" "'.join(pure) + '"')
        for n, d in sorted(fx.items()):
            print(f"  bare-name-effect: {n} -> {d}")
        for it in bare:
            if it.get("domain") in ("dynamic",) or it.get("domain") not in ("pure", "io", "network", "db", "time", "process"):
                print(f"  (skip/edge) {it['name']}: {it.get('domain')} — {it.get('reason','')}")
        print()

    if recv:
        rules = defaultdict(lambda: {"domains": set(), "sites": 0, "reasons": []})
        for it in recv:
            mod = it.get("module") or "?"
            dom = it.get("domain") or "dynamic"
            rules[mod]["domains"].add(dom)
            rules[mod]["reasons"].append(it.get("reason", ""))
        print(f"-- receiver modules ({len(recv)}) --")
        for mod in sorted(rules):
            doms = sorted(rules[mod]["domains"])
            dom = doms[0] if len(doms) == 1 else "/".join(doms)
            if dom in ("pure", "io", "network", "db", "time", "process"):
                print(f"  effect-rule \"{mod}\" '{dom}'")
            else:
                print(f"  (skip/edge) {mod}: {dom} — {'; '.join(rules[mod]['reasons'][:2])}")
        print()

    if fnfx:
        decl = []
        print(f"-- declared-function-effects ({len(fnfx)}) --")
        for it in fnfx:
            dom = it.get("domain")
            comp, _, fn = it["name"].rpartition("::")
            if dom in ("pure", "io", "network", "db", "time", "process", "dynamic"):
                print(f"  (cons \"{comp}\" '{fn}) '{dom}'   ; {it.get('reason','')}")
                decl.append({"name": it["name"], "domain": dom,
                             "confidence": it.get("confidence"),
                             "reason": it.get("reason")})
            else:
                print(f"  (skip) {it['name']}: {dom}")
        print()

    print("== summary JSON ==")
    print(json.dumps({"bare": bare, "receivers": recv, "functions": fnfx}, indent=2))


if __name__ == "__main__":
    main()
