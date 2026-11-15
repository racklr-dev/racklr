#!/usr/bin/env python3
"""calibrate.py — off-corpus agreement per lexicon surface: blind-feed the
hand-curated bare-name / method / module DATA to the matching classifier and
measure accuracy + per-domain confusion."""
import json, os, subprocess, sys
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
LEXICON_DUMP = os.path.join(HERE, "dump-lexicon.rkt")
CLASSIFY_BARE = os.path.join(HERE, "classify-effects.sh")
CLASSIFY_MODULE = os.path.join(HERE, "classify-receivers.sh")


def ground_truth():
    env = dict(os.environ)
    env["PLTCOLLECTS"] = (
        os.path.abspath(os.path.join(HERE, "..")) + ":" +
        os.path.abspath(os.path.join(HERE, "..", "effervesce")) + ":"
    )
    res = subprocess.run(["racket", LEXICON_DUMP], capture_output=True,
                         text=True, env=env, cwd=os.path.join(HERE, ".."))
    if res.returncode != 0:
        sys.stderr.write(res.stderr)
        raise SystemExit(1)
    return json.loads(res.stdout)


def run(script, items):
    res = subprocess.run([script], input=json.dumps(items),
                         capture_output=True, text=True, timeout=900)
    if res.returncode != 0:
        sys.stderr.write(res.stderr)
        raise SystemExit(1)
    return json.loads(res.stdout)


def report(kind, gt, pred_of):
    correct = 0
    conf = defaultdict(lambda: defaultdict(int))
    disagree = []
    for e in gt:
        actual, pred = e["domain"], pred_of(e)
        conf[actual][pred] += 1
        if pred == actual:
            correct += 1
        else:
            disagree.append((e["name"], actual, pred))
    n = len(gt)
    print(f"\n=== {kind} ({n}) ===")
    print(f"accuracy: {correct}/{n} = {correct/n:.1%}")
    for a in sorted(conf):
        row = conf[a]
        print(f"  {a:10s}: " + ", ".join(f"{p}={row[p]}" for p in sorted(row)))
    for name, a, p in disagree:
        print(f"  ~ {name:36s} actual={a:9s} predicted={p}")


def main():
    gt = ground_truth()
    by_kind = defaultdict(list)
    for e in gt:
        by_kind[e["kind"]].append(e)

    if "bare" in by_kind or "method" in by_kind:
        items = [{"name": e["name"], "import": "", "sites": 1}
                 for e in by_kind["bare"] + by_kind["method"]]
        preds = {p["name"]: p.get("domain") for p in run(CLASSIFY_BARE, items)}
        for kind in ("bare", "method"):
            if by_kind[kind]:
                report(kind, by_kind[kind],
                       lambda e, preds=preds: preds.get(e["name"], "?"))

    if by_kind["module"]:
        items = [{"site": e["name"], "receiver": e["name"], "method": "op",
                  "function": "", "hint": e["name"], "sites": 1}
                 for e in by_kind["module"]]
        outs = run(CLASSIFY_MODULE, items)
        # the classifier answers in input order; compare domains by position.
        preds = {e["name"]: (outs[i].get("domain") if i < len(outs) else "?")
                 for i, e in enumerate(by_kind["module"])}
        report("module", by_kind["module"],
               lambda e, preds=preds: preds.get(e["name"], "?"))


if __name__ == "__main__":
    main()
