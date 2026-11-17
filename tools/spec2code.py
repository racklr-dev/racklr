#!/usr/bin/env python3
"""spec2code.py <spec.fizz> — the kernel guardrail: have the LLM generate host
code for a .fizz spec's components, then loop: lower/infer/check against the
spec, and feed every kernel violation back to the LLM until it is accepted.
"""
import json, os, subprocess, sys, urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))
CHECK_SPEC = os.path.join(HERE, "check-spec.rkt")


def llm(system, user):
    key = os.environ.get("DEEPSEEK_API_KEY") or os.environ.get("CLASSIFY_KEY")
    if not key:
        raise SystemExit("set DEEPSEEK_API_KEY")
    req = {"model": os.environ.get("CLASSIFY_MODEL", "deepseek-flash"),
           "messages": [{"role": "system", "content": system},
                         {"role": "user", "content": user}],
           "temperature": 0, "max_tokens": 8000}
    r = urllib.request.Request("https://api.deepseek.com/v1/chat/completions",
                              data=json.dumps(req).encode(),
                              headers={"Authorization": f"Bearer {key}",
                                       "Content-Type": "application/json"})
    return json.loads(urllib.request.urlopen(r, timeout=300).read())["choices"][0]["message"]["content"]


def check(spec_file, components, interp):
    env = dict(os.environ)
    env["PLTCOLLECTS"] = ROOT + ":" + os.path.join(ROOT, "effervesce") + ":"
    import tempfile
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
        json.dump({"components": components, "interpretations": interp}, f)
        hc = f.name
    try:
        res = subprocess.run(["racket", CHECK_SPEC, spec_file, hc],
                             capture_output=True, text=True, env=env, cwd=ROOT,
                             timeout=300)
    finally:
        os.unlink(hc)
    if res.returncode != 0:
        raise SystemExit(f"check-spec failed: {res.stderr[-300:]}")
    return json.loads(res.stdout)


def main():
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    spec_file = sys.argv[1]
    spec = open(spec_file).read()
    # interpretation: capability -> effect domain (spec-specific DATA)
    interp = json.loads(sys.argv[2]) if len(sys.argv) > 2 else {"Db": ["db"], "Log": ["io"], "Fs": ["io"]}
    system = (
        "You write Python functions for a static-effect system. "
        "The spec declares capabilities and components with `needs` effect rows. "
        "Write a plain Python function body for each op that performs ONLY the "
        "component's declared effects, nothing else. "
        "Use these verbs so the effect checker can classify them: "
        "`db.execute(...)` or `db.query(...)` for db; `log.write(...)` or `print(...)` "
        "for io; `requests.get(...)` for network; `time.sleep(...)` for time. "
        "Reply with a JSON object "
        '{"components": {"<ComponentName>": "<python source>"}} — one Python function '
        "per op, named exactly as the op. No prose."
    )
    components = {}
    history = []
    for attempt in range(5):
        user = f"SPEC:\n{spec}\n\n" + ("PRIOR VIOLATIONS (fix them):\n" + "\n".join(history)
                                         if history else "")
        msg = llm(system, user).strip()
        if msg.startswith("```"):
            msg = msg.split("\n", 1)[1].rsplit("```", 1)[0]
        try:
            components = json.loads(msg)["components"]
        except Exception as e:
            history.append(f"(invalid JSON: {e})")
            continue
        print(f"attempt {attempt + 1} generated:")
        for k, v in components.items():
            print(f"  {k}: {v!r}")
        issues = check(spec_file, components, interp)
        if not issues:
            print("ACCEPTED after", attempt + 1, "attempt(s)")
            print(json.dumps(components, indent=2))
            return
        history = [f"{i.get('component')}: {i.get('effect')} ({", ".join(i.get('sites') or [])})"
                   for i in issues]
        print(f"attempt {attempt + 1}: {len(issues)} violation(s):", "; ".join(history))
    print("NOT ACCEPTED after 5 attempts; last violations:", "; ".join(history))
    sys.exit(1)


if __name__ == "__main__":
    main()
