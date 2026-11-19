#!/usr/bin/env python3
"""Floating composer performance gate against the option off.

Usage: bench-floating-composer.py [--rounds N] [--output RAW.json]

Compiles the package and benchmark/mevedel-floating-composer-bench.el into
a temporary directory, then runs one private presenting KWin session
(scripts/graphical-check-private-kwin.py) whose client runs three graphical
Emacs processes: a presentation probe, the benchmark, and a second probe.
The probes log Wayland traffic (WAYLAND_DEBUG=client) and must show
completed frame callbacks; the benchmark runs without that logging.  The
report lists, for each workload, every scenario against its baseline with
paired per-round ratios.  Never contacts the running editor.
"""

import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
import shlex
import statistics
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
BENCH = "mevedel-floating-composer-bench"
MIN_CALLBACKS = 20
ROUNDS = 6
# (scenario, baseline)
COMPARISONS = [("on-never", "off-inline"), ("on-hidden", "off-inline"),
               ("on-visible", "off-inline"), ("on-visible", "off-away")]
INLINE = ["typing", "wrapped-deletion"]


def load_kwin_runner():
    spec = importlib.util.spec_from_file_location(
        "private_kwin", ROOT / "scripts" / "graphical-check-private-kwin.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def compile_build(temporary):
    """Byte-compile everything into TEMPORARY/compiled; return the load path."""
    compiled = temporary / "compiled"
    compiled.mkdir()
    files = sorted(str(p) for p in ROOT.glob("mevedel*.el"))
    files.append(str(ROOT / "benchmark" / f"{BENCH}.el"))
    load_path = temporary / "load-path.eld"
    expression = f"""
(progn
  (setq byte-compile-error-on-warn t)
  (let ((byte-compile-dest-file-function
         (lambda (f) (expand-file-name (concat (file-name-base f) ".elc") {json.dumps(str(compiled))}))))
    (dolist (f '({" ".join(json.dumps(f) for f in files)}))
      (unless (byte-compile-file f)
        (princ (with-current-buffer "*Compile-Log*" (buffer-string)))
        (kill-emacs 1))))
  (with-temp-file {json.dumps(str(load_path))}
    (prin1 (cons {json.dumps(str(compiled))} load-path) (current-buffer))))"""
    env = dict(os.environ, HOME=str(temporary),
               NPM_CONFIG_CACHE=os.environ.get("NPM_CONFIG_CACHE", str(Path.home() / ".npm")))
    result = subprocess.run(["npx", "--offline", "@emacs-eask/cli", "emacs", "--batch",
                             "-L", ".", "-L", "benchmark", "--eval", expression],
                            cwd=ROOT, env=env, capture_output=True, text=True)
    if result.returncode:
        raise SystemExit(f"compilation failed:\n{result.stdout[-4000:]}{result.stderr[-4000:]}")
    return load_path.read_text()


def emacs_command(load_path, form):
    """Return a graphical Emacs command evaluating FORM with compiled code."""
    bootstrap = (f"(progn (setq load-path '{load_path} native-comp-jit-compilation nil)"
                 f" (condition-case err (progn (require '{BENCH}) {form} (kill-emacs 0))"
                 " (error (princ (format \"bench error: %S\\n\" err) #'external-debugging-output)"
                 " (kill-emacs 1))))")
    return ["emacs", "-Q", "--no-splash", "--eval", bootstrap]


def client_command(load_path, output, probes):
    """Return the session client: probe, benchmark, probe; fail on any failure."""
    probe = emacs_command(load_path, "(mevedel-fc-bench-probe)")
    steps = [f"WAYLAND_DEBUG=client {shlex.join(probe)} 2> {shlex.quote(str(probes[0]))}",
             shlex.join(emacs_command(load_path, f"(mevedel-fc-bench-run {json.dumps(str(output))} {ROUNDS})")),
             f"WAYLAND_DEBUG=client {shlex.join(probe)} 2> {shlex.quote(str(probes[1]))}"]
    return ["sh", "-c", " && ".join(steps)]


def callbacks(log):
    """Return (frame requests, completed frame callbacks) in a WAYLAND_DEBUG LOG."""
    text = log.read_text(errors="replace")
    return (len(re.findall(r"-> wl_surface[#@]\d+\.frame\(", text)),
            len(re.findall(r"wl_callback[#@]\d+\.done\(", text)))


def median(values):
    return statistics.median(values) if values else float("nan")


def quantile(values, q):
    values = sorted(values)
    return values[min(len(values) - 1, int(q * len(values)))] if values else float("nan")


def metrics(record):
    """Return RECORD's per-run metrics as {name: (unit, value)}."""
    rows = {"Emacs CPU": ("ms", record["emacs-ns"] / 1e6),
            "KWin CPU": ("ms", record["kwin-ns"] / 1e6),
            "Xwayland CPU": ("ms", record["xwayland-ns"] / 1e6),
            "redisplay cycles": ("count", record["redisplays"]),
            "child-frame move/resize": ("count", record["placements"]),
            "child-frame show/hide": ("count", record["visibility"]),
            "GCs": ("count", record["gcs"])}
    if record["samples-ms"]:
        rows["latency (run median)"] = ("ms", median(record["samples-ms"]))
    return rows


def paired(name, ours, base, rounds):
    """Print one paired metric line over ROUNDS; skip all-zero metrics."""
    unit = metrics(ours[rounds[0]])[name][0]
    a = [metrics(ours[r])[name][1] for r in rounds]
    b = [metrics(base[r])[name][1] for r in rounds]
    if not any(a) and not any(b):
        return
    diffs = [x - y for x, y in zip(a, b)]
    ratios = [x / y for x, y in zip(a, b) if y]
    ratio = (f", ratio {median(ratios):.2f} [{min(ratios):.2f}, {max(ratios):.2f}]"
             if len(ratios) == len(a) else "")
    print(f"- {name}: {median(a):.2f} vs {median(b):.2f} {unit}; median per run; "
          f"paired diff {median(diffs):+.2f} [{min(diffs):+.2f}, {max(diffs):+.2f}]{ratio}; "
          f"n {len(rounds)} rounds")


def report(data):
    runs = {}
    for record in data["records"]:
        runs.setdefault((record["scenario"], record["workload"]), {})[record["round"]] = record
    workloads = sorted({w for _, w in runs})
    print(f"Emacs {data['emacs-version']}; library {data['library']}")
    for (scenario, workload), by_round in sorted(runs.items()):
        for key in ("focused", "attended"):
            states = [r.get(key) for r in by_round.values()]
            if workload != "idle-unfocused" and False in states:
                print(f"- WARNING scenario {scenario}; workload {workload}: view not {key} "
                      f"in {states.count(False)} of {len(states)} runs")
    print("Values: scenario vs baseline; paired = per-round scenario minus baseline "
          "(ratio = scenario / baseline), median [min, max] over rounds.")
    for scenario, baseline in COMPARISONS:
        for workload in workloads:
            if baseline == "off-away" and workload in INLINE:
                continue
            if (scenario, workload) not in runs or (baseline, workload) not in runs:
                continue
            ours, base = runs[(scenario, workload)], runs[(baseline, workload)]
            rounds = sorted(set(ours) & set(base))
            print(f"\n## {workload}: {scenario} vs {baseline}")
            a = [x for r in rounds for x in ours[r]["samples-ms"]]
            b = [x for r in rounds for x in base[r]["samples-ms"]]
            if a:
                print(f"- latency incl. redisplay: {median(a):.2f} vs {median(b):.2f} ms median, "
                      f"p90 {quantile(a, 0.9):.2f} vs {quantile(b, 0.9):.2f} ms; pooled commands; "
                      f"n {len(a)} vs {len(b)}")
                paired("latency (run median)", ours, base, rounds)
            for name in metrics(ours[rounds[0]]):
                if name != "latency (run median)":
                    paired(name, ours, base, rounds)
    first = runs.get(("on-never", "first-show"))
    if first:
        values = [x for r in first.values() for x in r["samples-ms"]]
        print(f"\n## first-show: on-never (child frame created; no off counterpart)\n"
              f"- latency incl. redisplay: {median(values):.2f} ms median, "
              f"p90 {quantile(values, 0.9):.2f} ms; pooled commands; n {len(values)}")


def main(argv=None):
    global ROUNDS
    parser = argparse.ArgumentParser(prog="bench-floating-composer.py")
    parser.add_argument("--rounds", type=int, default=6)
    parser.add_argument("--output", help="new file for the raw JSON samples")
    parser.add_argument("--report", help="only report an existing raw JSON file")
    options = parser.parse_args(argv)
    if options.report:
        report(json.loads(Path(options.report).read_text()))
        return 0
    if options.output and Path(options.output).exists():
        parser.error(f"output exists: {options.output}")
    ROUNDS = options.rounds
    kwin = load_kwin_runner()
    with tempfile.TemporaryDirectory(prefix="mevedel-fc-bench-") as directory:
        temporary = Path(directory)
        load_path = compile_build(temporary)
        output = temporary / "raw.json"
        probes = [temporary / "probe-before.log", temporary / "probe-after.log"]
        status = kwin.run_session(client_command(load_path, output, probes),
                                  session_timeout=7200)
        for probe in probes:
            if probe.exists():
                requested, done = callbacks(probe)
                print(f"- presentation probe {probe.stem}; frame requests {requested}; "
                      f"completed callbacks {done}; n 1 run")
                if done < MIN_CALLBACKS:
                    print("Presentation not verified: too few completed frame callbacks",
                          file=sys.stderr)
                    status = status or 1
        if status:
            print(f"Benchmark session failed with status {status}", file=sys.stderr)
            return status
        data = json.loads(output.read_text())
        if options.output:
            Path(options.output).write_text(json.dumps(data))
        report(data)
    return 0


if __name__ == "__main__":
    sys.exit(main())
