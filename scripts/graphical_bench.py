"""Compiled graphical benchmark support for a private presenting KWin.

Compiles the package and the chosen benchmark into a temporary directory,
then runs one private presenting KWin session
(scripts/graphical-check-private-kwin.py) whose client runs three graphical
Emacs processes: a presentation probe, the benchmark, and a second probe.
The probes log Wayland traffic (WAYLAND_DEBUG=client) and must show
completed frame callbacks; the benchmark runs without that logging.  The
report lists, for each workload, every scenario against its baseline with
paired per-round ratios.  Never contacts the running editor.
"""

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

MIN_CALLBACKS = 20


def load_kwin_runner():
    spec = importlib.util.spec_from_file_location(
        "private_kwin", Path(__file__).with_name("graphical-check-private-kwin.py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def compile_build(root, temporary, bench):
    """Byte-compile everything into TEMPORARY/compiled; return the load path."""
    compiled = temporary / "compiled"
    compiled.mkdir()
    files = sorted(str(p) for p in root.glob("mevedel*.el"))
    files.append(str(bench.resolve()))
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
                            cwd=root, env=env, capture_output=True, text=True)
    if result.returncode:
        raise SystemExit(f"compilation failed:\n{result.stdout[-4000:]}{result.stderr[-4000:]}")
    return load_path.read_text()


def emacs_command(load_path, bench, form):
    """Return a graphical Emacs command evaluating FORM with compiled code."""
    bootstrap = (f"(progn (setq load-path '{load_path} native-comp-jit-compilation nil)"
                 f" (condition-case err (progn (require '{bench}) {form} (kill-emacs 0))"
                 " (error (princ (format \"bench error: %S\\n\" err) #'external-debugging-output)"
                 " (kill-emacs 1))))")
    return ["emacs", "-Q", "--no-splash", "--eval", bootstrap]


def client_command(load_path, bench, form, probe_form, probes):
    """Return the session client: probe, benchmark, probe; fail on any failure."""
    probe = emacs_command(load_path, bench, probe_form)
    steps = [f"WAYLAND_DEBUG=client {shlex.join(probe)} 2> {shlex.quote(str(probes[0]))}",
             shlex.join(emacs_command(load_path, bench, form)),
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


def report(data, comparisons):
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
    for scenario, baseline in comparisons:
        for workload in workloads:
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


def run_benchmark(root, bench, run_form, probe_form):
    """Run BENCH (an Elisp file) with ROOT's sources; return raw JSON data.

    RUN_FORM takes the raw output path and returns the benchmark Lisp form.
    PROBE_FORM drives redisplay in a separate Emacs before and after the run.
    """
    kwin = load_kwin_runner()
    with tempfile.TemporaryDirectory(prefix="mevedel-graphical-bench-") as directory:
        temporary = Path(directory)
        load_path = compile_build(root, temporary, bench)
        output = temporary / "raw.json"
        probes = [temporary / "probe-before.log", temporary / "probe-after.log"]
        status = kwin.run_session(client_command(load_path, bench.stem, run_form(output),
                                                 probe_form, probes),
                                  session_timeout=7200)
        for probe in probes:
            requested, done = callbacks(probe) if probe.exists() else (0, 0)
            print(f"- presentation probe {probe.stem}; frame requests {requested}; "
                  f"completed callbacks {done}; n 1 run")
            if done < MIN_CALLBACKS:
                print("Presentation not verified: too few completed frame callbacks",
                      file=sys.stderr)
                status = status or 1
        if status:
            print(f"Benchmark session failed with status {status}", file=sys.stderr)
            raise SystemExit(status)
        return json.loads(output.read_text())
