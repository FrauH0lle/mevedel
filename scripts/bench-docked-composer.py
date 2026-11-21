#!/usr/bin/env python3
"""Compare two compiled composer builds in a presenting private KWin."""

import argparse
import hashlib
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
BENCH = "niwa-docked-composer-bench"
BUILDS = ("baseline", "candidate")
STEADY = ("typing", "wrapped-deletion", "stream", "idle-request", "idle-quiet", "scrolling")
LAYOUT = ("checkpoint", "split", "switch-away", "switch-back", "popup-open", "popup-close", "resume")
MIN_CALLBACKS = 20


def load_kwin_runner():
    spec = importlib.util.spec_from_file_location(
        "private_kwin", ROOT / "scripts" / "graphical-check-private-kwin.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def compile_build(temporary, revision):
    """Byte-compile everything into TEMPORARY/compiled; return the load path."""
    temporary.mkdir()
    archive = subprocess.check_output(["git", "archive", revision], cwd=ROOT)
    subprocess.run(["tar", "-xf", "-", "-C", temporary], input=archive, check=True)
    (temporary / ".eask").symlink_to((ROOT / ".eask").resolve(), target_is_directory=True)
    compiled = temporary / "compiled"
    compiled.mkdir()
    # The native loader looks for its C sources beside the loaded bytecode.
    (compiled / "native").symlink_to("../native", target_is_directory=True)
    files = sorted(str(p) for p in temporary.glob("niwa*.el"))
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
                             "-L", ".", "--eval", expression],
                            cwd=temporary, env=env, capture_output=True, text=True)
    if result.returncode:
        raise SystemExit(f"compilation failed:\n{result.stdout[-4000:]}{result.stderr[-4000:]}")
    return load_path.read_text()


def emacs_command(load_path, grammars, require_native, form):
    """Return a graphical Emacs command evaluating FORM with compiled code."""
    bootstrap = (f"(progn (setq load-path '{load_path} native-comp-jit-compilation nil "
                 f" niwa-dc-bench--require-native {'t' if require_native else 'nil'}"
                 f" treesit-extra-load-path '({' '.join(map(json.dumps, grammars))}))"
                 f" (condition-case err (progn (require '{BENCH}) {form} (kill-emacs 0))"
                 " (error (princ (format \"bench error: %S\\n\" err) #'external-debugging-output)"
                 " (kill-emacs 1))))")
    return ["emacs", "-Q", "--no-splash", "--eval", bootstrap]


def run_session(temporary, revisions, grammars, rounds, smoke, require_native):
    paths = {build: compile_build(temporary / build, revisions[build]) for build in BUILDS}
    steps, probes, runs = [], [], []
    for index in range(rounds + 2):
        measured = 1 <= index <= rounds
        if measured:
            steps.append(f"printf 'pair {index} before load: ' && cat /proc/loadavg")
        for build in BUILDS if index % 2 else reversed(BUILDS):
            if index in (0, rounds + 1):
                log = temporary / f"probe-{index}-{build}.log"
                command = emacs_command(paths[build], grammars, require_native, "(niwa-dc-bench-probe)")
                steps.append(f"WAYLAND_DEBUG=client {shlex.join(command)} 2> {shlex.quote(str(log))}")
                probes.append((build, "before" if index == 0 else "after", log))
            else:
                output = temporary / f"round-{index}-{build}.json"
                form = f"(niwa-dc-bench-run {json.dumps(str(output))} {'t' if smoke else 'nil'})"
                command = emacs_command(paths[build], grammars, require_native, form)
                steps.append(f"printf '%s\\n' 'round {index}: {build}' && {shlex.join(command)}")
                runs.append((index, build, output))
        if measured:
            steps.append(f"printf 'pair {index} after load: ' && cat /proc/loadavg")
    status = load_kwin_runner().run_session(["sh", "-c", " && ".join(steps)], session_timeout=7200)
    evidence = []
    for build, phase, log in probes:
        requested, done = callbacks(log.read_text(errors="replace")) if log.exists() else (0, 0)
        evidence.append(dict(build=build, phase=phase, requested=requested, completed=done))
        print(f"- {build}/{phase}: Wayland frame requests {requested}, completed {done}", flush=True)
        if done < MIN_CALLBACKS:
            status = status or 1
            if log.exists():
                print(log.read_text(errors="replace")[-4000:], file=sys.stderr)
    if status:
        raise SystemExit(f"Benchmark session failed (status {status}); no verdict.")
    data = dict(revisions=revisions, smoke=smoke, probes=evidence, rounds=[])
    for index, build, output in runs:
        run = json.loads(output.read_text())
        if Path(run["library"]).parent != temporary / build / "compiled":
            raise ValueError(f"{build}: unexpected library {run['library']}")
        gptel = Path(run["gptel"]).resolve(strict=True)
        run["gptel"] = str(gptel)
        run["gptel-sha256"] = hashlib.sha256(gptel.read_bytes()).hexdigest()
        data["rounds"].append(dict(round=index, build=build, **run))
    provenance = {(run["emacs-version"], run["gptel"], run["gptel-sha256"])
                  for run in data["rounds"]}
    if len(provenance) != 1:
        raise ValueError(f"Emacs or gptel provenance differs across runs: {provenance}")
    return data


def callbacks(text):
    """Count frame requests and their completions (not wl_display.sync)."""
    outstanding, requests, done = set(), 0, 0
    pattern = re.compile(r"\.frame\(new id wl_callback[#@](\d+)\)|wl_callback[#@](\d+)\.done\(")
    for match in pattern.finditer(text):
        request, completion = match.groups()
        if request is not None:
            requests += 1
            outstanding.add(request)
        elif completion in outstanding:
            done += 1
            outstanding.remove(completion)
    return requests, done


def report(data):
    print(f"Baseline {data['revisions']['baseline']}; candidate {data['revisions']['candidate']}")
    print(f"Emacs {data['rounds'][0]['emacs-version']}; gptel {data['rounds'][0]['gptel']}")
    for run in data["rounds"]:
        print(f"- {run['build']} round {run['round']}: rail {run.get('rail-renderer', 'unverified')}")
    if data["smoke"]:
        print("SMOKE: not performance evidence")
    print("Values: baseline → candidate; paired delta (candidate minus baseline), median [min, max].")
    for scenario in STEADY + LAYOUT:
        groups = {}
        for run in data["rounds"]:
            records = [row for row in run["records"] if row["scenario"] == scenario]
            if not records:
                raise ValueError(f"missing {scenario} in {run['build']} round {run['round']}")
            groups.setdefault(run["round"], {})[run["build"]] = records
        if any(set(pair) != set(BUILDS) for pair in groups.values()):
            raise ValueError(f"unpaired {scenario}")
        metrics = []
        if scenario not in ("idle-request", "idle-quiet"):
            metric = {"stream": "render latency", "checkpoint": "explicit save latency"}.get(
                scenario, "command-to-redisplay latency")
            metrics.append((metric, "ms", lambda r: statistics.median(r["samples-ms"])))
        if scenario in STEADY:
            metrics.extend((metric, "ms/s", lambda r, field=field: r[field] / (1e6 * r["wall"]))
                           for metric, field in (("Emacs CPU", "emacs-ns"), ("KWin CPU", "kwin-ns"),
                                                 ("Xwayland CPU", "xwayland-ns")))
            metrics.extend((metric, "count", lambda r, field=field: r[field])
                           for metric, field in (("redisplay cycles", "redisplays"), ("GCs", "gcs")))
        metrics.extend((metric, unit, lambda r, field=field: r[field]) for metric, unit, field in
                       (("pair repairs", "count", "repairs"), ("pair repair time", "ms", "repair-ms"),
                        ("dock fits", "count", "fits")))
        for metric, unit, get in metrics:
            before, after = ([statistics.median(map(get, pair[build])) for pair in groups.values()]
                             for build in BUILDS)
            delta = [b - a for a, b in zip(before, after)]
            if not (any(before) or any(after)) and metric not in ("pair repairs", "pair repair time"):
                continue
            print(f"- scenario: {scenario}; metric: {metric}; "
                  f"value: {statistics.median(before):.3f} → {statistics.median(after):.3f}; "
                  f"unit: {unit}; statistic: median of round medians; n: {len(delta)} paired rounds; "
                  f"paired delta: {statistics.median(delta):+.3f} [{min(delta):+.3f}, {max(delta):+.3f}]")
        if scenario not in ("idle-request", "idle-quiet"):
            for build in BUILDS:
                values = sorted(sample for pair in groups.values() for r in pair[build]
                                for sample in r["samples-ms"])
                print(f"- scenario: {scenario}/{build}; metric: {metrics[0][0]}; "
                      f"value: {values[(9 * len(values) - 1) // 10]:.3f}; unit: ms; "
                      f"statistic: pooled p90; n: {len(values)} samples")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", help="docked baseline revision (required with --output)")
    parser.add_argument("--candidate", default="HEAD", help="docked candidate revision")
    parser.add_argument("--rounds", type=int, default=6)
    parser.add_argument("--smoke", action="store_true", help="shortened scenarios; not a verdict")
    parser.add_argument("--require-native", action="store_true", help="reject Lisp rail fallback")
    parser.add_argument("--grammar-dir", action="append", default=[])
    output = parser.add_mutually_exclusive_group(required=True)
    output.add_argument("--output", type=Path, help="new path for the raw JSON samples")
    output.add_argument("--report", type=Path, help="report an existing raw JSON file")
    args = parser.parse_args()
    if args.report:
        report(json.loads(args.report.read_text()))
        return
    if args.baseline is None:
        parser.error("--baseline is required with --output")
    if args.rounds < 1 or args.output.exists():
        parser.error("use a positive round count and a new output path")
    revisions = {name: subprocess.check_output(["git", "rev-parse", "--verify", f"{rev}^{{commit}}"],
                                               text=True, cwd=ROOT).strip()
                 for name, rev in zip(BUILDS, (args.baseline, args.candidate))}
    grammars = [str(Path(path).resolve()) for path in args.grammar_dir]
    assert callbacks("wl_display#1.sync(new id wl_callback#2) wl_callback#2.done(0) "
                     "wl_surface#3.frame(new id wl_callback#2) wl_callback#2.done(1) "
                     "wl_surface#3.frame(new id wl_callback#2)") == (2, 1)
    with tempfile.TemporaryDirectory(prefix="niwa-dc-bench-") as directory:
        data = run_session(Path(directory), revisions, grammars, args.rounds, args.smoke,
                           args.require_native)
        with args.output.open("x") as raw:
            json.dump(data, raw)
        report(data)


if __name__ == "__main__":
    main()
