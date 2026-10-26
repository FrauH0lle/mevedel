#!/usr/bin/env python3
"""Compare compiled interior instruction edits with a git-archive baseline snapshot.

Uses installed .eask dependencies without launching Eask or altering its cache.
All source copies, bytecode, HOME/XDG roots and native caches are temporary.
Example: python3 scripts/bench-instruction-edits.py --max-ratio 1.15
"""

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import shutil
import statistics
import subprocess
import sys
import tarfile
import tempfile


FIXTURE = "mevedel-instruction-edit-bench.el"
ISOLATE = r'''
(setq native-comp-jit-compilation nil)
(when (native-comp-available-p)
  (startup-redirect-eln-cache (expand-file-name "eln" (getenv "HOME"))))
'''
COMPILE = r'''
(require 'bytecomp)
(let* ((source (getenv "MEVEDEL_BENCH_SOURCE"))
       (destination (getenv "MEVEDEL_BENCH_BUILD"))
       (load-path (append (list source) load-path))
       (byte-compile-error-on-warn t)
       (byte-compile-dest-file-function
        (lambda (file)
          (file-name-concat destination (concat (file-name-base file) ".elc")))))
  (dolist (file (directory-files source t "\\`mevedel.*\\.el\\'"))
    (unless (byte-compile-file file)
      (error "Compilation failed: %s" file))))
'''
MEASURE = r'''
(require 'mevedel-instruction-edit-bench)
(mevedel-instruction-bench-run (getenv "MEVEDEL_BENCH_BUILD"))
'''


def run(argv, cwd, env=None, timeout=300, **kwargs):
    """Run a bounded command, retaining diagnostics on failure."""
    result = subprocess.run(argv, cwd=cwd, env=env, timeout=timeout,
                            stderr=subprocess.PIPE, **kwargs)
    if result.returncode:
        stderr = result.stderr.decode(errors="replace")
        stdout = result.stdout or b""
        raise RuntimeError(f"{argv[0]} exited {result.returncode}:\n"
                           f"{stdout.decode(errors='replace')}\n{stderr}")
    return result


def digest_files(directory):
    """Return a path-keyed content manifest, independent of temporary roots."""
    return {str(path.relative_to(directory)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in sorted(directory.rglob("*")) if path.is_file()}


def tree_digest(manifest):
    """Hash the names and contents of a content manifest deterministically."""
    return hashlib.sha256(json.dumps(manifest, sort_keys=True).encode()).hexdigest()


def isolated_environment(directory):
    """Create private HOME/XDG roots before any child Emacs starts."""
    env = os.environ.copy()
    for name in list(env):
        if name.startswith(("EASK", "PKG_", "EMACS")):
            env.pop(name)
    for name in ("HOME", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME",
                 "XDG_STATE_HOME", "XDG_RUNTIME_DIR"):
        path = directory / name.lower()
        path.mkdir(parents=True, mode=0o700)
        env[name] = str(path)
    return env


def snapshot_sources(root, temporary, baseline):
    """Archive BASELINE and freeze the current worktree without writing to either."""
    master = temporary / "master"
    master.mkdir()
    archive = temporary / "master.tar"
    with archive.open("wb") as output:
        run(["git", "archive", "--format=tar", baseline], root, stdout=output)
    with tarfile.open(archive) as source:
        source.extractall(master, filter="data")
    current = temporary / "current"
    current.mkdir()
    for path in root.glob("mevedel*.el"):
        shutil.copyfile(path, current / path.name)
    # Tool definitions expand prompt files at compile time.
    if (root / "prompts").is_dir():
        shutil.copytree(root / "prompts", current / "prompts")
    # Use precisely the same benchmark implementation for both revisions.
    fixture = root / "benchmark" / FIXTURE
    for destination in (master, current):
        shutil.copyfile(fixture, destination / FIXTURE)
    return {"master": master, "current": current}


def freeze_dependencies(elpa, destination):
    """Freeze installed package directories, excluding mutable archive metadata."""
    destination.mkdir()
    packages = sorted(path for path in elpa.iterdir()
                      if path.is_dir() and list(path.glob("*-pkg.el")))
    if not packages:
        raise RuntimeError(f"No installed package directories in {elpa}")
    for package in packages:
        shutil.copytree(package, destination / package.name,
                        ignore=shutil.ignore_patterns("*.eln", "__pycache__", ".git"))
    return [destination / package.name for package in packages]


def measure(emacs, source, directory, packages, dependency_root):
    """Compile one source snapshot, then measure in a fresh isolated process."""
    directory.mkdir()
    build = directory / "compiled"
    build.mkdir()
    env = isolated_environment(directory)
    env.update(MEVEDEL_BENCH_SOURCE=str(source), MEVEDEL_BENCH_BUILD=str(build),
               MEVEDEL_BENCH_DEPS=str(dependency_root))
    dependency_args = [arg for package in packages for arg in ("-L", str(package))]
    command = [emacs, "--batch", "-Q", "--eval", f"(progn {ISOLATE})"] + dependency_args
    compiled = run(command + ["--eval", f"(progn {COMPILE})"], directory,
                   env, stdout=subprocess.PIPE)
    if compiled.stderr:
        print(compiled.stderr.decode(errors="replace"), file=sys.stderr, end="")
    result = run(command + ["-L", str(build), "--eval", f"(progn {MEASURE})"], directory,
                 env, stdout=subprocess.PIPE)
    # A successful but noisy compilation/measurement must remain visible.
    if result.stderr:
        print(result.stderr.decode(errors="replace"), file=sys.stderr, end="")
    report = json.loads(result.stdout)
    for hotpath in report["hotpaths"]:
        path = Path(hotpath["loaded"])
        if path.parent != build or path.suffix != ".elc":
            raise RuntimeError(f"Unexpected hotpath provenance: {hotpath}")
        hotpath["source_sha256"] = hashlib.sha256(
            (source / path.with_suffix(".el").name).read_bytes()).hexdigest()
        hotpath["bytecode_sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
    report["source_manifest"] = {
        path.name: hashlib.sha256(path.read_bytes()).hexdigest()
        for path in sorted(source.glob("mevedel*.el"))}
    if (source / "prompts").is_dir():
        report["source_manifest"].update({f"prompts/{path}": digest for path, digest
                                          in digest_files(source / "prompts").items()})
    report["source_sha256"] = tree_digest(report["source_manifest"])
    return report


def summarize(reports, max_ratio=None):
    """Validate raw samples and return median ratios, never hiding a slow result."""
    for report in reports.values():
        if (report["sample_count"], report["warmups"], report["keys"]) != (5, 5, 2000):
            raise RuntimeError("Unexpected benchmark protocol")
        if [entry["references"] for entry in report["scenarios"]] != [1, 50, 200]:
            raise RuntimeError("Unexpected scenario roster")
        for scenario in report["scenarios"]:
            samples = scenario["samples"]
            if len(samples) != 5 or any(
                    not math.isfinite(sample["us_per_key"]) or sample["us_per_key"] <= 0
                    or not isinstance(sample["apply_entries"], int)
                    or sample["apply_entries"] < 0 for sample in samples):
                raise RuntimeError("Invalid benchmark samples")
    if any(sample["apply_entries"] for scenario in reports["current"]["scenarios"]
           for sample in scenario["samples"]):
        raise RuntimeError("Interior edits added function undo entries")
    rows = []
    for before, after in zip(reports["master"]["scenarios"], reports["current"]["scenarios"]):
        baseline = statistics.median(s["us_per_key"] for s in before["samples"])
        current = statistics.median(s["us_per_key"] for s in after["samples"])
        ratio = current / baseline
        rows.append({"references": before["references"], "sample_count": 5,
                     "master_us_per_key_median": baseline,
                     "current_us_per_key_median": current, "current_over_master": ratio,
                     "master_apply_entries": [s["apply_entries"] for s in before["samples"]],
                     "current_apply_entries": [s["apply_entries"] for s in after["samples"]],
                     "passes": max_ratio is None or ratio <= max_ratio})
    return rows


def positive_ratio(value):
    """Parse a finite positive timing gate."""
    ratio = float(value)
    if not math.isfinite(ratio) or ratio <= 0:
        raise argparse.ArgumentTypeError("Ratio must be finite and positive")
    return ratio


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--emacs", default="emacs")
    parser.add_argument("--base", default="master", metavar="REV",
                        help="Baseline Git revision (default: master)")
    parser.add_argument("--elpa", type=Path, help="Installed ELPA package directory")
    parser.add_argument("--max-ratio", type=positive_ratio,
                        help="Fail if any current/master median exceeds this ratio")
    parser.add_argument("--output", type=Path, help="Write raw samples/provenance JSON (new file)")
    args = parser.parse_args(argv)
    root = Path(__file__).resolve().parent.parent
    try:
        if args.output and args.output.exists():
            raise RuntimeError(f"Output already exists: {args.output}")
        baseline = run(["git", "rev-parse", "--verify", f"{args.base}^{{commit}}"], root,
                       stdout=subprocess.PIPE).stdout.decode().strip()
        head = run(["git", "rev-parse", "HEAD"], root,
                   stdout=subprocess.PIPE).stdout.decode().strip()
        status = run(["git", "status", "--short"], root,
                     stdout=subprocess.PIPE).stdout.decode()
        candidates = list((root / ".eask").glob("*/elpa"))
        elpa = args.elpa or (candidates[0] if len(candidates) == 1 else None)
        if elpa is None:
            raise RuntimeError("Pass --elpa: could not select one .eask/*/elpa directory")
        with tempfile.TemporaryDirectory(prefix="mevedel-instruction-bench-") as name:
            temporary = Path(name)
            sources = snapshot_sources(root, temporary, baseline)
            dependencies = temporary / "dependencies"
            packages = freeze_dependencies(elpa, dependencies)
            dependency_manifest = digest_files(dependencies)
            reports = {}
            for label, source in sources.items():
                print(f"Compiling and measuring {label} snapshot...", file=sys.stderr, flush=True)
                reports[label] = measure(args.emacs, source, temporary / f"run-{label}",
                                         packages, dependencies)
                for scenario in reports[label]["scenarios"]:
                    median = statistics.median(s["us_per_key"] for s in scenario["samples"])
                    print(f"{label} N={scenario['references']} samples=5 "
                          f"median={median:.3f} us/key", file=sys.stderr, flush=True)
            if digest_files(dependencies) != dependency_manifest:
                raise RuntimeError("Frozen dependencies changed during measurement")
            rows = summarize(reports, args.max_ratio)
            report = {"worktree": str(root), "head": head, "git_status": status,
                      "master_commit": baseline, "dependency_origin": str(elpa.resolve()),
                      "dependency_manifest": dependency_manifest,
                      "dependency_sha256": tree_digest(dependency_manifest),
                      "order": ["master", "current"], "max_ratio": args.max_ratio,
                      "reports": reports, "comparison": rows}
            if args.output:
                with args.output.open("x") as output:
                    json.dump(report, output, indent=2)
                    output.write("\n")
            print(f"master={baseline} current_HEAD={head} (worktree snapshot)")
            print(f"dependencies_sha256={report['dependency_sha256']}")
            for label, measured in reports.items():
                print(f"{label}: Emacs={measured['emacs_version']} "
                      f"source_sha256={measured['source_sha256']} "
                      f"bytecompiled_hotpaths={len(measured['hotpaths'])}")
            print(f"fixture_sha256={reports['current']['source_manifest'][FIXTURE]}")
            print("Batch compiled hooks; 2000 keys/sample; warmups=5; samples=5 per N/build")
            for row in rows:
                print(f"N={row['references']} samples={row['sample_count']} "
                      f"master_median={row['master_us_per_key_median']:.3f} us/key "
                      f"current_median={row['current_us_per_key_median']:.3f} us/key "
                      f"current/master={row['current_over_master']:.3f} "
                      f"master_apply_entries={row['master_apply_entries']} "
                      f"current_apply_entries={row['current_apply_entries']}")
            if args.max_ratio is not None:
                passed = all(row["passes"] for row in rows)
                print(f"max_ratio={args.max_ratio:.3f}: {'PASS' if passed else 'FAIL'}")
                return 0 if passed else 1
        return 0
    except (OSError, RuntimeError, ValueError, subprocess.TimeoutExpired, tarfile.TarError) as error:
        print(f"Instruction edit benchmark failed: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
