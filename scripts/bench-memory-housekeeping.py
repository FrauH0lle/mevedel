#!/usr/bin/env python3
"""Measure local housekeeping in a private interactive terminal Emacs.

Requires Bubblewrap and bootstrapped .eask dependencies. No running editor is
contacted. Host files are read-only; only temporary snapshots are writable.
"""

import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path
import pty
import runpy
import select
import shutil
import statistics
import subprocess
import tempfile
import termios
import time


ROOT = Path(__file__).resolve().parent.parent
COMMON = runpy.run_path(str(ROOT / "scripts/bench-instruction-edits.py"))
LISP = r''';;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'json)
(require 'memory-history-fixtures)
(require 'mevedel-memory-pass)
(require 'mevedel-native)
(setq mevedel-native--load-state "benchmark uses fallback timer"
      mevedel-telemetry-enabled t
      mevedel-telemetry-lag-threshold 0.001
      mevedel-telemetry--lag-count-threshold 0.001
      mevedel-memory-consolidation-mode 'auto
      mevedel-memory-history-max-age-days 14
      mevedel-journal-max-age-days 14)
(when (getenv "BENCH_SHIM") (load (getenv "BENCH_SHIM") nil t))
(let* ((root (file-name-as-directory (getenv "BENCH_WORKSPACE")))
       (workspace (mevedel-workspace--create :root root :id root :type 'project))
       (step (getenv "BENCH_STEP"))
       (output (getenv "BENCH_REPLY"))
       accepted payload handle started finished result delays diagnostics)
  (condition-case failure
      (progn
        ;; Fence copied live owners, preserving settled authentication evidence.
        (let ((directory (file-name-concat root ".mevedel/state/journal")))
          (when (file-directory-p directory)
            (dolist (file (directory-files-recursively directory "[0-9]\\{20\\}\\.claim\\'"))
              (unless (file-exists-p (concat (file-name-sans-extension file) ".outcome"))
                (let* ((record (mevedel-journal-claim--decode
                                (with-temp-buffer (insert-file-contents file) (buffer-string)) nil))
                       (claim (append (list :directory (file-name-directory file)) record)))
                  (when (> (plist-get record :expires-at) (float-time))
                    (unless (mevedel-journal-claim-settle claim 'cancelled "")
                      (error "Could not fence copied live claim"))))))))
        (if (equal step "generate")
          (progn
            ;; Build each pass with the real helper, staging settled history
            ;; between passes to avoid quadratic recovery during fixture setup.
            (dotimes (batch 80)
              (mevedel-test-memory-history-create workspace 1 4 16384)
              (dolist (relative '(".mevedel/journal" ".mevedel/state/journal/passes"
                                  ".mevedel/state/journal/decisions" ".mevedel/state/journal/writes"))
                (let ((destination (file-name-concat (getenv "BENCH_SHARDS")
                                                   (number-to-string batch) relative)))
                  (make-directory (file-name-directory destination) t)
                  (rename-file (file-name-concat root relative) destination))))
            (with-temp-file output (insert "{}"))
            (kill-emacs 0))
        ;; Fixture preparation and GC are outside the watched interval.
        (when (member step '("application" "publication"))
          (setq accepted (car (mevedel-test-memory-history-create workspace 1 1 128 t)))
          (when (equal step "publication")
            ;; A new unaccepted no-action review exercises the normal publisher.
            (let* ((claim (mevedel-journal-claim-acquire
                           (mevedel-memory-store--claim-directory workspace) 180))
                   (scope (mevedel-memory-scope-capture workspace)))
              (setq payload (list :prepared (mevedel-memory-store-prepare workspace claim scope nil "")
                                  :reply "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none"
                                  :entries nil :model "test:model" :references nil :notes nil)))))
        (garbage-collect)
        (setq mevedel-memory-decision--inhibit-recovery nil)
        (when (equal step "cleanup")
          (setq mevedel-journal-cleanup--inhibit-scheduling nil))
        (advice-add 'mevedel-telemetry-record-workspace :before
                    (lambda (_workspace event &rest props)
                      (when (and started (eq event 'event-loop-lag))
                        (push (plist-get props :delay-ms) delays))))
        (advice-add 'mevedel-journal-worker--read :filter-return
                    (lambda (reply)
                      (when (plist-get reply :error) (push (plist-get reply :error) diagnostics))
                      (when (and (equal step "cleanup")
                                 (not (and (integerp (plist-get reply :count))
                                           (numberp (plist-get reply :cleanup-at)))))
                        (push "Cleanup skipped admission" diagnostics))
                      reply))
        (advice-add 'mevedel-memory-decision--recovery-failed :before
                    (lambda (_key reason)
                      (push reason diagnostics)
                      (when (equal step "recovery")
                        (setq result (list :error reason) finished (float-time)))))
        (advice-add 'message :before
                    (lambda (format-string &rest arguments)
                      (when (and (stringp format-string)
                                 (string-match-p "journal cleanup failed" format-string))
                        (push (apply #'format-message format-string arguments) diagnostics))))
        (setq handle (mevedel-telemetry-memory-pass-start workspace "housekeeping-benchmark"))
        (unless (and handle mevedel-telemetry--lag-timer (not noninteractive))
          (error "Interactive heartbeat did not start"))
        (cl-labels
            ((complete (value) (setq result value finished (float-time))))
          (run-at-time
           .2 nil
           (lambda ()
             (setq started (float-time))
             (condition-case error-data
                 (pcase step
                   ("recovery"
                    (mevedel-memory-decision-schedule-recovery workspace (lambda () (complete '(:status recovered)))))
                   ("application"
                    (mevedel-memory-pass--apply-async
                     (list :workspace workspace :origin-buffer (current-buffer)) accepted #'complete))
                   ("publication"
                    (mevedel-journal-worker-start workspace 'memory-publish #'complete nil payload))
                   ("cleanup" (mevedel-journal-cleanup-schedule workspace t))
                   ("control"
                    ;; Busy Lisp, not sleep-for: sleeping dispatches timers.
                    (let ((until (+ (float-time) .12)))
                      (while (< (float-time) until)))
                    (complete '(:status injected)))
                   (_ (error "Unknown benchmark step")))
               (error (complete (list :error (error-message-string error-data)))))))
          (run-at-time
           .05 .05
           (lambda ()
             (when (and started (not finished) (equal step "cleanup")
                        (not (gethash root mevedel-journal-cleanup--pending))
                        (not (mevedel-journal-cleanup-active-p workspace)))
               (complete '(:status collected)))
             (when (and started (not finished) (> (- (float-time) started) 300))
               (complete '(:error "Completion deadline exceeded")))
             ;; One full fallback heartbeat after completion flushes callback lag.
             (when (and finished (> (- (float-time) finished) .2))
               (mevedel-telemetry-memory-pass-settle handle)
               (mevedel-telemetry--lag-stop)
               (let* ((success (and (not diagnostics) (not (plist-get result :error))
                                    (pcase step
                                      ("application" (and (= 1 (or (plist-get result :applied-count) 0))
                                                          (zerop (plist-get result :unapplied-count))
                                                          (zerop (plist-get result :held-count))))
                                      ("publication" (plist-get result :ok))
                                      (_ t))))
                      (row `((step . ,step) (noninteractive . ,json-false)
                            (success . ,(if success t json-false))
                            (diagnostics . ,(vconcat diagnostics))
                            (sampler . "production fallback timer; 100 ms; 1 ms reporting threshold")
                            (wall_ms . ,(* 1000 (- finished started)))
                            (longest_block_ms . ,(if delays (apply #'max delays) 0))
                            (sampled_total_blocked_ms . ,(apply #'+ delays))
                            (lag_samples_ms . ,(vconcat (nreverse delays)))
                            (result . ,(prin1-to-string result))
                            (messages . ,(when (get-buffer "*Messages*")
                                           (with-current-buffer "*Messages*" (buffer-string)))))))
                 (with-temp-file output (insert (json-encode row))))
               (kill-emacs 0)))))
        ;; Return to the real command loop. No wait loop drives idle timers.
        ))
    (error
     (with-temp-file output
       (insert (json-encode `((error . ,(error-message-string failure))))))
     (kill-emacs 1))))
'''


def interactive(command, environment, directory, timeout=360):
    """Run an independent terminal editor, retaining output for failures."""
    master, slave = pty.openpty()
    process = subprocess.Popen(command, stdin=slave, stdout=slave, stderr=slave,
                               env=environment, cwd=directory, start_new_session=True,
                               preexec_fn=lambda: fcntl.ioctl(slave, termios.TIOCSCTTY, 0))
    os.close(slave)
    transcript = bytearray()
    deadline = time.monotonic() + timeout
    try:
        while process.poll() is None:
            if time.monotonic() > deadline:
                raise TimeoutError("Interactive benchmark deadline exceeded")
            if select.select([master], [], [], .1)[0]:
                try:
                    transcript.extend(os.read(master, 65536))
                except OSError:
                    break
        process.wait(timeout=5)
        if process.returncode:
            raise RuntimeError(transcript.decode(errors="replace")[-4000:])
    finally:
        if process.poll() is None:
            os.killpg(process.pid, 9)
            process.wait()
        os.close(master)


def copy_state(source, destination, backup=None):
    """Copy journal evidence without symlinks or host-side claim mutations."""
    state = destination / ".mevedel"
    (state / "state").mkdir(parents=True)
    shutil.copy2(source / ".mevedel/workspace-id", state / "workspace-id")
    for origin, target in ((backup / "state-journal" if backup else source / ".mevedel/state/journal",
                            state / "state/journal"),
                           (backup / "journal" if backup else source / ".mevedel/journal",
                            state / "journal")):
        if origin.exists():
            if origin.is_symlink() or any(path.is_symlink() for path in origin.rglob("*")):
                raise RuntimeError(f"Symlinks are not allowed in copied state: {origin}")
            shutil.copytree(origin, target)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workspace", type=Path, help="Source workspace (read-only)")
    parser.add_argument("--backup", type=Path, help="Backup containing journal/ and state-journal/")
    parser.add_argument("--legacy-shim", type=Path, help="External diagnostic shim; labels this a legacy replay")
    parser.add_argument("--synthetic", action="store_true", help="Add valid 80-pass/320-proposal history")
    parser.add_argument("--self-test", action="store_true", help="Check a known 120 ms editor block")
    parser.add_argument("--output", type=Path, help="New JSON result file")
    args = parser.parse_args()
    if not args.self_test and (not args.workspace or not args.output):
        parser.error("--workspace and --output are required")
    if args.legacy_shim and not args.backup:
        parser.error("--legacy-shim requires --backup")
    if args.synthetic and args.backup:
        parser.error("--synthetic and --backup are separate scenarios")
    if args.output and args.output.exists():
        parser.error("Output already exists")
    source = args.workspace.resolve() if args.workspace else None
    elpas = list((ROOT / ".eask").glob("*/elpa"))
    if len(elpas) != 1:
        parser.error("Bootstrap .eask dependencies first")
    with tempfile.TemporaryDirectory(prefix="mevedel-housekeeping-", dir="/tmp") as name:
        temporary = Path(name)
        if args.self_test:
            fixture = temporary / "symlink-source"
            (fixture / ".mevedel/state/journal").mkdir(parents=True)
            (fixture / ".mevedel/workspace-id").write_text("fixture")
            host_claim = temporary / "must-survive.claim"
            host_claim.write_text("unchanged")
            (fixture / ".mevedel/state/journal/mutation").symlink_to(temporary, target_is_directory=True)
            try:
                copy_state(fixture, temporary / "rejected-copy")
            except RuntimeError:
                pass
            else:
                raise AssertionError("Symlinked claim scope was accepted")
            assert host_claim.read_text() == "unchanged"
        environment = COMMON["isolated_environment"](temporary / "environment")
        environment.update(TERM="xterm", TMPDIR=name, BENCH_WORKSPACE=str(temporary / "workspace"))
        packages = COMMON["freeze_dependencies"](elpas[0], temporary / "dependencies")
        snapshot = temporary / "sources"
        snapshot.mkdir()
        for path in ROOT.glob("mevedel*.el"):
            shutil.copy2(path, snapshot / path.name)
        shutil.copytree(ROOT / "prompts", snapshot / "prompts")
        shutil.copytree(ROOT / "test", snapshot / "test", ignore=shutil.ignore_patterns("*.elc"))
        build = temporary / "compiled"
        build.mkdir()
        environment.update(MEVEDEL_BENCH_SOURCE=str(snapshot), MEVEDEL_BENCH_BUILD=str(build))
        dependencies = [item for package in packages for item in ("-L", str(package))]
        batch = ["emacs", "--batch", "-Q", "--eval", f'(progn {COMMON["ISOLATE"]})'] + dependencies
        print("Compiling private source snapshot...", flush=True)
        COMMON["run"](batch + ["--eval", f'(progn {COMMON["COMPILE"]})'], temporary,
                      environment, timeout=300, stdout=subprocess.PIPE)
        script = temporary / "benchmark.el"
        script.write_text(LISP)
        baseline = temporary / "workspace"
        baseline.mkdir()
        if source:
            if args.synthetic:
                (baseline / ".mevedel").mkdir()
                shutil.copy2(source / ".mevedel/workspace-id", baseline / ".mevedel/workspace-id")
            else:
                copy_state(source, baseline, args.backup.resolve() if args.backup else None)
        paths = ["-L", str(build), "-L", str(snapshot / "test")] + dependencies
        if args.legacy_shim:
            shim = temporary / "legacy-shim.el"
            shutil.copy2(args.legacy_shim, shim)
            environment["BENCH_SHIM"] = str(shim)
            wrapper = temporary / "legacy-loader"
            wrapper.mkdir()
            (wrapper / "mevedel-journal-worker.el").write_text(
                f'(load {json.dumps(str(build / "mevedel-journal-worker.elc"))} nil t)\n'
                f'(load {json.dumps(str(shim))} nil t)\n')
            paths = ["-L", str(wrapper)] + paths

        def measure(step, sample):
            run = temporary / f"{step}-{sample}"
            run.mkdir()
            copied = run / "workspace"
            shutil.copytree(baseline, copied, symlinks=True)
            reply = run / "reply.json"
            env = dict(environment, BENCH_STEP=step, BENCH_REPLY=str(reply),
                       BENCH_SHARDS=str(run / "shards"))
            # The source workspace and every captured external target remain
            # read-only. Namespace aliases point workspace-local targets at copies.
            mounts = ["--bind", str(copied), str(baseline)]
            if source and (source / ".mevedel").exists():
                mounts += ["--bind", str(copied / ".mevedel"), str(source / ".mevedel")]
            command = ["bwrap", "--die-with-parent", "--unshare-net", "--ro-bind", "/", "/",
                       "--dev-bind", "/dev", "/dev",
                       "--bind", name, name] + mounts + ["--", "emacs", "-Q", "-nw",
                       "--eval", f'(progn {COMMON["ISOLATE"]})'] + paths + ["-l", str(script)]
            interactive(command, env, run, timeout=1800 if step == "generate" else 360)
            row = json.loads(reply.read_text())
            if "error" in row:
                raise RuntimeError(row["error"])
            if step == "generate":
                for shard in sorted((run / "shards").iterdir()):
                    shutil.copytree(shard, copied, dirs_exist_ok=True)
                journal = copied / ".mevedel/state/journal"
                counts = [len(list((journal / "passes").glob("*/accepted.el"))),
                          len(list((journal / "decisions").glob("*.el"))),
                          len(list((journal / "writes").glob("*.el")))]
                if counts != [80, 320, 320]:
                    raise RuntimeError(f"Synthetic fixture has unexpected pass/decision/write counts: {counts}")
                shutil.copytree(copied, baseline, dirs_exist_ok=True)
            return row

        if args.synthetic:
            print("Generating valid 80-pass history...", flush=True)
            measure("generate", 0)
        steps = ["control"] if args.self_test else ["recovery", "application", "publication", "cleanup"]
        journal = baseline / ".mevedel/state/journal"
        inventory = {"accepted_passes": len(list((journal / "passes").glob("*/accepted.el"))),
                     "decisions": len(list((journal / "decisions").glob("*.el"))),
                     "writes": len(list((journal / "writes").glob("*.el")))}
        samples = []
        for step in steps:
            for sample in range(1 if args.self_test else 3):
                print(f"Measuring {step}, sample {sample + 1}...", flush=True)
                try:
                    samples.append(measure(step, sample))
                except (RuntimeError, TimeoutError, subprocess.TimeoutExpired) as failure:
                    samples.append({"step": step, "success": False, "error": str(failure)})
        if args.self_test:
            assert 100 <= samples[0]["longest_block_ms"] <= 300, samples
        summary = []
        for step in steps:
            rows = [row for row in samples if row["step"] == step]
            row = {"step": step, "runs": len(rows)}
            if not all(item["success"] for item in rows):
                row["status"] = "not measured successfully"
                summary.append(row)
                print(f"- Scenario: {step}; status: {row['status']}; attempts: {len(rows)}")
                continue
            for metric in ("longest_block_ms", "sampled_total_blocked_ms", "wall_ms"):
                row[metric + "_median"] = statistics.median(item[metric] for item in rows)
                print(f"- Scenario: {step}; metric: {metric}; "
                      f"value: {row[metric + '_median']:.1f}; unit: ms; "
                      f"statistic: median; samples: {len(rows)}")
            summary.append(row)
        report = {"configuration": "legacy replay (validation shim)" if args.legacy_shim else
                  "valid synthetic history" if args.synthetic else "current-state copy",
                  "workspace": str(source), "backup": str(args.backup),
                  "legacy_shim_sha256": hashlib.sha256(args.legacy_shim.read_bytes()).hexdigest()
                  if args.legacy_shim else None,
                  "retention_days": 14, "cleanup_forced": True,
                  "baseline_inventory": inventory, "synthetic_note_bytes": 16384 if args.synthetic else None,
                  "measurement_lisp_sha256": hashlib.sha256(LISP.encode()).hexdigest(),
                  "head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
                  "source_manifest": COMMON["digest_files"](snapshot),
                  "dependency_manifest": COMMON["digest_files"](temporary / "dependencies"),
                  "blocking_definition": "Sum of heartbeat worst-delay samples; sampled lower bound, not exact CPU or total blocking",
                  "samples": samples, "summary": summary}
        if args.output:
            with args.output.open("x") as output:
                json.dump(report, output, indent=2)
        if not all(row["success"] for row in samples):
            raise SystemExit(1)


if __name__ == "__main__":
    main()
