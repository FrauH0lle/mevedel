#!/usr/bin/env python3
"""Focused checks for the isolated compiled instruction-edit benchmark runner."""

import argparse
import copy
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


SPEC = importlib.util.spec_from_file_location(
    "instruction_bench", Path(__file__).with_name("bench-instruction-edits.py"))
BENCH = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BENCH)


class RunnerTests(unittest.TestCase):
    def test_archive_and_current_are_independent_source_snapshots(self):
        with tempfile.TemporaryDirectory() as name:
            root = Path(name) / "repository"
            root.mkdir()
            env = os.environ | {"GIT_AUTHOR_NAME": "Runner Test",
                                "GIT_COMMITTER_NAME": "Runner Test",
                                "GIT_AUTHOR_EMAIL": "test@example.invalid",
                                "GIT_COMMITTER_EMAIL": "test@example.invalid"}
            def git(*args):
                return BENCH.run(["git", *args], root, env, stdout=subprocess.PIPE)

            git("init", "-q", "-b", "master")
            (root / "mevedel.el").write_text("baseline")
            (root / "benchmark").mkdir()
            (root / "prompts" / "tools").mkdir(parents=True)
            (root / "prompts" / "tools" / "ask.md").write_text("compile-time prompt")
            fixture = root / "benchmark" / BENCH.FIXTURE
            fixture.write_text("fixture")
            git("add", ".")
            git("-c", "commit.gpgsign=false", "commit", "-qm", "fixture")
            (root / "mevedel.el").write_text("working edit")
            (root / "mevedel.elc").write_text("stale bytecode")
            destination = Path(name) / "snapshot"
            destination.mkdir()
            sources = BENCH.snapshot_sources(root, destination, "master")
            self.assertEqual((sources["master"] / "mevedel.el").read_text(), "baseline")
            self.assertEqual((sources["current"] / "mevedel.el").read_text(), "working edit")
            self.assertFalse((sources["current"] / "mevedel.elc").exists())
            for source in sources.values():
                self.assertEqual((source / BENCH.FIXTURE).read_bytes(), fixture.read_bytes())
                self.assertEqual((source / "prompts" / "tools" / "ask.md").read_text(),
                                 "compile-time prompt")
            self.assertEqual((root / "mevedel.el").read_text(), "working edit")
            self.assertFalse((root / ".git" / "worktrees").exists())

    def test_frozen_dependencies_exclude_archive_cache_and_keep_hashes(self):
        with tempfile.TemporaryDirectory() as name:
            root = Path(name)
            package = root / "elpa" / "example-1.0"
            package.mkdir(parents=True)
            (package / "example-pkg.el").write_text("descriptor")
            (package / "example.el").write_text("source")
            (package / "example.elc").write_text("bytecode")
            archives = package.parent / "archives"
            archives.mkdir()
            (archives / "archive-contents").write_text("mutable metadata")
            frozen = root / "frozen"
            paths = BENCH.freeze_dependencies(package.parent, frozen)
            self.assertEqual(paths, [frozen / package.name])
            self.assertFalse((frozen / "archives").exists())
            manifest = BENCH.digest_files(frozen)
            digest = BENCH.tree_digest(manifest)
            (package / "example.el").write_text("concurrent dependency edit")
            self.assertEqual(BENCH.digest_files(frozen), manifest)
            self.assertEqual(BENCH.tree_digest(BENCH.digest_files(frozen)), digest)
            (paths[0] / "example.el").write_text("changed snapshot")
            self.assertNotEqual(BENCH.digest_files(frozen), manifest)

    def test_isolation_removes_inherited_emacs_and_eask_settings(self):
        with tempfile.TemporaryDirectory() as name:
            with patch.dict(os.environ, {"EMACSLOADPATH": "/foreign/code", "EASK_PATH": "x",
                                         "PKG_EXECPATH": "x"}):
                env = BENCH.isolated_environment(Path(name))
            for key in ("EMACSLOADPATH", "EASK_PATH", "PKG_EXECPATH"):
                self.assertNotIn(key, env)
            for key in ("HOME", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME",
                        "XDG_STATE_HOME", "XDG_RUNTIME_DIR"):
                self.assertTrue(Path(env[key]).is_relative_to(name))
                self.assertEqual(Path(env[key]).stat().st_mode & 0o777, 0o700)

    def test_medians_gate_every_scenario_without_dropping_outliers(self):
        report = {"sample_count": 5, "warmups": 5, "keys": 2000,
                  "scenarios": [{"references": n, "samples": [
                      {"us_per_key": value, "apply_entries": 0, "gcs": 0}
                      for value in (1, 2, 3, 4, 1000)]} for n in (1, 50, 200)]}
        reports = {"master": copy.deepcopy(report), "current": copy.deepcopy(report)}
        reports["current"]["scenarios"][2]["samples"][2]["us_per_key"] = 3.6
        rows = BENCH.summarize(reports, 1.15)
        self.assertEqual([r["passes"] for r in rows], [True, True, False])
        self.assertEqual(rows[2]["master_us_per_key_median"], 3)
        self.assertAlmostEqual(rows[2]["current_over_master"], 1.2)
        self.assertEqual(len(reports["current"]["scenarios"][2]["samples"]), 5)
        reports["current"]["scenarios"][0]["samples"].pop()
        with self.assertRaisesRegex(RuntimeError, "Invalid benchmark samples"):
            BENCH.summarize(reports)

    def test_interior_edits_reject_function_undo_entries(self):
        report = {"sample_count": 5, "warmups": 5, "keys": 2000,
                  "scenarios": [{"references": n, "samples": [
                      {"us_per_key": 1, "apply_entries": 0, "gcs": 0}
                      for _ in range(5)]} for n in (1, 50, 200)]}
        reports = {"master": copy.deepcopy(report), "current": copy.deepcopy(report)}
        reports["current"]["scenarios"][1]["samples"][2]["apply_entries"] = 2
        with self.assertRaisesRegex(RuntimeError, "Interior edits added function undo entries"):
            BENCH.summarize(reports, 1.15)

    def test_ratio_rejects_nonfinite_and_nonpositive_values(self):
        for value in ("nan", "inf", "-1", "0"):
            with self.subTest(value=value), self.assertRaises(argparse.ArgumentTypeError):
                BENCH.positive_ratio(value)
        self.assertEqual(BENCH.positive_ratio("1.15"), 1.15)

    def test_subprocess_failure_retains_stdout_and_stderr(self):
        with tempfile.TemporaryDirectory() as name:
            with self.assertRaisesRegex(RuntimeError, "(?s)exited 7.*out.*err"):
                BENCH.run([sys.executable, "-c",
                           "import sys; print('out'); print('err', file=sys.stderr); sys.exit(7)"],
                          name, stdout=subprocess.PIPE)

    @unittest.skipUnless(shutil.which("emacs"), "Emacs is required for compilation checks")
    def test_real_child_compiles_runs_and_rejects_wrong_provenance(self):
        fixture = r''';;; fixture.el --- Runner probe -*- lexical-binding: t; -*-
(require 'json)
(defun mevedel-instruction-bench-run (directory)
  "Probe real child compilation and isolation in DIRECTORY."
  (unless (and (byte-code-function-p (symbol-function 'mevedel-instruction-bench-run))
               (not native-comp-jit-compilation)
               (string-prefix-p (getenv "HOME") (car native-comp-eln-load-path)))
    (error "Child is not isolated bytecode"))
  (princ (json-encode
          `((hotpaths . [((symbol . "mevedel-instruction-bench-run")
                          (loaded . ,(symbol-file 'mevedel-instruction-bench-run 'defun)))])
            (executed . ,directory)))))
(provide 'mevedel-instruction-edit-bench)
'''
        with tempfile.TemporaryDirectory() as name:
            root = Path(name)
            source = root / "current"
            source.mkdir()
            path = source / BENCH.FIXTURE
            path.write_text(fixture)
            dependencies = root / "dependencies"
            dependencies.mkdir()
            report = BENCH.measure("emacs", source, root / "good", [], dependencies)
            self.assertTrue(report["executed"].endswith("/good/compiled"))
            self.assertEqual(report["source_manifest"], BENCH.digest_files(source))
            self.assertIn("source_sha256", report["hotpaths"][0])
            path.write_text(fixture.replace(
                "(symbol-file 'mevedel-instruction-bench-run 'defun)",
                '"/foreign/mevedel-instruction-edit-bench.elc"'))
            with self.assertRaisesRegex(RuntimeError, "Unexpected hotpath provenance"):
                BENCH.measure("emacs", source, root / "foreign", [], dependencies)
            path.write_text(fixture.replace("(require 'json)",
                                            "(require 'json)\n(defun bad () undeclared-variable)"))
            with self.assertRaisesRegex(RuntimeError, "Compilation failed"):
                BENCH.measure("emacs", source, root / "warning", [], dependencies)


if __name__ == "__main__":
    unittest.main()
