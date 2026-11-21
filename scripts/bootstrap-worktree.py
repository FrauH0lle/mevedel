#!/usr/bin/env python3
"""Prepare a niwa checkout for Eask tests without changing dependencies."""

import argparse
import hashlib
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


# Run inside Eask, not the interactive editor.  Requiring the libraries catches
# incomplete installs that merely finding an elpa directory would miss.
PROBE = r'''
(progn
  (require 'package)
  (when (version< emacs-version "31.1")
    (error "Emacs >=31.1 is required, found %s" emacs-version))
  (dolist (dependency (append eask-depends-on eask-depends-on-dev))
    (let ((name (intern (car dependency)))
          (minimum (or (cadr dependency) "0")))
      (unless (package-installed-p name (version-to-list minimum))
        (error "Missing or outdated test dependency: %s >=%s" name minimum))))
  (princ (format "Emacs: %s (%s)\n" emacs-version invocation-directory))
  (dolist (entry '((gptel-request . gptel) (acp . acp) (yaml . yaml)
                   (orderless . orderless) (websocket . websocket)
                   (qrencode . qrencode) (mcp . mcp) (org . org)))
    (let* ((library (car entry))
           (path (locate-library (symbol-name library)))
           (descriptor (cadr (assq (cdr entry) package-alist))))
      (unless path (error "Missing test dependency: %s" library))
      (require library)
      (let ((source (if (string-suffix-p ".elc" path)
                        (substring path 0 -1) path)))
        (when (and (not (file-readable-p source))
                   (file-readable-p (concat source ".gz")))
          (setq source (concat source ".gz")))
        (unless (file-readable-p source)
          (error "Cannot report dependency source: %s" source))
        (princ
         (format "%s: version=%s loaded=%s source=%s sha256=%s\n"
                 library
                 (if descriptor
                     (package-version-join (package-desc-version descriptor))
                   (if (eq library 'org) (org-version) "built-in"))
                 (file-truename path) (file-truename source)
                 (with-temp-buffer
                   (insert-file-contents-literally source)
                   (secure-hash 'sha256 (current-buffer))))))))
  (princ "Worktree bootstrap: ready\n"))
'''


def run(argv, cwd, **kwargs):
    """Run ARGV in CWD, preserving failures and captured diagnostics."""
    result = subprocess.run(argv, cwd=cwd, text=True, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, **kwargs)
    if result.returncode:
        raise RuntimeError(f"{' '.join(map(str, argv[:3]))} exited "
                           f"{result.returncode}:\n{result.stdout}")
    return result.stdout.strip()


def pin_affected_base(root, base):
    """Pin BASE or the branch's recorded creation commit in this worktree."""
    config = ["git", "config", "--worktree"]
    key = "niwa.affectedBase"
    if base:
        commit = run(["git", "rev-parse", "--verify", "--end-of-options",
                      f"{base}^{{commit}}"], root)
    else:
        if run([*config, "--default=", "--get", key], root):
            return
        branch = run(["git", "rev-parse", "--symbolic-full-name", "HEAD"], root)
        if not branch.startswith("refs/heads/"):
            return
        log = run(["git", "reflog", "show", "--format=%H%x09%gs", branch], root).splitlines()
        if not log or "\tbranch: Created from " not in log[-1]:
            return
        commit = log[-1].split("\t", 1)[0]
    run([*config, key, commit], root)
    print(f"Affected base: {commit}", flush=True)


def prepare(root, base=None):
    """Reuse the primary checkout's cache when safe, then prove loadability."""
    root = root.resolve(strict=True)
    top = Path(run(["git", "rev-parse", "--show-toplevel"], root)).resolve()
    if root != top or not (root / "Eask").is_file():
        raise RuntimeError("Expected a niwa repository root containing Eask")
    common = Path(run(["git", "rev-parse", "--git-common-dir"], root))
    if not common.is_absolute():
        common = root / common
    primary = common.resolve().parent
    cache = root / ".eask"
    shared = primary / ".eask"
    if cache.is_symlink() and not cache.is_dir():
        raise RuntimeError(f"Broken .eask symlink: {cache}")
    if not cache.exists() and shared.is_dir():
        cache.symlink_to(shared, target_is_directory=True)
    if not cache.is_dir():
        raise RuntimeError("No .eask dependency environment is available")
    # A slash suffix ignores directories but not symlinks.  Do not modify the
    # common info/exclude: it belongs to all concurrently active worktrees.
    try:
        run(["git", "check-ignore", "-q", ".eask"], root)
    except RuntimeError as error:
        raise RuntimeError(".eask must be ignored as a directory or symlink; "
                           "use `.eask`, not `.eask/`, in .gitignore.\n"
                           f"{error}") from error
    if root != primary:
        # Enabling worktreeConfig changes the meaning of shared core.worktree
        # and core.bare=true.  Refuse those layouts rather than migrate the
        # primary checkout's settings behind its owner's back.
        configuration = run(["git", "config", "--local", "--list"], root).splitlines()
        bare = run(["git", "config", "--local", "--type=bool", "--default=false",
                    "--get", "core.bare"], root)
        if bare == "true" or any(line.startswith("core.worktree=")
                                 for line in configuration):
            raise RuntimeError("Cannot safely enable worktree hooks with shared "
                               "core.bare=true or core.worktree; move these settings "
                               "to the primary checkout's config.worktree first")
        run(["git", "config", "--local", "extensions.worktreeConfig", "true"], root)
        run(["git", "config", "--worktree", "core.hooksPath", ".githooks"], root)
        print("Worktree hook: .githooks (worktree-only)", flush=True)
        pin_affected_base(root, base)
    elif base:
        raise RuntimeError("--base can only pin a linked worktree")
    command = ([shutil.which("eask")] if shutil.which("eask") else
               ["npx", "--offline", "--silent", "--no-install", "@emacs-eask/cli"])
    environment = os.environ.copy()
    # A standalone Eask parent exports its own executable path.  A new Eask
    # launch must resolve its entry point independently, including --version.
    environment.pop("PKG_EXECPATH", None)
    print(f"Worktree: {root}", flush=True)
    print(f"HEAD: {run(['git', 'rev-parse', 'HEAD'], root)}", flush=True)
    print(f"Eask manifest sha256: {hashlib.sha256((root / 'Eask').read_bytes()).hexdigest()}",
          flush=True)
    print(f".eask: {cache.resolve()} ({'symlink reuse' if cache.is_symlink() else 'local'})",
          flush=True)
    print(f"Eask: {run(command + ['--version'], root, env=environment)}", flush=True)
    # npx needs its normal cache to find Eask offline.  Isolate Emacs itself,
    # before loading any dependency, including its startup native cache.
    with tempfile.TemporaryDirectory(prefix="niwa-bootstrap-") as home:
        environment["NIWA_BOOTSTRAP_HOME"] = home
        isolation = r'''
(let ((home (getenv "NIWA_BOOTSTRAP_HOME")))
  (setenv "HOME" home)
  (dolist (name '("XDG_CACHE_HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME"
                  "XDG_STATE_HOME" "XDG_RUNTIME_DIR"))
    (setenv name (expand-file-name name home)))
  (when (native-comp-available-p)
    (startup-redirect-eln-cache (expand-file-name "eln" home))))
'''
        checked_probe = (f"(condition-case error {PROBE} "
                         '(error (princ (format "Dependency probe failed: %s\\n" '
                         '(error-message-string error))) (kill-emacs 1)))')
        print(run(command + ["emacs", "--batch", "--eval", isolation,
                             "--eval", checked_probe], root, env=environment), flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("worktree", nargs="?", type=Path, default=Path.cwd())
    parser.add_argument("--base", help="pin this linked worktree's affected base to REV")
    args = parser.parse_args()
    try:
        prepare(args.worktree, args.base)
    except (OSError, RuntimeError) as error:
        print(f"Worktree bootstrap failed: {error}", file=sys.stderr)
        print("No dependencies were installed or upgraded. Recovery must use the "
              "failing worktree's Eask manifest. If .eask is shared, get approval "
              "to change shared dependencies or use an independent .eask directory. "
              "In the failing worktree, run `npx @emacs-eask/cli install-deps --dev` "
              "(upgrade stale packages explicitly), then retry "
              "`python3 scripts/bootstrap-worktree.py`.",
              file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
