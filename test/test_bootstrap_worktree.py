#!/usr/bin/env python3
"""CLI integration tests for offline worktree preparation (real Git fixtures)."""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "scripts/bootstrap-worktree.py"


class BootstrapTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="mevedel-bootstrap-test-")
        self.addCleanup(self.temporary.cleanup)
        self.base = Path(self.temporary.name)
        self.root = self.base / "primary checkout"
        self.root.mkdir()
        self.git("init")
        self.git("config", "user.email", "test@example.invalid")
        self.git("config", "user.name", "Test")
        (self.root / "Eask").write_text('(depends-on "gptel")\n')
        (self.root / ".gitignore").write_text(".eask\n")
        self.git("add", "Eask", ".gitignore")
        self.git("commit", "-m", "fixture")
        (self.root / ".eask").mkdir()
        self.tree = self.base / "linked checkout"
        self.git("worktree", "add", "--detach", str(self.tree))
        binary = self.base / "bin"
        binary.mkdir()
        fake = binary / "eask"
        fake.write_text(f'''#!{sys.executable}
import os, sys
if os.path.basename(sys.argv[0]) == "npx":
    assert sys.argv[1:5] == ["--offline", "--silent", "--no-install", "@emacs-eask/cli"]
    del sys.argv[1:5]
if sys.argv[1:] == ["--version"]:
    print("test-eask-1.0")
elif os.getenv("BOOTSTRAP_FAIL"):
    print("Missing test dependency: gptel-request")
    sys.exit(23)
else:
    assert sys.argv[1:3] == ["emacs", "--batch"]
    assert "gptel-request" in sys.argv[-1]
    print("Worktree bootstrap: ready")
''')
        fake.chmod(0o755)
        self.environment = dict(os.environ, PATH=f"{binary}:{os.environ['PATH']}")

    def git(self, *args):
        return subprocess.run(["git", *args], cwd=self.root, check=True,
                              text=True, stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT).stdout

    def bootstrap(self, root=None, success=True):
        result = subprocess.run([sys.executable, str(SCRIPT), str(root or self.tree)],
                                env=self.environment, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        if success:
            self.assertEqual(result.returncode, 0, result.stdout)
            self.assertIn("Worktree bootstrap: ready", result.stdout)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn("Worktree bootstrap failed:", result.stdout)
            self.assertNotIn("Worktree bootstrap: ready", result.stdout)
        return result.stdout

    def test_reuse_is_idempotent_and_ignored(self):
        output = self.bootstrap()
        cache = self.tree / ".eask"
        self.assertTrue(cache.is_symlink())
        self.assertEqual(cache.resolve(), self.root / ".eask")
        self.assertIn("test-eask-1.0", output)
        self.assertIn("Eask manifest sha256:", output)
        self.assertIn("symlink reuse", self.bootstrap())
        self.assertEqual(self.git("-C", str(self.tree), "status", "--short"), "")
        self.assertIn(".eask", self.git("-C", str(self.tree), "check-ignore", ".eask"))

    def test_existing_directory_is_not_replaced(self):
        (self.tree / ".eask").mkdir()
        self.assertIn("(local)", self.bootstrap())
        self.assertFalse((self.tree / ".eask").is_symlink())

    def test_npx_fallback_is_offline_and_cannot_install(self):
        binary = self.base / "bin"
        (binary / "eask").rename(binary / "npx")
        (binary / "git").symlink_to(shutil.which("git"))
        self.environment["PATH"] = str(binary)
        self.assertIn("test-eask-1.0", self.bootstrap())

    def test_primary_checkout(self):
        self.assertIn("(local)", self.bootstrap(self.root))

    def test_missing_cache_has_install_recovery(self):
        (self.root / ".eask").rmdir()
        output = self.bootstrap(success=False)
        self.assertIn("No .eask dependency environment", output)
        self.assertIn("install-deps --dev", output)
        self.assertIn("In the failing worktree, run", output)
        self.assertFalse((self.tree / ".eask").exists())

    def test_manifest_script_changes_do_not_block_reuse(self):
        with (self.tree / "Eask").open("a") as manifest:
            manifest.write('(script "check" "true")\n')
        self.bootstrap()
        self.assertTrue((self.tree / ".eask").is_symlink())

    def test_existing_shared_link_accepts_manifest_script_changes(self):
        self.bootstrap()
        with (self.tree / "Eask").open("a") as manifest:
            manifest.write('(script "check" "true")\n')
        self.bootstrap()
        self.assertTrue((self.tree / ".eask").is_symlink())

    def test_broken_link_is_not_replaced(self):
        cache = self.tree / ".eask"
        cache.symlink_to(self.base / "absent")
        self.assertIn("Broken .eask symlink", self.bootstrap(success=False))
        self.assertEqual(cache.readlink(), self.base / "absent")

    def test_unignored_symlink_fails(self):
        (self.tree / ".gitignore").write_text(".eask/\n")
        output = self.bootstrap(success=False)
        self.assertIn("use `.eask`, not `.eask/`", output)
        self.assertIn("check-ignore", output)
        self.assertIn("exited 1", output)

    def test_probe_failure_is_not_success(self):
        self.environment["BOOTSTRAP_FAIL"] = "1"
        output = self.bootstrap(success=False)
        self.assertIn("exited 23", output)
        self.assertIn("Missing test dependency: gptel-request", output)
        self.assertIn("No dependencies were installed", output)

    def test_subdirectory_is_rejected(self):
        subdirectory = self.tree / "subdirectory"
        subdirectory.mkdir()
        self.assertIn("repository root containing Eask",
                      self.bootstrap(subdirectory, success=False))


if __name__ == "__main__":
    unittest.main()
