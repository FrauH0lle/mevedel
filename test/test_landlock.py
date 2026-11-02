"""Standard-library tests for the Landlock Unix socket launcher.

The kernel cases run the launcher in a child process against servers this
process owns, outside the child's Landlock domain, and skip on kernels
without Landlock ABI 9."""

import importlib.util
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


SCRIPT = Path(__file__).resolve().parents[1] / "scripts/mevedel-landlock.py"
SPEC = importlib.util.spec_from_file_location("landlock", SCRIPT)
landlock = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(landlock)

# Prints one REACH or block line per target; "@name" is an abstract socket.
# Then creates its own pathname and abstract servers and connects to both.
CONNECT = """
import socket, sys
def reach(address):
    try:
        socket.socket(socket.AF_UNIX).connect(address)
        return "REACH"
    except OSError:
        return "block"
for target in sys.argv[2:]:
    address = "\\0" + target[1:] if target.startswith("@") else target
    print(reach(address), target)
for address in (sys.argv[1], "\\0" + sys.argv[1]):
    server = socket.socket(socket.AF_UNIX)
    server.bind(address)
    server.listen(1)
    print(reach(address), "own")
"""


class ArgumentTests(unittest.TestCase):
    def test_usage_errors(self):
        for argv in (["l"], ["l", "exec"], ["l", "exec", "--"], ["l", "exec", "/a", "true"],
                     ["l", "run", "--", "true"]):
            with self.subTest(argv=argv), patch.object(sys, "stderr"):
                self.assertEqual(2, landlock.main(argv))

    def test_restriction_failure_stops_before_command(self):
        with patch.object(landlock, "restrict", side_effect=OSError(22, "Invalid argument")), \
             patch.object(landlock.os, "execvp") as execvp, patch.object(sys, "stderr"):
            self.assertEqual(125, landlock.main(["l", "exec", "/a", "--", "true"]))
        execvp.assert_not_called()

    def test_allowed_paths_and_command_are_separated(self):
        with patch.object(landlock, "restrict") as restrict, \
             patch.object(landlock.os, "execvp") as execvp:
            landlock.main(["l", "exec", "/a", "/b", "--", "cmd", "--", "x"])
        restrict.assert_called_once_with(["/a", "/b"])
        execvp.assert_called_once_with("cmd", ["cmd", "--", "x"])


@unittest.skipUnless(landlock.abi() >= landlock.RESOLVE_UNIX_ABI,
                     "Landlock ABI 9 is unavailable")
class KernelTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / "allowed").mkdir()

    def server(self, address):
        server = socket.socket(socket.AF_UNIX)
        self.addCleanup(server.close)
        server.bind(address)
        server.listen(1)

    def run_child(self, launcher, targets):
        own = str(self.root / "own")
        result = subprocess.run(
            launcher + [sys.executable, "-I", "-c", CONNECT, own] + targets,
            capture_output=True, text=True, timeout=30)
        self.assertEqual(0, result.returncode, result.stderr)
        return result.stdout.splitlines()

    def test_only_allowed_host_sockets_are_reachable(self):
        allowed = str(self.root / "allowed" / "socket")
        host = str(self.root / "host")
        abstract = f"mevedel-landlock-test-{os.getpid()}"
        self.server(allowed)
        self.server(host)
        self.server("\0" + abstract)
        targets = [allowed, host, "@" + abstract]
        launcher = [sys.executable, "-I", "-S", str(SCRIPT), "exec",
                    str(self.root / "allowed"), str(self.root / "missing"), "--"]
        self.assertEqual(
            [f"REACH {allowed}", f"block {host}", f"block @{abstract}",
             "REACH own", "REACH own"],
            self.run_child(launcher, targets))
        # Negative control: the same child without the launcher.
        os.unlink(self.root / "own")
        self.assertEqual(
            [f"REACH {allowed}", f"REACH {host}", f"REACH @{abstract}",
             "REACH own", "REACH own"],
            self.run_child([], targets))


if __name__ == "__main__":
    unittest.main()
