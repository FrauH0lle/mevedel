"""Standard-library tests for the Landlock Unix socket launcher.

The kernel cases run the launcher in a child process against servers this
process owns, outside the child's Landlock domain, and skip on kernels
without Landlock ABI 9."""

import importlib.util
import ctypes
import errno
import io
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

FILE_OPERATIONS = """
import os, sys
from pathlib import Path
root = Path(sys.argv[1])
allowed = root / "allowed"
other = root / "other"
directories = [allowed / "a", allowed / "b", other / "a", other / "b"]
for directory in directories:
    directory.mkdir(parents=True, exist_ok=True)
for source_dir, target_dir in ((directories[0], directories[1]),
                               (directories[2], directories[3]),
                               (directories[0], directories[2]),
                               (directories[2], directories[0])):
    assert source_dir.stat().st_dev == target_dir.stat().st_dev
    for name, operation in (("rename", os.rename), ("replace", os.replace),
                            ("link", os.link)):
        source, target = source_dir / "source", target_dir / "target"
        source.write_bytes(b"new contents")
        if name == "replace":
            target.write_bytes(b"old contents")
        operation(source, target)
        assert target.read_bytes() == b"new contents"
        if name == "link":
            assert source.stat().st_ino == target.stat().st_ino
            source.unlink()
        else:
            assert not source.exists()
        target.unlink()
print("file operations succeeded")
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
    def test_root_rule_failures_stop_before_exec_and_close_descriptors(self):
        syscall = landlock.LIBC.syscall

        def fail_add_rule(number, *arguments):
            if number.value == landlock.SYS_ADD_RULE:
                ctypes.set_errno(errno.EIO)
                return -1
            return syscall(number, *arguments)

        for failure in (patch.object(landlock.os, "open",
                                     side_effect=OSError(errno.EACCES, "root denied")),
                        patch.object(landlock.LIBC, "syscall", side_effect=fail_add_rule)):
            with self.subTest(failure=failure.attribute):
                before = set(os.listdir("/proc/self/fd"))
                with failure, patch.object(landlock.os, "execvp") as execvp, \
                        patch.object(sys, "stderr", new_callable=io.StringIO) as stderr:
                    self.assertEqual(125, landlock.main(["l", "exec", "--", "true"]))
                execvp.assert_not_called()
                self.assertIn("cannot restrict Unix sockets", stderr.getvalue())
                self.assertEqual(before, set(os.listdir("/proc/self/fd")))


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

    def test_regular_file_operations_do_not_require_socket_access(self):
        socket_path = str(self.root / "allowed" / "socket")
        self.server(socket_path)
        for allowed in ([], [socket_path]):
            with self.subTest(allowed=allowed):
                result = subprocess.run(
                    [sys.executable, "-I", "-S", str(SCRIPT), "exec", *allowed,
                     "--", sys.executable, "-I", "-c", FILE_OPERATIONS,
                     str(self.root)], capture_output=True, text=True, timeout=30)
                self.assertEqual(0, result.returncode, result.stderr)
                self.assertEqual("file operations succeeded\n", result.stdout)

    def test_directory_arguments_do_not_authorize_host_sockets(self):
        host = str(self.root / "allowed" / "socket")
        self.server(host)
        launcher = [sys.executable, "-I", "-S", str(SCRIPT), "exec",
                    str(self.root / "allowed"), "--"]
        self.assertEqual([f"block {host}", "REACH own", "REACH own"],
                         self.run_child(launcher, [host]))

    def test_moving_host_sockets_cannot_gain_access(self):
        allowed = str(self.root / "allowed" / "socket")
        host = str(self.root / "host")
        self.server(allowed)
        self.server(host)
        program = """
import errno, os, socket, sys
from pathlib import Path
root = Path(sys.argv[1])
def blocked(path):
    with socket.socket(socket.AF_UNIX) as client:
        try:
            client.connect(str(path))
        except OSError as error:
            assert error.errno == errno.EACCES, error
        else:
            raise AssertionError("host socket gained access: " + str(path))
host = root / "host"
moved = root / "allowed" / "moved"
linked = root / "allowed" / "linked"
os.rename(host, moved)
blocked(moved)
os.link(moved, linked)
blocked(linked)
# Replacing the approved name must not transfer the approved inode's rights.
os.replace(linked, root / "allowed" / "socket")
blocked(root / "allowed" / "socket")
print("relocated sockets stay blocked")
"""
        result = subprocess.run(
            [sys.executable, "-I", "-S", str(SCRIPT), "exec", allowed, "--",
             sys.executable, "-I", "-c", program, str(self.root)],
            capture_output=True, text=True, timeout=30)
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual("relocated sockets stay blocked\n", result.stdout)

    def test_only_allowed_host_sockets_are_reachable(self):
        allowed = str(self.root / "allowed" / "socket")
        host = str(self.root / "host")
        abstract = f"mevedel-landlock-test-{os.getpid()}"
        self.server(allowed)
        self.server(host)
        self.server("\0" + abstract)
        targets = [allowed, host, "@" + abstract]
        launcher = [sys.executable, "-I", "-S", str(SCRIPT), "exec",
                    allowed, str(self.root / "missing"), "--"]
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
