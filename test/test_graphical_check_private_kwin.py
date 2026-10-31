"""Display-free tests for the private-KWin graphical-check runner.

Fake `dbus-run-session' and `kwin_wayland' executables stand in for the
real ones; each leaves a stray process behind (one detached with setsid) so
the tests can prove the runner removes every descendant on each exit path.
"""

from contextlib import redirect_stderr
import importlib.util
import io
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "scripts/graphical-check-private-kwin.py"
SPEC = importlib.util.spec_from_file_location("private_kwin", SCRIPT)
kwin = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(kwin)

FAKE_DBUS = """#!/bin/sh
sleep 300 & echo $! >> "$KWIN_TEST_DIR/pids"
[ "$1" = -- ] && shift
"$@"
"""

FAKE_KWIN = """#!/bin/sh
printf '%s\\n' "$@" > "$KWIN_TEST_DIR/kwin-args"
setsid sleep 300 & echo $! >> "$KWIN_TEST_DIR/pids"
while [ $# -gt 0 ]; do
  [ "$1" = --exit-with-session ] && client=$2
  shift
done
"$client"
exit 0
"""

RUNNER = """import importlib.util, sys
spec = importlib.util.spec_from_file_location("k", sys.argv[1])
k = importlib.util.module_from_spec(spec); spec.loader.exec_module(k)
sys.exit(k.run_session(["sh", "-c", sys.argv[2]], int(sys.argv[3])))
"""


def alive(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    return True


class ArgumentTests(unittest.TestCase):
    def test_forwards_everything_after_separator(self):
        options = kwin.parse_arguments(
            ["--session-timeout", "60", "--", "--grammar-dir", "/g", "--timeout", "9"])
        self.assertEqual(options.session_timeout, 60)
        self.assertEqual(options.check_args, ["--grammar-dir", "/g", "--timeout", "9"])
        self.assertEqual(kwin.check_command(options.check_args)[-5:],
                         ["--", "--grammar-dir", "/g", "--timeout", "9"])

    def test_defaults_and_rejections(self):
        options = kwin.parse_arguments([])
        self.assertEqual((options.session_timeout, options.check_args), (900, []))
        for argv in (["--grammar-dir", "/g"], ["--session-timeout", "0"]):
            with self.assertRaises(SystemExit), redirect_stderr(io.StringIO()):
                kwin.parse_arguments(argv)


class EnvironmentTests(unittest.TestCase):
    def test_private_roots_and_no_host_display_or_bus(self):
        base = {"HOME": "/home/u", "DISPLAY": ":0", "WAYLAND_DISPLAY": "wayland-0",
                "DBUS_SESSION_BUS_ADDRESS": "unix:path=/run/bus",
                "XAUTHORITY": "/home/u/.Xauthority", "PATH": "/bin"}
        with tempfile.TemporaryDirectory() as private:
            env = kwin.private_environment(base, private)
            for name in ("DISPLAY", "WAYLAND_DISPLAY", "DBUS_SESSION_BUS_ADDRESS",
                         "XAUTHORITY"):
                self.assertNotIn(name, env)
            for name in ("HOME", "XDG_CONFIG_HOME", "XDG_CACHE_HOME",
                         "XDG_DATA_HOME", "XDG_STATE_HOME", "XDG_RUNTIME_DIR"):
                path = Path(env[name])
                self.assertEqual(path.parent, Path(private))
                self.assertEqual(path.stat().st_mode & 0o777, 0o700)
            self.assertEqual(env["NPM_CONFIG_CACHE"], "/home/u/.npm")
            self.assertEqual((env["KWIN_COMPOSE"], env["PATH"]), ("O2", "/bin"))
        self.assertEqual(base["DISPLAY"], ":0")

    def test_kwin_is_unlocked_and_client_quotes_arguments(self):
        with tempfile.TemporaryDirectory() as private:
            client, status = kwin.write_client(
                private, ["printf", "%s|", "a b", "it's"], Path(private) / "sock et")
            command = kwin.kwin_command(client)
            self.assertIn("--no-lockscreen", command)
            self.assertEqual(command[-2:], ["--exit-with-session", str(client)])
            subprocess.run([str(client)], check=True, env={"PATH": os.environ["PATH"]},
                           stdout=subprocess.DEVNULL)
            self.assertEqual(status.read_text().strip(), "0")
            self.assertIn("WAYLAND_DISPLAY='", client.read_text())


class SessionTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.dir = Path(temp.name)
        bin_dir = self.dir / "bin"
        bin_dir.mkdir()
        for name, text in (("dbus-run-session", FAKE_DBUS), ("kwin_wayland", FAKE_KWIN)):
            (bin_dir / name).write_text(text)
            (bin_dir / name).chmod(0o755)
        self.env = os.environ | {"PATH": f"{bin_dir}:{os.environ['PATH']}",
                                 "KWIN_TEST_DIR": str(self.dir)}
        self.addCleanup(self.kill_strays)

    def pids(self):
        path = self.dir / "pids"
        return [int(p) for p in path.read_text().split()] if path.exists() else []

    def kill_strays(self):
        for pid in self.pids():
            if alive(pid):
                os.kill(pid, signal.SIGKILL)

    def start(self, client, timeout=30):
        return subprocess.Popen([sys.executable, "-I", "-c", RUNNER, str(SCRIPT),
                                 client, str(timeout)], env=self.env)

    def assert_cleaned(self):
        pids = self.pids()
        self.assertEqual(len(pids), 3, "bus, detached KWin child and client")
        self.assertEqual([pid for pid in pids if alive(pid)], [])
        home = Path((self.dir / "home").read_text().strip())
        self.assertFalse(home.parent.exists(), "private directory removed")

    def client(self, tail):
        return (f'echo "$HOME" > "$KWIN_TEST_DIR/home"; '
                f'echo "$WAYLAND_DISPLAY" > "$KWIN_TEST_DIR/socket"; '
                f'sleep 300 & echo $! >> "$KWIN_TEST_DIR/pids"; {tail}')

    def test_check_status_returned_and_strays_removed(self):
        self.assertEqual(self.start(self.client("exit 3")).wait(60), 3)
        socket = (self.dir / "socket").read_text().strip()
        self.assertTrue(socket.endswith("/runtime/mevedel-kwin"), socket)
        self.assertTrue(os.path.isabs(socket))
        self.assertIn("--virtual", (self.dir / "kwin-args").read_text().split())
        self.assert_cleaned()

    def test_session_timeout_terminates_everything(self):
        self.assertEqual(self.start(self.client("wait"), timeout=2).wait(60), 124)
        self.assert_cleaned()

    def test_client_dying_without_status_fails(self):
        # Kill the generated client shell before it records a status.
        self.assertEqual(self.start(self.client("kill -9 $PPID")).wait(60), 1)
        self.assert_cleaned()

    def test_sigterm_terminates_everything(self):
        runner = self.start(self.client("wait"))
        deadline = time.monotonic() + 30
        while len(self.pids()) < 3 and time.monotonic() < deadline:
            time.sleep(0.05)
        runner.send_signal(signal.SIGTERM)
        self.assertEqual(runner.wait(60), 128 + signal.SIGTERM)
        self.assert_cleaned()


if __name__ == "__main__":
    unittest.main()
