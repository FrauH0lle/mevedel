#!/usr/bin/env python3
"""Run the graphical check inside a private, unlocked virtual KWin.

Usage: graphical-check-private-kwin.py [--session-timeout SECONDS] [-- CHECK-ARGS...]

Starts `dbus-run-session -- kwin_wayland --virtual ... --exit-with-session
CLIENT` with a private HOME, XDG roots and DBus session, and an OpenGL
compositor (KWIN_COMPOSE=O2) that never locks. CLIENT runs
`eask run script graphical-check -- CHECK-ARGS` against KWin's absolute
Wayland socket, so `--grammar-dir`, `--output` and `--timeout` are the
graphical check's own options.  The exit status is the check's.

Cleanup: the runner is a child subreaper, so every descendant (DBus, KWin,
Xwayland, Eask, Emacs) stays its descendant even when it detaches.  On
success, failure, session timeout, SIGTERM, SIGHUP or SIGINT it terminates
all of them, escalates to SIGKILL after a grace period, reaps them, and removes
the private directory.  SIGKILL of the runner itself cannot be handled.
"""

import argparse
import ctypes
import os
from pathlib import Path
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
SOCKET = "mevedel-kwin"
GRACE = 5.0
PR_SET_CHILD_SUBREAPER = 36


def parse_arguments(argv):
    """Split ARGV into runner options and arguments for graphical-check."""
    forwarded = []
    if "--" in argv:
        index = argv.index("--")
        argv, forwarded = argv[:index], argv[index + 1:]
    parser = argparse.ArgumentParser(
        prog="graphical-check-private-kwin.py",
        usage="%(prog)s [--session-timeout SECONDS] [-- CHECK-ARGS...]")
    parser.add_argument("--session-timeout", type=int, default=900,
                        help="deadline for the whole KWin session (default 900)")
    options = parser.parse_args(argv)
    if options.session_timeout <= 0:
        parser.error("--session-timeout must be positive")
    options.check_args = forwarded
    return options


def private_environment(base, private):
    """Return BASE's environment confined to directories under PRIVATE."""
    env = dict(base)
    for name in ("DISPLAY", "WAYLAND_DISPLAY", "WAYLAND_SOCKET",
                 "DBUS_SESSION_BUS_ADDRESS", "XAUTHORITY"):
        env.pop(name, None)
    # Eask runs through npx; keep the user's npm cache so --offline works.
    env.setdefault("NPM_CONFIG_CACHE",
                   str(Path(base.get("HOME", Path.home())) / ".npm"))
    for name, leaf in (("HOME", "home"), ("XDG_CONFIG_HOME", "config"),
                       ("XDG_CACHE_HOME", "cache"), ("XDG_DATA_HOME", "data"),
                       ("XDG_STATE_HOME", "state"),
                       ("XDG_RUNTIME_DIR", "runtime")):
        path = Path(private) / leaf
        path.mkdir(mode=0o700)
        env[name] = str(path)
    env.update(KWIN_COMPOSE="O2", QT_QUICK_BACKEND="software")
    return env


def check_command(check_args):
    """Return the graphical-check command for CHECK-ARGS."""
    return ["npx", "--offline", "@emacs-eask/cli", "run", "script",
            "graphical-check", "--", *check_args]


def write_client(private, command, socket):
    """Write the KWin session client running COMMAND; return its path.
The client records COMMAND's exit status, which KWin does not propagate."""
    client = Path(private) / "client.sh"
    status = Path(private) / "status"
    client.write_text(
        "#!/bin/sh\n"
        f"cd {shlex.quote(str(ROOT))} || exit 1\n"
        f"WAYLAND_DISPLAY={shlex.quote(str(socket))} {shlex.join(command)}\n"
        f"echo $? > {shlex.quote(str(status))}\n")
    client.chmod(0o700)
    return client, status


def kwin_command(client):
    """Return the private KWin session command starting CLIENT."""
    return ["dbus-run-session", "--", "kwin_wayland", "--virtual",
            "--width", "2560", "--height", "2160", "--socket", SOCKET,
            "--no-lockscreen", "--no-global-shortcuts", "--no-kactivities",
            "--exit-with-session", str(client)]


def descendants(root):
    """Return the PIDs below ROOT, read from /proc."""
    children = {}
    for entry in Path("/proc").iterdir():
        if not entry.name.isdigit():
            continue
        try:
            stat = (entry / "stat").read_text()
        except OSError:
            continue
        parent = int(stat[stat.rindex(")") + 2:].split()[1])
        children.setdefault(parent, []).append(int(entry.name))
    found, pending = [], [root]
    while pending:
        for child in children.get(pending.pop(), ()):
            found.append(child)
            pending.append(child)
    return found


def reap():
    """Collect every exited child without blocking."""
    while True:
        try:
            pid, _ = os.waitpid(-1, os.WNOHANG)
        except ChildProcessError:
            return
        if pid == 0:
            return


def terminate_descendants(grace=GRACE):
    """Terminate and reap every descendant of this process."""
    for sig in (signal.SIGTERM, signal.SIGKILL):
        pids = descendants(os.getpid())
        for pid in pids:
            try:
                os.kill(pid, sig)
            except ProcessLookupError:
                pass
        deadline = time.monotonic() + grace
        while pids and time.monotonic() < deadline:
            reap()
            pids = descendants(os.getpid())
            if pids:
                time.sleep(0.05)
        if not pids:
            return
    reap()


class Interrupted(Exception):
    """A termination signal arrived."""


def _interrupt(signum, _frame):
    raise Interrupted(signum)


def run_session(command, session_timeout, base_env=None):
    """Run COMMAND as the client of a private KWin; return its exit status."""
    if sys.platform != "linux":
        raise SystemExit("graphical-check-private-kwin: Linux only")
    for program in ("dbus-run-session", "kwin_wayland"):
        if not shutil.which(program):
            raise SystemExit(f"graphical-check-private-kwin: {program} not found")
    libc = ctypes.CDLL(None, use_errno=True)
    if libc.prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) != 0:
        raise SystemExit("graphical-check-private-kwin: cannot become subreaper")
    signals = (signal.SIGTERM, signal.SIGHUP, signal.SIGINT)
    previous = {sig: signal.getsignal(sig) for sig in signals}
    private = tempfile.mkdtemp(prefix="mevedel-kwin-")
    try:
        for sig in signals:
            signal.signal(sig, _interrupt)
        env = private_environment(base_env or os.environ, private)
        socket = Path(env["XDG_RUNTIME_DIR"]) / SOCKET
        client, status = write_client(private, command, socket)
        session = subprocess.Popen(kwin_command(client), env=env,
                                   stdin=subprocess.DEVNULL)
        try:
            session.wait(timeout=session_timeout)
        except subprocess.TimeoutExpired:
            print(f"graphical-check-private-kwin: session exceeded "
                  f"{session_timeout}s; terminating", file=sys.stderr)
            return 124
        try:
            return int(status.read_text())
        except (OSError, ValueError):
            print("graphical-check-private-kwin: KWin exited before the "
                  f"check finished (status {session.returncode})",
                  file=sys.stderr)
            return session.returncode or 1
    except Interrupted as signum:
        return 128 + signum.args[0]
    finally:
        # A repeated signal must not abort cleanup half-way.
        for sig in signals:
            signal.signal(sig, signal.SIG_IGN)
        terminate_descendants()
        shutil.rmtree(private, ignore_errors=True)
        for sig, handler in previous.items():
            signal.signal(sig, handler)


def main(argv=None):
    options = parse_arguments(sys.argv[1:] if argv is None else argv)
    return run_session(check_command(options.check_args),
                       options.session_timeout)


if __name__ == "__main__":
    sys.exit(main())
