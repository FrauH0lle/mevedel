"""Standard-library tests for the fetch-only Git SSH broker.

Threat model: a confined process controls Git's argv, environment and the
client program, and tries to push, run another remote command, inject ssh
options, reach a host other than the repository's fetch remotes, or keep a
remote session alive after its command ends.  Each case is refused before
the host ssh starts; the stub ssh below records every invocation."""

import importlib.util
import json
import os
from pathlib import Path
import shutil
import socket
import subprocess
import sys
import tempfile
import time
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "scripts/mevedel-git-ssh.py"
SPEC = importlib.util.spec_from_file_location("git_ssh", SCRIPT)
broker = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(broker)

# Stands in for host ssh: record argv, then run the remote command locally
# through a shell, as sshd does on the server.
STUB_SSH = """#!/bin/sh
printf '%s\\n' "$*" >> "$STUB_SSH_LOG"
for last; do :; done
exec sh -c "$last"
"""


class RemoteCommandTests(unittest.TestCase):
    def test_allowed_forms(self):
        for command, expected in [
                ("git-upload-pack '/srv/a.git'", ("git-upload-pack", "/srv/a.git")),
                ("git-upload-archive 'org/r'", ("git-upload-archive", "org/r")),
                ("git-upload-pack 'it'\\''s'\\!'x.git'", ("git-upload-pack", "it's!x.git")),
                ("git-upload-pack '~/r'", ("git-upload-pack", "~/r")),
                ("git-upload-pack '$(id) `id`; rm -rf /'",
                 ("git-upload-pack", "$(id) `id`; rm -rf /")),
                ("git-upload-pack 'a b'", ("git-upload-pack", "a b"))]:
            with self.subTest(command=command):
                self.assertEqual(expected, broker.parse_remote_command(command))

    def test_round_trip_matches_git_quoting(self):
        for path in ["plain", "it's", "a!b", "'!'", "''", "x'\\!'y", "ünï"]:
            command = "git-upload-pack " + broker.sq_quote(path)
            self.assertEqual(("git-upload-pack", path),
                             broker.parse_remote_command(command))

    def test_refused_forms(self):
        for command in [
                "git-receive-pack '/srv/a.git'",
                "git upload-pack '/srv/a.git'",
                "sh -c 'id'",
                "git-upload-pack",
                "git-upload-pack ",
                "git-upload-pack /srv/a.git",
                "git-upload-pack  '/srv/a.git'",
                "git-upload-pack '/srv/a.git' ",
                "git-upload-pack '/srv/a.git'; id",
                "git-upload-pack '/srv/a.git' && git-receive-pack '/x'",
                "git-upload-pack '/srv/a.git'\ngit-receive-pack '/x'",
                "git-upload-pack '/srv/a.git",
                "git-upload-pack 'a'\"'\"'b'",
                "git-upload-pack 'a!b'",
                "git-upload-pack 'a'\\x'b'",
                "git-upload-pack ''",
                "git-upload-pack '--help'",
                "git-upload-pack '-oProxyCommand=id'",
                "git-upload-pack 'a\nb'",
                "git-upload-pack 'a\x00b'",
                "git-upload-pack\t'/srv/a.git'",
                "/usr/bin/git-upload-pack '/srv/a.git'"]:
            with self.subTest(command=command):
                with self.assertRaises(broker.Refused):
                    broker.parse_remote_command(command)

    def test_push_refusal_names_the_reason(self):
        with self.assertRaisesRegex(broker.Refused, "fetch-only"):
            broker.parse_remote_command("git-receive-pack 'x'")


class RequestTests(unittest.TestCase):
    COMMAND = "git-upload-pack '/a.git'"

    def test_git_argument_shapes(self):
        request = broker.parse_request(
            ["-o", "SendEnv=GIT_PROTOCOL", "-4", "-p", "2222", "git@h", self.COMMAND],
            "version=2")
        self.assertEqual(("git@h", "2222", "-4", "version=2"),
                         (request["destination"], request["port"],
                          request["family"], request["protocol"]))
        self.assertEqual(
            ["ssh", "-T", "-o", "BatchMode=yes", "-o", "SendEnv=GIT_PROTOCOL",
             "-4", "-p", "2222", "--", "git@h", self.COMMAND],
            broker.ssh_command("ssh", request))
        request = broker.parse_request(["h", self.COMMAND], "version=2")
        self.assertIsNone(request["protocol"], "protocol is forwarded only on request")
        self.assertEqual(["ssh", "-T", "-o", "BatchMode=yes", "--", "h", self.COMMAND],
                         broker.ssh_command("ssh", request))

    def test_refused_options_and_destinations(self):
        for argv in [
                ["-o", "ProxyCommand=id", "h", self.COMMAND],
                ["-oProxyCommand=id", "h", self.COMMAND],
                ["-oSendEnv=GIT_PROTOCOL", "h", self.COMMAND],
                ["-o", "SendEnv=GIT_PROTOCOL", "-o", "SendEnv=GIT_PROTOCOL", "h", self.COMMAND],
                ["-p", "22", "-4", "h", self.COMMAND],
                ["-v", "h", self.COMMAND],
                ["-L", "1:h:2", "h", self.COMMAND],
                ["-F", "/tmp/config", "h", self.COMMAND],
                ["-p", "0", "h", self.COMMAND],
                ["-p", "65536", "h", self.COMMAND],
                ["-p", "022", "h", self.COMMAND],
                ["-p", "22 -oProxyCommand=id", "h", self.COMMAND],
                ["-p"],
                ["-oProxyCommand=id", self.COMMAND],
                ["", self.COMMAND],
                ["h x", self.COMMAND],
                ["h\n", self.COMMAND],
                ["h", self.COMMAND, "extra"],
                ["h"],
                [],
                None,
                ["h", 7]]:
            with self.subTest(argv=argv):
                with self.assertRaises(broker.Refused):
                    broker.parse_request(argv)

    def test_refused_protocol(self):
        for protocol in ["version=2:x", "version=22", "ver", 2]:
            with self.subTest(protocol=protocol):
                with self.assertRaises(broker.Refused):
                    broker.parse_request(["-o", "SendEnv=GIT_PROTOCOL", "h", self.COMMAND],
                                         protocol)


COMMAND = RequestTests.COMMAND


class UrlTests(unittest.TestCase):
    URLS = ["ssh://git@example.com:2222/a/b.git", "ssh://example.com/x",
            "git+ssh://u@host/x", "ssh+git://host:22/x", "git@example.com:org/r.git",
            "alias:repo", "ex:~/r", "ssh://[::1]/x", "ssh://u@[::1]:2200/x",
            "[host]:path", "ssh://a@b@host/x"]

    def test_matches_what_git_passes_to_ssh(self):
        """Ground truth: run real Git with a recording ssh for every URL."""
        if not shutil.which("git"):
            self.skipTest("git unavailable")
        with tempfile.TemporaryDirectory() as temp:
            recorder = Path(temp) / "record"
            recorder.write_text("#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$RECORD\"\nexit 1\n")
            recorder.chmod(0o700)
            for url in self.URLS:
                with self.subTest(url=url):
                    record = Path(temp) / "argv"
                    environment = dict(os.environ, GIT_SSH_COMMAND=str(recorder),
                                       GIT_SSH_VARIANT="ssh", RECORD=str(record))
                    subprocess.run(["git", "ls-remote", url], cwd=temp, env=environment,
                                   capture_output=True)
                    argv = record.read_text().splitlines()
                    request = broker.parse_request(argv, "version=2")
                    self.assertEqual((request["destination"], request["port"]),
                                     broker.url_destination(url))

    def test_non_ssh_urls_are_never_allowed(self):
        for url in ["https://example.com/r.git", "http://h/r", "git://h/r",
                    "file:///srv/r", "/srv/r.git", "../r", "./a:b", "-oProxyCommand=id:r",
                    "ssh://-oProxyCommand=id/x", "@host:r", "user@:r", "ssh://h:0/x",
                    "ssh://h:22x/x", "ssh://[::1/x"]:
            with self.subTest(url=url):
                self.assertIsNone(broker.url_destination(url))


class BrokerTests(unittest.TestCase):
    """End to end through the real client, broker, Git and a stub ssh."""

    def setUp(self):
        if not (shutil.which("git") and hasattr(socket, "send_fds")):
            self.skipTest("git or SCM_RIGHTS unavailable")
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        root = Path(self.temp.name)
        self.root = root
        self.log = root / "ssh.log"
        self.log.touch()
        stub = root / "ssh"
        stub.write_text(STUB_SSH)
        stub.chmod(0o700)
        self.git_env = dict(os.environ, GIT_CONFIG_NOSYSTEM="1", HOME=str(root),
                            GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                            GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t")
        self.remote = root / "remote.git"
        self.git("init", "-q", "--bare", "-b", "main", str(self.remote))
        seed = root / "seed"
        self.git("init", "-q", "-b", "main", str(seed))
        self.git("-C", str(seed), "commit", "-q", "--allow-empty", "-m", "one")
        self.git("-C", str(seed), "push", "-q", str(self.remote), "main")
        self.url = f"ssh://fake{self.remote}"
        self.work = root / "work"
        self.git("init", "-q", "-b", "main", str(self.work))
        self.git("-C", str(self.work), "remote", "add", "origin", self.url)
        self.socket = root / "socket"
        self.broker = subprocess.Popen(
            [sys.executable, "-I", str(SCRIPT), "serve", str(self.socket),
             json.dumps({"ssh": str(stub), "urls": [self.url]})],
            stdin=subprocess.PIPE, env=dict(self.git_env, STUB_SSH_LOG=str(self.log)))
        self.addCleanup(self.stop_broker)
        self.client_env = dict(
            self.git_env, GIT_SSH_VARIANT="ssh", MEVEDEL_GIT_SSH_SOCKET=str(self.socket),
            GIT_SSH_COMMAND=f"{sys.executable} -I {SCRIPT} client")

    def stop_broker(self):
        if self.broker.poll() is None:
            self.broker.stdin.close()
            self.broker.wait(5)

    def git(self, *args, env=None, check=True):
        return subprocess.run(["git", *args], env=env or self.git_env, check=check,
                              capture_output=True, text=True)

    def confined_git(self, *args):
        return self.git("-C", str(self.work), *args, env=self.client_env, check=False)

    def test_fetch_ls_remote_and_pull(self):
        result = self.confined_git("ls-remote", "origin")
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertIn("refs/heads/main", result.stdout)
        result = self.confined_git("fetch", "origin")
        self.assertEqual(0, result.returncode, result.stderr)
        result = self.confined_git("pull", "--ff-only", "origin", "main")
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual(self.git("-C", str(self.remote), "rev-parse", "main").stdout,
                         self.git("-C", str(self.work), "rev-parse", "HEAD").stdout)
        self.assertIn("git-upload-pack", self.log.read_text())

    def test_push_is_refused_before_ssh(self):
        self.confined_git("pull", "-q", "origin", "main")
        self.git("-C", str(self.work), "commit", "-q", "--allow-empty", "-m", "two")
        before = self.git("-C", str(self.remote), "rev-parse", "main").stdout
        log = self.log.read_text()
        result = self.confined_git("push", "origin", "HEAD:main")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("git-receive-pack refused", result.stderr)
        self.assertEqual(before, self.git("-C", str(self.remote), "rev-parse", "main").stdout)
        self.assertEqual(log, self.log.read_text(), "ssh never started for the push")

    def test_other_hosts_and_options_are_refused_before_ssh(self):
        for args in [["ls-remote", f"ssh://other{self.remote}"],
                     ["ls-remote", f"ssh://fake:2222{self.remote}"],
                     ["ls-remote", f"ssh://user@fake{self.remote}"],
                     ["-c", "remote.origin.uploadpack=sh -c id", "ls-remote", "origin"]]:
            with self.subTest(args=args):
                result = self.confined_git(*args)
                self.assertNotEqual(0, result.returncode)
                self.assertIn("mevedel-git-ssh:", result.stderr)
        # A client that bypasses Git entirely gets the same verdicts.
        for argv in [["-o", "ProxyCommand=touch pwned", "fake", COMMAND],
                     ["fake", "sh -c 'touch pwned'"]]:
            with self.subTest(argv=argv):
                result = subprocess.run(
                    [sys.executable, "-I", str(SCRIPT), "client", *argv],
                    env=self.client_env, capture_output=True, text=True, cwd=self.root)
                self.assertEqual(broker.REFUSED_STATUS, result.returncode)
        self.assertEqual("", self.log.read_text())
        self.assertFalse((self.root / "pwned").exists())

    def test_client_exit_stops_its_ssh(self):
        stub = self.root / "ssh"
        pid_file = self.root / "ssh.pid"
        stub.write_text(f"#!/bin/sh\necho $$ > {pid_file}\nexec sleep 30\n")
        client = subprocess.Popen(
            [sys.executable, "-I", str(SCRIPT), "client", "fake",
             f"git-upload-pack '{self.remote}'"],
            env=self.client_env, stdin=subprocess.PIPE, stdout=subprocess.DEVNULL)
        pid = self.wait_for_pid(pid_file)
        client.kill()
        client.wait()
        client.stdin.close()
        self.assert_exits(pid)

    def test_broker_exit_stops_ssh(self):
        stub = self.root / "ssh"
        pid_file = self.root / "ssh.pid"
        stub.write_text(f"#!/bin/sh\necho $$ > {pid_file}\nexec sleep 30\n")
        client = subprocess.Popen(
            [sys.executable, "-I", str(SCRIPT), "client", "fake",
             f"git-upload-pack '{self.remote}'"],
            env=self.client_env, stdin=subprocess.PIPE, stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE)
        pid = self.wait_for_pid(pid_file)
        self.stop_broker()
        self.assertFalse(self.socket.exists())
        self.assert_exits(pid)
        self.assertNotEqual(0, client.wait(5))
        client.stderr.close()
        client.stdin.close()

    def wait_for_pid(self, pid_file):
        deadline = time.monotonic() + 10
        while not (pid_file.exists() and pid_file.read_text().strip()):
            self.assertLess(time.monotonic(), deadline, "stub ssh did not start")
            time.sleep(0.01)
        return int(pid_file.read_text())

    def assert_exits(self, pid):
        deadline = time.monotonic() + 5
        while True:
            try:
                state = Path(f"/proc/{pid}/stat").read_text().split()[2]
            except OSError:
                return  # Gone, including between the lookup and the read.
            if state == "Z":
                return
            self.assertLess(time.monotonic(), deadline, "ssh child outlived its owner")
            time.sleep(0.01)


if __name__ == "__main__":
    unittest.main()
