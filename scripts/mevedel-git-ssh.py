"""Fetch-only Git SSH transport for Bubblewrap-confined commands.

`client` runs inside confinement as GIT_SSH_COMMAND.  It hands Git's argv and
its own stdin, stdout and stderr descriptors to the per-execution broker over
a Unix socket and exits with the broker's status.

`serve SOCKET CONFIG` runs on the host for the lifetime of one confined
command.  It accepts only `git-upload-pack` and `git-upload-archive` for the
SSH destinations of the repository's fetch remotes, and runs the host's ssh
on the passed descriptors with the user's agent and configuration.  Keys,
known_hosts and ssh configuration never enter the sandbox.  Every other
remote command, option and destination is refused without being evaluated.
The broker exits when its stdin closes and stops its ssh children with it.

Threat model: the confined process controls Git's argv and environment and
can replace the client entirely.
- Push or another remote command: only git-upload-pack and git-upload-archive
  in Git's exact quoting are accepted; the path is re-quoted canonically and
  must reproduce the request, so the remote shell sees one quoted argument.
- ssh option injection (ProxyCommand, -F, forwarding): only Git's own
  SendEnv, -4/-6 and -p forms are recognized; the host ssh argv is rebuilt
  from parsed values with `--' before the destination.
- Other hosts: the destination and port must match a fetch remote of the
  repository, read on the host before the confined command starts.
- Keys: ssh runs on the host; the confined side receives only its own pipes.
  The harness also masks SSH_AUTH_SOCK inside confinement.
- Lingering sessions: a client that disconnects takes its ssh child with it,
  and the broker exits with the confined command.
Out of scope: a remote that is itself hostile, and a writable development
checkout of this script.
"""

import json
import os
import re
import select
import signal
import socket
import subprocess
import sys
import threading
import time

SERVICES = ("git-upload-pack", "git-upload-archive")
REFUSED_STATUS = 128
MAX_REQUEST_BYTES = 64 * 1024
SOCKET_ENV = "MEVEDEL_GIT_SSH_SOCKET"
CONNECT_TIMEOUT = 10.0


class Refused(Exception):
    """A request outside the fetch-only contract."""


def _plain(text):
    """Return True when TEXT has no control characters."""
    return not any(ord(char) < 32 or ord(char) == 127 for char in text)


def sq_quote(text):
    """Quote TEXT exactly as Git's sq_quote_buf does."""
    return "'" + text.replace("'", "'\\''").replace("!", "'\\!'") + "'"


def sq_dequote(text):
    """Strictly undo `sq_quote'; raise Refused on any other form."""
    if not text.startswith("'"):
        raise Refused("remote path is not single-quoted")
    parts = []
    index = 1
    while True:
        end = text.find("'", index)
        if end < 0:
            raise Refused("unterminated quote in remote path")
        parts.append(text[index:end])
        index = end + 1
        if index == len(text):
            return "".join(parts)
        if text[index:index + 1] == "\\" and text[index + 2:index + 3] == "'" \
           and text[index + 1:index + 2] in ("'", "!"):
            parts.append(text[index + 1])
            index += 3
            continue
        raise Refused("unexpected text after quoted remote path")


def parse_remote_command(command):
    """Return (SERVICE, PATH) for an allowed remote COMMAND or raise Refused.

    Only Git's own form is accepted: a service name, one space and the path
    quoted by sq_quote.  The re-quoted path must reproduce COMMAND exactly,
    so nothing ambiguous reaches the remote shell."""
    service, separator, quoted = command.partition(" ")
    if service not in SERVICES:
        name = service if _plain(service) and len(service) <= 64 else "command"
        if service == "git-receive-pack":
            raise Refused("git-receive-pack refused: confined Git over SSH is "
                          "fetch-only; pushing stays with the user")
        raise Refused(f"remote {name!r} refused: only git-upload-pack and "
                      "git-upload-archive are allowed")
    if not separator:
        raise Refused("remote command has no path")
    path = sq_dequote(quoted)
    if not path or not _plain(path) or path.startswith("-"):
        raise Refused("remote path is empty, starts with '-' or has control characters")
    if f"{service} {sq_quote(path)}" != command:
        raise Refused("remote command is not in Git's canonical form")
    return service, path


def _valid_destination(destination):
    return (bool(destination) and not destination.startswith("-")
            and _plain(destination) and " " not in destination)


def parse_request(argv, protocol=None):
    """Return the validated request dict for Git's ssh ARGV or raise Refused.

    Git's OpenSSH variant emits, in this order: an optional
    `-o SendEnv=GIT_PROTOCOL', an optional `-4' or `-6', an optional
    `-p PORT', the destination and the remote command.  PROTOCOL is the
    client's GIT_PROTOCOL value; it is forwarded only when Git asked ssh to
    send it, and must then be a plain `version=N'."""
    if not isinstance(argv, list) or not all(isinstance(arg, str) for arg in argv):
        raise Refused("malformed request")
    rest = list(argv)
    send_env = rest[:2] == ["-o", "SendEnv=GIT_PROTOCOL"]
    if send_env:
        rest = rest[2:]
    family = None
    if rest[:1] in (["-4"], ["-6"]):
        family = rest.pop(0)
    port = None
    if rest[:1] == ["-p"]:
        if len(rest) < 2 or not re.fullmatch(r"[1-9][0-9]{0,4}", rest[1]) \
           or int(rest[1]) > 65535:
            raise Refused("invalid ssh port")
        port = rest[1]
        rest = rest[2:]
    if len(rest) != 2:
        raise Refused("unsupported ssh options: only Git's own fetch arguments are allowed")
    destination, command = rest
    if not _valid_destination(destination):
        raise Refused("invalid ssh destination")
    service, path = parse_remote_command(command)
    if not send_env or protocol in (None, ""):
        protocol = None
    elif not isinstance(protocol, str) or not re.fullmatch(r"version=[0-9]", protocol):
        raise Refused("invalid GIT_PROTOCOL")
    return {"destination": destination, "port": port, "family": family,
            "protocol": protocol, "service": service, "path": path}


def url_destination(url):
    """Return the (DESTINATION, PORT) Git passes to ssh for URL, or None.

    Unrecognized forms return None and are therefore never allowed."""
    match = re.fullmatch(r"(?:ssh|git\+ssh|ssh\+git)://([^/]*)(/.*)", url)
    if match:
        authority = match.group(1)
        user, at, hostport = authority.rpartition("@")
        port = None
        if hostport.startswith("["):
            close = hostport.find("]")
            if close < 0:
                return None
            host, tail = hostport[1:close], hostport[close + 1:]
            if tail:
                if not tail.startswith(":"):
                    return None
                port = tail[1:] or None
        else:
            host, colon, port = hostport.partition(":")
            port = port or None
        if port is not None and not re.fullmatch(r"[1-9][0-9]{0,4}", port):
            return None
        destination = f"{user}@{host}" if at else host
    else:
        # scp-like [user@]host:path, where no '/' precedes the first ':'.
        colon = url.find(":")
        if colon <= 0 or "/" in url[:colon] or "://" in url:
            return None
        destination, port = url[:colon], None
        if destination.startswith("["):
            if not destination.endswith("]"):
                return None
            destination = destination[1:-1]
    if not _valid_destination(destination) or destination.endswith("@") \
       or destination.startswith("@"):
        return None
    return destination, port


def allowed_destinations(urls):
    """Return the set of (DESTINATION, PORT) pairs allowed by remote URLS."""
    return {pair for pair in map(url_destination, urls) if pair}


def ssh_command(ssh, request):
    """Return the host ssh argv for a validated REQUEST."""
    command = [ssh, "-T", "-o", "BatchMode=yes"]
    if request["protocol"]:
        command += ["-o", "SendEnv=GIT_PROTOCOL"]
    if request["family"]:
        command.append(request["family"])
    if request["port"]:
        command += ["-p", request["port"]]
    return command + ["--", request["destination"],
                      f"{request['service']} {sq_quote(request['path'])}"]


# Host broker

class Broker:
    """Serve fetch-only ssh requests on one listening socket."""

    def __init__(self, path, ssh, urls):
        self.path = path
        self.ssh = ssh
        self.allowed = allowed_destinations(urls)
        self.children = set()
        self.lock = threading.Lock()
        self.stopping = False
        old = os.umask(0o177)
        try:
            self.listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            self.listener.bind(path)
        finally:
            os.umask(old)
        self.listener.listen(16)

    def run(self):
        """Accept requests until stdin closes."""
        stdin = sys.stdin.fileno()
        while not self.stopping:
            readable, _, _ = select.select([self.listener, stdin], [], [])
            if stdin in readable and not os.read(stdin, 4096):
                break
            if self.listener in readable:
                connection, _ = self.listener.accept()
                threading.Thread(target=self.handle, args=(connection,),
                                 daemon=True).start()
        self.stop()

    def stop(self):
        """Stop every ssh child and remove the socket."""
        self.stopping = True
        with self.lock:
            children = list(self.children)
        for child in children:
            if child.poll() is None:
                child.kill()
        try:
            os.unlink(self.path)
        except OSError:
            pass

    def handle(self, connection):
        """Serve one client CONNECTION."""
        fds = []
        try:
            connection.settimeout(CONNECT_TIMEOUT)
            uid = connection.getsockopt(socket.SOL_SOCKET, socket.SO_PEERCRED,
                                        12)[4:8]
            if int.from_bytes(uid, sys.byteorder) != os.getuid():
                return
            message, fds, flags, _ = socket.recv_fds(connection, MAX_REQUEST_BYTES, 3)
            if len(fds) != 3 or flags & (socket.MSG_TRUNC | socket.MSG_CTRUNC):
                return
            try:
                payload = json.loads(message.decode("utf-8"))
                if not isinstance(payload, dict):
                    raise Refused("malformed request")
                request = parse_request(payload.get("argv"), payload.get("protocol"))
                if (request["destination"], request["port"]) not in self.allowed:
                    raise Refused(f"ssh destination {request['destination']!r} is not "
                                  "a fetch remote of this repository")
            except (Refused, ValueError, UnicodeDecodeError) as error:
                reason = error if isinstance(error, Refused) else "malformed request"
                os.write(fds[2], f"mevedel-git-ssh: {reason}\n".encode())
                connection.sendall(f"{REFUSED_STATUS}\n".encode())
                return
            environment = dict(os.environ)
            environment.pop("GIT_PROTOCOL", None)
            if request["protocol"]:
                environment["GIT_PROTOCOL"] = request["protocol"]
            with self.lock:
                if self.stopping:
                    return
                child = subprocess.Popen(ssh_command(self.ssh, request),
                                         stdin=fds[0], stdout=fds[1], stderr=fds[2],
                                         env=environment, close_fds=True)
                self.children.add(child)
            for fd in fds:
                os.close(fd)
            fds = []
            status = self.wait(child, connection)
            with self.lock:
                self.children.discard(child)
            if status is not None:
                connection.sendall(f"{status}\n".encode())
        except OSError:
            pass
        finally:
            for fd in fds:
                os.close(fd)
            connection.close()

    def wait(self, child, connection):
        """Return CHILD's shell-style status, or None if the client left.
        A client that disconnects first takes its ssh child with it."""
        poller = select.poll()
        poller.register(connection, select.POLLIN | select.POLLHUP)
        pidfd = None
        if hasattr(os, "pidfd_open"):
            pidfd = os.pidfd_open(child.pid)
            poller.register(pidfd, select.POLLIN)
        try:
            while child.poll() is None:
                events = poller.poll(None if pidfd is not None else 200)
                if any(fd == connection.fileno() for fd, _ in events):
                    child.kill()
                    child.wait()
                    return None
        finally:
            if pidfd is not None:
                os.close(pidfd)
        code = child.returncode
        return 128 - code if code < 0 else code


def serve(path, config):
    """Run the host broker on socket PATH with JSON CONFIG."""
    settings = json.loads(config)
    broker = Broker(path, settings["ssh"], settings["urls"])
    signal.signal(signal.SIGTERM, lambda *_: (broker.stop(), os._exit(0)))
    broker.run()


# Confined client

def client(argv):
    """Hand ARGV and stdio to the broker; return the remote status."""
    path = os.environ.get(SOCKET_ENV)
    if not path:
        sys.stderr.write(f"mevedel-git-ssh: {SOCKET_ENV} is not set\n")
        return REFUSED_STATUS
    payload = json.dumps({"argv": argv,
                          "protocol": os.environ.get("GIT_PROTOCOL")}).encode()
    deadline = time.monotonic() + CONNECT_TIMEOUT
    connection = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    while True:
        try:
            connection.connect(path)
            break
        except (FileNotFoundError, ConnectionRefusedError):
            # The broker starts beside the confined command.
            if time.monotonic() > deadline:
                sys.stderr.write("mevedel-git-ssh: transport broker unavailable\n")
                return REFUSED_STATUS
            time.sleep(0.01)
    with connection:
        socket.send_fds(connection, [payload], [0, 1, 2])
        reply = b""
        while not reply.endswith(b"\n"):
            chunk = connection.recv(16)
            if not chunk:
                sys.stderr.write("mevedel-git-ssh: transport broker closed\n")
                return REFUSED_STATUS
            reply += chunk
    return int(reply)


def main(argv):
    if argv[:1] == ["client"]:
        return client(argv[1:])
    if len(argv) == 3 and argv[0] == "serve":
        serve(argv[1], argv[2])
        return 0
    sys.stderr.write("usage: mevedel-git-ssh.py client ARGS... | serve SOCKET CONFIG\n")
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
