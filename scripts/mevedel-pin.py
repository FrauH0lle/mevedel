#!/usr/bin/env python3
"""Pin authorized canonical paths without following symlinks.

mevedel authorizes a file tool on a canonical path, then opens it.  A
concurrent confined process can replace a directory on that path with a
symlink between the two, so an ordinary open would follow it into a
protected tree.  This helper walks each requested path from the root with
openat(O_PATH | O_NOFOLLOW), refuses any symlink component, and keeps the
descriptor open.  Emacs then opens or writes through /proc/PID/fd/N, which
names the pinned inode whatever happens to the path afterwards.

Protocol: one JSON object per line on stdin, one answer per line on stdout.
  {"id": I, "op": "file", "path": P}              pin an existing file or directory
  {"id": I, "op": "dir", "path": P, "create": B}  pin a directory, creating missing
                                                  components with mkdirat when B
  {"id": I, "op": "release", "pin": N}            close pin N
Answers are {"id": I, "ok": true, "pin": N, "path": "/proc/PID/fd/N"} or
{"id": I, "ok": false, "error": TEXT}.  The helper exits when stdin closes.
"""

import json
import os
import stat
import sys

FLAGS = os.O_PATH | os.O_NOFOLLOW | os.O_CLOEXEC


class Refused(Exception):
    """A path the helper will not pin."""


def components(path):
    """Return PATH's components, refusing a non-canonical spelling."""
    if not isinstance(path, str) or not path.startswith("/"):
        raise Refused("path is not absolute")
    parts = [part for part in path.split("/") if part]
    if any(part in (".", "..") for part in parts):
        raise Refused("path is not canonical")
    return parts


def step(parent, name, shown):
    """Open NAME below PARENT without following it; refuse a symlink."""
    child = os.open(name, FLAGS, dir_fd=parent)
    if stat.S_ISLNK(os.fstat(child).st_mode):
        os.close(child)
        raise Refused(f"symlink in authorized path: {shown}")
    return child


def walk(path, directory=False, create=False):
    """Return a descriptor for PATH reached without symlinks."""
    parts = components(path)
    fd = os.open("/", FLAGS | os.O_DIRECTORY)
    try:
        for index, name in enumerate(parts):
            shown = "/" + "/".join(parts[:index + 1])
            try:
                child = step(fd, name, shown)
            except FileNotFoundError:
                if not create:
                    raise Refused(f"no such file: {shown}") from None
                try:
                    os.mkdir(name, 0o777, dir_fd=fd)
                except FileExistsError:
                    pass
                child = step(fd, name, shown)
            last = index == len(parts) - 1
            if (not last or directory) and not stat.S_ISDIR(os.fstat(child).st_mode):
                os.close(child)
                raise Refused(f"not a directory: {shown}")
            os.close(fd)
            fd = child
        return fd
    except BaseException:
        os.close(fd)
        raise


def answer(request, pins):
    """Return the answer object for REQUEST."""
    op = request.get("op")
    if op == "release":
        fd = pins.pop(request.get("pin"), None)
        if fd is not None:
            os.close(fd)
        return {"ok": True}
    if op not in ("file", "dir"):
        raise Refused("unknown operation")
    fd = walk(request.get("path"), directory=(op == "dir"),
              create=(op == "dir" and bool(request.get("create"))))
    pins[fd] = fd
    return {"ok": True, "pin": fd, "path": f"/proc/{os.getpid()}/fd/{fd}"}


def main():
    pins = {}
    for line in sys.stdin:
        request = {}
        try:
            request = json.loads(line)
            reply = answer(request, pins)
        except (Refused, OSError, ValueError) as error:
            text = str(error) if isinstance(error, Refused) else (
                f"{type(error).__name__}: {error.strerror or error}"
                if isinstance(error, OSError) else "malformed request")
            reply = {"ok": False, "error": text}
        reply["id"] = request.get("id") if isinstance(request, dict) else None
        sys.stdout.write(json.dumps(reply) + "\n")
        sys.stdout.flush()


if __name__ == "__main__":
    main()
