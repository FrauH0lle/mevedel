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
  {"id": I, "op": "file", "path": P}                    pin an existing file
  {"id": I, "op": "write", "path": P, "mode": M, "data": B64}
                                                        atomically replace P
  {"id": I, "op": "unlink", "path": P}                  remove P
  {"id": I, "op": "release", "pin": N}                  close pin N
Writes never reopen anything by name: the directory is walked (creating
missing components with mkdirat), a temporary file is created there with
O_CREAT|O_EXCL|O_NOFOLLOW, written and fchmodded through its descriptor and
renamed over the target within the directory descriptor.
Answers are {"id": I, "ok": true, "pin": N, "path": "/proc/PID/fd/N"} or
{"id": I, "ok": false, "error": TEXT}.  The helper exits when stdin closes.
"""

import base64
import json
import os
import secrets
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


def split(path):
    """Return PATH's parent and final component."""
    parts = components(path)
    if not parts:
        raise Refused("path names the root")
    return "/" + "/".join(parts[:-1]), parts[-1]


def write(path, mode, data):
    """Atomically replace PATH with DATA at MODE through descriptors only."""
    parent, name = split(path)
    if not isinstance(mode, int) or not 0 <= mode <= 0o7777:
        raise Refused("invalid mode")
    directory = walk(parent, directory=True, create=True)
    try:
        temporary = None
        for _ in range(16):
            candidate = ".mevedel-write-" + secrets.token_hex(8)
            try:
                fd = os.open(candidate, os.O_WRONLY | os.O_CREAT | os.O_EXCL
                             | os.O_NOFOLLOW | os.O_CLOEXEC, 0o600, dir_fd=directory)
            except FileExistsError:
                continue
            temporary = candidate
            break
        if temporary is None:
            raise Refused("could not create a temporary file")
        try:
            with os.fdopen(fd, "wb") as stream:
                stream.write(data)
                stream.flush()
                os.fchmod(stream.fileno(), mode)
                written = os.fstat(stream.fileno())
            os.rename(temporary, name, src_dir_fd=directory, dst_dir_fd=directory)
            temporary = None
            # A swap of the temporary name can only move the attacker's own
            # entry into place, never redirect these bytes; report it.
            placed = os.stat(name, dir_fd=directory, follow_symlinks=False)
            if (placed.st_dev, placed.st_ino) != (written.st_dev, written.st_ino):
                raise Refused(f"written file was replaced: {path}")
        finally:
            if temporary is not None:
                try:
                    os.unlink(temporary, dir_fd=directory)
                except OSError:
                    pass
    finally:
        os.close(directory)


def unlink(path):
    """Remove PATH's final entry without following any symlink."""
    parent, name = split(path)
    directory = walk(parent, directory=True)
    try:
        os.unlink(name, dir_fd=directory)
    finally:
        os.close(directory)


def answer(request, pins):
    """Return the answer object for REQUEST."""
    op = request.get("op")
    if op == "release":
        fd = pins.pop(request.get("pin"), None)
        if fd is not None:
            os.close(fd)
        return {"ok": True}
    if op == "write":
        data = request.get("data")
        if not isinstance(data, str):
            raise Refused("missing data")
        write(request.get("path"), request.get("mode"),
              base64.b64decode(data, validate=True))
        return {"ok": True}
    if op == "unlink":
        unlink(request.get("path"))
        return {"ok": True}
    if op != "file":
        raise Refused("unknown operation")
    fd = walk(request.get("path"))
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
