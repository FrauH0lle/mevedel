#!/usr/bin/env python3
"""One-shot local execution compression and disposable logical file views.

The caller owns retirement decisions and the private destination lifetime.
No output bytes are sent to stdout; the CLI returns only bounded JSON facts.
"""

import argparse
from contextlib import contextmanager
import gzip
import json
import os
from pathlib import Path
import shutil
import stat
import tempfile
import zlib


MIN_COMPRESS_BYTES = 64 * 1024
MAX_INFLATE_BYTES = 64 * 1024 * 1024
MAX_VIEW_BYTES = 512 * 1024 * 1024
CHUNK_BYTES = 64 * 1024
TEMP_PREFIX = ".mevedel-spool-"
PENDING = ".mevedel-pending-executions"


def hidden(path):
    """Recognize private execution paths at every depth."""
    return any(part == PENDING or part.startswith(TEMP_PREFIX)
               for part in Path(path).parts)


@contextmanager
def directory_fd(path):
    """Pin a local directory without traversing any directory symlinks."""
    fd = os.open("/", os.O_RDONLY | os.O_DIRECTORY)
    try:
        for part in Path(os.path.abspath(path)).parts[1:]:
            child = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                            dir_fd=fd)
            os.close(fd)
            fd = child
        yield fd
    finally:
        os.close(fd)


def identity(info):
    """Fingerprint a regular source without accepting size/mtime changes."""
    return info.st_dev, info.st_ino, info.st_size, info.st_mtime_ns


def unchanged(fd, parent, name, initial):
    """Check both the open source and its still-current directory entry."""
    try:
        current = os.stat(name, dir_fd=parent, follow_symlinks=False)
    except FileNotFoundError:
        return False
    return (stat.S_ISREG(current.st_mode)
            and identity(os.fstat(fd)) == identity(initial) == identity(current))


def compress(path, temporary=None):
    """Publish a smaller gzip; the parent retires raw bytes after reader leases."""
    path = Path(os.path.abspath(path))
    if temporary is not None:
        temporary = Path(os.path.abspath(temporary))
        if temporary.parent != path.parent or not temporary.name.startswith(TEMP_PREFIX):
            raise ValueError("Compression temporary must be private and in the source directory")
    if path.suffix != ".log" or any(part.startswith(TEMP_PREFIX) for part in path.parts):
        return {"status": "skipped"}
    with directory_fd(path.parent) as parent:
        try:
            os.stat(path.name + ".gz", dir_fd=parent, follow_symlinks=False)
        except FileNotFoundError:
            pass
        else:
            return {"status": "skipped"}
        info = os.stat(path.name, dir_fd=parent, follow_symlinks=False)
        if (not stat.S_ISREG(info.st_mode)
                or not MIN_COMPRESS_BYTES <= info.st_size <= MAX_INFLATE_BYTES):
            return {"status": "skipped"}
        fd = os.open(path.name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK,
                     dir_fd=parent)
        created_temporary = None
        try:
            if not unchanged(fd, parent, path.name, info):
                return {"status": "changed"}
            if temporary is None:
                # /proc pins default temporary creation to the checked directory
                # even if an ancestor is renamed while this worker runs.
                output, generated = tempfile.mkstemp(
                    prefix=TEMP_PREFIX, dir=f"/proc/self/fd/{parent}")
                created_temporary = Path(generated).name
            else:
                output = os.open(temporary.name,
                                 os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                                 0o600, dir_fd=parent)
                created_temporary = temporary.name
            with os.fdopen(output, "wb") as target:
                if not stat.S_ISREG(os.fstat(target.fileno()).st_mode):
                    raise ValueError("Compression temporary is not a regular file")
                with gzip.GzipFile(filename="", mode="wb", fileobj=target,
                                   compresslevel=1, mtime=0) as zipped:
                    remaining = info.st_size
                    while remaining:
                        block = os.read(fd, min(CHUNK_BYTES, remaining))
                        if not block:
                            break
                        zipped.write(block)
                        remaining -= len(block)
                target.flush()
                os.fsync(target.fileno())
                compressed_info = os.fstat(target.fileno())
                compressed_size = compressed_info.st_size
            if remaining or not unchanged(fd, parent, path.name, info):
                return {"status": "changed"}
            if compressed_size >= info.st_size:
                return {"status": "not-smaller"}
            # link publishes atomically without replacing even evidence that
            # arrived after the initial existence check.
            try:
                os.link(created_temporary, path.name + ".gz", src_dir_fd=parent, dst_dir_fd=parent,
                        follow_symlinks=False)
            except FileExistsError:
                return {"status": "skipped"}
            published = os.stat(path.name + ".gz", dir_fd=parent, follow_symlinks=False)
            if not stat.S_ISREG(published.st_mode) or identity(published) != identity(compressed_info):
                return {"status": "changed"}
            if not unchanged(fd, parent, path.name, info):
                return {"status": "changed"}
            return {"status": "compressed", "raw_bytes": info.st_size,
                    "gzip_bytes": compressed_size,
                    "source_identity": identity(info),
                    "gzip_identity": identity(compressed_info)}
        finally:
            os.close(fd)
            if created_temporary is not None:
                os.unlink(created_temporary, dir_fd=parent)


def inflate(source, target, max_lines=None):
    """Copy bounded decompressed bytes, optionally stopping after a line prefix."""
    total = 0
    lines = 0
    with gzip.GzipFile(fileobj=source, mode="rb") as zipped:
        while True:
            block = zipped.read(min(CHUNK_BYTES, MAX_INFLATE_BYTES - total + 1))
            if not block:
                return total
            if max_lines is not None:
                end = 0
                while lines < max_lines:
                    newline = block.find(b"\n", end)
                    if newline < 0:
                        break
                    lines += 1
                    end = newline + 1
                if lines == max_lines:
                    block = block[:end]
            total += len(block)
            if total > MAX_INFLATE_BYTES:
                raise ValueError("Execution log exceeds 64 MiB inflation limit")
            target.write(block)
            if max_lines is not None and lines == max_lines:
                return total


def materialize(parent, name, dest, compressed, placeholders, budget,
                max_lines=None):
    """Create one ordinary view file; never follow source file symlinks."""
    fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=parent)
    try:
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode):
            raise ValueError("View source is not a regular file")
        if compressed and placeholders:
            dest.touch(exist_ok=False)
            return
        if not compressed and info.st_size > budget[0]:
            raise ValueError("View exceeds 512 MiB total limit")
        with os.fdopen(fd, "rb", closefd=False) as source:
            if compressed:
                with dest.open("xb") as target:
                    size = inflate(source, target, max_lines)
            else:
                # Check the resulting link too: the source entry can be replaced
                # between open and link. Copy from the pinned fd on any mismatch.
                try:
                    os.link(name, dest, src_dir_fd=parent, follow_symlinks=False)
                except OSError:
                    pass
                else:
                    linked = dest.lstat()
                    if stat.S_ISREG(linked.st_mode) and identity(linked) == identity(info):
                        budget[0] -= info.st_size
                        return
                    dest.unlink()
                with dest.open("xb") as target:
                    while True:
                        block = source.read(min(CHUNK_BYTES, budget[0] - target.tell() + 1))
                        if not block:
                            break
                        if target.tell() + len(block) > budget[0]:
                            raise ValueError("View exceeds 512 MiB total limit")
                        target.write(block)
                    size = target.tell()
            budget[0] -= size
            if budget[0] < 0:
                raise ValueError("View exceeds 512 MiB total limit")
    except BaseException:
        # Only called with a fresh destination owned by this worker.
        if dest.exists() or dest.is_symlink():
            dest.unlink()
        raise
    finally:
        os.close(fd)


def walk_view(parent, dest, execution_tree, mode, budget):
    """Walk pinned directories, exposing raw-preferred logical execution names."""
    names = set(os.listdir(parent))
    for name in sorted(names):
        if hidden(name):
            continue
        try:
            info = os.stat(name, dir_fd=parent, follow_symlinks=False)
        except FileNotFoundError:
            continue
        if stat.S_ISDIR(info.st_mode):
            child = os.open(name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                            dir_fd=parent)
            try:
                (dest / name).mkdir()
                walk_view(child, dest / name, execution_tree or name == "executions",
                          mode, budget)
            finally:
                os.close(child)
        elif stat.S_ISREG(info.st_mode):
            compressed = execution_tree and name.endswith(".log.gz")
            logical = name[:-3] if compressed else name
            if compressed:
                if (dest / logical).exists():
                    continue
                try:
                    os.stat(logical, dir_fd=parent, follow_symlinks=False)
                except FileNotFoundError:
                    pass
                else:
                    continue
            try:
                materialize(parent, name, dest / logical, compressed,
                            mode != "grep", budget)
            except FileNotFoundError:
                # A retired raw entry may disappear after directory discovery.
                if execution_tree and name.endswith(".log"):
                    materialize(parent, name + ".gz", dest / name, True,
                                mode != "grep", budget)
                else:
                    raise


def view(source, destination, mode, max_lines=None):
    """Build a disposable local view at the caller-selected final destination."""
    source = Path(os.path.abspath(source))
    dest = Path(os.path.abspath(destination))
    if mode not in ("read", "grep", "glob"):
        raise ValueError("Unknown view mode")
    if max_lines is not None and (mode != "read" or max_lines < 1):
        raise ValueError("Line prefix requires Read and a positive line count")
    if hidden(source):
        raise ValueError("Private execution path")
    if dest == source or source in dest.parents:
        raise ValueError("View destination must be outside source")
    with directory_fd(source.parent) as parent:
        name = source.name
        try:
            info = os.stat(name, dir_fd=parent, follow_symlinks=False)
        except FileNotFoundError:
            if not name.endswith(".log"):
                raise
            name += ".gz"
            info = os.stat(name, dir_fd=parent, follow_symlinks=False)
        if stat.S_ISLNK(info.st_mode):
            raise ValueError("Symlink view source")
        budget = [MAX_VIEW_BYTES]
        if stat.S_ISDIR(info.st_mode):
            if max_lines is not None:
                raise ValueError("Line prefix requires a file source")
            if dest.exists():
                if dest.is_symlink() or not dest.is_dir() or any(dest.iterdir()):
                    raise ValueError("View destination must be a new empty directory")
            else:
                dest.mkdir(mode=0o700)
            child = os.open(name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                            dir_fd=parent)
            try:
                walk_view(child, dest, "executions" in source.parts, mode, budget)
            except BaseException:
                shutil.rmtree(dest)
                raise
            finally:
                os.close(child)
        else:
            if dest.exists() or dest.is_symlink():
                raise ValueError("View destination already exists")
            compressed = name.endswith(".log.gz")
            # Even an explicit compressed handle prefers its coexisting raw log.
            if compressed:
                raw = name[:-3]
                try:
                    raw_info = os.stat(raw, dir_fd=parent, follow_symlinks=False)
                except FileNotFoundError:
                    pass
                else:
                    if not stat.S_ISREG(raw_info.st_mode):
                        raise ValueError("Raw execution source is not a regular file")
                    name, compressed = raw, False
            try:
                materialize(parent, name, dest, compressed, mode == "glob", budget,
                            max_lines)
            except FileNotFoundError:
                if not name.endswith(".log"):
                    raise
                materialize(parent, name + ".gz", dest, True, mode == "glob",
                            budget, max_lines)
    return {"path": str(dest)}


def main(argv=None):
    """Run one compression or view job, returning facts rather than file bodies."""
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_mutually_exclusive_group(required=True)
    commands.add_argument("--compress", metavar="PATH")
    commands.add_argument("--view", nargs=3, metavar=("SOURCE", "DEST", "MODE"))
    parser.add_argument("--max-lines", type=int)
    parser.add_argument("--temporary", metavar="PATH")
    args = parser.parse_args(argv)
    try:
        if args.compress:
            if args.max_lines is not None:
                raise ValueError("Line prefix requires a view")
            result = compress(args.compress, temporary=args.temporary)
        else:
            if args.temporary is not None:
                raise ValueError("Temporary path requires compression")
            result = view(*args.view, max_lines=args.max_lines)
    except (OSError, ValueError, EOFError, zlib.error) as error:
        print(json.dumps({"error": str(error)[:512]}))
        return 1
    print(json.dumps(result))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
