"""Standard-library tests for the one-shot execution spool worker."""

from contextlib import redirect_stdout
import gzip
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


WORKER = Path(__file__).resolve().parents[1] / "scripts/mevedel-execution-spool.py"
SPEC = importlib.util.spec_from_file_location("execution_spool", WORKER)
spool = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(spool)


class SpoolTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.source = self.root / "source"
        self.source.mkdir()
        self.dest = self.root / "view"

    def file(self, name, body=b"ordinary\x00\xff\r\n"):
        path = self.source / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(body)
        return path

    def zipped(self, name="executions/job.log.gz", body=b"hello\x00\xff\r\nworld\n"):
        return self.file(name, gzip.compress(body, mtime=0))

    def cli(self, *args, success=True):
        result = subprocess.run([sys.executable, "-B", str(WORKER), *map(str, args)],
                                capture_output=True, text=True, check=False)
        self.assertEqual(0 if success else 1, result.returncode, result.stderr)
        self.assertEqual("", result.stderr)
        self.assertEqual(1, len(result.stdout.splitlines()))
        self.assertLess(len(result.stdout), 2048)
        return json.loads(result.stdout)

    def assert_no_temps(self):
        self.assertEqual([], list(self.source.rglob(spool.TEMP_PREFIX + "*")))

    def test_compress_cli_deterministic_level_one_and_raw_preserved(self):
        body = b"output\x00\xff\n" * 20000
        first = self.file("first.log", body)
        second = self.file("second.log", body)
        facts = self.cli("--compress", first)
        self.assertEqual("compressed", facts["status"])
        self.cli("--compress", second)
        zipped = first.with_suffix(".log.gz").read_bytes()
        self.assertEqual(zipped, second.with_suffix(".log.gz").read_bytes())
        self.assertEqual(body, gzip.decompress(zipped))
        self.assertEqual(0, int.from_bytes(zipped[4:8], "little"))
        self.assertEqual(4, zipped[8])  # gzip's fastest-compression flag
        self.assertEqual(body, first.read_bytes())
        self.assertLess(facts["gzip_bytes"], facts["raw_bytes"])
        self.assertEqual(list(spool.identity(first.stat())), facts["source_identity"])
        self.assertEqual(list(spool.identity(first.with_suffix(".log.gz").stat())),
                         facts["gzip_identity"])
        self.assert_no_temps()

    def test_compress_threshold_suffix_private_and_nonregular(self):
        for name, body in (("short.log", b"x" * 65535),
                           ("wrong.txt", b"x" * 65536),
                           (".mevedel-spool-private.log", b"x" * 65536),
                           (".mevedel-spool-private/job.log", b"x" * 65536)):
            with self.subTest(name=name):
                path = self.file(name, body)
                self.assertEqual("skipped", spool.compress(path)["status"])
                self.assertEqual(body, path.read_bytes())
                self.assertFalse(Path(str(path) + ".gz").exists())
        directory = self.source / "directory.log"
        directory.mkdir()
        self.assertEqual("skipped", spool.compress(directory)["status"])
        threshold = self.file("threshold.log", b"x" * 65536)
        self.assertEqual("compressed", spool.compress(threshold)["status"])

    def test_compress_retired_pending_log_but_view_stays_hidden(self):
        body = b"retired\n" * (64 * 1024 * 1024 // 8)
        path = self.file("executions/.mevedel-pending-executions/foreground.log", body)
        facts = self.cli("--compress", path)
        self.assertEqual("compressed", facts["status"])
        self.assertEqual(64 * 1024 * 1024, facts["raw_bytes"])
        self.assertEqual(body, path.read_bytes())
        self.assertEqual(body, gzip.decompress(Path(str(path) + ".gz").read_bytes()))
        spool.view(self.source, self.dest, "grep")
        self.assertEqual([], list((self.dest / "executions").iterdir()))
        self.cli("--view", path, self.root / "private", "read", success=False)
        self.assert_no_temps()

    def test_compress_preserves_existing_gzip_evidence(self):
        raw = self.file("existing.log", b"new raw" * 20000)
        zipped = Path(str(raw) + ".gz")
        zipped.write_bytes(b"existing evidence")
        with patch.object(spool.gzip, "GzipFile", side_effect=AssertionError("must skip early")):
            self.assertEqual("skipped", spool.compress(raw)["status"])
        self.assertEqual(b"existing evidence", zipped.read_bytes())
        zipped.unlink()
        zipped.symlink_to(self.root / "absent-evidence")
        self.assertEqual("skipped", spool.compress(raw)["status"])
        self.assertTrue(zipped.is_symlink())
        self.assertEqual(b"new raw" * 20000, raw.read_bytes())
        self.assert_no_temps()

    def test_compress_does_not_replace_gzip_created_during_publication(self):
        raw = self.file("race.log", b"new raw" * 20000)
        zipped = Path(str(raw) + ".gz")
        original_link = spool.os.link
        raced = []

        def racing_link(*args, **kwargs):
            zipped.write_bytes(b"concurrent evidence")
            raced.append(True)
            return original_link(*args, **kwargs)

        with patch.object(spool.os, "link", side_effect=racing_link):
            self.assertEqual("skipped", spool.compress(raw)["status"])
        self.assertTrue(raced)
        self.assertEqual(b"concurrent evidence", zipped.read_bytes())
        self.assertEqual(b"new raw" * 20000, raw.read_bytes())
        self.assert_no_temps()

    def test_compress_incompressible(self):
        body = os.urandom(65536)
        path = self.file("noise.log", body)
        self.assertEqual("not-smaller", spool.compress(path)["status"])
        self.assertFalse(Path(str(path) + ".gz").exists())
        self.assertEqual(body, path.read_bytes())
        self.assert_no_temps()

    def test_compress_above_view_cap_keeps_sparse_raw_without_reading(self):
        raw = self.file("above-cap.log", b"")
        with raw.open("r+b") as target:
            target.truncate(spool.MAX_INFLATE_BYTES + 1)
        with patch.object(spool.os, "read", side_effect=AssertionError("must not read oversized raw")):
            self.assertEqual("skipped", spool.compress(raw)["status"])
        self.assertEqual(spool.MAX_INFLATE_BYTES + 1, raw.stat().st_size)
        self.assertFalse(Path(str(raw) + ".gz").exists())
        self.assert_no_temps()

    def test_compress_parent_selected_temporary_cli_and_private_mode(self):
        raw = self.file("parent-owned.log", b"raw output\n" * 20000)
        temporary = raw.parent / ".mevedel-spool-parent-chosen"
        original_open = spool.os.open
        opened = []

        def record_open(name, flags, *args, **kwargs):
            fd = original_open(name, flags, *args, **kwargs)
            if name == temporary.name:
                opened.append((flags, os.fstat(fd).st_mode & 0o777))
            return fd

        with patch.object(spool.os, "open", side_effect=record_open):
            self.assertEqual("compressed", spool.compress(raw, temporary)["status"])
        self.assertEqual(1, len(opened))
        flags, mode = opened[0]
        self.assertEqual(0o600, mode)
        self.assertTrue(flags & os.O_EXCL)
        self.assertTrue(flags & os.O_NOFOLLOW)
        self.assertTrue(flags & os.O_CREAT)
        self.assertFalse(temporary.exists())
        self.assertEqual(raw.read_bytes(), gzip.decompress(Path(str(raw) + ".gz").read_bytes()))
        Path(str(raw) + ".gz").unlink()
        self.assertEqual("compressed", self.cli("--compress", raw, "--temporary", temporary)["status"])
        self.assertFalse(temporary.exists())
        self.assert_no_temps()

    def test_compress_parent_temporary_rejects_unsafe_paths_without_changes(self):
        raw = self.file("parent-owned.log", b"raw output\n" * 20000)
        for temporary in (self.root / ".mevedel-spool-wrong-directory",
                          self.source / "not-private",
                          raw):
            with self.subTest(temporary=temporary):
                facts = self.cli("--compress", raw, "--temporary", temporary, success=False)
                self.assertIn("private", facts["error"])
                self.assertEqual(b"raw output\n" * 20000, raw.read_bytes())
                self.assertFalse(Path(str(raw) + ".gz").exists())
        self.assert_no_temps()

    def test_compress_parent_temporary_never_overwrites_existing_entry(self):
        raw = self.file("parent-owned.log", b"raw output\n" * 20000)
        temporary = self.source / ".mevedel-spool-parent-chosen"
        target = self.file("precious", b"preserve this")
        for kind in ("file", "symlink", "dangling-symlink", "directory"):
            with self.subTest(kind=kind):
                if kind == "file":
                    temporary.write_bytes(b"existing temporary")
                elif kind == "directory":
                    temporary.mkdir()
                else:
                    temporary.symlink_to(target if kind == "symlink" else self.root / "missing")
                facts = self.cli("--compress", raw, "--temporary", temporary, success=False)
                self.assertIn("File exists", facts["error"])
                self.assertEqual(b"preserve this", target.read_bytes())
                self.assertFalse(Path(str(raw) + ".gz").exists())
                if kind == "file":
                    self.assertEqual(b"existing temporary", temporary.read_bytes())
                elif kind == "directory":
                    self.assertTrue(temporary.is_dir())
                else:
                    self.assertTrue(temporary.is_symlink())
                if kind == "directory":
                    temporary.rmdir()
                else:
                    temporary.unlink()
        self.assertEqual(b"raw output\n" * 20000, raw.read_bytes())
        self.assert_no_temps()

    def test_compress_parent_temporary_removed_on_error_and_incompressible(self):
        raw = self.file("parent-owned.log", b"raw output\n" * 20000)
        temporary = self.source / ".mevedel-spool-parent-chosen"
        with patch.object(spool.os, "fsync", side_effect=OSError("write fault")):
            with self.assertRaisesRegex(OSError, "write fault"):
                spool.compress(raw, temporary)
        self.assertFalse(temporary.exists())
        self.assertEqual(b"raw output\n" * 20000, raw.read_bytes())
        raw.write_bytes(os.urandom(65536))
        self.assertEqual("not-smaller", spool.compress(raw, temporary)["status"])
        self.assertFalse(temporary.exists())
        self.assertFalse(Path(str(raw) + ".gz").exists())
        self.assert_no_temps()

    def test_view_rejects_compression_temporary_option(self):
        raw = self.file("ordinary")
        temporary = self.source / ".mevedel-spool-not-for-view"
        facts = self.cli("--view", raw, self.dest, "read", "--temporary", temporary, success=False)
        self.assertIn("requires compression", facts["error"])
        self.assertFalse(self.dest.exists())
        self.assertFalse(temporary.exists())

    def test_compress_rejects_file_and_directory_symlinks(self):
        path = self.file("real.log", b"x" * 65536)
        alias = self.source / "alias.log"
        alias.symlink_to(path)
        self.assertEqual("skipped", spool.compress(alias)["status"])
        directory = self.root / "alias"
        directory.symlink_to(self.source, target_is_directory=True)
        self.cli("--compress", directory / "real.log", success=False)
        self.assertTrue(path.exists())
        self.assertFalse(Path(str(alias) + ".gz").exists())

    def test_compress_faults_preserve_original_and_clean_private_temp(self):
        body = b"x" * 100000
        for function in ("gzip.GzipFile.write", "os.fsync", "os.link"):
            with self.subTest(function=function):
                path = self.file("fault.log", body)
                with patch.object(spool.gzip.GzipFile, "write", side_effect=OSError("write fault")) if function == "gzip.GzipFile.write" else patch.object(spool.os, function.split(".")[1], side_effect=OSError("publish fault")):
                    with self.assertRaises(OSError):
                        spool.compress(path)
                self.assertEqual(body, path.read_bytes())
                self.assertFalse(Path(str(path) + ".gz").exists())
                self.assert_no_temps()

    def test_compress_size_mtime_identity_races_do_not_publish(self):
        original_read = spool.os.read
        for change in ("size", "mtime", "identity"):
            with self.subTest(change=change):
                path = self.file("race.log", b"a" * 100000)
                touched = []

                def racing_read(fd, count):
                    block = original_read(fd, count)
                    if not touched:
                        touched.append(True)
                        if change == "size":
                            with path.open("ab") as target:
                                target.write(b"changed")
                        elif change == "mtime":
                            info = path.stat()
                            os.utime(path, ns=(info.st_atime_ns, info.st_mtime_ns + 1000000))
                        else:
                            replacement = self.file("replacement.log", b"b" * 100000)
                            os.replace(replacement, path)
                    return block

                with patch.object(spool.os, "read", side_effect=racing_read):
                    self.assertEqual("changed", spool.compress(path)["status"])
                self.assertTrue(touched)
                self.assertTrue(path.exists())
                self.assertFalse(Path(str(path) + ".gz").exists())
                self.assert_no_temps()

    def test_compress_post_publish_change_keeps_raw(self):
        path = self.file("race.log", b"a" * 100000)
        link = spool.os.link

        def racing_link(*args, **kwargs):
            link(*args, **kwargs)
            path.write_bytes(b"new output")

        with patch.object(spool.os, "link", side_effect=racing_link):
            self.assertEqual("changed", spool.compress(path)["status"])
        self.assertEqual(b"new output", path.read_bytes())
        self.assertTrue(Path(str(path) + ".gz").exists())

    def test_compress_post_publish_gzip_replacement_is_not_success(self):
        raw = self.file("race.log", b"a" * 100000)
        zipped = Path(str(raw) + ".gz")
        link = spool.os.link
        replaced = []

        def racing_link(*args, **kwargs):
            link(*args, **kwargs)
            replacement = self.file("replacement.gz", b"replacement evidence")
            os.replace(replacement, zipped)
            replaced.append(True)

        with patch.object(spool.os, "link", side_effect=racing_link):
            self.assertEqual("changed", spool.compress(raw)["status"])
        self.assertTrue(replaced)
        self.assertEqual(b"a" * 100000, raw.read_bytes())
        self.assertEqual(b"replacement evidence", zipped.read_bytes())
        self.assert_no_temps()

    def test_identity_and_unchanged_checks_open_and_named_file(self):
        path = self.file("identity.log")
        with spool.directory_fd(self.source) as parent:
            fd = os.open(path, os.O_RDONLY)
            try:
                info = os.fstat(fd)
                self.assertTrue(spool.unchanged(fd, parent, path.name, info))
                self.assertEqual((info.st_dev, info.st_ino, info.st_size, info.st_mtime_ns),
                                 spool.identity(info))
                path.unlink()
                self.assertFalse(spool.unchanged(fd, parent, path.name, info))
            finally:
                os.close(fd)

    def test_view_file_cli_preserves_binary_and_logical_destination(self):
        body = b"binary\x00\xff\r\nlast"
        path = self.zipped("arbitrary/job.log.gz", body)
        dest = self.root / "job.log"
        self.assertEqual({"path": str(dest)}, self.cli("--view", path, dest, "read"))
        self.assertEqual(body, dest.read_bytes())
        self.assertEqual(body, gzip.decompress(path.read_bytes()))

    def test_view_raw_wins_compressed_explicit_and_directory(self):
        path = self.zipped(body=b"stale compressed")
        raw = self.file("executions/job.log", b"current raw")
        spool.view(path, self.dest, "read")
        self.assertEqual(raw.read_bytes(), self.dest.read_bytes())
        self.dest.unlink()
        spool.view(self.source, self.dest, "grep")
        self.assertEqual(b"current raw", (self.dest / "executions/job.log").read_bytes())
        self.assertFalse((self.dest / "executions/job.log.gz").exists())

    def test_view_missing_raw_file_falls_back_to_same_parent_gzip(self):
        path = self.zipped(body=b"retired raw")
        self.cli("--view", str(path)[:-3], self.dest, "read")
        self.assertEqual(b"retired raw", self.dest.read_bytes())

    def test_view_raw_disappears_during_open_falls_back(self):
        path = self.zipped(body=b"compressed")
        raw = self.file("executions/job.log", b"raw")
        original_open = spool.os.open
        removed = []

        def racing_open(name, *args, **kwargs):
            if name == raw.name and not removed:
                raw.unlink()
                removed.append(True)
            return original_open(name, *args, **kwargs)

        with patch.object(spool.os, "open", side_effect=racing_open):
            spool.view(raw, self.dest, "read")
        self.assertTrue(removed)
        self.assertEqual(b"compressed", self.dest.read_bytes())
        self.assertTrue(path.exists())

    def test_directory_raw_disappears_after_listing(self):
        self.zipped(body=b"compressed")
        raw = self.file("executions/job.log", b"raw")
        original_stat = spool.os.stat
        removed = []

        def racing_stat(name, *args, **kwargs):
            if name == raw.name and not removed:
                removed.append(True)
                raw.unlink()
            return original_stat(name, *args, **kwargs)

        with patch.object(spool.os, "stat", side_effect=racing_stat):
            spool.view(self.source, self.dest, "grep")
        self.assertTrue(removed)
        self.assertEqual(b"compressed", (self.dest / "executions/job.log").read_bytes())

    def test_directory_read_and_glob_use_placeholders_without_inflation(self):
        self.file("executions/job.log.gz", b"deliberately corrupt gzip")
        raw = self.file("executions/raw.log", b"raw bytes")
        for mode in ("read", "glob"):
            with self.subTest(mode=mode):
                destination = self.root / mode
                spool.view(self.source, destination, mode)
                self.assertEqual(b"", (destination / "executions/job.log").read_bytes())
                self.assertEqual(raw.read_bytes(), (destination / "executions/raw.log").read_bytes())

    def test_file_glob_is_placeholder_but_file_read_and_grep_inflate(self):
        path = self.zipped(body=b"inflate me")
        for mode in ("read", "grep", "glob"):
            with self.subTest(mode=mode):
                dest = self.root / mode
                spool.view(path, dest, mode)
                self.assertEqual(b"" if mode == "glob" else b"inflate me", dest.read_bytes())

    def test_directory_only_normalizes_execution_gzip_names(self):
        ordinary_gzip = self.zipped("notes.log.gz", b"not execution output")
        ordinary_suffix = self.zipped("executions/data.gz", b"not a log")
        self.zipped("nested/executions/job.log.gz", b"execution")
        spool.view(self.source, self.dest, "grep")
        self.assertEqual(ordinary_gzip.read_bytes(), (self.dest / "notes.log.gz").read_bytes())
        self.assertEqual(ordinary_suffix.read_bytes(), (self.dest / "executions/data.gz").read_bytes())
        self.assertEqual(b"execution", (self.dest / "nested/executions/job.log").read_bytes())

    def test_view_hardlink_and_copy_fallback(self):
        path = self.file("ordinary")
        spool.view(path, self.dest, "read")
        self.assertEqual(path.stat().st_ino, self.dest.stat().st_ino)
        self.dest.unlink()
        with patch.object(spool.os, "link", side_effect=OSError("cross device")):
            spool.view(path, self.dest, "read")
        self.assertNotEqual(path.stat().st_ino, self.dest.stat().st_ino)
        self.assertEqual(path.read_bytes(), self.dest.read_bytes())

    def test_hardlink_entry_replaced_by_symlink_copies_pinned_bytes(self):
        path = self.file("ordinary", b"authorized")
        outside = self.root / "outside"
        outside.write_bytes(b"not authorized")
        original_link = spool.os.link
        raced = []

        def racing_link(*args, **kwargs):
            path.unlink()
            path.symlink_to(outside)
            raced.append(True)
            original_link(*args, **kwargs)

        with patch.object(spool.os, "link", side_effect=racing_link):
            spool.view(path, self.dest, "read")
        self.assertTrue(raced)
        self.assertFalse(self.dest.is_symlink())
        self.assertEqual(b"authorized", self.dest.read_bytes())

    def test_view_hides_private_paths_at_every_depth(self):
        self.file("top/.mevedel-pending-executions/private.log", b"secret")
        self.file("top/.mevedel-spool-temporary", b"secret")
        self.file("executions/.mevedel-pending-executions/private.log.gz", b"secret")
        self.file("visible", b"public")
        spool.view(self.source, self.dest, "grep")
        self.assertEqual(["executions", "top", "visible"], sorted(p.name for p in self.dest.iterdir()))
        self.assertEqual([], list((self.dest / "top").iterdir()))
        self.assertEqual([], list((self.dest / "executions").iterdir()))
        for name in ("a/.mevedel-pending-executions/z", "a/.mevedel-spool-x"):
            self.assertTrue(spool.hidden(name))
            with self.assertRaisesRegex(ValueError, "Private"):
                spool.view(self.source / name, self.root / "private", "read")
        self.assertFalse(spool.hidden("a/public.log"))

    def test_view_skips_symlinks_and_special_files(self):
        raw = self.file("executions/raw.log", b"inside")
        outside = self.root / "outside"
        outside.write_bytes(b"outside")
        (self.source / "outside-link").symlink_to(outside)
        (self.source / "inside-link").symlink_to(raw)
        (self.source / "directory-link").symlink_to(self.root, target_is_directory=True)
        os.mkfifo(self.source / "pipe")
        spool.view(self.source, self.dest, "grep")
        self.assertEqual(["executions"], [p.name for p in self.dest.iterdir()])
        for name in ("outside-link", "inside-link", "directory-link", "pipe"):
            with self.subTest(name=name):
                self.cli("--view", self.source / name, self.root / name, "read", success=False)
        self.assertEqual(b"outside", outside.read_bytes())

    def test_raw_symlink_does_not_expose_gzip(self):
        path = self.zipped(body=b"compressed")
        (path.parent / "job.log").symlink_to(self.file("other", b"raw"))
        spool.view(path.parent, self.dest, "grep")
        self.assertEqual([], list(self.dest.iterdir()))
        self.cli("--view", path, self.root / "explicit", "read", success=False)

    def test_corrupt_and_truncated_gzip_fail_cleanly(self):
        for body in (b"not gzip", gzip.compress(b"valid")[:-4],
                     gzip.compress(b"valid")[:-8] + b"\0" * 8):
            with self.subTest(body=body):
                path = self.file("executions/job.log.gz", body)
                facts = self.cli("--view", path, self.dest, "read", success=False)
                self.assertIn("error", facts)
                self.assertFalse(self.dest.exists())
                self.assertEqual(body, path.read_bytes())

    def test_oversize_gzip_actual_64_mib_cap_preserves_source(self):
        path = self.zipped(body=b"x" * (spool.MAX_INFLATE_BYTES + 1))
        original = path.read_bytes()
        result = self.cli("--view", path, self.dest, "grep", success=False)
        self.assertIn("64 MiB", result["error"])
        self.assertFalse(self.dest.exists())
        self.assertEqual(original, path.read_bytes())

    def test_exact_inflate_cap_and_prefix_bytes(self):
        with patch.object(spool, "MAX_INFLATE_BYTES", 8):
            output = io.BytesIO()
            self.assertEqual(8, spool.inflate(io.BytesIO(gzip.compress(b"12345678")), output))
            self.assertEqual(b"12345678", output.getvalue())
            output = io.BytesIO()
            self.assertEqual(4, spool.inflate(io.BytesIO(gzip.compress(b"a\nb\n" + b"x" * 100)), output, 2))
            self.assertEqual(b"a\nb\n", output.getvalue())

    def test_line_prefix_cli_preserves_original_line_positions(self):
        path = self.zipped(body=b"one\r\ntwo\x00\xff\nthree\nfour\n")
        self.cli("--view", path, self.dest, "read", "--max-lines", 3)
        self.assertEqual(b"one\r\ntwo\x00\xff\nthree\n", self.dest.read_bytes())

    def test_prefix_reads_stop_early_and_handle_split_and_unterminated_lines(self):
        for body, lines, expected in ((b"a\nb\nc\n", 2, b"a\nb\n"),
                                      (b"a\nb", 3, b"a\nb"),
                                      (b"", 2, b""),
                                      (b"abcd\nefgh\nx", 2, b"abcd\nefgh\n")):
            with self.subTest(body=body):
                output = io.BytesIO()
                with patch.object(spool, "CHUNK_BYTES", 3):
                    spool.inflate(io.BytesIO(gzip.compress(body)), output, lines)
                self.assertEqual(expected, output.getvalue())
        path = self.zipped(body=b"first\n" + b"x" * (spool.MAX_INFLATE_BYTES + 1))
        self.cli("--view", path, self.dest, "read", "--max-lines", 1)
        self.assertEqual(b"first\n", self.dest.read_bytes())

    def test_directory_total_budget_for_hardlinks_copy_and_inflation(self):
        self.file("first", b"12345")
        self.file("second", b"67890")
        for copy in (False, True):
            with self.subTest(copy=copy), patch.object(spool, "MAX_VIEW_BYTES", 9):
                with patch.object(spool.os, "link", side_effect=OSError("copy")) if copy else patch.object(spool, "CHUNK_BYTES", 3):
                    with self.assertRaisesRegex(ValueError, "512 MiB"):
                        spool.view(self.source, self.dest, "grep")
                self.assertFalse(self.dest.exists())
        self.zipped(body=b"1234567890")
        with patch.object(spool, "MAX_VIEW_BYTES", 9):
            with self.assertRaisesRegex(ValueError, "512 MiB"):
                spool.view(self.source / "executions", self.dest, "grep")
        self.assertFalse(self.dest.exists())

    def test_directory_failure_removes_partial_view_not_sources(self):
        good = self.file("a-good", b"still here")
        bad = self.file("executions/job.log.gz", b"corrupt")
        self.cli("--view", self.source, self.dest, "grep", success=False)
        self.assertFalse(self.dest.exists())
        self.assertEqual(b"still here", good.read_bytes())
        self.assertEqual(b"corrupt", bad.read_bytes())

    def test_view_accepts_empty_directory_but_preserves_existing_destinations(self):
        self.dest.mkdir()
        self.file("ordinary")
        spool.view(self.source, self.dest, "read")
        with self.assertRaisesRegex(ValueError, "empty directory"):
            spool.view(self.source, self.dest, "read")
        self.assertTrue((self.dest / "ordinary").exists())
        existing = self.root / "existing"
        existing.write_bytes(b"preserve me")
        self.cli("--view", self.source / "ordinary", existing, "read", success=False)
        self.assertEqual(b"preserve me", existing.read_bytes())

    def test_invalid_view_options_and_source_ancestor_symlink(self):
        path = self.zipped()
        for mode, lines in (("wrong", None), ("grep", 1), ("read", 0), ("read", -1)):
            with self.subTest(mode=mode, lines=lines), self.assertRaises(ValueError):
                spool.view(path, self.dest, mode, lines)
        with self.assertRaisesRegex(ValueError, "file source"):
            spool.view(self.source, self.dest, "read", 1)
        with self.assertRaisesRegex(ValueError, "outside source"):
            spool.view(self.source, self.source / "view", "read")
        alias = self.root / "alias"
        alias.symlink_to(self.source, target_is_directory=True)
        self.cli("--view", alias / "executions/job.log.gz", self.dest, "read", success=False)
        self.cli("--compress", path, "--max-lines", 1, success=False)

    def test_main_error_is_bounded_json_and_missing_raw_fails(self):
        with patch.object(spool, "compress", side_effect=OSError("x" * 5000)):
            output = io.StringIO()
            with redirect_stdout(output):
                self.assertEqual(1, spool.main(["--compress", "missing.log"]))
            self.assertEqual(512, len(json.loads(output.getvalue())["error"]))
        self.cli("--view", self.source / "missing.log", self.dest, "read", success=False)
        self.cli("--view", self.source / "missing.txt", self.dest, "read", success=False)
        self.assertFalse(self.dest.exists())


if __name__ == "__main__":
    unittest.main()
