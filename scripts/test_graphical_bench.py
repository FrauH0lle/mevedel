"""Check the presentation gate without running a compositor."""

import io
from pathlib import Path
import unittest
from unittest.mock import patch

import graphical_bench


class GraphicalBenchTests(unittest.TestCase):
    def test_requires_both_presentation_probes(self):
        def client(load_path, bench, form, probe_form, probes):
            probes[0].write_text("wl_callback@7.done(123)\n" * 20)
            return []

        with (patch.object(graphical_bench, "compile_build", return_value="nil"),
              patch.object(graphical_bench, "client_command", side_effect=client),
              patch.object(graphical_bench, "load_kwin_runner") as kwin,
              patch("sys.stdout", new_callable=io.StringIO),
              patch("sys.stderr", new_callable=io.StringIO),
              self.assertRaises(SystemExit) as failure):
            kwin.return_value.run_session.return_value = 0
            graphical_bench.run_benchmark(Path("."), Path("bench.el"), str, "(ignore)")
        self.assertEqual(failure.exception.code, 1)
