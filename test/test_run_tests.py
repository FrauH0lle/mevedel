"""Checks for complete-suite partitioning and measurement reporting."""

import csv
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import run_tests


class RunnerTests(unittest.TestCase):
    def test_default_concurrency_respects_cpu_affinity(self):
        original = os.sched_getaffinity(0)
        try:
            for count in sorted({1, min(2, len(original)), len(original)}):
                with self.subTest(cpus=count):
                    os.sched_setaffinity(0, set(sorted(original)[:count]))
                    result = subprocess.run(
                        [sys.executable, str(run_tests.ROOT / 'test/run_tests.py'), '--help'],
                        capture_output=True, text=True, check=True)
                    self.assertIn(f'default: {min(8, count)}; {count} CPUs available',
                                  ' '.join(result.stdout.split()))
        finally:
            os.sched_setaffinity(0, original)

    def test_launcher_prefers_installed_eask_and_preserves_cleanup_failure(self):
        for installed in (True, False):
            with self.subTest(installed=installed), tempfile.TemporaryDirectory() as directory:
                path = Path(directory)
                for name in (('eask', 'npx') if installed else ('npx',)):
                    executable = path / name
                    executable.write_text(f'#!/bin/sh\necho {name} "$@"\nexit 3\n')
                    executable.chmod(0o755)
                output = path / 'reports'
                result = subprocess.run(
                    [sys.executable, str(run_tests.ROOT / 'test/run_tests.py'),
                     '--output', str(output)],
                    env={**os.environ, 'PATH': str(path)},
                    capture_output=True, text=True)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn('Cleanup failed', result.stderr)
                launcher = 'eask' if installed else 'npx --offline @emacs-eask/cli'
                self.assertEqual((output / 'clean.log').read_text(),
                                 f'{launcher} clean elc\n')

    def test_partition_covers_inventory_and_balances_measured_cost(self):
        names = ['slow', 'medium', 'short', 'new']
        groups = run_tests.partition_tests(names, {'slow': 10, 'medium': 6, 'short': 4}, 2)
        self.assertEqual(sorted(sum(groups, [])), sorted(names))
        self.assertEqual(groups, [['slow', 'new'], ['medium', 'short']])
        self.assertEqual(run_tests.partition_tests(names, {}, 20), [[n] for n in sorted(names)])
        self.assertTrue(all(run_tests.partition_tests(names, dict.fromkeys(names, 0), 2)))
        for inventory, weights, jobs in [([], {}, 1), (['a', 'a'], {}, 1),
                                         (names, {}, 0), (names, {'slow': -1}, 2),
                                         (names, {'slow': float('nan')}, 2)]:
            with self.assertRaises(ValueError):
                run_tests.partition_tests(inventory, weights, jobs)

    def test_results_reject_missing_duplicate_and_extra_cases(self):
        run_tests.check_results(['a', 'b'], [{'test': 'b'}, {'test': 'a'}])
        for observed in [[], ['a'], ['a', 'a'], ['a', 'b', 'extra']]:
            with self.assertRaises(ValueError):
                run_tests.check_results(['a', 'b'], [{'test': name} for name in observed])

    def test_report_and_subprocess_failure_remain_observable(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory)
            code, seconds = run_tests.run_logged(
                [sys.executable, '-c', 'print("failure details"); raise SystemExit(3)'],
                path / 'worker.log')
            self.assertEqual(code, 3)
            self.assertGreater(seconds, 0)
            self.assertIn('failure details', (path / 'worker.log').read_text())
            rows = [dict(test=name, seconds=duration, status='ert-test-passed', expected=True)
                    for name, duration in [('fast', 1), ('slow', 3)]]
            run_tests.write_durations(path / 'durations.csv', rows, 2)
            with (path / 'durations.csv').open() as stream:
                report = list(csv.DictReader(stream))
            self.assertEqual([row['test'] for row in report], ['slow', 'fast'])
            self.assertEqual(float(report[-1]['cumulative_seconds']), 4)
            self.assertEqual(float(report[0]['test_time_percent']), 75)
            self.assertEqual(float(report[0]['wall_time_percent']), 150)


if __name__ == '__main__':
    unittest.main()
