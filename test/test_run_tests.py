"""Checks for complete-suite partitioning and measurement reporting."""

import csv
import contextlib
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch
from pathlib import Path

import run_tests


class RunnerTests(unittest.TestCase):
    def test_public_cli_preserves_native_worker_exit_and_bounds_failures(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory)
            executable = path / 'eask'
            executable.write_text(f'''#!{sys.executable}
import json, os, pathlib, sys
if sys.argv[1] == 'clean':
    sys.exit(0)
names = ['failing-' + str(i) for i in range(30)]
output = pathlib.Path(os.environ['MEVEDEL_TEST_RESULTS'])
if 'MEVEDEL_TEST_PARTITION' not in os.environ:
    output.write_text(json.dumps(names))
else:
    request = json.loads(pathlib.Path(os.environ['MEVEDEL_TEST_PARTITION']).read_text())
    output.write_text(json.dumps(dict(ert_seconds=1, tests=[dict(
        test=name, seconds=0.01, status='ert-test-failed', expected=False)
        for name in request['tests']])))
    print('native failure diagnostics')
    sys.exit(7)
''')
            executable.chmod(0o755)
            output = path / 'reports'
            result = subprocess.run(
                [sys.executable, str(run_tests.ROOT / 'test/run_tests.py'),
                 'test/test-mevedel-test-runner.el', '--jobs', '1', '--output', str(output)],
                env={**os.environ, 'PATH': str(path)}, capture_output=True, text=True)
            self.assertEqual(result.returncode, 7, result.stderr)
            self.assertEqual(result.stdout.count('FAIL: '), 20)
            self.assertIn('10 more', result.stdout)
            self.assertLess(len(result.stdout.splitlines()), 30)
            self.assertIn('native failure diagnostics', (output / '0.log').read_text())
            self.assertEqual(json.loads((output / 'summary.json').read_text())['unexpected'], 30)

    def test_file_selection_expands_deduplicates_and_rejects_typos(self):
        expected = ['test/test-mevedel-test-runner.el']
        self.assertEqual(run_tests.select_files(['test/test-mevedel-test-runne?.el',
                                                expected[0]]), expected)
        self.assertEqual(run_tests.select_files([str(run_tests.ROOT / expected[0])]), expected)
        for pattern in ('test/not-present*.el', 'README.md', '/etc/passwd'):
            with self.subTest(pattern=pattern), self.assertRaises(ValueError):
                run_tests.select_files([pattern])

    def test_replay_keeps_order_and_validates_saved_roster(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'partition.json'
            request = dict(files=['test/test-mevedel-test-runner.el'],
                           inventory=['a', 'b'], tests=['b', 'a'])
            path.write_text(json.dumps(request))
            self.assertEqual(run_tests.read_replay(path), request)
            for change in (dict(tests=['a', 'a']), dict(tests=['missing']),
                           dict(files=['test/missing.el']), dict(inventory=[])):
                path.write_text(json.dumps({**request, **change}))
                with self.subTest(change=change), self.assertRaises(ValueError):
                    run_tests.read_replay(path)

    def test_focused_fresh_and_replay_lanes_preserve_failures_and_history(self):
        for lane in ('focused', 'fresh', 'replay'):
            with self.subTest(lane=lane), tempfile.TemporaryDirectory() as directory:
                path = Path(directory)
                output = path / 'reports'
                history = path / 'latest-durations.csv'
                history.write_text('previous timings\n')
                files = ['test/test-mevedel-test-runner.el']
                request = dict(files=files, inventory=['a', 'b'], tests=['b', 'a'])
                saved = path / 'saved.json'
                saved.write_text(json.dumps(request))
                arguments = ['--replay', str(saved)] if lane == 'replay' else files
                if lane == 'fresh':
                    arguments = ['--fresh-files', *files]
                commands = []

                def run(command, log, env=None):
                    commands.append(command)
                    log.write_text('native log\n')
                    if log.name == 'inventory.log':
                        Path(env['MEVEDEL_TEST_RESULTS']).write_text('["a", "b"]')
                    elif log.name == '0.log':
                        if lane == 'fresh':
                            self.assertEqual(env['MEVEDEL_TEST_FRESH_FILE'], '1')
                            names = ['a', 'b']
                        else:
                            assignment = json.loads(Path(env['MEVEDEL_TEST_PARTITION']).read_text())
                            self.assertEqual(assignment['files'], files)
                            names = assignment['tests']
                            if lane == 'replay':
                                self.assertEqual(assignment, request)
                        Path(env['MEVEDEL_TEST_RESULTS']).write_text(json.dumps(dict(
                            inventory=['a', 'b'], ert_seconds=1,
                            tests=[dict(test=name, seconds=0.5, status='ert-test-failed',
                                        expected=False) for name in names])))
                        return 7, 1
                    return 0, 1

                console = io.StringIO()
                with patch.object(sys, 'argv', ['run_tests.py', '--jobs', '1',
                                               '--output', str(output), '--durations',
                                               str(path / 'absent.csv'), *arguments]), \
                     patch.object(run_tests, 'REPORTS', path), \
                     patch.object(run_tests, 'run_logged', side_effect=run), \
                     contextlib.redirect_stdout(console):
                    self.assertEqual(run_tests.main(), 7)
                self.assertEqual(history.read_text(), 'previous timings\n')
                self.assertIn('FAIL: b', console.getvalue())
                self.assertEqual(commands[0], run_tests.EASK + ['clean', 'elc'])
                self.assertEqual(commands[-1][-1:], files)
                self.assertEqual(json.loads((output / '0-partition.json').read_text())['files'], files)
                self.assertEqual(json.loads((output / 'summary.json').read_text())['count'], 2)

    def test_fresh_lane_rejects_cross_file_duplicate_names_in_summary(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / 'reports'

            def run(command, log, env=None):
                log.write_text('native log\n')
                if log.name != 'clean.log':
                    Path(env['MEVEDEL_TEST_RESULTS']).write_text(json.dumps(dict(
                        inventory=['duplicate'], ert_seconds=1,
                        tests=[dict(test='duplicate', seconds=1,
                                    status='ert-test-passed', expected=True)])))
                return 0, 1

            with patch.object(sys, 'argv', ['run_tests.py', '--fresh-files', '--output',
                                           str(output), 'test/test-mevedel-test-runner.el',
                                           'test/test-mevedel-test-helpers.el']), \
                 patch.object(run_tests, 'run_logged', side_effect=run), \
                 contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(run_tests.main(), 1)
            summary = json.loads((output / 'summary.json').read_text())
            self.assertTrue(summary['failed'])
            self.assertIn('exactly once', summary['error'])

    def test_crashed_fresh_file_does_not_claim_complete_inventory(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / 'reports'

            def run(command, log, env=None):
                log.write_text('load failed\n')
                return (3 if log.name == '0.log' else 0), 1

            with patch.object(sys, 'argv', ['run_tests.py', '--fresh-files', '--output',
                                           str(output), 'test/test-mevedel-test-runner.el']), \
                 patch.object(run_tests, 'run_logged', side_effect=run), \
                 contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(run_tests.main(), 3)
            summary = json.loads((output / 'summary.json').read_text())
            self.assertTrue(summary['failed'])
            self.assertIsNone(summary['expected_count'])
            self.assertEqual(summary['workers'][0]['files'], ['test/test-mevedel-test-runner.el'])

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

    def test_launcher_does_not_inherit_the_parent_eask_launch(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory)
            executable = path / 'eask'
            executable.write_text('#!/bin/sh\necho "${PKG_EXECPATH-unset}"\nexit 3\n')
            executable.chmod(0o755)
            output = path / 'reports'
            subprocess.run(
                [sys.executable, str(run_tests.ROOT / 'test/run_tests.py'),
                 '--output', str(output)],
                env={**os.environ, 'PATH': str(path), 'PKG_EXECPATH': '/parent/eask'},
                capture_output=True, text=True)
            self.assertEqual((output / 'clean.log').read_text(), 'unset\n')

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

    def test_crashed_worker_keeps_summary_and_identifies_its_log(self):
        for broken, exit_code in ((None, 1), ('{', 0), ('{"tests": []}', 0)):
            with self.subTest(results=broken), tempfile.TemporaryDirectory() as directory:
                path = Path(directory)
                output = path / 'reports'
                history = path / 'latest-durations.csv'
                history.write_text('previous successful timings\n')

                def run(command, log, env=None):
                    log.write_text('error in process filter: No match data\n')
                    if log.name == 'inventory.log':
                        Path(env['MEVEDEL_TEST_RESULTS']).write_text('["a", "b"]')
                    elif log.name == '0.log':
                        Path(env['MEVEDEL_TEST_RESULTS']).write_text(json.dumps({
                            'ert_seconds': 1,
                            'tests': [dict(test='a', seconds=1,
                                           status='ert-test-passed', expected=True)]}))
                    elif log.name == '1.log':
                        if broken is not None:
                            Path(env['MEVEDEL_TEST_RESULTS']).write_text(broken)
                        return exit_code, 1
                    return 0, 1

                console = io.StringIO()
                with patch.object(sys, 'argv', ['run_tests.py', '--jobs', '2',
                                               '--output', str(output), '--durations',
                                               str(path / 'absent.csv')]), \
                     patch.object(run_tests, 'REPORTS', path), \
                     patch.object(run_tests, 'run_logged', side_effect=run), \
                     contextlib.redirect_stdout(console):
                    self.assertEqual(run_tests.main(), 1)
                summary = json.loads((output / 'summary.json').read_text())
                self.assertEqual(summary['count'], 1)
                self.assertEqual(summary['expected_count'], 2)
                self.assertEqual(summary['workers'][1]['exit_code'], exit_code)
                self.assertTrue(summary['workers'][1]['error'])
                self.assertIn(str(output / '1.log'), console.getvalue())
                self.assertEqual(history.read_text(), 'previous successful timings\n')
                with (output / 'durations.csv').open() as stream:
                    self.assertEqual([row['test'] for row in csv.DictReader(stream)], ['a'])

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
