#!/usr/bin/env python3
"""Run selected ERT files (or the full suite), with bounded output and native logs."""

import argparse
import concurrent.futures
import csv
import datetime
import json
import math
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

import affected_tests

ROOT = Path(__file__).resolve().parent.parent
REPORTS = ROOT / '.scratch/test-suite-performance'
EASK = ['eask'] if shutil.which('eask') else ['npx', '--offline', '@emacs-eask/cli']


def partition_tests(names, weights, jobs, files=None):
    """Balance known durations, assigning every discovered case exactly once.

    Cases stay together so per-worker fixtures are built once: a whole file
    (FILES maps names to defining files), else a whole deftest (names differing
    only by a final @N), else contiguous runs of its cases.  A unit is split only
    when it alone exceeds one worker's share of the total.  Within a unit, cases
    keep inventory order; units are placed longest first on the least loaded
    worker.  Cases without a timing have unknown cost, so they are dealt
    round-robin afterwards, least loaded worker first: a new deftest spreads
    evenly instead of landing on one worker.  Timings below a millisecond
    count one millisecond."""
    if jobs < 1 or not names or len(names) != len(set(names)):
        raise ValueError('Expected unique tests and at least one worker')
    if any(not math.isfinite(value) or value < 0 for value in weights.values()):
        raise ValueError('Durations must be finite and nonnegative')
    measured = [name for name in names if name in weights]
    unmeasured = [name for name in names if name not in weights]
    cost = {name: max(weights[name], 0.001) for name in measured}
    share = sum(cost.values()) / min(jobs, len(names))
    units = []

    def split(cases, keys):
        if sum(cost[name] for name in cases) <= share:
            units.append(cases)
        elif keys:
            parts = {}
            for name in cases:
                parts.setdefault(keys[0](name), []).append(name)
            for part in parts.values():
                split(part, keys[1:])
        else:
            chunk = []
            for name in cases:
                if chunk and sum(cost[item] for item in chunk) + cost[name] > share:
                    units.append(chunk)
                    chunk = []
                chunk.append(name)
            units.append(chunk)

    if measured:
        split(measured, [lambda name: (files or {}).get(name, ''),
                         lambda name: re.sub(r'@[0-9]+\Z', '', name)])
    groups = [[] for _ in range(min(jobs, len(units) + len(unmeasured)))]
    totals = [0.0] * len(groups)
    for unit in sorted(units, key=lambda item: (-sum(cost[name] for name in item), item[0])):
        worker = min(range(len(groups)), key=lambda i: (totals[i], len(groups[i])))
        groups[worker].extend(unit)
        totals[worker] += sum(cost[name] for name in unit)
    order = sorted(range(len(groups)), key=lambda i: (totals[i], len(groups[i]), i))
    for index, name in enumerate(unmeasured):
        groups[order[index % len(order)]].append(name)
    return groups


def check_results(names, rows):
    """Reject missing, duplicate, and extra results, including crashed workers."""
    observed = [row['test'] for row in rows]
    if sorted(observed) != sorted(names) or len(observed) != len(set(observed)):
        raise ValueError('Results do not cover the complete inventory exactly once')


def write_durations(path, rows, wall_seconds):
    """Write slowest first; overlapping worker times are labeled explicitly."""
    total = sum(row['seconds'] for row in rows)
    cumulative = 0.0
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=[
            'test', 'seconds', 'cumulative_seconds', 'test_time_percent',
            'wall_time_percent', 'status', 'expected'])
        writer.writeheader()
        for row in sorted(rows, key=lambda item: (-item['seconds'], item['test'])):
            cumulative += row['seconds']
            writer.writerow({**row, 'cumulative_seconds': cumulative,
                             'test_time_percent': 100 * row['seconds'] / total if total else 0,
                             'wall_time_percent': 100 * row['seconds'] / wall_seconds})


def run_logged(command, path, env=None):
    """Retain native output and return exit status and command wall time."""
    start = time.monotonic()
    with path.open('w') as log:
        code = subprocess.run(command, cwd=ROOT, env=env, stdout=log,
                              stderr=subprocess.STDOUT).returncode
    return code, time.monotonic() - start


def select_files(patterns):
    """Expand repository-relative file patterns, rejecting typos and foreign files."""
    files = set()
    for pattern in patterns or ['test/test-*.el']:
        path = Path(pattern)
        matches = [path] if path.is_absolute() else ROOT.glob(pattern)
        selected = []
        for match in matches:
            resolved = match.resolve()
            if resolved.is_file() and resolved.suffix == '.el' and resolved.is_relative_to(ROOT / 'test'):
                selected.append(str(resolved.relative_to(ROOT)))
        if not selected:
            raise ValueError(f'No test files matched: {pattern}')
        files.update(selected)
    return sorted(files)


def read_replay(path):
    """Read a saved worker assignment, retaining file and case order exactly."""
    request = json.loads(path.read_text())
    if not isinstance(request, dict) or not all(key in request for key in ('files', 'inventory', 'tests')):
        raise ValueError('Replay requires files, inventory, and tests from a saved N-partition.json')
    for key in ('files', 'inventory', 'tests'):
        values = request[key]
        if (not isinstance(values, list) or not values
                or not all(isinstance(value, str) for value in values)
                or len(values) != len(set(values))):
            raise ValueError(f'Invalid replay {key}')
    if not set(request['tests']).issubset(request['inventory']):
        raise ValueError('Replay tests are absent from inventory')
    for file in request['files']:
        if select_files([file]) != [file]:
            raise ValueError(f'Invalid replay file: {file}')
    return request


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    available_cpus = len(os.sched_getaffinity(0))
    parser.add_argument('--jobs', type=int, default=min(8, available_cpus),
                        help=f'worker limit (default: {min(8, available_cpus)}; '
                             f'{available_cpus} CPUs available)')
    parser.add_argument('--durations', type=Path, default=REPORTS / 'latest-durations.csv')
    parser.add_argument('--output', type=Path)
    parser.add_argument('files', nargs='*', help='test files or quoted glob patterns (default: all)')
    parser.add_argument('--affected', nargs='?', const='master', metavar='REV',
                        help='fast tier: test files for changes since the merge base with '
                             'REV (default master), including uncommitted and untracked files')
    lane = parser.add_mutually_exclusive_group()
    lane.add_argument('--fresh-files', action='store_true',
                      help='run each selected file in its own fresh Emacs')
    lane.add_argument('--replay', type=Path, metavar='PARTITION.json',
                      help='replay a saved worker assignment in its original order')
    args = parser.parse_args()
    if args.jobs < 1:
        parser.error('--jobs must be positive')
    if args.replay and args.files:
        parser.error('--replay uses its saved file roster; do not pass files')
    if args.affected and (args.files or args.replay):
        parser.error('--affected selects its own files; do not pass files or --replay')
    try:
        replay = read_replay(args.replay) if args.replay else None
        if args.affected:
            changed = affected_tests.changed_files(ROOT, args.affected)
            selected, notes = affected_tests.select(ROOT, changed)
            for note in notes[:20]:
                print(note, flush=True)
            if len(notes) > 20:
                print(f'... {len(notes) - 20} more changed files', flush=True)
            files = select_files(sorted(set(selected) | set(affected_tests.SMOKE))
                                 if selected is not None else [])
            print(f'Affected tier: {len(changed)} changed files; '
                  + ('full suite' if selected is None
                     else f'{len(selected)} affected test files plus the smoke set'), flush=True)
        else:
            files = replay['files'] if replay else select_files(args.files)
    except (OSError, ValueError, KeyError, TypeError, subprocess.CalledProcessError) as exc:
        parser.error(str(exc))
    complete = set(files) == set(select_files([]))
    # Provisioned acceptance cases share target roots; keep their run serial.
    if any(value and key.startswith(('MEVEDEL_TEST_SSH_', 'MEVEDEL_TEST_PODMAN_',
                                     'MEVEDEL_TEST_DOCKER_'))
           for key, value in os.environ.items()):
        args.jobs = 1
        print('Provisioned remote target detected: running serially.', flush=True)
    base = (args.output or REPORTS / datetime.datetime.now().strftime('%Y%m%d-%H%M%S-%f')).resolve()
    base.parent.mkdir(parents=True, exist_ok=True)
    output = base
    suffix = 0
    while True:
        try:
            os.mkdir(output)
            break
        except FileExistsError:
            if args.output is not None:
                raise
            suffix += 1
            output = base.with_name(f'{base.name}-{suffix:06d}')
    # A standalone (pkg-built) Eask running this script exports PKG_EXECPATH;
    # a nested Eask inheriting it starts as plain Node and treats `clean' as a
    # script path.  Every Eask this runner starts must see its own launch.
    os.environ.pop('PKG_EXECPATH', None)
    code, clean_seconds = run_logged(EASK + ['clean', 'elc'], output / 'clean.log')
    if code:
        print(f'Cleanup failed (exit {code}); see {output / "clean.log"}', file=sys.stderr)
        return code if code > 0 else 1
    command = EASK + ['test', 'ert', 'test/ert-runner.el', *files]
    start = time.monotonic()
    env = {key: value for key, value in os.environ.items()
           if key not in ('MEVEDEL_TEST_PARTITION', 'MEVEDEL_TEST_RESULTS',
                          'MEVEDEL_TEST_FRESH_FILE')}
    discovery_seconds = 0
    if args.fresh_files:
        names = []
        groups = [None] * len(files)
    elif replay:
        names = replay['tests']
        groups = [names]
    else:
        code, discovery_seconds = run_logged(
            command, output / 'inventory.log',
            {**env, 'MEVEDEL_TEST_RESULTS': str(output / 'inventory.json')})
        if code:
            print(f'Discovery failed (exit {code}); see {output / "inventory.log"}', file=sys.stderr)
            return code if code > 0 else 1
        discovered = json.loads((output / 'inventory.json').read_text())
        names = discovered['inventory']
        defining = dict(zip(names, discovered['files']))
    if not args.fresh_files and not replay:
        weights = {}
        if args.durations.exists():
            with args.durations.open() as stream:
                weights = {row['test']: float(row['seconds']) for row in csv.DictReader(stream)}
        groups = partition_tests(names, weights, args.jobs, defining)
    jobs = min(args.jobs, len(groups))
    mode = 'fresh-files' if args.fresh_files else 'replay' if replay else 'partitioned'
    print(f'{len(files)} files; {available_cpus} CPUs available; '
          f'{jobs} concurrent Eask workers; {mode}; reports: {output}', flush=True)

    def run(index):
        request = output / f'{index}-partition.json'
        results = output / f'{index}-results.json'
        worker_files = [files[index]] if args.fresh_files else files
        worker_env = {**env, 'MEVEDEL_TEST_RESULTS': str(results)}
        if args.fresh_files:
            worker_env['MEVEDEL_TEST_FRESH_FILE'] = '1'
        else:
            request.write_text(json.dumps(dict(files=worker_files,
                                              inventory=replay['inventory'] if replay else names,
                                              tests=groups[index])))
            worker_env['MEVEDEL_TEST_PARTITION'] = str(request)
        worker_command = EASK + ['test', 'ert', 'test/ert-runner.el', *worker_files]
        code, seconds = run_logged(worker_command, output / f'{index}.log', worker_env)
        error = None
        try:
            data = json.loads(results.read_text())
            if args.fresh_files:
                groups[index] = data['inventory']
                if not groups[index] or len(set(groups[index])) != len(groups[index]):
                    raise ValueError('Invalid or empty file inventory')
                request.write_text(json.dumps(dict(files=worker_files,
                                                  inventory=groups[index], tests=groups[index])))
            # Validate each assignment, not just the merged set.
            check_results(groups[index], data['tests'])
            if [row['test'] for row in data['tests']] != groups[index]:
                raise ValueError('Worker changed the assigned test sequence')
            ert_seconds = data['ert_seconds']
        except (OSError, ValueError, KeyError, TypeError) as exc:
            error = str(exc)
            data = {'tests': []}
            ert_seconds = None
            if args.fresh_files:
                groups[index] = None
        return dict(worker=index, exit_code=code, wall_seconds=seconds,
                    ert_seconds=ert_seconds, tests=data['tests'], error=error,
                    files=worker_files, command=worker_command)

    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
        workers = list(pool.map(run, range(len(groups))))
    rows = [row for worker in workers for row in worker.pop('tests')]
    incomplete = any(worker['error'] for worker in workers)
    coverage_error = None
    if args.fresh_files:
        names = [name for group in groups if group for name in group]
    expected_count = len(names) if not args.fresh_files or all(groups) else None
    if not incomplete:
        try:
            check_results(names, rows)
        except ValueError as exc:
            incomplete = True
            coverage_error = str(exc)
            print(coverage_error, flush=True)
    seconds = time.monotonic() - start
    unexpected = sum(not row['expected'] for row in rows)
    failed = incomplete or unexpected or any(worker['exit_code'] for worker in workers)
    summary = dict(files=files, mode=mode, wall_seconds=seconds,
                   clean_seconds=clean_seconds, discovery_seconds=discovery_seconds,
                   workers=workers, count=len(rows), expected_count=expected_count,
                   unexpected=unexpected, failed=bool(failed), error=coverage_error,
                   skipped=sum(row['status'] == 'ert-test-skipped' for row in rows))
    (output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    write_durations(output / 'durations.csv', rows, seconds)
    if not failed and complete and not args.fresh_files and not replay:
        REPORTS.mkdir(parents=True, exist_ok=True)
        write_durations(REPORTS / 'latest-durations.csv', rows, seconds)
    print(f'{seconds:.2f}s; {len(rows)}/{expected_count if expected_count is not None else "?"} results; '
          f'{unexpected} unexpected; {summary["skipped"]} skipped', flush=True)
    failures = [row['test'] for row in rows if not row['expected']]
    for name in failures[:20]:
        print(f'FAIL: {name}', flush=True)
    if len(failures) > 20:
        print(f'... {len(failures) - 20} more; all names in durations.csv', flush=True)
    broken = [worker for worker in workers if worker['exit_code'] or worker['error']]
    for worker in broken[:10]:
        label = worker['files'][0] if args.fresh_files else f'Worker {worker["worker"]}'
        print(f'{label}: exit {worker["exit_code"]}; '
              f'{worker["error"] or "unexpected results"}; '
              f'log: {output / str(worker["worker"])}.log', flush=True)
    if len(broken) > 10:
        print(f'... {len(broken) - 10} more failed workers; see summary.json', flush=True)
    return next((worker['exit_code'] if worker['exit_code'] > 0 else 1
                 for worker in workers if worker['exit_code']), int(bool(failed)))


if __name__ == '__main__':
    raise SystemExit(main())
