#!/usr/bin/env python3
"""Run the complete ERT suite in isolated Eask workers and report durations."""

import argparse
import concurrent.futures
import csv
import datetime
import json
import math
import os
from pathlib import Path
import shutil
import subprocess
import time

ROOT = Path(__file__).resolve().parent.parent
REPORTS = ROOT / '.scratch/test-suite-performance'
EASK = ['eask'] if shutil.which('eask') else ['npx', '--offline', '@emacs-eask/cli']


def partition_tests(names, weights, jobs):
    """Balance known durations, assigning every discovered case exactly once."""
    if jobs < 1 or not names or len(names) != len(set(names)):
        raise ValueError('Expected unique tests and at least one worker')
    if any(not math.isfinite(value) or value < 0 for value in weights.values()):
        raise ValueError('Durations must be finite and nonnegative')
    groups = [[] for _ in range(min(jobs, len(names)))]
    totals = [0.0] * len(groups)
    for name in sorted(names, key=lambda item: (-weights.get(item, 0.01), item)):
        worker = min(range(len(groups)), key=lambda i: (totals[i], len(groups[i])))
        groups[worker].append(name)
        totals[worker] += weights.get(name, 0.01)
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    available_cpus = len(os.sched_getaffinity(0))
    parser.add_argument('--jobs', type=int, default=min(8, available_cpus),
                        help=f'worker limit (default: {min(8, available_cpus)}; '
                             f'{available_cpus} CPUs available)')
    parser.add_argument('--output', type=Path, default=REPORTS / datetime.datetime.now().strftime('%Y%m%d-%H%M%S'))
    parser.add_argument('--durations', type=Path, default=REPORTS / 'latest-durations.csv')
    args = parser.parse_args()
    if args.jobs < 1:
        parser.error('--jobs must be positive')
    # Provisioned acceptance cases share target roots; keep their run serial.
    if any(value and key.startswith(('MEVEDEL_TEST_SSH_', 'MEVEDEL_TEST_PODMAN_',
                                     'MEVEDEL_TEST_DOCKER_'))
           for key, value in os.environ.items()):
        args.jobs = 1
        print('Provisioned remote target detected: running serially.', flush=True)
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    code, clean_seconds = run_logged(EASK + ['clean', 'elc'], output / 'clean.log')
    if code:
        raise SystemExit(f'Cleanup failed; see {output / "clean.log"}')
    files = sorted(str(path.relative_to(ROOT)) for path in (ROOT / 'test').glob('test-*.el'))
    command = EASK + ['test', 'ert', 'test/ert-runner.el', *files]
    start = time.monotonic()
    env = {key: value for key, value in os.environ.items()
           if key not in ('MEVEDEL_TEST_PARTITION', 'MEVEDEL_TEST_RESULTS')}
    code, discovery_seconds = run_logged(
        command, output / 'inventory.log',
        {**env, 'MEVEDEL_TEST_RESULTS': str(output / 'inventory.json')})
    if code:
        raise SystemExit(f'Discovery failed; see {output / "inventory.log"}')
    names = json.loads((output / 'inventory.json').read_text())
    weights = {}
    if args.durations.exists():
        with args.durations.open() as stream:
            weights = {row['test']: float(row['seconds']) for row in csv.DictReader(stream)}
    groups = partition_tests(names, weights, args.jobs)
    print(f'{len(names)} tests; {available_cpus} CPUs available; '
          f'{len(groups)} isolated Eask workers; reports: {output}', flush=True)

    def run(index):
        request = output / f'{index}-partition.json'
        results = output / f'{index}-results.json'
        request.write_text(json.dumps(dict(inventory=names, tests=groups[index])))
        code, seconds = run_logged(command, output / f'{index}.log',
                                   {**env, 'MEVEDEL_TEST_PARTITION': str(request),
                                    'MEVEDEL_TEST_RESULTS': str(results)})
        data = json.loads(results.read_text()) if results.exists() else {}
        # Validate each assignment, not just the merged set.
        check_results(groups[index], data.get('tests', []))
        print(f'Worker {index}: exit {code}, {seconds:.2f}s', flush=True)
        return dict(worker=index, exit_code=code, wall_seconds=seconds,
                    ert_seconds=data['ert_seconds'], tests=data['tests'])

    with concurrent.futures.ThreadPoolExecutor(max_workers=len(groups)) as pool:
        workers = list(pool.map(run, range(len(groups))))
    rows = [row for worker in workers for row in worker.pop('tests')]
    check_results(names, rows)
    seconds = time.monotonic() - start
    unexpected = sum(not row['expected'] for row in rows)
    failed = unexpected or any(worker['exit_code'] for worker in workers)
    summary = dict(command=command, files=files, wall_seconds=seconds,
                   clean_seconds=clean_seconds, discovery_seconds=discovery_seconds,
                   workers=workers, count=len(rows), unexpected=unexpected,
                   skipped=sum(row['status'] == 'ert-test-skipped' for row in rows))
    (output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    write_durations(output / 'durations.csv', rows, seconds)
    if not failed:
        REPORTS.mkdir(parents=True, exist_ok=True)
        write_durations(REPORTS / 'latest-durations.csv', rows, seconds)
    print(f'{seconds:.2f}s; {unexpected} unexpected; {summary["skipped"]} skipped', flush=True)
    return int(bool(failed))


if __name__ == '__main__':
    raise SystemExit(main())
