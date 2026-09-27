"""Validate the complete primary matrix and emit shareable numeric evidence."""
import hashlib
import json
import statistics
from collections import defaultdict
from pathlib import Path

base = Path('.scratch/transcript-redraw-2026-09-27')
manifest = json.loads((base / 'manifest.json').read_text())
rows = []
groups = []
for label in ['control', 'root', 'agent']:
    histories, mappings, harnesses = set(), set(), set()
    for mode in ['cold', 'warm']:
        runs = []
        for trial in ['1', '2', '3']:
            path = base / 'results' / f'{label}-{mode}-{trial}' / 'result.json'
            r = json.loads(path.read_text())
            assert r['history_equal'] and r['source_unchanged'] and r['source_map_equal']
            assert r['point_at_end'] and not r['diagnostics'] and r['child_exit_status'] == 0
            assert r['fixture_sha256'] == r['fixture']['sha256']
            assert r['draft'] == '> draft\nsecond line\n' + 'x' * len(r['sent'])
            assert len(r['sent']) == len(r['keys'])
            assert r['input_overlapping_work']['n'] > 0
            histories.add(r['history_sha256'])
            mappings.add(r['source_map_sha256'])
            # This summary helper can be added without changing the executed harness.
            harnesses.add(tuple((k, v) for k, v in sorted(r['harness_hashes'].items())
                                if k in ['driver.el', 'runner.py', 'launch-test.el']))
            row = {k: r[k] for k in ['label', 'mode', 'completion_ms', 'initial-return-ms',
                                     'restore-ms', 'gcs', 'gc-seconds', 'input_all',
                                     'input_during_work', 'input_overlapping_work',
                                     'sender_lateness', 'segments', 'turns', 'largest_turn_chars',
                                     'history_chars', 'history_sha256', 'source_map_sha256',
                                     'fixture_sha256', 'markdown', 'libraries']}
            row.update(trial=trial, raw_result=str(path),
                       raw_sha256=hashlib.sha256(path.read_bytes()).hexdigest())
            rows.append(row)
            runs.append(r)
        group = dict(label=label, mode=mode,
                     completion_median_ms=statistics.median(r['completion_ms'] for r in runs),
                     max_input_median_ms=statistics.median(r['input_all']['maximum'] for r in runs),
                     max_input_range_ms=[min(r['input_all']['maximum'] for r in runs),
                                         max(r['input_all']['maximum'] for r in runs)],
                     gc_median_ms=1000*statistics.median(r['gc-seconds'] for r in runs))
        groups.append(group)
        print(label, mode, 'completion', round(group['completion_median_ms'], 1),
              'median_max_input', round(group['max_input_median_ms'], 1),
              'max_range', [round(v, 1) for v in group['max_input_range_ms']],
              'gc_ms', round(group['gc_median_ms'], 1))
    assert len(histories) == len(mappings) == len(harnesses) == 1, (label, histories, mappings, harnesses)
followup = []
for label in ['root', 'agent']:
    reference = next(r for r in rows if r['label'] == label)
    for trial in ['1', '2', '3']:
        path = base / 'results' / f'{label}-cold-scheduled-{trial}' / 'result.json'
        r = json.loads(path.read_text())
        assert r['history_equal'] and r['source_unchanged'] and r['source_map_equal']
        assert r['point_at_end'] and not r['diagnostics'] and r['child_exit_status'] == 0
        assert r['history_sha256'] == reference['history_sha256']
        assert r['source_map_sha256'] == reference['source_map_sha256']
        assert r['fixture_sha256'] == reference['fixture_sha256']
        followup.append({k: r[k] for k in ['label', 'mode', 'completion_ms', 'input_all',
                                          'input_overlapping_work', 'gc-seconds', 'gcs',
                                          'history_sha256', 'source_map_sha256']}
                        | dict(trial=trial, raw_result=str(path),
                               raw_sha256=hashlib.sha256(path.read_bytes()).hexdigest()))
    runs = [r for r in followup if r['label'] == label]
    print(label, 'cold-scheduled', 'completion',
          round(statistics.median(r['completion_ms'] for r in runs), 1),
          'median_max_input', round(statistics.median(r['input_all']['maximum'] for r in runs), 1),
          'max_range', [round(min(r['input_all']['maximum'] for r in runs), 1),
                        round(max(r['input_all']['maximum'] for r in runs), 1)])
profiles = []
for label in ['control', 'root', 'agent']:
    reference = next(r for r in rows if r['label'] == label)
    for mode in ['cold', 'warm']:
        path = base / 'results' / f'{label}-{mode}-profile1' / 'result.json'
        r = json.loads(path.read_text())
        assert r['history_sha256'] == reference['history_sha256']
        assert r['source_map_sha256'] == reference['source_map_sha256']
        assert r['history_equal'] and r['source_unchanged'] and r['source_map_equal']
        assert r['child_exit_status'] == 0 and not r['diagnostics']
        phases = defaultdict(list)
        for p in r['phases']:
            phases[p['name']].append(p)
        profiles.append(dict(label=label, mode=mode, raw_result=str(path),
                             raw_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                             completion_ms=r['completion_ms'], gc_ms=1000*r['gc-seconds'],
                             phases={k: dict(calls=len(vs), total_ms=sum(v['ms'] for v in vs),
                                             max_ms=max(v['ms'] for v in vs),
                                             inclusive_gc_ms=sum(v['gc-ms'] for v in vs))
                                     for k, vs in phases.items()}))
out = Path(__file__).parent / 'results.json'
out.write_text(json.dumps(dict(commit=manifest['commit'], inputs=manifest['inputs'],
                               groups=groups, runs=rows, cold_scheduled=followup, profiles=profiles,
                               cross_mode_history_and_source_maps_equal=True), indent=2))
print('Validated 18 primary + 6 cold-scheduled + 6 instrumented runs; history and source maps equal.')
