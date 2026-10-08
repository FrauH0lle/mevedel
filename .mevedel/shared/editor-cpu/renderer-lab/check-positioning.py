#!/usr/bin/env python3
"""Reject native child commits that can present before their parent position."""
import argparse
import json
import re
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('trace', type=Path)
parser.add_argument('--minimum-surfaces', type=int, default=1)
args = parser.parse_args()
surfaces = {}
created = 0
independent = 0
violations = []
for number, line in enumerate(args.trace.read_text().splitlines(), 1):
    match = re.search(r'get_subsurface\(new id wl_subsurface#(\d+), wl_surface#(\d+), wl_surface#(\d+)\)', line)
    if match:
        sub, child, parent = map(int, match.groups())
        surfaces[sub] = dict(child=child, parent=parent, sync=True, pending=True)
        created += 1
        continue
    match = re.search(r' -> wl_subsurface#(\d+)\.(\w+)\(', line)
    if match and int(match[1]) in surfaces:
        sub, operation = int(match[1]), match[2]
        state = surfaces[sub]
        if operation == 'set_position':
            state['pending'] = True
        elif operation == 'set_sync':
            state['sync'] = True
        elif operation == 'set_desync':
            state['sync'] = False
            independent += 1
        elif operation == 'destroy':
            del surfaces[sub]
        continue
    match = re.search(r' -> wl_surface#(\d+)\.commit\(', line)
    if match:
        surface = int(match[1])
        for state in surfaces.values():
            if state['parent'] == surface:
                state['pending'] = False
            if state['child'] == surface and state['pending'] and not state['sync']:
                violations.append(dict(line=number, child=surface, parent=state['parent']))
result = dict(created=created, independent_transitions=independent,
              premature_child_commits=len(violations), examples=violations[:3])
print(json.dumps(result))
raise SystemExit(0 if created >= args.minimum_surfaces and independent > 0 and not violations else 1)
