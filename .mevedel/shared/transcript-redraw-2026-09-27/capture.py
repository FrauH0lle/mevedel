"""Copy selected immutable published transcripts without resuming sessions."""
import hashlib
import json
import re
import shutil
import subprocess
from pathlib import Path

ROOT = Path.cwd()
OUT = ROOT / '.scratch/transcript-redraw-2026-09-27'
OUT.mkdir(exist_ok=False)
(OUT / 'fixtures').mkdir()
(OUT / 'results').mkdir()
records = []
for sid in ['2026-09-25T20-48-9f6729a8d04c',
            '2026-09-25T20-50-e6b129234a36']:
    session = ROOT / '.mevedel/sessions' / sid
    lease = sorted((session / '.lease').glob('*.el'))[-1]
    before = lease.read_bytes()
    head = re.search(rb':publication-head "([^"]+)"', before).group(1).decode()
    manifest = (session / head).read_bytes()
    entries = re.findall(
        r'\("([^"]+)" :published "([^"]+)" :sha256 "([a-f0-9]+)"\)',
        manifest.decode())
    candidates = []
    for logical, physical, digest in entries:
        if not logical.endswith('.org'):
            continue
        path = session / physical
        assert path.resolve().is_relative_to(session.resolve())
        raw = path.read_bytes()
        assert hashlib.sha256(raw).hexdigest() == digest, logical
        candidates.append((len(raw), logical, physical, digest, raw))
    groups = [('control' if '20-48' in sid else 'root',
               [e for e in candidates if e[1].startswith('segment-')])]
    if '20-50' in sid:
        groups.append(('agent', [e for e in candidates if e[1].startswith('agents/')]))
    for label, group in groups:
        size, logical, physical, digest, raw = max(group)
        fixture = OUT / 'fixtures' / (label + '.org')
        fixture.write_bytes(raw)
        records.append(dict(label=label, session=sid, logical=logical,
                            physical=physical, sha256=digest, bytes=size,
                            chars=len(raw.decode()), fixture=str(fixture),
                            manifest=head,
                            manifest_sha256=hashlib.sha256(manifest).hexdigest()))
    assert before == lease.read_bytes(), 'Publication moved during capture'
    assert lease == sorted((session / '.lease').glob('*.el'))[-1]
grammars = Path('/home/roland/.emacs.d/.local/etc/tree-sitter')
copied = []
if grammars.is_dir():
    target = OUT / 'grammars'
    target.mkdir()
    for path in grammars.glob('*.so'):
        shutil.copy2(path, target / path.name)
        copied.append(dict(name=path.name, sha256=hashlib.sha256(path.read_bytes()).hexdigest()))
source_hashes = {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                 for p in ROOT.glob('mevedel*.el')}
(OUT / 'manifest.json').write_text(json.dumps(dict(
    commit=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
    inputs=records, grammars=copied, source_hashes=source_hashes), indent=2))
for record in records:
    print(record['label'], record['logical'], record['bytes'], record['sha256'])
