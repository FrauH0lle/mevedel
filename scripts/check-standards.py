#!/usr/bin/env python3
"""Check cheap repository invariants and ratchet checkdoc against a merge base."""

import argparse
from collections import Counter
import json
import os
from pathlib import Path
import re
import string
import subprocess
import sys
import tempfile
from urllib.parse import unquote, urlsplit


ROOT = Path(__file__).resolve().parent.parent
COLLECTOR = Path(__file__).with_name('checkdoc-collect.el')


def git(root, *args):
    """Return Git output, preserving command failures."""
    return subprocess.check_output(['git', '-C', str(root), *args], stderr=subprocess.PIPE)


def inventory(root):
    """Include tracked and non-ignored new files, but not deleted files."""
    names = git(root, 'ls-files', '-z', '--cached', '--others', '--exclude-standard')
    return sorted({name for name in names.decode().split('\0')
                   if name and (root / name).is_file()})


def lisp_boundaries(name, text):
    """Check library and primary test headers/footers, not script fixtures/data."""
    path = Path(name)
    if (path.suffix != '.el' or path.name.startswith('.') or
            path.parent.as_posix() not in ('.', 'test', 'benchmark', 'scripts')):
        return []
    lines = text.splitlines()
    errors = []
    header = re.search(r'-\*-(.*?)-\*-', lines[0]) if lines else None
    if not header or not re.search(r'(?:^|;)\s*lexical-binding:\s*t\s*(?:;|$)', header[1]):
        errors.append(f'{name}:1: missing lexical-binding: t in first-line header')
    last = text.rstrip().splitlines()
    if not last or not re.fullmatch(r';;; ' + re.escape(path.name) +
                                   r' ends here\.?', last[-1]):
        errors.append(f'{name}: missing matching final ends-here footer')
    return errors


def prose(text):
    """Blank fenced code and inline code, keeping offsets for diagnostics."""
    fence = None
    result = []
    for line in text.splitlines(keepends=True):
        marker = re.match(r'^\s{0,3}(`{3,}|~{3,})', line)
        if fence:
            if marker and marker[1][0] == fence[0] and len(marker[1]) >= len(fence):
                fence = None
            result.append(re.sub(r'[^\n]', ' ', line))
        elif marker:
            fence = marker[1]
            result.append(re.sub(r'[^\n]', ' ', line))
        else:
            result.append(line)
    return ''.join(result)


def anchors(text):
    """Return GitHub-style heading slugs and explicit HTML anchor IDs."""
    text = prose(text)
    found = set(re.findall(r'<a\s+(?:id|name)=["\']([^"\']+)["\']', text))
    counts = Counter()
    for match in re.finditer(r'^ {0,3}#{1,6}\s+(.+?)(?:\s+#+)?\s*$', text, re.M):
        title = re.sub(r'!?\[([^\]]+)\]\([^)]*\)', r'\1', match[1])
        slug = re.sub(r'[^\w\- ]', '', title.lower()).replace(' ', '-')
        number = counts[slug]
        counts[slug] += 1
        found.add(slug + (f'-{number}' if number else ''))
    return found


def markdown_links(text):
    """Extract inline/image and reference links, ignoring code examples."""
    text = prose(text)
    text = re.sub(r'(`+).*?\1', lambda m: ' ' * len(m[0]), text)
    definitions = {}
    links = []
    for match in re.finditer(r'^ {0,3}\[([^\]]+)\]:\s*(<[^>]+>|\S+)', text, re.M):
        definitions[' '.join(match[1].lower().split())] = match[2].strip('<>')
        links.append((match.start(), match[2].strip('<>')))
    # One balanced parenthesis level handles ordinary repository filenames.
    destination = r'(<[^>]+>|(?:\\.|[^\s()]|\([^()\n]*\))+?)'
    for match in re.finditer(r'!?\[[^\]]*\]\(\s*' + destination +
                            r'(?:\s+["\'][^\n]*?["\'])?\s*\)', text):
        links.append((match.start(), match[1].strip('<>')))
    for match in re.finditer(r'!?\[([^\]]+)\]\[([^\]]*)\]', text):
        label = ' '.join((match[2] or match[1]).lower().split())
        links.append((match.start(), definitions.get(label)))
    return [(text.count('\n', 0, position) + 1, target)
            for position, target in links]


def link_errors(root, name, text):
    """Validate local paths and Markdown anchors without network requests."""
    errors = []
    for line, target in markdown_links(text):
        if target is None:
            errors.append(f'{name}:{line}: undefined Markdown link reference')
            continue
        target = re.sub(r'\\([' + re.escape(string.punctuation) + r'])', r'\1', target)
        url = urlsplit(target)
        if url.scheme or url.netloc:
            continue
        path = ((root if url.path.startswith('/') else (root / name).parent) /
                unquote(url.path).lstrip('/')).resolve() if url.path else root / name
        if not path.exists():
            errors.append(f'{name}:{line}: missing link target {target}')
        elif url.fragment and path.suffix == '.md' and path.is_file():
            if unquote(url.fragment) not in anchors(path.read_text()):
                errors.append(f'{name}:{line}: missing Markdown anchor {target}')
    return errors


def adr_entries(text):
    """Read original-ID table rows and additional current-record entries."""
    entries = []
    for match in re.finditer(r'^\s*\|\s*(\d{4})\s*\|[^\n]*?\]\(([^)]+)\)', text, re.M):
        entries.append((match[1], match[2]))
    for match in re.finditer(r'^\s*-\s+\[ADR (\d{4}):[^\n]*?\]\(([^)]+)\)', text, re.M):
        entries.append((match[1], match[2]))
    return entries


def adr_errors(root, names, previous=''):
    """Check unique, preserved IDs and indexed current/historical destinations."""
    index = root / 'docs/adr/README.md'
    entries = adr_entries(index.read_text())
    errors = []
    ids = [number for number, _ in entries]
    for number, count in Counter(ids).items():
        if count != 1:
            errors.append(f'docs/adr/README.md: duplicate ADR ID {number}')
    for number, _ in adr_entries(previous):
        if number not in ids:
            errors.append(f'docs/adr/README.md: removed original ADR ID {number}')
    destinations = {}
    for number, target in entries:
        path, _, fragment = target.partition('#')
        destinations[number] = path
        if not re.fullmatch(r'\d{4}-[^/]+\.md', path):
            errors.append(f'docs/adr/README.md: invalid ADR destination {target}')
        elif not path.startswith(number + '-') and fragment != 'decision-history':
            errors.append(f'docs/adr/README.md: ADR {number} must link to decision-history')
        errors.extend(link_errors(root, 'docs/adr/README.md', f'[ADR]({target})'))
    for name in names:
        match = re.fullmatch(r'docs/adr/(\d{4})-[^/]+\.md', name)
        if match and destinations.get(match[1]) != Path(name).name:
            errors.append(f'{name}: current ADR is missing its own index entry')
    return errors


def checkdoc(root, sources):
    """Run installed Emacs checkdoc on snapshots, never loading project code."""
    if not sources:
        return []
    with tempfile.TemporaryDirectory(prefix='mevedel-checkdoc-') as tmp:
        directory = Path(tmp)
        files = []
        for number, (name, text) in enumerate(sources):
            path = directory / str(number) / Path(name).name
            path.parent.mkdir()
            path.write_text(text)
            files.append(str(path))
        request, response = directory / 'input.json', directory / 'output.json'
        request.write_text(json.dumps(files))
        env = {**os.environ, 'HOME': tmp, 'XDG_CONFIG_HOME': tmp,
               'XDG_CACHE_HOME': tmp, 'XDG_DATA_HOME': tmp}
        subprocess.run([os.environ.get('EMACS', 'emacs'), '--batch', '-Q',
                        '--load', str(COLLECTOR), '--', str(request), str(response)],
                       cwd=root, env=env, check=True, capture_output=True, text=True)
        result = json.loads(response.read_text())
        if len(result) != len(sources):
            raise ValueError('Incomplete checkdoc result')
        return result


def new_diagnostics(old, current):
    """Compare per-definition message multisets; line shifts do not add debt."""
    remaining = Counter((item['owner'], item['message']) for item in old)
    new = []
    for item in current:
        key = item['owner'], item['message']
        if remaining[key]:
            remaining[key] -= 1
        else:
            new.append(item)
    return new


def changed_lisp(root, revision):
    """Map changed Lisp paths to their base paths, preserving Git-detected renames."""
    fields = iter(git(root, 'diff', '--name-status', '-z', '--find-renames',
                      revision, '--', '*.el').decode().rstrip('\0').split('\0'))
    changed = {}
    for status in fields:
        if not status:
            continue
        name = next(fields)
        if status.startswith('R'):
            changed[next(fields)] = name
        elif status != 'D':
            changed[name] = name
    return changed


def run(root, base):
    """Check the working tree against the resolved merge base, including new files."""
    revision = git(root, 'merge-base', base, 'HEAD').decode().strip()
    names = inventory(root)
    base_names = set(git(root, 'ls-tree', '-r', '--name-only', revision).decode().splitlines())
    origins = changed_lisp(root, revision)
    errors = []
    changed, before, after = [], [], []
    for name in names:
        if not name.endswith(('.el', '.md')):
            continue
        text = (root / name).read_text()
        if name.endswith('.el'):
            errors.extend(lisp_boundaries(name, text))
            if name in origins or name not in base_names:
                origin = origins.get(name, name)
                old = (git(root, 'show', f'{revision}:{origin}').decode()
                       if origin in base_names else '')
                changed.append(name)
                before.append((name, old))
                after.append((name, text))
        elif name.startswith('docs/'):
            errors.extend(link_errors(root, name, text))
    index = 'docs/adr/README.md'
    previous = git(root, 'show', f'{revision}:{index}').decode() if index in base_names else ''
    errors.extend(adr_errors(root, names, previous))
    # An absent base file must not acquire diagnostics from an empty buffer.
    old_results, results = checkdoc(root, before), checkdoc(root, after)
    advisory = 0
    for name, source, old, current in zip(changed, before, old_results, results):
        new = new_diagnostics(old if source[1] else [], current)
        advisory += len(current) - len(new)
        errors.extend(f'{name}:{item["line"]}: checkdoc: {item["message"]}' for item in new)
    for error in errors:
        print(error, file=sys.stderr)
    print(f'Standards: {len(errors)} errors; checkdoc: {len(changed)} changed Lisp files, '
          f'{advisory} existing diagnostics advisory; merge base {revision[:12]}')
    return 1 if errors else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--base', default=os.environ.get('CHECK_BASE', 'master'),
                        help='compare with merge-base(BASE, HEAD); default CHECK_BASE or master')
    args = parser.parse_args()
    try:
        return run(ROOT, args.base)
    except (subprocess.CalledProcessError, OSError, ValueError) as error:
        print(f'Standards check failed: {error}', file=sys.stderr)
        if isinstance(error, subprocess.CalledProcessError) and error.stderr:
            detail = error.stderr
            print(detail.decode() if isinstance(detail, bytes) else detail, file=sys.stderr)
        return 2


if __name__ == '__main__':
    sys.exit(main())
