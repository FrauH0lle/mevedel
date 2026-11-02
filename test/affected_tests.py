"""Map changed files to the ERT files that load or test them: the fast tier.

A changed root or test library selects the test files that require one of its
features directly, the test files it owns by name (`test-STEM.el' and
`test-STEM-SUBJECT.el', where STEM is the longest matching library), and
itself when it is a test file.  Transitive requires are not followed: through
the package entry point they reach most of the suite for a typical module.
Another file selects the test files that name its path, its quoted file name
or one of its directories.

The full suite (None) replaces the selection when a change touches the runner
or shared test support, a library directly reaches more than a quarter of the
test files, or a non-documentation change maps to no test file."""

from pathlib import Path
import re
import subprocess

# Every ERT run depends on these; the require graph does not show it.
FULL_SUITE = frozenset({'Eask', 'scripts/check.sh', 'test/affected_tests.py',
                        'test/ert-runner.el', 'test/helpers.el', 'test/run_tests.py'})
# Fast files (under a second together) covering the data model, the package
# entry point and agent execution; always added to an affected selection.
SMOKE = ('test/test-mevedel-agent-exec.el', 'test/test-mevedel-persistence.el',
         'test/test-mevedel-structs.el')
DOCUMENTATION = ('.md', '.org', '.txt', '.png', '.jpg', '.svg', '.gif')
BROAD_SHARE = 0.25
REQUIRE = re.compile(r"\(require\s+'([^\s()]+)")
PROVIDE = re.compile(r"^\(provide\s+'([^\s()]+)", re.M)


def git(root, *arguments):
    """Return Git's output lines in ROOT, raising on failure."""
    return subprocess.run(['git', *arguments], cwd=root, check=True,
                          capture_output=True, text=True).stdout.splitlines()


def changed_files(root, rev):
    """Return files changed since the merge base of REV and HEAD, worktree included."""
    base = git(root, 'merge-base', rev, 'HEAD')[0]
    return sorted(set(git(root, 'diff', '--name-only', '--no-renames', base))
                  | set(git(root, 'ls-files', '--others', '--exclude-standard')))


def documentation_p(path):
    """Return non-nil when PATH is prose or an image rather than code."""
    return path.endswith(DOCUMENTATION) or path.startswith(('docs/', '.mevedel/'))


def select(root, changed):
    """Return (TEST-FILES or None for the full suite, NOTES) for CHANGED paths."""
    root = Path(root)
    texts = {str(path.relative_to(root)): path.read_text(errors='replace')
             for path in sorted([*root.glob('*.el'), *root.glob('test/*.el')])}
    tests = [path for path in texts if path.startswith('test/test-')]
    requires = {test: set(REQUIRE.findall(texts[test])) for test in tests}
    stems = {Path(path).stem for path in texts if '/' not in path}

    def owner(test):
        name = Path(test).stem.removeprefix('test-')
        return max((stem for stem in stems if name == stem or name.startswith(stem + '-')),
                   key=len, default=None)

    owners = {test: owner(test) for test in tests}
    selected, notes, full = set(), [], False
    for path in changed:
        if path in FULL_SUITE:
            matches, reason = None, 'shared test support'
        elif path.endswith('.el') and (
                '/' not in path or (path.startswith('test/') and path.count('/') == 1)):
            if path.startswith('test/test-') and path not in texts:
                notes.append(f'{path}: deleted test file; nothing to run')
                continue
            features = set(PROVIDE.findall(texts.get(path, ''))) or {Path(path).stem}
            matches = ({test for test in tests if requires[test] & features}
                       | {test for test in tests
                          if '/' not in path and owners[test] == Path(path).stem}
                       | ({path} if path in tests else set()))
            reason = 'no test file requires or owns it'
            if len(matches) > BROAD_SHARE * len(tests):
                matches, reason = None, f'broad library: {len(matches)} test files require it'
        else:
            names = [path, f'"{Path(path).name}"'] + [
                str(parent) + '/' for parent in Path(path).parents
                if str(parent) not in ('.', 'test')]
            pattern = re.compile('|'.join(r'(?<![\w.-])' + re.escape(name) for name in names))
            matches = {test for test in tests if pattern.search(texts[test])}
            reason = 'no test file names it or its directory'
            if not matches and documentation_p(path):
                notes.append(f'{path}: documentation; smoke set only')
                continue
        if not matches:
            notes.append(f'{path}: full suite ({reason})')
            full = True
        else:
            notes.append(f'{path}: {len(matches)} test files')
            selected |= matches
    return (None if full else sorted(selected)), notes
