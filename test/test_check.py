"""Check the fail-fast build entry point and opt-in Git hook."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parent.parent


class CheckTests(unittest.TestCase):
    def test_standards_base_after_rewriting_branch(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            remote, source, checkout = (directory / name for name in
                                        ('remote.git', 'source', 'checkout'))

            def git(*args, cwd=directory):
                return subprocess.run(['git', *args], cwd=cwd, check=True,
                                      capture_output=True, text=True).stdout.strip()

            git('init', '--bare', '-q', str(remote))
            git('init', '-q', '-b', 'master', str(source))
            git('config', 'user.email', 'test@example.invalid', cwd=source)
            git('config', 'user.name', 'Test', cwd=source)
            (source / 'file').write_text('base')
            git('add', '.', cwd=source)
            git('commit', '-qm', 'Base', cwd=source)
            base = git('rev-parse', 'HEAD', cwd=source)
            git('switch', '-qc', 'feature', cwd=source)
            (source / 'file').write_text('before')
            git('commit', '-qam', 'Before', cwd=source)
            before = git('rev-parse', 'HEAD', cwd=source)
            git('push', str(remote), 'master', 'feature', cwd=source)
            (source / 'file').write_text('after')
            git('commit', '--amend', '-qam', 'After', cwd=source)
            git('push', '--force', str(remote), 'feature', cwd=source)
            git('clone', '--no-local', '-b', 'feature', str(remote), str(checkout))
            self.assertNotEqual(subprocess.run(
                ['git', 'cat-file', '-e', before], cwd=checkout,
                capture_output=True).returncode, 0)
            output = directory / 'github-env'
            for event_base, expected in ((before, before), ('0' * 40, 'origin/master'),
                                         ('', 'origin/master')):
                with self.subTest(event_base=event_base):
                    output.write_text('')
                    subprocess.run(
                        ['sh', str(ROOT / 'scripts/ci-check-base.sh')], cwd=checkout,
                        env={**os.environ, 'EVENT_BASE': event_base,
                             'DEFAULT_BRANCH': 'master', 'GITHUB_ENV': str(output)},
                        capture_output=True, text=True, check=True)
                    self.assertEqual(output.read_text(), f'CHECK_BASE={expected}\n')
                    self.assertEqual(git('merge-base', expected, 'HEAD', cwd=checkout),
                                     base)

    def test_check_sequence_and_failures(self):
        for fail_at, expected in (
            ('', ['standards', 'clean elc', 'compile --strict', 'run script test -- --jobs 2']),
            ('standards', ['standards']),
            ('clean elc', ['standards', 'clean elc']),
            ('compile --strict', ['standards', 'clean elc', 'compile --strict']),
            ('run script test -- --jobs 2',
             ['standards', 'clean elc', 'compile --strict', 'run script test -- --jobs 2']),
        ):
            with self.subTest(fail_at=fail_at), tempfile.TemporaryDirectory() as tmp:
                directory = Path(tmp)
                fake = directory / 'eask'
                fake.write_text(
                    '#!/bin/sh\n'
                    '[ -z "${PKG_EXECPATH+x}" ] || exit 99\n'
                    'printf "%s\\n" "$*" >> "$COMMAND_LOG"\n'
                    '[ "$*" != "$FAIL_AT" ] || exit 23\n')
                fake.chmod(0o755)
                python = directory / 'python3'
                python.write_text('#!/bin/sh\n'
                                  '[ "$*" = "scripts/check-standards.py" ] || exit 99\n'
                                  'echo standards >> "$COMMAND_LOG"\n'
                                  '[ "$FAIL_AT" != standards ] || exit 23\n')
                python.chmod(0o755)
                log = directory / 'commands'
                result = subprocess.run(
                    ['sh', str(ROOT / 'scripts/check.sh'), '--jobs', '2'],
                    env={**os.environ, 'PATH': f'{directory}:{os.environ["PATH"]}',
                         'COMMAND_LOG': str(log), 'FAIL_AT': fail_at,
                         'PKG_EXECPATH': '/standalone/eask'})
                self.assertEqual(result.returncode, 23 if fail_at else 0)
                self.assertEqual(log.read_text().splitlines(), expected)

    def test_hook_dispatch_and_exit_status(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            subprocess.run(['git', 'init', '-q', tmp], check=True)
            fake = directory / 'npx'
            fake.write_text('#!/bin/sh\n'
                            '[ -z "${GIT_INDEX_FILE+x}" ] || exit 99\n'
                            'printf "%s\\n%s\\n" "$PWD" "$*" '
                            '> "$COMMAND_LOG"\nexit 23\n')
            fake.chmod(0o755)
            nested = directory / 'nested'
            nested.mkdir()
            log = directory / 'commands'
            result = subprocess.run(
                [str(ROOT / '.githooks/pre-commit')], cwd=nested,
                env={**os.environ, 'PATH': f'{directory}:{os.environ["PATH"]}',
                     'COMMAND_LOG': str(log),
                     'GIT_INDEX_FILE': str(directory / 'commit-index')})
            self.assertEqual(result.returncode, 23)
            self.assertEqual(log.read_text().splitlines(),
                             [tmp, '@emacs-eask/cli run script check'])


if __name__ == '__main__':
    unittest.main()
