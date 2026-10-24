"""Check the fail-fast build entry point and opt-in Git hook."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parent.parent


class CheckTests(unittest.TestCase):
    def test_check_sequence_and_failures(self):
        for fail_at, expected in (
            ('', ['clean elc', 'compile --strict', 'run script test -- --jobs 2']),
            ('clean elc', ['clean elc']),
            ('compile --strict', ['clean elc', 'compile --strict']),
            ('run script test -- --jobs 2',
             ['clean elc', 'compile --strict', 'run script test -- --jobs 2']),
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
