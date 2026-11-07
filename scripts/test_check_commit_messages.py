"""Check commit-message rules and history boundaries with real Git fixtures."""

from contextlib import redirect_stdout, redirect_stderr
import importlib.util
import io
from pathlib import Path
import tempfile
import unittest


SPEC = importlib.util.spec_from_file_location(
    'standards', Path(__file__).with_name('check-standards.py'))
standards = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(standards)


class CommitMessageTests(unittest.TestCase):
    def test_good_messages(self):
        for message in (
            'fix(check): Validate messages\n\n* scripts/check-standards.py (run): Check commits.\n',
            'docs: Explain checks\n\nRationale.\n\n* docs/development.md: Document checks.\n',
            'refactor(session)!: Replace format\n\n* session.el (save, load): Replace storage.\n'
            '\nBREAKING CHANGE: Replace the old format.\n',
            'feat!: Change defaults\n\n* settings.el: Change defaults.\n',
            'fix: Update callers\n\n* source.el (first, second)\n'
            '(third): Share the behavior.\n',
        ):
            with self.subTest(message=message):
                self.assertEqual(standards.commit_message_errors(message), [])

    def test_bad_messages(self):
        for subject in ('Fix the check', 'unknown: Change it', 'fix: lowercase',
                        'fix(): Empty scope', 'fix: ', 'fix(scope):Missing space'):
            with self.subTest(subject=subject):
                errors = standards.commit_message_errors(subject + '\n\n* file: Change.\n')
                self.assertEqual(len(errors), 1)
                self.assertIn('subject', errors[0])
        for body in ('', 'Explain the change.', '* file: ', '* file (): Change.',
                     '* file (function):', '- file: Change.'):
            with self.subTest(body=body):
                errors = standards.commit_message_errors('fix: Change it\n\n' + body)
                self.assertEqual(len(errors), 1)
                self.assertIn('GNU entry', errors[0])
        self.assertEqual(len(standards.commit_message_errors('Bad message')), 2)

    def test_history_checks_docs_and_side_commits_but_skips_merges_and_base(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)

            def git(*args):
                return standards.git(root, *args).decode().strip()

            def commit(name, message):
                (root / name).write_text(message)
                git('add', name)
                git('commit', '-qm', message)
                return git('rev-parse', 'HEAD')

            git('init', '-q', '-b', 'master')
            git('config', 'user.name', 'Test')
            git('config', 'user.email', 'test@example.invalid')
            index = root / 'docs/adr/README.md'
            index.parent.mkdir(parents=True)
            index.write_text('# Decisions\n')
            git('add', str(index))
            base = commit('base.txt', 'Legacy base message')
            git('switch', '-qc', 'topic')
            good = commit('code.py', 'fix: Validate messages\n\n* code.py: Check messages.')
            self.assertEqual(standards.commit_errors(root, base), [])
            with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
                self.assertEqual(standards.run(root, 'master'), 0)
            git('switch', '-qc', 'side')
            bad = commit('README.md', 'docs: Explain checks without a GNU entry')
            git('switch', '-q', 'topic')
            commit('other.py', 'test: Add coverage\n\n* other.py (test): Cover checks.')
            git('merge', '--no-ff', '-qm', 'Merge with an otherwise invalid message', 'side')
            errors = standards.commit_errors(root, base)
            self.assertEqual(len(errors), 1)
            self.assertIn(bad[:12], errors[0])
            self.assertIn('GNU entry', errors[0])
            self.assertNotIn(good[:12], errors[0])
            diagnostics = io.StringIO()
            with redirect_stdout(io.StringIO()), redirect_stderr(diagnostics):
                self.assertEqual(standards.run(root, 'master'), 1)
            self.assertIn(f'{bad[:12]}: commit-message:', diagnostics.getvalue())
            # A divergent mainline commit after the merge base is not in HEAD.
            git('switch', '-q', 'master')
            commit('main.txt', 'Unrelated invalid mainline message')
            git('switch', '-q', 'topic')
            revision = git('merge-base', 'master', 'HEAD')
            self.assertEqual(revision, base)
            self.assertEqual(standards.commit_errors(root, revision), errors)


if __name__ == '__main__':
    unittest.main()
