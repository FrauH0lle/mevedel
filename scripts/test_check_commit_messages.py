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
                     '* file (function):', '- file: Change.', '* file\nNo description.',
                     '* file:\n\nUnrelated paragraph.', '* a.el, : Missing file.'):
            with self.subTest(body=body):
                errors = standards.commit_message_errors('fix: Change it\n\n' + body)
                self.assertEqual(len(errors), 1)
                self.assertIn('GNU entry', errors[0])
        self.assertEqual(len(standards.commit_message_errors('Bad message')), 2)

    def test_wrapped_and_grouped_gnu_entries(self):
        # The wrapped entry is copied from reviewed commit 056476b69b55.
        for entry in (
            '* test/test-niwa-session-durability-heartbeat.el\n'
            '(niwa-session-durability-heartbeat): Add the case "a named\n'
            'lease-update decline is a lost lease: it reclaims and is no failure".',
            '* source.el (first):\nDescribe the change on the next line.',
            '* a.el, b.el: Share the behavior.',
            '* a.el,\n  b.el (first, second): Share the behavior.',
            '* a.el\n* b.el\n(first): Share the behavior.',
            '* a.el\n* b.el:\nShare the behavior.',
        ):
            with self.subTest(entry=entry):
                self.assertEqual(standards.commit_message_errors(
                    'fix: Share the behavior\n\n' + entry), [])

    def test_default_git_revert(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for arguments in (('init', '-q'), ('config', 'user.name', 'Test'),
                              ('config', 'user.email', 'test@example.invalid')):
                standards.git(root, *arguments)
            (root / 'file').write_text('Initial content')
            standards.git(root, 'add', 'file')
            standards.git(root, 'commit', '-qm', 'fix: Add content\n\n* file: Add content.')
            commit = standards.git(root, 'rev-parse', 'HEAD').decode().strip()
            standards.git(root, 'revert', '--no-edit', commit)
            message = standards.git(root, 'show', '-s', '--format=%B', 'HEAD').decode()
            self.assertTrue(message.startswith('Revert "fix: Add content"\n'))
            self.assertIn(f'This reverts commit {commit}.', message)
            self.assertEqual(standards.commit_message_errors(message), [])
            self.assertEqual(standards.commit_errors(root, commit), [])
            for invalid in (message.replace('This reverts commit', 'Undoes'),
                            message.replace(commit, 'not-a-commit'),
                            message.replace('Revert "', 'Undo "', 1)):
                with self.subTest(message=invalid):
                    self.assertTrue(standards.commit_message_errors(invalid))
        merge_revert = ('Revert "Merge branch"\n\nThis reverts commit ' + 'a' * 40
                        + ', reversing\nchanges made to ' + 'b' * 40 + '.\n')
        self.assertEqual(standards.commit_message_errors(merge_revert), [])

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
