"""Exercise standards checks with real Git repositories and batch Emacs."""

from contextlib import redirect_stdout, redirect_stderr
import importlib.util
import io
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parent.parent
SPEC = importlib.util.spec_from_file_location('standards', ROOT / 'scripts/check-standards.py')
standards = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(standards)


def library(name='demo', doc='Return VALUE.'):
    return (f';;; {name}.el --- Example -*- lexical-binding: t; -*-\n'
            ';;; Commentary:\n;; An example library.\n;;; Code:\n'
            f'(defun {name}-value (value)\n  "{doc}"\n  value)\n'
            f"(provide '{name})\n;;; {name}.el ends here\n")


class StandardsTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.git('init', '-q', '-b', 'master')
        self.git('config', 'user.email', 'test@example.invalid')
        self.git('config', 'user.name', 'Test')
        self.write('docs/adr/README.md', '# Decisions\n')
        self.write('demo.el', library(doc='Return a value.'))
        self.commit()

    def git(self, *args):
        return standards.git(self.root, *args).decode().strip()

    def write(self, name, text):
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)

    def commit(self):
        self.git('add', '.')
        self.git('commit', '-qm', 'Fixture')

    def check(self, expected, base='master'):
        output, errors = io.StringIO(), io.StringIO()
        with redirect_stdout(output), redirect_stderr(errors):
            status = standards.run(self.root, base)
        self.assertEqual(status, expected, errors.getvalue())
        return output.getvalue(), errors.getvalue()

    def test_boundaries(self):
        self.assertEqual(standards.lisp_boundaries('demo.el', library()), [])
        self.assertEqual(standards.lisp_boundaries('demo.el', library() + '\n'), [])
        self.assertEqual(standards.lisp_boundaries(
            'demo.el', library().replace('lexical-binding: t;',
                                         'lexical-binding: t; coding: utf-8;')), [])
        self.assertEqual(len(standards.lisp_boundaries(
            'demo.el', library().replace('lexical-binding: t', 'lexical-binding: nil'))), 1)
        self.assertEqual(len(standards.lisp_boundaries(
            'demo.el', library().replace('lexical-binding:', 'not-lexical-binding:'))), 1)
        self.assertEqual(len(standards.lisp_boundaries('wrong.el', library())), 1)
        self.assertEqual(len(standards.lisp_boundaries('demo.el', library() + '(foo)')), 1)
        self.assertEqual(standards.lisp_boundaries('test/fixtures/demo.el', '(foo)'), [])
        self.assertEqual(standards.lisp_boundaries('.dir-locals.el', '((nil))'), [])

    def test_markdown_links(self):
        self.write('docs/target.md', '# A `code` heading\n# Duplicate\n# Duplicate\n'
                   '<a id="custom"></a>\n')
        self.write('space file.txt', 'Data')
        self.write('docs/page(v1).md', '# Versioned page\n')
        text = ('# Source\n[ok](target.md#a-code-heading)\n[twice](target.md#duplicate-1)\n'
                '[custom](target.md#custom)\n[encoded](../space%20file.txt)\n'
                '![image](../space%20file.txt)\n[title](target.md "A title")\n'
                '[external](https://invalid.example/missing)\n'
                '[reference][ok]\n[ok]: target.md\n'
                r'[escaped](page\(v1\).md)' '\n'
                '`[code](missing.md)`\n```md\n[example](missing.md)\n```\n'
                '~~~md\n[example](missing.md)\n~~~\n')
        self.assertEqual(standards.link_errors(self.root, 'docs/source.md', text), [])
        for link, diagnostic in [('[bad](missing.md)', 'missing link target'),
                                 ('[bad](target.md#missing)', 'missing Markdown anchor'),
                                 (r'[bad](missing\(v1\).md)', 'missing link target'),
                                 ('[bad][missing]', 'undefined Markdown link reference')]:
            with self.subTest(link=link):
                errors = standards.link_errors(self.root, 'docs/source.md', link)
                self.assertEqual(len(errors), 1)
                self.assertIn(diagnostic, errors[0])
        self.write('docs/page.md', '[bad](missing.md)')
        _, errors = self.check(1)
        self.assertIn('docs/page.md:1: missing link target', errors)

    def test_adr_index(self):
        self.write('docs/adr/0002-current.md', '# Current\n## Decision history\n')
        index = ('| 0001 | Old | [history](0002-current.md#decision-history) |\n'
                 '| 0002 | Current | [current](0002-current.md) |\n')
        self.write('docs/adr/README.md', index)
        self.assertEqual(standards.adr_errors(self.root, standards.inventory(self.root)), [])
        self.commit()
        self.write('docs/adr/README.md', index.splitlines()[1] + '\n')
        _, errors = self.check(1)
        self.assertIn('removed original ADR ID 0001', errors)
        self.write('docs/adr/README.md', index + index)
        _, errors = self.check(1)
        self.assertIn('duplicate ADR ID 0002', errors)
        compact = '|0002|Duplicate|[current](0002-current.md)|\n'
        self.write('docs/adr/README.md', index + compact)
        _, errors = self.check(1)
        self.assertIn('duplicate ADR ID 0002', errors)
        self.write('docs/adr/README.md', index.replace('#decision-history', ''))
        _, errors = self.check(1)
        self.assertIn('must link to decision-history', errors)
        self.write('docs/adr/0003-unindexed.md', '# Unindexed\n')
        _, errors = self.check(1)
        self.assertIn('current ADR is missing its own index entry', errors)
        self.write('docs/adr/README.md', index.replace('0002-current.md', '0002-missing.md'))
        _, errors = self.check(1)
        self.assertIn('missing link target', errors)

    def test_checkdoc_real_diagnostics_and_line_shifts(self):
        old, good = standards.checkdoc(self.root, [('demo.el', library(doc='Return a value.')),
                                                 ('demo.el', library())])
        self.assertTrue(old, 'Negative control must produce a real checkdoc diagnostic')
        self.assertTrue(any('VALUE' in item['message'] for item in old))
        self.assertTrue(all(item['owner'] == 'demo-value' for item in old))
        self.assertEqual(good, [])
        self.write('demo.el', library(doc='Return a value.').replace(';;; Code:', '\n;;; Code:'))
        output, _ = self.check(0)
        self.assertIn('1 changed Lisp files', output)
        self.assertIn(f'{len(old)} existing diagnostics advisory', output)
        self.write('demo.el', library())
        self.check(0)
        self.write('demo.el', library(doc='return a value.'))
        _, errors = self.check(1)
        self.assertIn('checkdoc:', errors)

    def test_checkdoc_scopes_messages_to_definition(self):
        item = {'owner': 'first', 'message': 'Missing argument', 'line': 5}
        shifted = {**item, 'line': 50}
        moved = {**item, 'owner': 'second'}
        self.assertEqual(standards.new_diagnostics([item], [shifted]), [])
        self.assertEqual(standards.new_diagnostics([item], [moved]), [moved])
        self.assertEqual(standards.new_diagnostics([item], [item, item]), [item])
        text = library().replace("(provide 'demo)",
                                 '(defun demo-second (value)\n  "Return VALUE."\n  value)\n'
                                 "(provide 'demo)")
        for form in ('defun', 'cl-defun', 'cl-defmacro'):
            with self.subTest(form=form):
                source = text.replace('(defun ', f'({form} ')
                before = source.replace('Return VALUE.', 'Return a value.', 1)
                after = 'Return a value.'.join(source.rsplit('Return VALUE.', 1))
                old, current = standards.checkdoc(self.root, [('demo.el', before),
                                                            ('demo.el', after)])
                self.assertEqual({item['owner'] for item in old}, {'demo-value'})
                self.assertEqual({item['owner'] for item in current}, {'demo-second'})
                self.assertEqual(standards.new_diagnostics(old, current), current)

    def test_changed_only_new_deleted_renamed_and_committed_files(self):
        output, _ = self.check(0)
        self.assertIn('0 changed Lisp files', output)
        self.write('new.el', library('new', 'Return a value.'))
        _, errors = self.check(1)
        self.assertIn('new.el:', errors)
        (self.root / 'new.el').unlink()
        self.git('mv', 'demo.el', 'renamed.el')
        self.write('renamed.el', library('demo', 'Return a value.').replace('demo.el', 'renamed.el'))
        self.check(0)
        (self.root / 'renamed.el').unlink()
        output, _ = self.check(0)
        self.assertIn('0 changed Lisp files', output)
        self.git('reset', '--hard', '-q', 'HEAD')
        self.git('switch', '-qc', 'topic')
        self.write('demo.el', library(doc='return a value.'))
        self.commit()
        _, errors = self.check(1)
        self.assertIn('checkdoc:', errors)
        self.git('switch', '-q', 'master')
        self.write('demo.el', library(doc='return a value.'))
        self.write('other.txt', 'Ensure distinct divergent commits')
        self.commit()
        self.git('switch', '-q', 'topic')
        self.check(1)  # Same new warning on master cannot mask the topic regression.

    def test_collector_does_not_evaluate_file_code_or_local_variables(self):
        sentinel = self.root / 'must-not-exist'
        text = library() + f'\n(write-region "bad" nil "{sentinel}")\n'
        text += f';; Local Variables:\n;; eval: (write-region "bad" nil "{sentinel}")\n;; End:\n'
        standards.checkdoc(self.root, [('demo.el', text)])
        self.assertFalse(sentinel.exists())

    def test_missing_base_fails_closed(self):
        with self.assertRaises(subprocess.CalledProcessError):
            standards.run(self.root, 'does-not-exist')

    def test_ci_base_selection(self):
        workflow = (ROOT / '.github/workflows/test.yml').read_text()
        block = workflow.split('    - name: Select standards comparison base\n', 1)[1]
        block = block.split('      run: |\n', 1)[1].split('\n    - ', 1)[0]
        script = '\n'.join(line[8:] for line in block.splitlines())
        for base, expected in [('', 'origin/master'), ('0' * 40, 'origin/master'),
                               ('a' * 40, 'a' * 40)]:
            with self.subTest(base=base):
                target = self.root / 'github-env'
                target.write_text('')
                subprocess.run(['sh', '-eu', '-c', script], check=True,
                               env={'EVENT_BASE': base, 'DEFAULT_BRANCH': 'master',
                                    'GITHUB_ENV': str(target)})
                self.assertEqual(target.read_text(), f'CHECK_BASE={expected}\n')


if __name__ == '__main__':
    unittest.main()
