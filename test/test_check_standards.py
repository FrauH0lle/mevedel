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

    def stub_source(self, body, requires=''):
        return library('test-demo').replace(
            '(defun test-demo-value (value)\n  "Return VALUE."\n  value)',
            requires + '\n(ert-deftest restore-test ()\n' + body + ')')

    def stub_diagnostics(self, body, requires=''):
        sources = [('demo.el', library()),
                   ('test/test-demo.el', self.stub_source(body, requires))]
        return standards.test_stubs(self.root, sources)['test/test-demo.el']

    def test_stub_owner_require_order_and_scope(self):
        stub = "(cl-letf (((symbol-function 'demo-value) #'ignore)) (demo-value))"
        bad = self.stub_diagnostics(stub)
        self.assertEqual(len(bad), 1)
        self.assertIn("require 'demo", bad[0]['message'])
        self.assertEqual(bad[0]['owner'], 'restore-test')
        for body, requires in [
                (stub, "(require 'demo)"),
                ("(require 'demo)\n" + stub, ''),
                ("(progn (require 'demo) " + stub + ')', ''),
                (stub.replace("'demo-value", "#'demo-value"), "(require 'demo)")]:
            with self.subTest(body=body, requires=requires):
                self.assertEqual(self.stub_diagnostics(body, requires), [])
        for body, requires in [
                (stub, "(eval-when-compile (require 'demo))"),
                (stub, "(require 'demo nil t)"),
                (stub, "(when nil (require 'demo))"),
                ("(when ready (require 'demo))\n" + stub, ''),
                ("(if ready (require 'demo) " + stub + ')', ''),
                ("(cl-letf (((symbol-function 'demo-value) #'ignore))\n"
                 "  (require 'demo) (demo-value))", ''),
                (stub + "\n(require 'demo)", '')]:
            with self.subTest(body=body, requires=requires):
                self.assertEqual(len(self.stub_diagnostics(body, requires)), 1)

    def test_stub_definition_resolution_and_other_mutators(self):
        for body in [
                "(cl-letf* (((symbol-function 'demo-value) #'ignore)) nil)",
                "(fset 'demo-value #'ignore)",
                "(setf (symbol-function 'demo-value) #'ignore)",
                "(defalias 'demo-value #'ignore)"]:
            with self.subTest(body=body):
                self.assertEqual(len(self.stub_diagnostics(body)), 1)
                self.assertEqual(self.stub_diagnostics(body, "(require 'demo)"), [])
        self.assertEqual(self.stub_diagnostics(
            "(cl-letf (((symbol-function 'external-function) #'ignore)) nil)"), [])
        self.assertEqual(self.stub_diagnostics(
            "'(cl-letf (((symbol-function 'demo-value) #'ignore)) nil)"), [])
        sources = [('different.el', library().replace("(provide 'demo)",
                                                     "(provide 'owner-feature)")),
                   ('test/test-demo.el', self.stub_source(
                       "(fset 'demo-value #'ignore)", "(require 'different)"))]
        bad = standards.test_stubs(self.root, sources)['test/test-demo.el']
        self.assertIn("require 'owner-feature", bad[0]['message'])
        sources[1] = (sources[1][0], sources[1][1].replace("'different", "'owner-feature"))
        self.assertEqual(standards.test_stubs(self.root, sources)['test/test-demo.el'], [])
        sources = [('test/helpers.el', library('helpers')),
                   ('test/test-demo.el', self.stub_source("(defalias 'helpers-value #'ignore)"))]
        bad = standards.test_stubs(self.root, sources)['test/test-demo.el']
        self.assertEqual(len(bad), 1)
        self.assertIn("require 'helpers", bad[0]['message'])

    def test_stub_executable_subforms_and_initializers(self):
        stub = "(fset 'demo-value #'ignore)"
        for body in [f'(let ((x {stub})) x)', f'`(data ,{stub})',
                     f'`(data ,@{stub})',
                     f"(condition-case nil (require 'demo) (error {stub}))",
                     f"(unwind-protect (require 'demo) {stub})"]:
            with self.subTest(body=body):
                self.assertEqual(len(self.stub_diagnostics(body)), 1)
        for body in [f"(if (require 'demo) {stub})",
                     f"(let () (require 'demo)) {stub}",
                     f"(let ((x (require 'demo))) {stub})",
                     "(fset 'demo-value (progn (require 'demo) #'ignore))",
                     "(setf (symbol-function 'demo-value) "
                     "(progn (require 'demo) #'ignore))",
                     "(cl-letf (((symbol-function 'demo-value) #'ignore)"
                     " ((symbol-function 'external) (progn (require 'demo) #'ignore))) nil)"]:
            with self.subTest(body=body):
                self.assertEqual(self.stub_diagnostics(body), [])
        self.assertEqual(len(self.stub_diagnostics(
            "(cl-letf* (((symbol-function 'demo-value) #'ignore)"
            " ((symbol-function 'external) (progn (require 'demo) #'ignore))) nil)")), 1)

    def test_stub_mevedel_cases_and_fixture_options(self):
        stub = "(cl-letf (((symbol-function 'demo-value) #'ignore)) nil)"

        def collect(options, template):
            text = library('test-demo').replace(
                '(defun test-demo-value (value)\n  "Return VALUE."\n  value)',
                f'(mevedel-deftest demo-value ({options}) {template})')
            return standards.test_stubs(self.root, [('demo.el', library()),
                                                     ('test/test-demo.el', text)])[
                                                         'test/test-demo.el']

        for option in (f':before-each {stub}', f':after-each {stub}',
                       f':vars ((x {stub}))', f':vars* ((x {stub}))'):
            with self.subTest(option=option):
                self.assertEqual(len(collect(option, '(should t)')), 1)
        self.assertEqual(collect(":before-each (require 'demo)", stub), [])
        separate = collect('', f",test (test) :doc \"one\" (require 'demo) "
                           f':doc "two" {stub}')
        self.assertEqual(len(separate), 1)
        self.assertEqual(separate[0]['owner'], 'demo-value/test@2')
        self.assertEqual(collect('', f"(progn (require 'demo) ,test) (test) {stub}"), [])

    def test_stub_ratchet_uses_base_definitions_and_per_test_counts(self):
        stub = "(cl-letf (((symbol-function 'demo-value) #'ignore)) nil)"
        self.write('test/test-demo.el', self.stub_source(stub))
        self.commit()
        output, _ = self.check(0)
        self.assertIn('1 existing stub diagnostics advisory', output)
        self.write('test/test-demo.el', self.stub_source('\n' + stub))
        self.check(0)  # Line shifts do not add debt.
        self.write('test/test-demo.el', self.stub_source(stub + '\n' + stub))
        _, errors = self.check(1)
        self.assertIn('test-stub:', errors)
        self.write('test/test-demo.el', self.stub_source(stub).replace(
            'restore-test', 'another-test'))
        self.check(1)  # A removed warning cannot buy one in a different test.
        self.write('test/test-demo.el', self.stub_source(stub))
        # An unchanged test must be rechecked if its defining feature changes.
        self.write('demo.el', library().replace("(provide 'demo)", "(provide 'new-owner)"))
        _, errors = self.check(1)
        self.assertIn("require 'new-owner", errors)

    def test_stub_transfer_shape_negative_control(self):
        # A cold restore loads the real chat implementation over the stub, then
        # cl-letf restores its saved autoload, losing the loaded definition.
        self.write('chat.el', library('chat').replace('chat-value', 'chat-open'))
        body = ("(cl-letf (((symbol-function 'chat-open) (lambda () 'stub)))\n"
                "  (require 'chat)\n  (should (eq (chat-open nil) 'stub)))")
        self.write('test/test-demo.el', self.stub_source(body))
        _, errors = self.check(1)
        self.assertIn("stubbing chat-open requires (require 'chat)", errors)
        self.write('test/test-demo.el', self.stub_source(body, "(require 'chat)"))
        self.check(0)
        # Exercise the actual cold autoload/stub interaction as well as linting
        # its source.  Both processes load only the disposable fixture library.
        for preload in ('', "(require 'chat)"):
            expression = f"""(progn
              (require 'cl-lib)
              (autoload 'chat-open "chat")
              {preload}
              (cl-letf (((symbol-function 'chat-open) (lambda (&rest _) 'stub)))
                (require 'chat)
                (unless (eq (chat-open 'real) '{'stub' if preload else 'real'})
                  (error "Negative control did not exercise cold replacement")))
              (unless (eq (autoloadp (symbol-function 'chat-open))
                          {'nil' if preload else 't'})
                (error "Unexpected restored function cell")))"""
            subprocess.run(['emacs', '--batch', '-Q', '-L', str(self.root),
                            '--eval', expression], check=True, capture_output=True)

    def test_stub_ratchet_preserves_renames_and_catches_removed_imports(self):
        stub = "(cl-letf (((symbol-function 'demo-value) #'ignore)) nil)"
        self.write('test/test-demo.el', self.stub_source(stub))
        self.commit()
        self.git('mv', 'test/test-demo.el', 'test/test-moved.el')
        self.write('test/test-moved.el', self.stub_source(stub).replace(
            'test-demo', 'test-moved'))
        output, _ = self.check(0)
        self.assertIn('1 existing stub diagnostics advisory', output)
        self.write('test/test-moved.el', self.stub_source(
            stub, "(require 'demo)").replace('test-demo', 'test-moved'))
        self.commit()
        self.write('test/test-moved.el', self.stub_source(stub).replace(
            'test-demo', 'test-moved'))
        _, errors = self.check(1)
        self.assertIn('test-stub:', errors)

    def test_stub_collector_does_not_evaluate_sources(self):
        sentinel = self.root / 'must-not-exist'
        source = library() + f'\n(write-region "bad" nil "{sentinel}")\n'
        source += f';; Local Variables:\n;; eval: (write-region "bad" nil "{sentinel}")\n;; End:\n'
        standards.test_stubs(self.root, [('demo.el', source)])
        self.assertFalse(sentinel.exists())

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
