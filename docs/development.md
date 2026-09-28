# Development guide

Read this guide before planning or implementing code changes, writing or running
tests, compiling, or committing. These are repository requirements.

## External dependencies

- **gptel**, **yaml**, **orderless**, **websocket**, **qrencode**, **Emacs >=31.1**,
  **org-mode**

Eask dependency installs can get stale.
Run `npx @emacs-eask/cli upgrade PACKAGE` to update. For example:

```bash
npx @emacs-eask/cli upgrade gptel yaml orderless
```

Provider experiments intended to represent the user's configured setup must
verify the dependencies actually loaded in that setup and in the isolated
runner. A refreshed upstream checkout or matching model configuration does not
establish equivalence when the user runs a local dependency branch. Record loaded
library paths and source hashes; use a frozen copy of the intended dependency
sources in the runner before loading mevedel. Before paid calls, check the native
dry-run request for consequential controls such as reasoning effort and output
limits in that same runner. A policy variable being set is insufficient if the
loaded transport does not serialize it. Keep any intentionally different
dependency configuration explicit in the protocol and conclusions.

Preserve and check the configured backend's concrete type, not just its name,
model and endpoint. Reconstructing a Codex OAuth backend as a generic Responses
backend changes native request controls: OAuth omits unsupported server output
limits while mevedel retains its client-side guards. Preflight must exercise the
configured backend type through the native request path; matching an expected
payload from a substituted type does not establish configuration equivalence.

## gptel source rule

mevedel is tightly coupled to gptel. Before
implementing or changing behavior that touches prompts, requests, callbacks,
tool calls, presets, buffers, transcripts, session flow, agents, or
coordination, consult gptel source and reuse its existing
APIs or patterns instead of duplicating them.

Ensure the repository is cloned:

```bash
# First time
mkdir -p .scratch/upstream
git clone https://github.com/karthink/gptel .scratch/upstream/gptel
```

Prefer a refreshed upstream checkout, because Eask dependency installs can get
stale:

```bash
# Refresh before consulting
git -C .scratch/upstream/gptel pull --ff-only
```

## Development Commands

### Testing
Clear stale bytecode with the command below before tests so it never shadows
edited source. Use Eask cleanup, not `find -delete`: Eask also clears related
build caches.

```bash
# Clear stale bytecode first
npx @emacs-eask/cli clean elc

# With Eask installed
eask test ert test/test-*.el

# Via npx
npx @emacs-eask/cli test ert test/test-*.el

# Single file
npx @emacs-eask/cli test ert test/test-mevedel-compact.el
```

For a faster complete ERT run, use the measured parallel runner through the
same Eask script as GitHub CI after installing the Eask dependencies:

```bash
eask run script test
# Via npx:
npx @emacs-eask/cli run script test
# Limit concurrency on smaller machines:
npx @emacs-eask/cli run script test -- --jobs 4
```

This command performs the required Eask bytecode cleanup itself, discovers every
case in `test/test-*.el`, and runs each exactly once across independent Eask/Emacs
processes. Each worker gets Eask's temporary HOME and XDG roots and loads the
complete file roster. The default uses the CPUs available to the process
(respecting Linux CPU affinity), capped at eight workers and the number of
discovered tests. `--jobs` overrides the CPU-based limit. The runner prints the
available CPU count and selected worker count. Tests must be
independent of execution order; setup belongs inside the case or `:before-each`,
not in a separate template form. Shared fixtures isolate tool registries and
their lookup cache as well as cleaning up runtime state.

Reports go under `.scratch/test-suite-performance/` in a new timestamped
directory (`--output DIRECTORY` chooses another new directory). Native worker
logs retain failures; missing, duplicate, or extra results fail the run.
`summary.json` records cleanup, discovery, worker ERT, and full-suite wall times.
`durations.csv` sorts cases slowest first with cumulative duration and shares of
test time and wall time. Worker durations overlap, so wall-time shares are not
additive. Startup, loading, and shutdown are included in wall time; the difference
between worker wall and ERT time measures their combined overhead. Fixture setup
and teardown are included in individual case durations.

A successful run updates `latest-durations.csv`, used to balance later runs.
`--durations CSV` supplies another report; without timing history, all discovered
cases are distributed evenly. No test is omitted because a timing is missing.
Provisioned SSH, Podman, or Docker acceptance environments force serial execution
because their tests share target roots. Real transport acceptance remains
`test/run-remote-acceptance.sh`; its provisioning and runtime are separate from
the ordinary ERT command. Conditional skips remain visible in reports.

The runner uses Python's standard library and Eask from `PATH`. Invoking it
through `npx` makes that Eask executable available to its workers too. Direct
`python3 test/run_tests.py` invocation falls back to cached `npx` Eask when
`eask` is absent from `PATH`. Dependency installation is a separate setup step.
GitHub CI also checks the runner itself and uploads the report directory,
including worker logs on failure. The runner's own checks are:

```bash
python3 test/test_run_tests.py
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-test-helpers.el test/test-mevedel-test-runner.el
```

Test files mirror modules: `test/test-mevedel-MODULE.el`. Shared helpers
(including the `mevedel-deftest` macro) are in `test/helpers.el`. Tests
use real temp files/directories rather than mocking. Eask gives ERT a temporary
`HOME` and XDG roots; the shared helper rejects unsafe test invocations that
could reach real user state.

### Byte compilation
```bash
npx @emacs-eask/cli compile

# Afterwards, use the Eask cleanup command in Testing above.
```
Compile before committing. Keep the compiler silent: no free-variable or
unknown-function warnings. Use `declare-function` and `defvar` for external
symbols and `eval-when-compile` for compile-time-only dependencies.

Compilation-order regressions need a fresh Emacs subprocess: preloaded test
helpers can hide missing special-variable declarations and struct setter
expanders. The agent observer, agent runtime, and Goal edit cold-compilation
tests exercise compiled behavior before loading their ordinary test fixtures.
Required dependencies loaded from source can also hide missing compile-time
imports; use compiled dependency copies when testing that boundary.

### Reloading into a running Emacs

Load compiled code into a live editor, never bare sources. Interpreted
mevedel is several times slower and allocates on every call: on a large
session the transcript parser took 67 ms and 6.4 MB interpreted against
19 ms and 1.6 MB compiled, and garbage collection dominated interactive
pauses. Compile into a temporary directory so no bytecode is left beside
the sources or in a package build, where it could shadow later edits:

```elisp
(let* ((dir (make-temp-file "mevedel-reload-" t))
       (byte-compile-dest-file-function
        (lambda (file)
          (file-name-concat dir (concat (file-name-base file) ".elc")))))
  (dolist (file '("mevedel-view-composer.el"))
    (let ((source (expand-file-name file "~/Projekte/mevedel/")))
      (byte-compile-file source)
      (load (funcall byte-compile-dest-file-function source) nil t))))
```

A package build without `.elc` files loads every module from source. Check
with `(interpreted-function-p (symbol-function 'mevedel-transcript-segments))`.

## Code style

- **Lexical binding**: `;;; file.el -- Description -*- lexical-binding: t -*-`
- **Headers**: standard `;;; Commentary:` / `;;; Code:` sections
- **Section headers**: two blank lines above. Major: `;;` + blank + `;;;`.
  Subsections add more semicolons: `;;;;`, `;;;;;`, ...
- **Forward declarations**: grouped at file top by source package with
  `;; \`gptel'` style comment headers. Sort source-package groups
  alphabetically; within each group, put all `declare-function` forms first
  alphabetically, then all `defvar` forms alphabetically.
- **Customization**: `defcustom` uses `:group 'mevedel`
- **Private symbols**: double-dash `--` (e.g. `mevedel--workspace`,
  `mevedel-tools--validate-params`)
- **Path construction**: use `file-name-concat`, not `concat`, to join
  filesystem path components.
- **Provide**: each file ends with `(provide 'mevedel-MODNAME)` and
  `;;; mevedel-MODNAME.el ends here`
- **Minimize explicit runtime `require`s**: prefer actual autoloaded entry
  points. Use `declare-function` and `defvar` for byte-compiler declarations
  only; they do not load libraries, and variable access does not trigger
  autoloading.
- **Load dependencies at feature boundaries**: when a runtime dependency is
  not autoloaded or otherwise guaranteed to be loaded, `require` it once in a
  cold command/setup entry point, or at top level when it is unconditional
  and acyclic. Use `eval-when-compile` only for compile-time dependencies.
- **Never call `require` on a hot path**: code reached per segment, chunk,
  redraw tick, or guest step must rely on an earlier load boundary. Avoid
  circular dependencies through module direction rather than scattering
  lazy `require`s through helpers.
- **ASCII in code, unicode only in UI-facing strings**: comments,
  identifiers, and non-UI strings stay ASCII (use `->` not `→`,
  `lambda`/`fn` not `λ`). Unicode is fine in `propertize`, overlays,
  prompts, and other strings the user actually sees.
- **No spec references in code comments**: don't write `(spec 13)` or
  `(see spec 19)`. Specs are implementation-phase artifacts; code
  comments must stand on their own. Describe what a slot/variable
  holds, not where it was designed.
- **`error` strings**: capitalized, no package prefix —
  `(error "Unknown tool: %s" name)`. The backtrace identifies the
  source. checkdoc enforces capitalization. When the first word is a
  literal binary, option, or parameter name that must stay lowercase,
  quote it instead of changing its spelling: `(error "'pdftoppm' not
  installed")`.
- **`message` strings**: lowercase `"mevedel: ..."` prefix is fine —
  `(message "mevedel: stale request found, replacing")`. Output goes
  to `*Messages*` where there's no backtrace, so the prefix earns its
  keep.

## Testing conventions

- **Framework**: ERT via `mevedel-deftest` macro (`test/helpers.el`)
- **Naming**: the primary `test/test-mevedel-{module}.el` matches source.
  Focused `test/test-mevedel-{module}-{subject}.el` supplements are allowed
  when a single-function suite would make the primary file unwieldy.
- **One deftest per function**: all cases in one macro call; label with
  `:doc` strings. Rare exceptions are allowed where setup differs drastically.
- **Real files**, not mocks. Clean up in teardown.
- **Helpers require**:
  ```elisp
  (require 'helpers
           (file-name-concat
            (file-name-directory
             (or buffer-file-name load-file-name byte-compile-current-file))
            "helpers"))
  ```
- **Generated test names**: `FUNCTION/test` or `FUNCTION/test@N`
- **Doc strings**: describe what is tested, group with shared prefix
- **View/status redraws**: when changing async view, status-zone, agent, or
  task redraw paths, test that an active composer draft stays unchanged,
  including a multiline draft whose first editable character is `>`.
- **New functions need tests**; modify tests when behavior changes
- **Silent output**: a test run prints only ERT's own progress lines
  (`passed N/M name (time)`) and its final summary. No `mevedel:` messages,
  no `Warning`/`Error` lines, no Emacs notices such as `Making
  gptel-org-branching-context buffer-local while locally let-bound!`, and
  nothing at all during the loading phase. Noise hides real failures and is
  treated as a defect in the test or in the code it exercises:
  - A test that deliberately injects a failure must capture the resulting
    message or warning instead of letting it reach the run log.
  - Product code must not warn on a path a passing test takes; if the warning
    is legitimate, the test must assert it rather than emit it.
  - Loading a test file must have no side effects: no sessions, no requests,
    no messages. Anything that runs belongs inside a test body.
  - `mevedel-test--with-captured-diagnostics` captures messages and warnings;
    `mevedel-test--with-captured-messages` captures messages only, for a case
    that still inspects the warnings it raises. Both take a place to bind the
    captured text, or nil when the durable state the diagnostic echoes is what
    the case asserts. A capture must not forward to the original function:
    that re-prints what it just captured.
  - `mevedel-test--muted-message-regexps` in `test/helpers.el` drops the few
    third-party progress messages mevedel cannot suppress at their source.
    Nothing mevedel itself emits belongs there.
  - `mevedel-deftest`'s `:quiet t` captures the messages and warnings every
    case of one deftest provokes. Use it when the function under test
    correctly reports to the echo area on the paths its cases take and those
    cases assert the durable state the diagnostic echoes. Prefer an explicit
    capture with a place when the diagnostic text is itself the behaviour
    under test.
  - A notice Emacs raises from C, such as `Making VAR buffer-local while
    locally let-bound!`, reaches the log through neither mechanism. Remove
    its cause: let-binding a variable that product code then sets
    buffer-locally is the test's mistake, not the code's.
- **Isolation**: a test leaves no global state behind — no live session in the
  execution registry, no workspace registry entry, no live timer, no target
  connection, and no file outside its own temporary directory. Leaked state
  makes later tests slow, noisy, and order-dependent.
