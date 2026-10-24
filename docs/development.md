# Development guide

Read this guide before planning or implementing code changes, writing or running
tests, compiling, or committing. These are repository requirements.

## External dependencies

- **gptel**, **acp >=0.15.2**, **yaml**, **orderless**, **websocket**, **qrencode**, **Emacs >=31.1**,
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

## Worktree bootstrap

Run `python3 scripts/bootstrap-worktree.py [WORKTREE]` from a checkout root
before tests. It reuses the primary checkout's `.eask`, validates this worktree's
dependency versions and loading through Eask, and reports Emacs/Eask versions, paths and
source hashes. It runs offline without installing or upgrading dependencies;
failure prints recovery instructions. `.eask` is ignored as directory or symlink.
This repo's `.dir-locals.el` configures creation-time setup, requiring Emacs
approval. The separate `mevedel-worktree-setup-enabled` flag defaults off;
`mevedel-worktree-setup-command` holds argv. Both creation paths and the skill
honor that authorization; see [the worktree ADR](adr/0001-worktree-command-and-skill.md).
Setup output stays in `*mevedel worktree setup*`; failure retains the checkout.

## Dependency maintenance

`.github/dependabot.yml` schedules weekly update PRs for the exact npm pins
and lockfile in `shared-editing/`, the Go module in `relay/`, and GitHub
Actions references. Tiptap updates are grouped, including major releases;
the dependency test checks that its schema packages remain synchronized.
`@tiptap/y-tiptap` has its own version series. Updates are reviewed, not
automatically merged. Dependabot security updates also depend on the
repository's dependency graph, alerts, and security-update settings.

The read-only `dependencies` workflow installs the lockfile, rebuilds the
helper and viewer assets, and rejects tracked differences or unexpected
generated files. It also runs helper/viewer tests, focused Chromium
regressions, and Go checks on PRs. It does not replace the full shared-editor
browser suite, which additionally needs the isolated Emacs/Eask environment.
The workflow uses no secrets, disables checkout credentials, and never writes
back to a PR or runs PR code through `pull_request_target`.

Dependabot does not regenerate committed bundles, WASM, or license notices.
For an npm update PR, check out that branch and use Node 26 to run:

```sh
npm ci --prefix shared-editing
npm run build --prefix shared-editing
```

Review and commit all generated changes along with the manifest and lockfile,
then run the checks in [shared editing](shared-editing.md#checks). A PR with
stale generated assets intentionally fails CI; testing a rebuilt bundle is
not proof that the packaged bundle was updated. New viewer assets enter the
relay release when its binary is rebuilt.

The bot does not maintain Eask/MELPA dependencies, the embedded tree-sitter
grammar pin, manually ported Excalidraw code and fonts, system packages, or
the unpinned documentation-site Python install. Actions using `@master` also
remain floating; update PRs do not turn branch references into immutable pins.
These still need explicit upstream/provenance review. Repository settings and
required branch checks must be configured on GitHub; the workflow files do
not establish them.

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

For design comparisons with Codex, Claude Code, or its ACP adapter, consult
[Local reference sources](reference-sources.md) for verified local entry points,
revision/version checks, and source-versus-installed-runtime limitations.

## Development Commands

### Checks before committing

After installing the Eask development dependencies, run the same fail-fast
check as GitHub CI:

```bash
npx @emacs-eask/cli run script check
# With Eask on PATH:
eask run script check
# Limit test concurrency on smaller machines:
npx @emacs-eask/cli run script check -- --jobs 4
```

The check first runs the deterministic standards checks below, cleans stale
bytecode, compiles the package with `--strict` (warnings fail the command), then
runs the complete isolated ERT suite through the existing
`test` script. The test runner cleans bytecode again before loading sources, so
compiled files cannot shadow edits. Each failed step stops the check and returns
a nonzero status. Dependency installation remains a separate setup step.

#### Opt-in pre-commit hook

To run that check automatically before local commits, from the repository root:

```bash
git config --local core.hooksPath .githooks
```

This replaces the repository's configured hooks path; inspect
`git config --get core.hooksPath` first if you already use hooks. Installation is
optional and is never performed by Eask or CI. To undo it, use
`git config --local --unset core.hooksPath` (or restore your previous value).
The hook requires Node/npx, Python 3, Emacs and installed Eask development
dependencies, and runs the full check, not a focused subset. It checks the
working tree, not a temporary copy of the index: when partially staging files,
ensure the staged change passes independently too. CI runs the check even when
the hook is not installed or is bypassed; required branch checks are a separate
GitHub repository setting.

#### Deterministic standards and checkdoc ratchet

`scripts/check-standards.py` runs first in `check` and CI. Run it independently
without Eask or dependencies (Python 3, Git and Emacs are sufficient):

```bash
python3 scripts/check-standards.py
python3 scripts/check-standards.py --base origin/master
CHECK_BASE=BASE npx @emacs-eask/cli run script check
```

The default base is `master`, overridden by `CHECK_BASE` or `--base`. The script
resolves `git merge-base BASE HEAD`; it checks the working tree, including
committed changes, unstaged edits, and non-ignored new files, not just the index.
An unavailable base fails with recovery-visible Git errors; fetch the base and
enough history first. CI fetches full history and uses the PR base SHA, the
push's previous SHA, or the default remote branch on a new branch/manual run.

Enforced invariants:

- First-line `lexical-binding: t` and a matching final `;;; FILENAME.el ends
  here` footer (optional final period) in root Lisp libraries and immediate
  `test/`, `benchmark/`, and `scripts/` Lisp files. Dotfiles, data, and nested
  executable fixtures are not libraries and are outside this layout check.
- Local inline/image and explicit reference Markdown links in `docs/` resolve
  to files/directories and, for Markdown fragments, heading or explicit HTML
  anchors. Fenced and inline code examples are ignored. This is not a full
  Markdown renderer: shortcut reference links, setext headings and arbitrary HTML
  links are outside the check; external URLs are not fetched.
- ADR index IDs are unique, base IDs remain present, every current ADR has its
  own index destination, and consolidated IDs point to a valid decision-history
  anchor. Both the original-ID table and additional-record list are checked.
- Checkdoc runs **only on changed `.el` files**, in clean batch Emacs without
  evaluating sources or file locals. Git-detected renames retain their base
  diagnostics; deleted files need no check. New diagnostics fail the command.
  The ratchet compares message counts per top-level definition, ignoring line
  shifts; fixing one definition cannot buy warnings in another. Existing
  diagnostics in changed files are summarized as advisory, not repeated.

The checker regression suite exercises real temporary Git repos and Emacs,
including injected violations and checkdoc negative controls:

```bash
python3 -m unittest test/test_check_standards.py test/test_check.py
```

Full-tree checkdoc remains available as an advisory inventory:

```bash
npx @emacs-eask/cli lint checkdoc '*.el'
```

The 2026-10-24 baseline on Emacs 31.1/Eask 0.12.9 reports 528 diagnostics across
146 of 261 root Lisp files, mostly undocumented arguments and sentence spacing.
Those existing diagnostics do not block the ratchet. Both sides are measured
with the same installed Emacs, not a frozen warning count. Use `--strict` with
the advisory command when a nonzero exit status on all findings is needed.

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

For a faster complete ERT-only run, use the measured parallel runner after
installing the Eask dependencies (the `check` script above also runs it):

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
including worker logs on failure. A crashed worker or invalid result file fails
the run with its worker log path; the summary and timings for other completed
workers are still written. The summary distinguishes the reported count from
the expected inventory, and incomplete runs never replace the balancing history.
The runner's own checks are:

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

The Eask startup hook also redirects the native compilation cache into that
temporary HOME before loading test files or their dependencies.  ERT's own
cache redirection happens after test-file loading and is not sufficient:
changing HOME alone leaves Emacs's startup native-cache path pointing at the
real home.  Both caches are removed when their owning test process exits.

### Byte compilation
```bash
npx @emacs-eask/cli compile --strict

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

### View allocation gate

```bash
npx @emacs-eask/cli run script bench-gate
```

This standalone gate compiles the package and measurement helpers into a
temporary directory, then measures in a fresh batch Emacs. It refuses
interpreted hot paths and removes its bytecode, private HOME, XDG roots and
native cache afterwards. No code is loaded into the running editor.

Each scenario uses a fresh 1 MiB synthetic rendered history shared with the
profiler harness, a selected 80-column, 25-line frame and a multiline composer
draft beginning with `>`. Five warmups precede each sample. Fixture setup and
output assertions are outside the measured operation:

- `stream-chunk`: append a fixed response chunk to a prepared in-flight turn,
  then call `mevedel-view--render-stream-update` directly.
- `composer-keystroke`: `self-insert-command` followed by the actual view's
  `post-command-hook`, with window geometry primed before measurement.
- `zone-tick`: cross a deterministic second boundary in
  `mevedel-view--spinner-tick`, rewriting the progress rail from `12s` to `13s`.
  Static spinner styles isolate the semantic elapsed-label update.

The gate checks that each update modified the buffer and produced its intended
output without corrupting the composer. The clock binding and reconciliation
counter are included in the operation, consistently across runs. Every raw
sample starts with `garbage-collect`; net counters subtract the componentwise
minimum of three no-op measurements, clamped at zero. Output includes conses,
string characters, vector cells and GC count. Only conses gate acceptance;
budgets are the recorded baseline plus 100 conses per operation. A breach
signals an error and returns a nonzero process status. The baseline and
repeatability evidence live in
[`benchmark/profiler-optimization-results.md`](../benchmark/profiler-optimization-results.md).
ERT covers counter arithmetic and path guards, not absolute budgets.

### Minimal-edit twin report

```bash
npx @emacs-eask/cli run script bench-twin
```

The twin reuses the allocation gate's stream-chunk and elapsed-zone fixtures.
It reports, not gates. It snapshots the buffer including properties, runs the
update, derives one contiguous character replacement from the longest common
character prefix and a nonoverlapping suffix, and adds only differing property
runs. Inserted text carries its final properties. Replaying on a snapshot copy
must be `equal-including-properties` to the live result.

`normal-bytecode` lines report floor deleted/inserted characters and explicit
property runs, plus five-sample median times for the unadvised whole update
and the floor replay. Five warmups precede the samples, with GC before each
sample. Snapshot creation, edit derivation and equality assertions are untimed.

`generic-call-counts` lines report observed real deleted/inserted characters
and nonempty property primitive calls (including redundant writes). These
come from a separate fresh-fixture process and count-only build: Emacs's
normal `insert` and `delete-region` bytecode opcodes bypass advice, so this
build temporarily disables those two byte-compile handlers to emit generic
calls instead. Timings always use the ordinary, uninstrumented bytecode build.
The runner restores the handlers and deletes both builds afterwards.

The observer covers successful calls to `delete-region`, `insert`,
`set-text-properties`, `add-text-properties` and `put-text-property` targeting
the view, even with modification hooks inhibited. It excludes edits to other
buffers/strings, properties carried by an inserted string, other mutation
primitives and internal C work. A reported property "run" on the real side is
one nonempty primitive call, not the number of internal interval splits.
The floor side counts explicit differing-property assignments. These counters
describe the observed writes, not a complete accounting of all renderer work.

This is the minimal *single-span* character edit plus property corrections,
not a globally minimal multi-edit script. The replay has the same text and
text properties, not equivalent hooks, overlays, markers, mode state, layout
or redisplay. Its time is a text-mutation floor, not an achievable UI latency
target or a graphical redisplay measurement.

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
  (first-line binding and final filename footer enforced by `check-standards.py`
  in the library/test scope above).
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
  source. The checkdoc ratchet gates new capitalization diagnostics. When the
  first word is a literal binary, option, or parameter name that must stay
  lowercase, quote it instead of changing its spelling: `(error "'pdftoppm' not
  installed")`.
- **`message` strings**: lowercase `"mevedel: ..."` prefix is fine —
  `(message "mevedel: stale request found, replacing")`. Output goes
  to `*Messages*` where there's no backtrace, so the prefix earns its
  keep.

## Loading and initialization

Prefer deferral at meaningful feature boundaries, not maximum laziness. Keep
cheap, unconditional, acyclic dependencies eager when that makes ownership
clearer. Loading and first-use setup belong to the implementation; callers
must not need a load-order recipe.

- **Use real autoloaded entry points**. `declare-function` and `defvar` are
  byte-compiler declarations; they do not load libraries, and variable access
  does not trigger autoloading. Use `eval-when-compile` only for compile-time
  dependencies. A caller that may run before its callee's module loads pairs
  the declaration with an `autoload` in the calling module; an `autoload` in a
  module that is not loaded yet has no effect. Buffer-local state that
  several owners read lives in an eagerly loaded module such as
  `mevedel-structs`, not behind `boundp` guards.
- **Keep M-x entry points reachable**. Top-level commands carry
  `;;;###autoload` cookies for package installs and an `autoload` in
  `mevedel.el` for source checkouts. Mode-local and transient suffix commands
  load with the module that binds them.
- **Load dependencies at feature boundaries**. When a runtime dependency is
  not autoloaded or otherwise guaranteed to be loaded, `require` it once in a
  cold command/setup entry point, or at top level when it is unconditional
  and acyclic. Trace transitive loads through aggregators: an autoload gains
  nothing if the package entry point already loads its implementation.
- **Defer optional runtime work with its feature**. Start processes,
  connections, watchers, scans, and expensive resource hydration when their
  owning feature needs them. Keep discovery metadata available when needed
  without unnecessarily initializing execution. Deferral must preserve
  permission checks, recovery, visible failures, and teardown.
- **Never call `require` on a hot path**. Code reached per segment, chunk,
  redraw tick, or guest step must rely on an earlier load boundary. Avoid
  circular dependencies through module direction rather than scattering
  lazy `require`s through helpers.
- **Verify cold first use and the tradeoff**. Exercise changed boundaries
  through caller entry points in a fresh isolated Emacs, including code
  byte-compiled in one process as package managers do; preloaded fixtures can
  hide missing dependencies. `test/test-mevedel-loading.el` runs installation,
  first composer send, session resume, directive commands and M-x
  reachability this way. Check that optional
  features remain unloaded or inactive until needed. Measure package load,
  installation, first use, and steady-state work separately before claiming
  performance gains. Moving a pause to the first interaction is not by itself
  an improvement.

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
