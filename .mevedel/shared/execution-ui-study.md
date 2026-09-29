# Background execution UI study — 2026-09-27

Source/task: user discussion of duplicate Bash, WriteStdin, and completion
delivery rows. User prefers a work-oriented default and requested investigation
plus multiple standalone Elisp previews, not production changes.

## Observations

- `mevedel-tool-exec.el:758` owns Bash/WriteStdin rendering. It already hides
  successful running polls without output and coalesces some successful polls.
  Poll results with output remain their own expandable rows.
- `mevedel-execution-transcript.el:101` stores whole terminal output in durable
  render data. `mevedel-view-render.el:2625` uses it for the original Bash row.
  Thus a poll's returned output can also appear in the original command.
- `mevedel-view-render.el:4474` independently renders EXECUTION mailbox
  deliveries as compact Bash completion cards. The `/root` header attribution
  is an agent path, **not** the command's working directory.
- Git commit `4d925cdf` introduced `Polled background process` and
  `Interacted with background process`. Commit `31f92cd2` replaced those with
  `WriteStdin: polled background process` and `WriteStdin: sent input to
  background process`. This confirms the user's recollection.
- Consulted the existing gptel checkout's `gptel--display-tool-results`:
  authoritative tool results remain separate from the human-facing projection.
  Attempted upstream refresh failed with read-only `.git/FETCH_HEAD`, including
  an additive checkout write grant. No upstream files were changed by this work.

## Discussion direction, not an implemented contract

One execution owns its output and state. Routine polling should not create
primary rows. Actual input/stopping, failures, and consequential constraints
must remain visible. The role and persistence of late-completion notifications
remain open for visual comparison. Compaction/archive fallback, missing original
rows, and child-agent completions will need explicit treatment before production
implementation; blindly hiding all completion messages is not sufficient.

## Standalone preview

Load `work://shared/execution-ui-preview.el`, then run
`M-x mevedel-execution-preview`. The buffer `*Execution UI variants*` has clickable
scenario/layout controls and independent Output, Execution details, and Execution
history disclosures. Fixtures are fictional and labeled as such. No processes,
timers, advice, themes, or session buffers are modified.

- A: minimal command row, collapsed output.
- B: two-line execution card, with one output-line preview while collapsed.
- C: minimal row plus a later completion breadcrumb linked to its result.

Running/success/failure/input scenarios share observations across all variants.
This is a layout and interaction study, not an async redraw or performance test.
The existing user changes to production files were left untouched.

Validation: ran `npx @emacs-eask/cli clean elc` followed by
`npx @emacs-eask/cli test ert .mevedel/shared/execution-ui-preview-tests.el`.
Both ERT tests passed, covering the 12 layout/scenario combinations, disclosures,
single full-output ownership, scenario/layout buttons, result navigation, and
the preview entry command. Opened the preview in live Emacs and confirmed its
buffer and read-only mode. Visual preference and actual async integration remain
for subsequent review; these tests establish only the standalone demo behavior.

## Subsequent production implementation (2026-09-28)

Source/task: accepted `work://plans/accepted-handoff.md`, implemented in the
`accepted-plan` worktree. The discussion direction above is now embodied in
`mevedel-tool-exec.el`, `mevedel-execution-transcript.el`,
`mevedel-view-audit.el`, and `mevedel-session-artifacts.el`; the standalone
preview remains separate. Archived display buffers lack a session, so
receiver-local breadcrumb deduplication uses the owning live session and only
earlier archives. Child compaction archives may have gaps in their numbering.

Observed pre-commit validation: post-fix focused roster
**1346/1346**, full suite with two isolated Eask workers **8751 tests,
0 unexpected, 30 skipped**, and **208 files compiled without warnings**.
Two earlier eight-worker full-suite attempts had unrelated timing/environment
failures (scheduler process-start, journal cleanup ownership, remote-hook stdin
readiness); the slower two-worker run completed. A headless real-process Emacs
smoke passed foreground/background commands, polling, PTY input, failed exit,
result access, and draft/reader preservation. Final read-only verification
also reproduced correct S1/S2/S3 archive deduplication and parent/child
receiver independence. Detailed current behavior belongs to `docs/tools/execution.md`
and `docs/view.md`, not this preliminary study.

Follow-up Goal verification found two gaps after that commit: guest results
became stale when completion lived in a later archive, and reloaded nested
ToolCall Bash rows kept a successful status marker after their process was
marked lost. The follow-up fixes were checked through archived guest history
and native rendered rows, including another compaction and unreadable
intermediate evidence. Current post-fix validation: the accepted focused roster
**1283/1283**, the complete suite **8760 tests, 0 unexpected, 30 skipped**
with two isolated workers, **208 files compiled without warnings**, and the
headless real-process Emacs smoke passed. The browser was not manually exercised.

Further accepted-Goal follow-up (2026-09-28, `accepted-plan` worktree): guest
result links now require trusted local breadcrumb or forwarded mailbox evidence,
with terminal row/audit lookup for root and child, including nested ToolCall
when an output artifact is unavailable. Guest breadcrumbs use the earliest
readable receiving segment; missing archives do not suppress later evidence.
The archive identity lookup is lazy and uses a source-signature-checked read
memo, never result-fetch authority. Independent source-loaded probes covered
forged identities, cold child fallback, cross-segment retries, and bounded
responses. Final current-source validation: focused Eask **1351/1351**, full
suite **8774 tests, 0 unexpected, 30 skipped**, **208 files compiled without
warnings**, viewer protocol, relay build/vet/Go tests, and real-process batch
Emacs smoke passed. Graphical validation remains unproven: a separate GUI Emacs
daemon/client stalled and a direct `:0` startup timed out before evaluation;
the batch smoke establishes rendered-buffer and focus/draft assertions, not
pixel-level behavior. Current contracts are in maintained `docs/` files.

Final acceptance follow-up: with the new cross-module declarations, the
focused roster passed **1351/1351**. After declaration-order cleanup, the
affected roster passed **290/290**, and the final source passed the complete
**8774-test suite with zero unexpected and
30 skips** (`/tmp/mevedel-goal-wrap-current-full.log`), and warning-free Eask
compilation of **208 files**. The viewer protocol passed. A separate terminal
Emacs frame exercised actual foreground success/failure, yielded PTY input
ending with exit 3, completion while another buffer and a multiline draft were
present, and RET on its breadcrumb expanded the original command's retained
output/history (`.scratch/plan-tty-acceptance.el`,
`/tmp/mevedel-goal-tty-acceptance.log`). This is terminal-frame acceptance,
not a claim of graphical or browser visual inspection. An intermediate
eight-worker suite had an unrelated hook-cancellation timing failure, and an
attempted two-worker rerun was stopped under competing checkout load; neither
is counted as passing. The final clean eight-worker run supersedes them.

The completion verifier then found that an available retained empty output
string was mistaken for missing evidence. The fallback now reports an explicit
no-output result for that string or a readable empty artifact; absent evidence
still reports unavailable. Final-source validation: focused roster **1290/1290**,
complete suite **8776 tests, 0 unexpected, 30 skipped** (all eight workers
exited zero; `/tmp/mevedel-empty-final-retry.log`), **208 files compiled without
warnings**, viewer protocol passed. Two preceding full-suite attempts each had
one timing-sensitive failure outside the changed path (remote stop threshold,
then hook cancellation); their isolated rosters passed **16/16** with 14
environment-dependent skips and **91/91**, respectively. The final clean
eight-worker run supersedes those attempts.

The next completion verifier reproduced two folded-turn defects: Show result
fell back despite a canonical row in the folded stash, and a completion retry
inserted a second visible breadcrumb. Source-backed lookup now opens the turn
before the nested row, and deduplication checks the retained propertized stash.
Two ERT regressions reproduced both defects before the fix and passed after it.
Final-source focused roster **1292/1292**, full suite **8778 tests, 0 unexpected,
30 skipped** (`/tmp/mevedel-fold-final-full.log`), **208 files compiled without
warnings**, viewer protocol, and the terminal-frame real-process acceptance
fixture passed. Browser pixels and graphical-frame presentation were not
visually inspected.

2026-09-28, accepted-plan final verifier follow-up: read-only Goal verification
found that a **direct ToolCall Bash child** lost terminal sandbox disclosures
and the expanded execution-history link. Two ERT regressions reproduced the
failures before the fix. `mevedel-view-render.el` now carries the direct
child's latest sandbox summary and execution ID on the outer rendering, while
keeping its outer tool-use/source ID. Archive-retry/history tests were converted
to real temporary segments after code review found mocked archive reads;
the remaining source-navigation/forwarded-delivery archive fixtures were
likewise converted. Committed as `49b56188` on `worktree/accepted-plan`.
Final-source evidence: `/tmp/mevedel-direct-final-focused3.log` **1294/1294**;
`/tmp/mevedel-direct-final-full3.log` and its structured report under
`.scratch/test-suite-performance/20260928-130231/` **8780 tests, 0 unexpected,
30 skipped, eight workers exited zero**; `/tmp/mevedel-direct-final-compile3.log`
**208 files compiled without warnings**; viewer protocol and real-process
terminal-frame acceptance fixture passed. An earlier concurrent full run had
one failure in unchanged remote-hook stdin-readiness code and was superseded
by the passing full run. Graphical/browser pixels remain uninspected.

2026-09-28, additional Goal verification against `49b56188` found two
presentation gaps: noteworthy sandbox disclosures disappeared from guest
projection, and the Bash renderer removed stdout lines resembling a model-facing
`<bash-execution .../>` trailer. The fixes now include a bounded, formatted
guest header disclosure and an explicit tool-envelope provenance bit, so
canonical stdout (including marker-shaped text) is displayed verbatim. The
new tests first reproduced the faults. Committed as `07e3bfdc` on
`worktree/accepted-plan`; the source passed the
accepted focused roster plus guest tests (1332/1332), the complete ERT suite
(8784 cases, 0 unexpected, 30 skipped), byte compilation (208 files, no
warnings), the browser protocol check, and real terminal-frame interaction
(including RET on Show result). Standards and Spec read-only reviews found no
new concrete gap. Graphical/browser pixels remain uninspected.

2026-09-28, pending-row verification found that an in-flight empty-input poll
still showed a transient `Calling WriteStdin` (or single-poll `ToolCall`) row.
The view now omits that pending row and pre-tool spinner only for confidently
classified routine polls; invalid/ambiguous calls retain their pending row,
and settled control errors remain visible. Red-first regressions covered
invalid direct and outer ToolCall arguments; the final fix is `cc34b712` on
`worktree/accepted-plan`. Final-source focused roster: **1308/1308**
(`/tmp/mevedel-pending-final6-focused.log`); full ERT **8796 cases, zero
unexpected, 30 skips** (`/tmp/mevedel-pending-final6-full.log`); **208 files
compiled without warnings** (`/tmp/mevedel-pending-final6-compile.log`). A
real-process smoke and terminal-frame RET-on-result check passed. Independent
Spec review found no further pending-poll gap; an independent Goal contract
audit returned PASS. Browser pixels and graphical-frame presentation remain
uninspected; prior browser protocol evidence predates this final view fix.
