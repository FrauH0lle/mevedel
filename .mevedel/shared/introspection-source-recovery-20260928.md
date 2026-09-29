# C-source introspection stall and recovery

User reported that the spinner Goal stalled on the Emacs C-source directory
prompt, and authorized fixing source lookup and aborting/resuming that request.

## Observations

- Session: `2026-09-27T16-24-5ca69305960c`. Root request
  `request-20260928T003432-79b9de8daf0c` remained in `TOOL` with no result for
  `(mevedel-introspection/function_source :function "scroll-left")`.
- Live `source-directory` was the absent build path
  `/build/emacs/src/emacs-31.1-wayland/`; `find-function-C-source-directory`
  was nil. No minibuffer remained active when inspected.
- Installed Emacs 31.1 `find-function-C-source` calls `read-directory-name`
  when that variable is nil, even from a noninteractive tool handler. The
  regression reproduced this prompt. The exact earlier prompt-dismissal stack
  was not captured.

## Fix and verification

`mevedel-tool-introspect--source` uses native alias/library resolution and source
search but checks the C-source configuration before entering the prompting
path. Missing configuration becomes an ordinary tool error. Configured C source
and Lisp lookup remain supported. Tests cover functions and variables through
the gptel tool interface, including exactly one error delivery and real temporary
C-source fixtures. `docs/tools.md` documents the boundary.

- Before fix: 11/12 tests passed; the new no-prompt regression failed as expected.
- After fix: 399/399 related introspection, registry, pipeline, and PTC tests
  passed with no product diagnostics.
- Eask compilation: 210 files, no warnings/errors; Eask bytecode cleanup followed.
- `git diff --check` passed. Subsequently committed at the user's request as
  `1df976e9` (source, tests, and documentation only).

## Live recovery

Hot-loaded only the changed lookup function. Live checks for `scroll-left` and
`fill-column` returned the expected missing-source error. Aborted the exact
stranded request through `mevedel-abort`, waited for both request and settlement
slots to clear, then called `mevedel-goal-resume` with an explanation and the
instruction to preserve these unrelated recovery edits. No manual tool-result
injection, FSM replay, C-source installation, or existing journal repair.

Follow-up confirmed new request `request-20260928T070414-7ae9114e7f58` completed
Bash, Read, and Grep calls and resumed the horizontal-scroll rearm investigation.
The Goal was active with no pending settlement; it acknowledged preserving the
recovery edits.

CPU profiling stayed active. One recovery inspection accidentally returned the
full pending-settlement object instead of a boolean, producing a very large
tool artifact. This is diagnostic overhead, not spinner workload; subsequent
status checks return bounded booleans. Treat recovery work as a contaminated
interval if comparing performance from the long-running CPU capture.

## Capture stopped at user request

On 2026-09-28 at approximately 09:35 +0200, the user explicitly requested
stopping the capture without waiting for Goal completion. Called the normal
`mevedel-telemetry-profiler-stop`. CPU and memory profiling both reported off,
the profiler owner was cleared, and the Goal remained active. Ordinary session
telemetry is not disabled by stopping the profiler.

Saved artifacts under the affected session's
`diagnostics/run-20260927T181246-1b68ad79/`:

- `profiler-cpu-profile.el`: 6,513,256 bytes.
- `profiler-cpu-report.txt`: 3,015 bytes.

This CPU-only run began on 2026-09-27 at 18:12:46, roughly 15 hours 22 minutes
before stopping. No profiling restart or Goal cancellation was requested or
performed. The saved capture still requires analysis; elapsed capture time is
not equivalent to active spinner work.
