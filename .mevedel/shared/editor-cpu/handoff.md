# Handoff: editor CPU and wakeups

## Current state

The user's laptop fans spun up during mevedel requests. On their Emacs 31.1
pgtk build (KDE Wayland, 2x scale, ~1536x888 logical frame) every Lisp/process
wakeup ends in a redisplay that presents the **whole frame surface**, even when
nothing changed: **~2% editor CPU + ~1% compositor per wakeup per second.** A
do-nothing 10 Hz timer costs 23-36% in `emacs -Q` at that size and 3% in a
400x300 frame.

Work lives on branch `review/cpu-wakeups` (worktree
`.worktrees/cpu-wakeups-review`), based on `3df91e1d`; master has moved since
(`abb09d64` and later), and integrating will likely conflict in `README.md`.
`fix/cpu-wakeups` (`.worktrees/cpu-wakeups`) holds commits 1-4 and the
original form of 6. Nothing is merged or pushed. Commits, in order:

1. `339c8702` wakeup reductions: telemetry heartbeat, cadenced shimmer, tool-row
   shimmer, spinner arming after redisplay, input-pause freeze, Bash progress
   and watch, collection pacing, unattended render timers, header caches.
2. `0f6585ec` this handoff and the measurement tools.
3. `a9f58a80` independent follow-up: per-tool banks, freeze consistency,
   accepted 8-fps breathe/bounce and half-speed glyphs, reversed contrast.
4. `9b6a6f5e` native presenter: `mevedel-view-native.el` and
   `native/mevedel-view-native.c` draw animation samples on Wayland
   subsurfaces without Emacs redisplay; ordinary text animation is the
   fallback everywhere else.
5. The review commit (see "Review, 2026-10-08" below).
6. Remaining request wakeups (`219b4974` on `fix/cpu-wakeups`, replayed onto
   the review commit): GC maintenance, the process watchdog, the telemetry
   heartbeat, quiet progress and metadata-only view updates share one host
   timer through `mevedel--ui-timer-activate`'s coalesced queue on integral
   clock ticks; current Markdown layout schedules no idle pass; tool-row
   recovery waits until its source exists. Evidence and protocol:
   [the wakeup lab](wakeup-lab/README.md).
7. Shared-timer review fixes: the TRAMP retained-timers wrapper no longer
   holds the shared host timer; untimed timers are rejected; uninstall
   cancels the queue; heartbeat due time and long progress intervals.
8. Streamed-read pacing: the stream bridge stops local curl between 0.4 s
   batches and the view renders each flushed batch in the same wakeup; the
   native presenter checks placement only when layout inputs change, moves
   displaced surfaces, reuses its presentation and timelines.
9. Claude Code pacing: the ACP adapter pauses after text-only reads; MCP
   calls and every message to it continue it; shared pause primitives in
   `mevedel-transport.el`. Before/after table in `renderer-lab/README.md`.

Product behavior is documented in `docs/view.md`, `docs/tools.md`,
`docs/telemetry.md`, `docs/sessions.md` and ADR 0119; open work is in
`docs/backlog.md` under "Editor CPU". Evidence: `../editor-cpu-investigation.md`
(live-editor measurements) and `renderer-lab/README.md` (native presenter
experiments, disposable editors).

Key numbers (single machine, directional):

| scenario | before branch | now |
|---|---:|---:|
| shimmer label, live editor | 70% | 26-29% |
| default config, watched, Bash running, live editor | 73% | 34-39% |
| breathe/bounce, live editor (ordinary renderer) | 64% | 26% |
| 30-fps native bounce, isolated disposable editor | 55% ordinary | ~2% |
| silent request, disposable editor: static / native bounce | 8% / 12% (before commit 6) | 6% / 8% |
| sleeping Bash, native 30-fps status, disposable editor (commit 6) | 18-19% | 8.3-9.9% |
| prose streaming 8 words/s: static / native bounce | 36% / 42% (before commit 8) | 16% / 21% |
| prose streaming 32 words/s: static / native bounce | 59% / — (before commit 8) | 17% / 22% |
| sleeping Bash / silent request after commit 8: native | — | 8.0-8.4% / 7.4% |

Streaming remains the largest request cost; what is left is mostly the
per-wakeup pgtk presentation of the 0.4-second stream cycle. The renderer
lab's `request-run.py --stream-rate N` verifies that every streamed word
arrives, in order.

## Open work

All open items are concise entries in `docs/backlog.md`, "Editor CPU": the
streaming render cost, GC maintenance, markdown realign, status strip, ACP
chunk timers, the upstream pgtk report, non-PGTK presentation costs, and the
review's unfixed lower-severity findings (frozen mid-sweep shimmer band,
native background with remapped faces, C display globals, natively closed
surfaces, header memo key, hidden tool-row refreshes, collection idle
threshold, executions-list sorting).

## Measuring

- Live editor (the user's): `tools/measure.sh OUT-DIR` (`QUICK=1` smoke test,
  `REARM=0` to check startup without a focus event) with
  `tools/cpuh-harness.el`. Ask first; see the safety rules.
- Disposable editor (preferred for experiments): from the worktree root,
  after `npx @emacs-eask/cli compile`,
  `python3 .mevedel/shared/editor-cpu/renderer-lab/request-run.py --output
  .scratch/renderer-lab/NAME --modes static ordinary native --seconds 8`.
  `--stream-rate N` streams prose at N words/s instead of a silent hold;
  `--observe S` records, for S seconds after the CPU sample, which renderer
  owns the label and how often surfaces open, close and invalidate (the
  sampler is a 10 Hz wakeup, so it runs after the CPU sample). The window
  needs focus throughout; a run aborts if it loses focus. Clean bytecode
  afterwards (`npx @emacs-eask/cli clean elc`) before running tests.

## Safety rules learned the hard way

- **Never let `emacsclient --eval` return or print a large object.** It prints
  without bounds; a profiler dump and, separately, a returned request struct
  each froze the user's Emacs (one OOM-killed it at 26 GB). Wrap forms so they
  return `t`, a number or a short string.
- Profiler logs: call `profiler-stop` first, then read
  `profiler-cpu-profile`; aggregate by frame *names* (`tools/cpuh-prof.el`).
- A locked screen or a covered/other-desktop frame skips the repaint: numbers
  drop to a fraction. Check compositor CPU (kwin ~2% means nothing is drawn)
  and `loginctl show-session … -p LockedHint`.
- Kill demo buffers before measuring; their timers contaminate every scenario.
- Animation only runs in a focused frame; `cpuh-attend` bypasses that gate,
  but the frame must be on screen.
- The user is present and uses this Emacs: ask before measuring, never while
  they type, and restore settings (`cpuh-unload`).
- Do not edit the main checkout: another agent works there and evaluates its
  work in progress into the same Emacs. Use a worktree.
- To test branch code in the live Emacs, byte-compile a copy and `load` the
  changed `.elc` files; reloading does not change `defcustom` values, so set
  new defaults explicitly.
- `cl-letf` cannot stub struct accessors (inlined); set the slot instead.
- `mevedel-deftest` with an existing name silently replaces those cases.
- `pgrep -f PATTERN` from a shell whose own command line contains PATTERN
  kills/matches that shell.
- The user's presets (`mevedel-gpt6`, `mevedel-claude55`) exist only in their
  running Emacs. Prefer the mock server for measurements; real requests use
  their Codex OAuth / Claude Code subscription.

## Files

- `tools/cpuh-harness.el` — counters, session driver, focus bypass, cleanup.
- `tools/measure.sh` — live-editor scenario matrix; `tools/mock_server.py` —
  SSE mock that holds a request, streams prose, or issues `Bash` tool calls.
- `tools/cpuh-prof.el` — bounded CPU+memory profile summaries.
- `tools/sampler.py` — per-second CPU of a process tree and the compositor.
- `tools/*-demo.el` — visual comparisons; `q` closes and cancels their timers.
- `renderer-lab/` — native presenter probes, the disposable request runner
  (`request-run.py`, `request.el`) and retained results.

## Review, 2026-10-08

Four independent reviews (native module, scheduler, non-view changes, docs)
covered `3df91e1d..9b6a6f5e`. Fixed, each with a test, on `review/cpu-wakeups`:

- **Streaming froze the label (pre-existing, both renderers).** Text streamed
  in at the label's start marker was absorbed into the span, whose start then
  lacked the label property, so the label read as hidden in about half of the samples
  and the native presenter tore its surface down after insertions. Start
  markers now advance; the label animated in every streaming sample and the
  presenter opened 2 surfaces in 6 s.
- **Native surface leak.** A label with XML-invalid characters (control
  characters from tool arguments) signalled mid-sync after opening other
  surfaces, which stayed alive with their timers. Escaping now drops such
  characters (the width check then declines) and any mid-sync error closes
  the surfaces opened so far.
- **Native teardown in an unredisplayed window.** Releasing surfaces waits
  for the parent redisplay, which runs pre-redisplay hooks only for windows
  being redisplayed; the view's windows are now marked for redisplay.
- **Native build robustness.** Runtime builds no longer use `-Werror`, find
  `emacs-module.h` beside the running Emacs, and resolve the source through
  `mevedel-library-source-directory`.
- **Hidden view held the GC threshold.** A batched history render parked on
  an unattended view kept its collection hold and the 1 Hz maintenance timer
  indefinitely; parking now releases it and resuming takes it again.
- **Resize did not rearm.** A span revealed by a resize stayed frozen;
  `window-size-change-functions` now rechecks after redisplay.
- **Timer churn.** Every semantic tick replaced the spinner timer and its
  closure; the delivered timer is now kept when the plan is unchanged.
- **Duplicate Bash progress under TRAMP.** Output read inside a section that
  suspends timers armed a second self-rearming progress chain; hastening now
  leaves a suspended event alone. A progress interval above one second is
  honoured.
- **Telemetry heartbeat outlived disabling telemetry**; it now stops at its
  next tick. A missing autoload in control-transfer polling was added.
- Docs and ADR 0119 corrections (band width, input-pause freeze in the
  current decision, unattended definition, 30/15 fps wording).

## Earlier history

The earlier task list for the first independent review (review areas,
measurement verification, continuous-style investigation) is complete; see
the investigation's "Independent review and follow-up" section and
`renderer-lab/README.md`. The user accepted 8-fps breathe/bounce and
half-speed glyphs (third speed looked laggy), rejected burst-and-pause glyph
variants, approved the reversed contrast (a faded band over normal text), and
confirmed that the native presenter's refocus flash is gone.
