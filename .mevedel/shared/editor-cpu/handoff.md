# Handoff: editor CPU and wakeups — independent review and investigation

## Situation

The user's laptop fans spun up during mevedel requests. Root cause: on their
Emacs 31.1 pgtk build (KDE Wayland, 2x scale, ~1536x888 logical frame) every
wakeup — a timer firing, process output — ends in a redisplay that presents the
**whole frame surface**, even when nothing changed. Rule of thumb on that frame:
**~2% editor CPU + ~1% compositor per wakeup per second.** A do-nothing 10 Hz
timer costs 23-36% in `emacs -Q` at that size and 3% in a 400x300 frame.

Fix commit: `339c8702` on branch `fix/cpu-wakeups`
(worktree `.worktrees/cpu-wakeups`), based on `3df91e1d`. Master has moved
(`abb09d64`, another agent's Claude Code work); merging will likely conflict in
`README.md`. Full numbers and the fix list: `../editor-cpu-investigation.md`.
Remaining known costs: `docs/backlog.md`, "Editor CPU".

Measured in the user's live Emacs (frame visible, 10 s windows):

| scenario                                  | before        | after         |
|-------------------------------------------|---------------|---------------|
| shimmer label (60 fps / cadenced 30)      | 70% / 34% kwin | 29% / 15%    |
| running Bash, all indicators static       | 42% / 11%     | 21% / 10%     |
| default config, watched, Bash running     | 73% / 21%     | 39% / 18%     |
| breathe/bounce @30 (unchanged design)     | —             | 64% / 30%     |
| braille/ascii label (unchanged design)    | 26%           | ~30%          |

## Your tasks

Work independently; do not take the previous agent's conclusions on trust.

### A. Review commit 339c8702 for correctness

Read `docs/development.md` first. Highest-risk areas:

1. **Spinner scheduler** (`mevedel-view-stream.el`,
   `mevedel-view--start-spinner-timer`, `--spinner-next-delay`): the timer is
   reused when the *plan* (styles + periods) is `equal`; the callback computes
   each delay from the plan. Check the plan always changes when cadence must
   change (color fallback, power transitions, tool rows appearing/leaving).
2. **Arming after redisplay** (`--probe-spinner-visibility`,
   `--recheck-spinner-after-redisplay`): a new span is probed once per target
   set; scroll hooks defer to a 0.2 s recheck. Check no path loops and that a
   genuinely off-screen span stays quiet.
3. **Cadenced shimmer** (`mevedel-view-animation.el`): 61-frame sweep bank
   (index 0 = rest), `--sweep-phase`, `--next-delay`; interaction with frozen
   (0 fps) motion, theme invalidation, multi-frame `:multiple` fallback,
   `mevedel-view--animation-display-seconds`.
4. **Shimmer tool rows** (`mevedel-view--pending-tool-line-body`): the span is
   the label up to ":" ("Calling Bash"); check snapshot/restore of tool phases,
   live-tail line matching (`mevedel-view-render.el` ~7440), overflow rows.
5. **Unattended now includes undisplayed views** in an interactive Emacs
   (`mevedel-view--unattended-p`). Every caller defers work until a resume
   hook. Check resumption through every way a view can reappear (other frame,
   tab-bar, persp-mode, `display-buffer` in a fresh window) and that nothing
   correct-by-construction depended on rendering a hidden view.
6. **Input pause** holds indicators still; the interaction sync rearms.
7. **Telemetry lateness** (`mevedel-telemetry--lag-time-callback`,
   `--lag-tick`): delay = max(own, timer lateness since previous tick); summary
   counts are now one per 0.5 s heartbeat window.
8. **Bash progress** (`mevedel-execution--hasten-progress`): quiet 1 s, 0.25 s
   while output flows; watch interval 1 s; elapsed shown in whole seconds.
9. **Collection pacing** (`mevedel-session-collection--arm`, `--retry`): slices
   1 s apart, retries 1→8 s. Pitfall found here: `setf` on `plist-get` for an
   *absent* key conses a new list instead of mutating, breaking `eq` identity
   checks — keys must be present in the initial plist.
10. **Decorative content tick** (`mevedel-view--content-tick`,
    `--put-decorative-display`) used by the header-line cache; the markdown
    realign precheck and image-layout memo; control-transfer polling skipped
    for non-portable sessions.

### B. Verify the measurements independently

`tools/measure.sh OUT-DIR` drives the whole matrix in the live Emacs and cleans
up after itself (`QUICK=1` for a smoke test). It is the previous agent's tool:
read it before trusting it. Also confirm on the branch that a new request's
spinner starts **without** the simulated focus event (`REARM=0`), and that on
`3df91e1d` it stays frozen.

### C. Open investigations

1. **Continuous styles** (breathe, bounce, braille, ascii, dots, ellipsis):
   the user wants cheaper variants with the cadenced shimmer as the reference.
   They rejected pausing glyph/breathe styles for three seconds between bursts
   ("looks stupid"). `tools/spinner-demo.el` and `tools/shimmer-demo.el` show
   variants side by side in the user's theme; `tools/tool-demo.el` shows
   synchronized tool rows. Possible directions: lower natural cadences, fewer
   distinct glyph states, color-cadenced equivalents, removal (no backwards
   compatibility is required; see CLAUDE.md).
2. `mevedel--gc-maintain` repeats every second during requests and a 30 s
   grace: it re-applies the GC threshold after idle tuning (gcmh lowers it
   after its own collection, so `post-gc-hook` alone cannot replace it).
3. `mevedel-view--realign-markdown` still wakes ~1/s while a Bash row
   refreshes (each pass now returns early).
4. `mevedel-view--status-strip` still builds its cache key from several live
   lookups on every redisplay (tab-line `:eval`).
5. `acp.el` (upstream dependency) drains each output chunk with a zero-delay
   timer, ~30/s from the Claude Code adapter.
6. Upstream Emacs: the pgtk whole-surface present per wakeup. A minimal repro
   is `emacs -Q` with `(run-at-time 0.1 0.1 #'ignore)` at two frame sizes.

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
- `tools/measure.sh` — scenario matrix; `tools/mock_server.py` — SSE mock that
  holds a request or issues a `Bash` tool call.
- `tools/cpuh-prof.el` — bounded CPU+memory profile summaries.
- `tools/sampler.py` — per-second CPU of a process tree and the compositor.
- `tools/*-demo.el` — visual comparisons; `q` closes and cancels their timers.
