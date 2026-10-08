# Handoff: editor CPU and wakeups — independent review and investigation

## Current independent review entry point

Review the native renderer change on `fix/cpu-wakeups`, in
`.worktrees/cpu-wakeups`, against `a9f58a80`:

```sh
git diff a9f58a80..HEAD
```

That base already contains the earlier scheduler/contrast fixes. The historical
tasks below explain their context; the new change is the native presenter and
its integration. Start with the [renderer report](renderer-lab/README.md) for
reproduction commands, measured comparisons, source hashes and test scope.

Review priorities:

1. C ownership and Wayland ordering: parent lookup, synchronized initial/moved
   presentation, callback cancellation, buffer releases and partial failures.
2. Lisp presentation ownership: coalescing at redisplay, invalidation, marker
   replacement, multiple windows, focus/occlusion, freeze phases and teardown.
3. Feature-boundary build/load, ordinary fallback, packaging and installation.
4. Measurement validity: distinguish isolated animation from total request CPU;
   synthetic concurrent tool events from actual sleeping Bash; earlier results
   from final results. Do not infer per-timer CPU attribution from counts alone.

The full 9,776-case suite predates two final Lisp changes (pre-command hook
removal and unload cleanup); the final 237-case focused suite covers both.
The later C positioning fix has a failing-before/passing-after protocol check,
30 graphical lifecycle checks, a passing CPU/delivery gate, and user-confirmed
absence of the refocus flicker. Eask compilation and C compilation are clean.
No performance claim is made for non-Wayland platforms, and the remaining
non-animation request wakeups are a separate follow-up in `docs/backlog.md`.

## Situation

The user's laptop fans spun up during mevedel requests. Root cause: on their
Emacs 31.1 pgtk build (KDE Wayland, 2x scale, ~1536x888 logical frame) every
Lisp/process wakeup — a timer firing, process output — ends in a redisplay that presents the
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

## Follow-up review completed (2026-10-08)

The independent follow-up is recorded in
[the investigation](../editor-cpu-investigation.md#independent-review-and-follow-up-2026-10-08).
It reproduces the startup fix and frame-size effect; fixes stale overflow text,
incomplete/evicted tool banks, and phase advancement while awaiting input; and
implements the user's visually accepted 8-fps breathe/bounce and half-speed
glyphs. See the investigation for measurements, test results and limitations.

[Canvas/native-module research](canvas-research.md) traces the Emacs 32 API and
its PGTK presentation path. At that earlier checkpoint Canvas had not been benchmarked and no native module
or custom Emacs build had been added; the investigation below supersedes it. Remaining presentation, GC,
markdown, status-strip and ACP costs remain in the backlog.


The user subsequently approved reversing animation contrast: shimmer and bounce
now fade a moving band over normal foreground text, and breathe fades from
normal foreground and back. Glyph styles retain their normal text. This visual
follow-up preserves the measured cadences; see the investigation's visual
contrast follow-up for final validation.


## New investigation goal: unrestricted approaches (2026-10-08)

Find and validate a substantially lower-CPU way to display smooth mevedel
status and tool animations. Explore any promising approach: Canvas, native
modules, and Emacs rendering patches are examples, not constraints or a
required sequence. The user authorizes measurements, tests, and experiments
and prefers a solution implemented entirely on mevedel's side. Preserve the
accepted visual behavior when assessing candidates. Implement and validate a
justified solution if found; otherwise retain reproducible experiments,
measurements, and a clear account of the remaining limitation.

The user recreated and activated this unrestricted goal after clarifying its
scope. The earlier pause is superseded.


## Broader renderer investigation: implemented and validated

The [renderer lab](renderer-lab/README.md) retains the reproducible experiments,
measurements, source hashes, screenshots and acceptance results. A mevedel-side
native module now presents the existing animation samples on independent Wayland
surfaces, without modifying Emacs. It builds automatically on first supported
animation use. Ordinary text animation remains the fallback.

At 30 fps, isolated native bounce uses about 1.5–2.6% editor CPU, versus 55–56%
for ordinary 30-Hz text/no-op Lisp wakeups. Canvas on an isolated Emacs 32 build
still used about 55%. The final repeated real sleeping-Bash + main status test
used 22–23% total editor CPU with native 30-fps bounce, versus 38.7% for ordinary
8-fps bounce. Warm static baseline was 18.7%. Semantic updates no longer churn
native surfaces: they coalesce at parent redisplay and retain unchanged text and
geometry. Remaining non-animation CPU costs are separate backlog items.

The accepted normal-text contrast, faded moving band, continuous color motion,
glyph cadence, freeze policy and phase semantics are preserved. Native main
color motion can use the configured 30 fps again. Construction failures,
unsupported displays and unsuitable geometry keep the ordinary renderer.

Validation includes all seven exact-sample styles; full request/tool lifecycle;
seven concurrent presentation events and changing overflow counts; theme/font,
splits, clipping, cursor/selection, child-frame and focus changes; composer draft
preservation; allocation failures and 50 actual create/close cycles with stable
file descriptors. Full Eask suite: 9,776 cases, zero unexpected, 31 conditional
skips. Final focused suite after local cleanup changes: 237 passed. Package
compilation: 242 files, no warnings. Details and exact scope are in the lab report.

Implementation: `mevedel-view-native.el`, `native/mevedel-view-native.c`, with
sample preparation and stream scheduling changes. README, view/module docs,
ADR 0119 and backlog reflect the implemented path. Native requires Linux
PGTK/Wayland, Emacs modules, cc/pkg-config and GTK 3/Wayland development headers.
The results do not claim the same acceleration on other platforms.

All graphical native checks ran in disposable editors. The user's live editor
was not loaded with the native module. The native renderer and evidence are
committed together on `fix/cpu-wakeups` for independent review.

The user then spotted a top-left flash on refocusing the standalone preview.
It was a native positioning race, not steady-state animation: a child image
could commit before its parent's pending position. The C presenter now stays
synchronized through the parent frame callback at creation/movement, then
resumes independent animation. The protocol reproduction fails before the fix
(17 premature commits), passes afterward, and the user confirmed the flicker
is gone. See `renderer-lab/results/positioning.json` and `check-positioning.py`.
