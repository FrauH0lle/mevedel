# Renderer experiments: low-CPU animation

Working investigation, 2026-10-08. Product base: `a9f58a80` on
`fix/cpu-wakeups`. Diagnostic probes stay in this directory; the integrated
candidate is `mevedel-view-native.el` and `native/mevedel-view-native.c`.
The implementation and measured results below cover the unrestricted investigation.

## Measured result

A mevedel-side C module can animate a separate Wayland subsurface without
patching or rebuilding the installed Emacs. The prototype renders a small
Pango/Cairo label into three shared-memory buffers, updates it with a native
GLib timer, and commits only that subsurface. Its empty input region lets
pointer events reach the underlying editor. `wl_subsurface.set_desync` allows
those commits independently of the parent frame.

At roughly 30 updates/second on the same large visible window:

| Path | Editor CPU | Compositor CPU | Evidence |
|---|---:|---:|---|
| Idle Emacs 31 | 0.4% | 4.4–5.4% | `results/baseline.json` |
| Lisp no-op timer | 55.0–56.2% | 31.4–32.4% | same |
| Prepared mevedel bounce text | 55.4–56.4% | 20.0–30.8% | same |
| Diagnostic GTK damage clipped to 600x150 | 48.6% | 16.2% | `results/clip.json` |
| Native GTK timer doing no drawing | 1.2–1.4% | 4.4–5.2% | `results/native-surface.json` |
| Small GTK drawing widget | 49.8% | 16.2% | `results/native-timer.json` |
| Native Wayland subsurface | 2.0–2.2% | 10.0–10.4% | `results/native-surface.json` |
| Emacs 32 prepared text | 55.6% | 31.0% | `results/canvas32.json` |
| Emacs 32 Canvas with native drawing | 55.4% | 31.4% | same |

An independent repeated eight-second check reports 1.87–2.0% for the native
subsurface, with 267 submitted buffers and 265–266 compositor releases per
nine-second settling-plus-measurement interval. Two screenshots per run show
changing text contrast, so low CPU is not a frozen or invisible animation.
A final run requesting 60 Hz delivered 56.12–56.25 submitted and released
buffers/second at 3.25% editor CPU and 11.87–12.0% compositor CPU; the stronger
CPU, callback, and buffer-delivery assertions all passed (`results/native60.json`).
The initial hardcoded position was visibly wrong; the prototype now converts
`posn-at-point` with `window-inside-pixel-edges`, as the Emacs manual specifies.
The final positioned screenshots are retained under `results/`.

CPU percentages are relative to one core. The compositor figures include the
whole desktop. This is a single machine, KDE Wayland at 2x scale, with actual
frame dimensions recorded in each JSON file (about 1545x864 logical pixels).
These short comparisons establish a candidate, not broad platform performance.
GTK-mode explorations and the profiling run overlapped a four-worker Emacs
build; the baseline, clipping, Canvas, and repeated native-surface comparisons
did not. Compilation CPU is not counted as editor CPU, but overlapping load
can still affect timings. See `results/environment.json`.

Gprofng places 86% of sampled CPU in Pixman pixel operations. The call tree
includes GDK frame preparation and fills; the complete profile was disposable
material in the original worktree's `.scratch/renderer-lab/profile-baseline/`
and is not retained. A function
summary is retained in `results/profile-functions.txt`. The experiment refines
the original diagnosis: **Lisp/process wakeups** trigger the expensive redisplay
path, while native GLib callbacks can execute without it. Rendering through a
normal GTK widget or Canvas still pays the expensive presentation cost here.

## Reproduce

Run from the branch worktree, with the temporary editor visible. The runner creates
its own server, compiles copied Lisp into a temporary directory, measures
`/proc` CPU using actual clock tick frequency and elapsed time, then closes
only its own editor. It never loads experimental modules into the live editor.

Baseline (expected failure against the initial five-percentage-point target):

```sh
python3 .mevedel/shared/editor-cpu/renderer-lab/run.py \
  --rate 30 --repeat 2 --output .scratch/renderer-lab/repro-baseline \
  --assert-overhead 5
```

Native GTK/Wayland module:

```sh
gcc -shared -fPIC -O2 -Wall -Wextra -Wno-deprecated-declarations \
  $(pkg-config --cflags gtk+-3.0 wayland-client) \
  .mevedel/shared/editor-cpu/renderer-lab/gtk-probe.c \
  -o /tmp/mevedel-gtk-probe.so \
  $(pkg-config --libs gtk+-3.0 wayland-client) -lm
python3 .mevedel/shared/editor-cpu/renderer-lab/run.py \
  --modes idle native-surface --gtk-probe /tmp/mevedel-gtk-probe.so \
  --seconds 8 --repeat 2 --output .scratch/renderer-lab/repro-native \
  --assert-overhead 5
```

The current assertion checks CPU overhead, callback cadence, and submitted and
released buffer rates. `--screenshots` uses Spectacle to capture the active
experiment window after sampling, outside the CPU interval. Native GTK modes
are `native-noop`, `native-widget`, `native-surface`, `single` and
`app-paintable`; ordinary modes are `idle`, `noop`, `text`, and `canvas`.
`--profile` wraps the disposable editor with `gprofng collect app`.
The native timeout rounds the requested period to integer milliseconds;
requested 30 Hz therefore runs around 29.6–30 Hz.

Canvas was tested in the isolated source checkout `.scratch/emacs-renderer`,
Emacs master `6d14004c581982231bd959f586c5c835ef347256`. Configure/build arguments
are recorded in `results/environment.json`. No installed editor was replaced.
Compile `canvas-probe.c` against that checkout's generated `src/emacs-module.h`
with `pkg-config --cflags --libs pangocairo` and `-lm`, then pass
`--emacs .scratch/emacs-renderer/src/emacs --canvas-probe PATH-TO-MODULE`.

`damage-probe.c` is an intentionally incomplete `LD_PRELOAD` diagnostic which
replaces full EmacsFixed damage with a fixed rectangle when
`MEVEDEL_DAMAGE_PROBE=clip`. It is not suitable for real editing: drawing outside
that rectangle is omitted. The runner's `--preload` and `--damage-probe` options
apply it only to the child editor.

## Integrated implementation

`mevedel-view-native.el` consumes the animation module's exact timed samples;
`native/mevedel-view-native.c` presents them with Pango/Cairo and a native GLib
timer. No callback enters Lisp. The module builds lazily in
`mevedel-user-dir/native/`, needs no Emacs patch, and adds no execution procedure
to callers. Missing build prerequisites, unsupported displays and unsuitable
text geometry retain ordinary animation. It is enabled by default on supported
PGTK/Wayland displays; `mevedel-view-native-enabled` disables it.

Lisp retains style semantics, power/reduced-motion policy, placement, phase
capture and lifecycle. Presentation coalesces at the parent's redisplay boundary,
so a tool-row refresh can remove and reinsert markers without briefly destroying
unchanged surfaces. A pre-redisplay guard checks label text and geometry, allowing
unrelated elapsed/metadata updates without surface churn. This fixes the earlier
29% tool result and trace retained in `results/request-tools*`.

### Performance of the integrated implementation

Isolated exact-sample checks: bounce 2.4–2.6% CPU at 30 delivered frames/s;
breathe 3%, braille/ascii 2%, dots/ellipsis 1.75–2%, shimmer 2.5%. Idle was
0.25–0.5%. Breathe coalesces identical colors; shimmer retains its existing
sweep/rest timing. See `results/owned-*.json`. A final six-second bounce check
used 1.5% versus 0.33% idle with exactly 30 submissions and releases per second
(`native-final-results.json`); the five-percentage-point overhead gate passed.
These are short, single-machine measurements, not cross-platform benchmarks.

The repeated **real sleeping Bash + main bounce** comparison was:

| Sequential sample | Editor CPU | Compositor CPU |
|---|---:|---:|
| Static, first | 14.17% | 15.33% |
| Ordinary 8-fps bounce + tool shimmer | 38.67% | 25.50% |
| Native 30-fps bounce + tool shimmer | 22.17% | 18.33% |
| Native, repeat | 23.00% | 17.83% |
| Static, last | 18.67% | 17.67% |

Native animation used about 40% less total editor CPU than ordinary animation
while restoring 30-fps main motion. The warmed static baseline leaves an
estimated 3.5–4.3 percentage points attributable to animation. This estimate is
approximate: collection and other editor timers vary through the sequential run.
Redisplay counts were 123 for ordinary, 53/54 for native and 53 for the warmed
static sample, supporting the intended elimination of per-frame Lisp redisplay.
`request-repeated-results.json` retains timing, source hashes, focus/activity,
submission/release counters and zero active surfaces after each completion;
`request-repeated-timers.txt` records unrelated timers.

A concurrent presentation test used seven pending tool events during a held real
request: main status, five rows and overflow yielded seven native surfaces at
20.5% CPU. These were synthetic view events, not seven concurrently executing
commands. The test then reduced seven to six to five pending tools, checking the
changed overflow text and its removal. See `request-overflow-results.json` and
`request-overflow-overflow.json`. The earlier attempt to make seven actual Bash
calls ran them serially and was rejected as concurrent-presentation evidence.

A separate main-only request used 12.75% native versus 24.5% ordinary and 7.62%
static (`request-bounce.json`). First native request start including module
build/load took 0.49 seconds, versus 0.07 warmed ordinary. The final tool runner's
first-use measurement includes setup/waiting for active tools and was about
1.35–1.45 seconds; these timings have different boundaries.

All full-request measurements use compiled package code, frozen Eask dependencies,
a temporary HOME/workspace and a localhost SSE mock. JIT native compilation is
disabled in the disposable editor. Loaded gptel paths and source hashes are
recorded. This represents the isolated mock configuration, not the user's live
configuration. Samples that lost focus or hid the label were rejected. Screenshots
are taken outside measurement intervals. No module was loaded into the user's
live editor, and no installed Emacs was rebuilt or replaced.

### Validation and acceptance evidence

| Requirement | Evidence |
|---|---|
| Preserve style samples, foreground fade, glyph cadence and phases | Shared animation sequence, ERT sequence/freeze tests, exact-sample graphical runs for all seven styles; inspected light/dark request screenshots |
| Actual request and tool lifecycle | `request-acceptance-acceptance.json`: freeze/thaw, backend toggle, switching all supported tool styles, theme/text scale, split/unsplit, clipping/restoration, typing and multiline draft; both surfaces zero after completion |
| Concurrent rows and semantic overflow updates | Seven-to-six-to-five event check described above; counts and native text asserted |
| Visibility, geometry, editing and teardown | `native-boundaries-lifecycle.json`: inhibited edits, cursor/selection, hide/show, font resize, child frame, buffer/tab switching, multiline fallback, focus transfer, explicit stop and feature unload |
| Safe native failures and bounded resources | Invalid parent ID rejected; failure injected at each of three buffer allocations; 50 real create/close cycles, no surviving surfaces or descriptor growth; `native-boundaries-descriptors.json` |
| Tests and compilation | Full Eask suite: 9,776 cases, zero unexpected, 31 conditional skips; latest focused suite: 237 passed; 242 files compile with zero warnings; C builds with `-Wall -Wextra -Werror`; `tests.json` |
| Portable fallback and optional construction | ERT verifies batch/disabled displays do not build, failed construction is cached and cleans up; graphical toggle/clipping tests exercise fallback and recovery |
| No Emacs modification or caller lifecycle recipe | Native module is a packaged mevedel source, built automatically at first use; all placement, cleanup and scheduling stay in presenter/stream owners; built tarball contains byte-identical native/Lisp sources (`package.json`) |

The full suite predates two final local changes: removing eager pre-command
invalidation (which interrupted animation during typing) and adding feature-unload
cleanup. The 237-case focused run and warning-free compilation include these;
the final full-request acceptance uses byte-compiled current sources and passes all 19 checks. Native construction also
returns nil when its first Cairo presentation fails, rather than returning an
already-closed handle. Native buffer failure injection runs only in a disposable
editor, through `allocation-probe.c`; it is not part of the product.

Restoration after clipping can wait for the next existing semantic check. The
acceptance loop allows 2.2 seconds; an initial 0.7-second assertion was too short.
Freeze assertions explicitly force the parent redisplay boundary before checking
native resource counts, matching the presenter's coalesced contract.

### Reproduce the integrated implementation

```sh
gcc -shared -fPIC -O2 -Wall -Wextra -Werror \
  $(pkg-config --cflags gtk+-3.0 wayland-client) native/mevedel-view-native.c \
  -o /tmp/mevedel-view-native.so \
  $(pkg-config --libs gtk+-3.0 wayland-client) -lm
python3 .mevedel/shared/editor-cpu/renderer-lab/run.py \
  --modes idle native-owned --native-module /tmp/mevedel-view-native.so \
  --seconds 6 --assert-overhead 5 --lifecycle \
  --output .scratch/renderer-lab/owned-repro
npx @emacs-eask/cli compile
python3 .mevedel/shared/editor-cpu/renderer-lab/request-run.py \
  --seconds 6 --tool --modes static ordinary native native static \
  --output .scratch/renderer-lab/request-repro
python3 .mevedel/shared/editor-cpu/renderer-lab/request-run.py \
  --seconds 6 --tool --modes native --acceptance --screenshots \
  --output .scratch/renderer-lab/acceptance-repro
python3 .mevedel/shared/editor-cpu/renderer-lab/request-run.py \
  --seconds 6 --tool --tool-count 7 --modes native \
  --output .scratch/renderer-lab/overflow-repro
```

For allocation failures, compile `allocation-probe.c` as a shared library with
`-ldl`, then add `--preload PATH --allocation-failures` to `run.py --lifecycle`.
Every runner owns and closes its editor. Original approximate GTK, Canvas and
clipped-damage probes remain clearly confined to this diagnostic directory.

### Follow-up: top-left flash on refocus

The user reproduced a flash over the preview heading whenever the window was
refocused. The original validation counted correct final placements but missed
the first presentation: `set_position` is pending state on the parent surface,
while the child had already switched to desynchronized commits. Its first image
could therefore appear at the protocol's initial `(0, 0)` position.

`check-positioning.py` checks actual `WAYLAND_DEBUG=client` traffic for child
commits before their parent applies position. The pre-fix reproduction fails
with 17 premature commits over 16 surface creations. The corrected presenter
keeps creation/movement synchronized until a parent frame callback, then enables
independent animation. The post-fix trace passes; `results/positioning.json`
and the adjacent excerpts retain the evidence. The user refocused the corrected
preview repeatedly and confirmed the flicker is gone. Pending frame callbacks
are destroyed on close, including partial allocation failures. The final
30-check lifecycle run passes; bounce still delivers 30 frames/s at 2.6% CPU
(0.4% idle), with stable descriptors. See `positioning-results.json` and
`positioning-lifecycle.json`.

To repeat the protocol check, set `WAYLAND_DEBUG=client` when running the
graphical lab, then run `python3 check-positioning.py PATH/TO/editor.log` from
this directory. The trace is diagnostic only; do not include its logging
overhead in CPU comparison results.

### Scope and remaining limitations

The measured low-CPU backend is Linux PGTK/Wayland with GTK 3, module support,
a C compiler, pkg-config and GTK/Wayland development headers. Other platforms
and clipped, wrapped or occluded labels use the existing ordinary renderer and
retain its costs. No equivalent low-CPU claim is made for X11, macOS or Windows.
The full request still has telemetry, GC, collection, metadata and transport
costs: 2% isolated animation does not mean 2% for the entire editor session.
These independent costs and platform extensions are recorded in `docs/backlog.md`.

Useful primary references:

- [Wayland subsurface protocol](https://wayland.freedesktop.org/docs/html/apa.html#protocol-spec-wl_subsurface)
- [GTK queue_draw_area](https://docs.gtk.org/gtk3/method.Widget.queue_draw_area.html)
- [Canvas upstream/backport](https://github.com/minad/emacs-canvas-patch)

## Streaming prose (review, 2026-10-08)

`request-run.py --stream-rate 8 --observe 6` streams prose above the progress
row and then samples renderer ownership every 0.1 s. Before the start-marker
fix, the label read as hidden in 26 of the 51 samples taken while text arrived
(ordinary; 27 for native), and the native presenter opened and closed 5
surfaces in 6 s; afterwards the label animated
in every sample while text arrived, and native opened 2. Editor CPU over 8 s
while streaming: static label 38%, native bounce 44%, ordinary bounce 47%;
a silent request: static 7.8%, native bounce 12%. Single runs in
`results/streaming/`; the stream's own render cost dominates.
