# Editor CPU during requests (2026-10-08)

## Root cause

On the user's Emacs 31.1 pgtk build (KDE Wayland, 2x scale, 1536x888 frame)
every wakeup ends in a redisplay that presents the **whole frame surface**,
even when nothing changed. A do-nothing 10 Hz timer in `emacs -Q`:

| frame          | emacs CPU | kwin CPU |
|----------------|-----------|----------|
| 1536x888 (2x)  | 23-36%    | ~21%     |
| 400x300        | 3%        | —        |

Rule of thumb on this frame: **~2% editor CPU + ~1% compositor per wakeup
per second.** mevedel's cost was dominated by how often it woke Emacs.

## Before / after (user's live Emacs, frame visible, 10 s windows)

Request held open by a local mock server; focus check bypassed; same view.

| scenario                                         | before          | after           |
|--------------------------------------------------|-----------------|-----------------|
| idle, no request                                 | 4% / 5% kwin    | 1% / 2%         |
| request waiting, static label                    | 8% / 7%         | 7% / 5%         |
| shimmer label (old default 60 fps / new cadence) | **70% / 34%**   | **29% / 15%**   |
| shimmer label on battery (30 fps / cadence 15)   | 62% / 31%       | 23% / 11%       |
| running `Bash` (sleep), everything static        | **42% / 11%**   | **21% / 10%**   |
| telemetry heartbeat alone (10 Hz / 2 Hz)         | 30% / 12%       | 18% / 9%        |
| **default config, watched, Bash running**        | **73% / 21%**   | **39% / 18%**   |
| same, on battery                                 | —               | 34% / 16%       |

Continuous styles are unchanged and remain expensive: breathe/bounce at
30 fps 64% / 30%, braille/ascii ~30% / 13%, dots 21%, ellipsis 17%.

## Claude Code backend

Adapter + `claude` CLI: ~230% CPU for ~2 s at start, 5-17% while streaming.
`acp.el` drains each output chunk via a zero-delay timer (~30/s), comparable
to a fast gptel stream. A header-line `vertical-motion` walk cost 13% of
editor CPU in its requests (fixed).

## Fixes (branch `fix/cpu-wakeups`)

- Telemetry heartbeat 100 ms -> 500 ms; timers report their own lateness.
- Shimmer cadenced (Codex-style 1 s sweep every 4 s); ceilings 30/15 fps.
- Tool rows default to shimmer, sweeping "Calling TOOL" with the label.
- Spinner arming fixed after new spans and scrolling (was frozen).
- Indicators hold still while waiting for user input.
- Bash: missed-sentinel watch 10/s -> 1/s; progress 4/s only while output
  flows, else 1/s; elapsed times shown in whole seconds everywhere.
- Unattended (incl. undisplayed) views arm no render timers.
- Collection: slices 1 s apart, exponential backoff while blocked.
- Control-transfer polling skipped for non-portable sessions.
- Header/status-line per-redisplay costs reduced; decorative frames no
  longer invalidate header caches; stale animation markers detached.
- Markdown realign skips passes with nothing to lay out.
- Stream insert batch 0.04 s -> 0.2 s.

Open items are listed in `docs/backlog.md` under "Editor CPU".
