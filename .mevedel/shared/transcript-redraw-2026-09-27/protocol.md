# Transcript redraw measurements, 2026-09-27

User-authorized measurement-only follow-up to `Bound remaining large transcript
redraws`. No production edits, session resume, lease acquisition, provider calls,
or original-session writes. Main agent owns harness and execution. Child
`/root/measurement_review` independently inspects measurement validity (read-only).

## First round

- Capture immutable published bytes, verify manifest SHA-256, and work on copies.
- Inputs: largest root segment and largest agent transcript in
  `2026-09-25T20-50-e6b129234a36`; root transcript in
  `2026-09-25T20-48-9f6729a8d04c` as control.
- Fresh Eask-isolated terminal Emacs per input/mode/repetition; source-loaded
  current Mevedel after Eask bytecode cleanup. Record actual dependency paths,
  hashes, Emacs version, fontification availability, and GC settings.
- Cold projection: restore transcript properties before the measured interval,
  then invoke the native synchronous full renderer on a new view. Record
  restoration separately; do not call this full-session resume time.
- Warm scheduled refresh: populate the view once, then request a native scheduled
  full redraw with its ordinary debounce. Include that debounce in completion
  time, but report actual key latency separately.
- Root typing occurs in the real multiline composer. Agent typing occurs in a
  separate visible editor buffer because agent inspection is read-only.
- Inject terminal keys every 10 ms beginning before work. Record send and receipt
  timestamps, all-work-window latency percentiles/max/counts, and baseline keys.
  Continue beyond work completion to capture buffered input.
- Three serial repetitions of each primary condition. Separate instrumented
  replays attribute time to segmentation, grouping, turns, parsing, fontification,
  disclosure restoration, and GC. Inclusive phase times are not additive.
- After timing, compare history text with another native full render; verify the
  exact typed draft, point offset, no pending preparation/batches, and unchanged
  source text. Broader selection/disclosure/live-stream checks are separate from
  this first-round settled-history replay.

Private fixtures and raw execution output live under
`.scratch/transcript-redraw-2026-09-27/`; shareable protocol, harness, and report
live here. These tests do not hydrate the durable session/agent registry or load
the user's init. Current-source isolated timings are not installed-runtime or
graphical typing measurements. Graphical corroboration is a follow-up where
available, not interchangeable with terminal key latency.

## Targeted follow-up

The first 18 runs compare two real paths, but cache warmth and scheduling both
differ. Add three `cold-scheduled` runs each for root and agent: a fresh view with
no initial full render, using the existing scheduled full-refresh API. This is
an experimental caller choice, not current cold-opening behavior and not a
production change. It separates batching effects from cache warming. The only
driver change is omitting the warmup for this mode; record its new harness hash.
