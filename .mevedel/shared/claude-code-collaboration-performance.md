# Collaboration projection stalls, 2026-10-06

The resumed shared-whiteboard session showed 3.8–4.9 second event-loop stalls
in `mevedel-collaboration--publish-timer`, recorded before diagnostic changes.
Most Read handlers completed in milliseconds; synchronous projection delayed
both editor input and the delivery of tool results to Claude.

A CPU profile of one actual publication took 4.47 seconds. Tool-boundary
recovery accounted for 4,127 of 4,447 samples, repeatedly searching backward
through historical results. Browser projection lacked the scoped boundary
index already used by the Emacs view renderer.

`mevedel-collaboration--canonical-records` now binds that existing index for
one projection, covering root, archived, and agent transcript callers. A
regression projects 32 tool calls across two stream edits, verifies every
result, and bounds boundary searches linearly. Before the fix the first
projection performed 1,027 searches and failed the 128-search bound.

The same live transcript, with its per-segment bounds memo cleared to model
a stream edit, took 4.50 seconds before and 0.31 seconds after hot-loading
compiled projection code; both produced 153 records. 57 targeted collaboration,
history, and transcript-cache tests passed; 229 production files compiled
without warnings. No model request was submitted for these measurements.

Diagnostic incident: an initial raw profiler-log export expanded captured
objects into a roughly 16 GB temporary buffer and separately froze Emacs.
The user authorized interruption; the export was interrupted, its temporary
buffer removed, and profiling instrumentation cleared. Subsequent profiles
used bounded function-name summaries, not raw runtime object serialization.
