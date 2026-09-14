# Keep views reconstructable and rendering bounded

Status: accepted

## Current decision

The data buffer and session records own durable conversation and runtime facts.
The view is a reconstructable projection: transcript parsing supplies history,
while session status and interaction descriptors supply managed chrome. Composer
text belongs to the user and every redraw preserves it. Source-backed anchors
identify disclosures and reader positions across projection changes.

Streaming updates retain completed semantic units and reconcile the mutable
tail. Full rerender is the correctness fallback. One scheduler coalesces redraws;
unattended graphical views defer visual work, then reconcile when attended.
Markdown fontification reuses a quiet hidden buffer, and rendering never prompts
to install missing grammars. The [view manual](../view.md) owns the detailed
rendering and recovery contracts.

## Rationale and alternatives

Durable state in view overlays would make rerender, resume, and multiple
projections disagree. Stable source identity survives a change in display length;
raw view offsets do not. Incremental rendering reduces repeated work without
requiring a second transcript model, because it falls back to complete projection
when source anchors are stale. Session state and interactions can refresh without
parsing the transcript.

## Consequences

Rendering caches and fold state are disposable. A missing Markdown grammar gives
plain text until the user installs it. A buffer displayed in differently sized
windows has one table/image layout, determined by the latest realignment.
Observers must not change execution or steal focus; a failed projection warns
and retains the last good display where possible.

## Decision history

This record consolidates rationale previously embedded in the view manual:

- Raw view positions drifted through live Bash output and jumped into unrelated
  turns after rerender. Semantic composer, fragment, and source anchors replaced
  raw-position restoration, retaining a clamped fallback for missing anchors.
- A 54-minute debug capture showed an accepted-plan turn reinserted on every
  live tick after a whole-buffer rewrite collapsed the source marker. Full
  projection now repairs both view and data turn anchors.
- An unattended session spent about one quarter of CPU in redisplay. Attention
  gating skips visual work while preserving pending rendering for focus return.
- Markdown mode setup measured about 4.4 ms versus about 0.1 ms for fontifying a
  typical response segment. Reusing the initialized buffer avoids setup per tick.
- The development guide recorded a session profile with 20% of CPU samples in
  repeated `require` calls. Its cold-load boundary rule applies to segment,
  redraw, and guest execution paths; the source note did not isolate view cost.

The original notes did not supply separate dates or general benchmark bounds.
These observations explain the implementation choices, not performance promises.
