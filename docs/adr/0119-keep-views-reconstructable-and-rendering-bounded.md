# Keep views reconstructable and rendering bounded

Status: accepted

## Current decision

The data buffer and session records own durable conversation and runtime facts.
The view is a reconstructable projection: transcript parsing supplies history,
while session status and interaction descriptors supply managed chrome. Composer
text belongs to the user and every redraw preserves it. Source-backed anchors
identify disclosures and reader positions across projection changes.

Streaming updates retain completed semantic units and reconcile the mutable
tail. A tool/reasoning/delivery activity run remains mutable until its surrounding
transcript boundary closes it; an individual completed call can still join a
growing group. Grouped rows retain their individual source identities across
changes in presentation. Full rerender is the correctness fallback. One scheduler coalesces redraws;
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

### September 2026: streaming groups and managed boundaries

An event-by-event replay of session `2026-09-15T07-24-a65f4ae874d0` and
small ERT reproductions exposed differences between incremental and full
projection. Retaining each individual tool/reasoning row prevented growing
activity groups from forming; treating a special row as a veto on the entire
mixed run dissolved existing groups. Activity runs now own the mutable boundary,
and special rows split only their surrounding runs. A late repair audit also
exposed that a grouped tool inherited its group's disclosure identity; it now
keeps its standalone source identity when it moves out of the group.

Delivery cards previously ended tool runs even though the assistant's activity
continued. They now participate in the same chronological group, retaining
their own folds and sender links. A live regression test also showed that
forming a collapsed group over an already open delivery moved the cursor off
the text being read. New groups preserve open rows; an explicit group fold
continues to take precedence. Full redraws retain source-keyed child states
hidden by folded groups; capturing only visible rows lost these states.
Explicit transcript-source changes still clear the table, and source anchors
prevent stale keys from applying to rewritten content.

Managed overlays previously included history inserted at their leading edge.
Their leading boundary now advances past that history, and progress spacing
participates in reconciliation. This removes the reproduced one-line jump
between incremental and full refreshes.

The aggregate agent roster previously excluded any agent with a handle anywhere
in rendered history. That made roster membership depend on history folding and
rendering. The roster now lists every active agent from the session registry.

### September 2026: agent refresh preserves adjacent audits

A replay of session `2026-09-15T15-02-332f99a6d0ca` showed one retained
reminder delivery becoming two visible rows after one agent-status refresh.
The refresh replaced the handle's source span but inserted its adjacent audit
again. It now updates only the handle, preserving the independently owned audit
rows and their fold state. Reminder delivery and retained history are unchanged.

### Earlier rendering decisions

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
