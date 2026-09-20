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
changes in presentation. Failed tool calls split activity groups and start
collapsed with a red `×`; warning rows remain groupable and mark their group
with `!`. Only markers receive severity highlighting, and an accompanying
sandbox warning cannot downgrade an error. Full rerender is the correctness
fallback. One scheduler coalesces redraws;
unattended graphical views defer visual work, retaining changed tool IDs rather
than forcing a full rebuild on focus return. Incremental projection also drains
those row updates; full projection subsumes them. Retained-agent metadata
replacements refresh source-backed handles, with full projection as the fallback
for unavailable source or generic rows without retained-agent metadata.
Transcript writers also share per-view mutation ownership: nested projection,
terminal, disclosure, and agent-refresh work coalesces rather than mutating
captured view coordinates recursively. Source replacement retires obsolete
intent, but not required terminal cleanup. Markdown fontification reuses a quiet
hidden buffer for ordinary calls and isolates nested calls. Rendering never
prompts to install missing grammars. The [view manual](../view.md) owns the
detailed rendering and recovery contracts.

Projection ownership also inhibits redisplay through queued work. Disclosure
expansion rolls back failed replacement. Reader preservation includes both
selection endpoints, neighboring managed zones, and table cells across wrapping.
Views defer table formatting until visible and idle, processing one complete
table per callback. Semantic marker relocation replaces whole-table text diffing.
A full or live-turn projection shares disposable boundary indexes and pure audit-decoding
results; callers still establish trust independently.

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

### September 2026: bounded routine progress and shared live parsing

Replay of session `2026-09-18T13-59-e77a57dec9b5` reproduced expensive redraws
without running tools or making provider requests. Previously, unattended Bash
progress upgraded the pending render to full, and retained-agent metadata
replacement also requested full projection. Both redrew unrelated history.
The existing source-backed refresh paths preserve draft text, point, and agent
audit rows across metadata growth, so these events now retain their narrower
scope. Generic rows and stale agent handles keep their full-render fallback.

Integration testing exposed two requirements for the narrow path: replacement
at the source endpoint can collapse a marker into deleted metadata, so refresh
recovers current bounds by source tool-use ID and the enclosing block (restored
properties can separate call text from metadata); and sampled `sxhash-equal` keys can
collide for equal-length `running`/`blocked` edits, so content keys now digest
the complete text. Agent renderer dispatch also retains transcript handles for
blocked or failed children while preserving their error status; a failed launch
without retained metadata still uses generic error rendering. Tests assert the
updated status and expansion of adjacent collaboration disclosures.

Every targeted tool refresh also used to clear all tool-rendering entries.
Invalidating only entries overlapping the changed source preserves unrelated
calls across subsequent stream projections. Live projection now uses the same
temporary indexes and pure audit cache as full projection: a regression case
decoded one repeated payload twelve times before, once afterward. Structural
validation advances through ordered property runs, and the audit-only predicate
checks uncovered whitespace once without stripping and reparsing the payload.

Three paired isolated graphical replays on Emacs 31.1, with Aporetic Serif Mono
and installed Markdown grammars, produced these median callback times:

| Workload | Before | After |
| --- | ---: | ---: |
| One Bash row after 20 unattended updates | 467 ms | 8.4 ms |
| Child-agent metadata refresh in a large parent | 461 ms | 19.8 ms |
| Agent live-turn update | 85 ms | 37 ms |
| Accumulated agent turn catch-up | 508 ms | 168 ms |

Final rendered text hashes agree across variants. Bash catch-up collections
fell from two to zero; agent catch-up collections fell from two to one.
The local protocol and source/fixture hashes are in
`.scratch/responsiveness-goal/report.md` and its results directory. These are
source-Lisp callback/redisplay measurements with simulated attention changes,
not actual keystroke latency or a live-provider comparison. Large full history
remains expensive: a second 11.5 MB transcript's scheduled rebuild was
2.05 s before and 2.10 s after the full-content key correction. Avoiding redundant
full rebuilds is the gain; this change does not solve full-history cost.

These changes reduce work and allocation instead of adding Lisp threads or
raising global GC thresholds. Emacs Lisp threads share the interpreter lock and
collector; moving these buffer operations to another thread would not make
them run in parallel. External processes remain the existing execution boundary
for tool and provider work. Large full-history projections are still synchronous.

### September 2026: faster opening and visible idle tables

Profiling an agent transcript and replaying its frozen source identified
whole-table replacement and repeated structural/audit decoding as opening costs.
The integrated renderer reduced batch projection from a median 1.93 s to
0.42 s on that capture (four runs each, 32 MiB GC threshold), with identical
fully formatted text. The real-table reflow matrix retained all 11,184 tested
markers. These measurements support integrating the validated prototype's
marker-aware replacement, projection-scoped caches, and visible idle rendering
into ordinary root and agent views.

The earlier native text-diff replacement preserved many positions but became
expensive on wrapped tables. Simply bounding that diff could fall back to
wholesale replacement and lose internal markers. Replacement now uses Emacs's
temporary deletion undo records to recover displaced markers, including those
held by callers, then maps them by cell and unwrapped offset or row geometry.
Overlay objects and window anchors are restored explicitly. Rollback and both
marker insertion types are covered by tests. This supersedes the native
non-destructive replacement decision below.

The first projection retains raw table source; a 250 ms idle callback formats
one visible table, then yields before scheduling the next. Scrolling discovers
new work through source properties, so source deletion needs no separate queue
cleanup. A table remains an indivisible unit; this is not a strict callback
time budget or partial-table renderer.

### September 2026: preserve readers through replacement and reflow

Five small regressions reproduced disappearing intermediate transcript text,
status growth moving point out of a permission prompt, rerender enlarging a
selection, failed expansion deleting its header, and table resizing moving
point out of a cell. Ownership alone prevented nested writers but allowed
redisplay during fontification after deletion. Redisplay inhibition now spans
the owner and queue drain; expansion uses an atomic change group.

Raw mark and neighboring-zone positions were inconsistent with point's existing
semantic preservation. Both selection endpoints now follow source anchors on
redraw, while advancing markers keep positions in unchanged neighboring zones.
Table replacement uses Emacs's native non-destructive replacement and refreshes
layout properties explicitly. A wrapped-cell test with repeated words showed
that text diffing alone can choose the wrong occurrence, so table cell identity
and unwrapped character offsets restore reader positions across reflow.

### September 2026: reentrant writers require ownership

A preserved live view duplicated an intact authoritative response and retained
streaming-tail markers after settlement. A deterministic replay reproduced the
failure when settlement ran inside an older incremental render's fontification:
the older invocation resumed and reinserted obsolete content. Ordinary streaming
and the reverse nesting order did not reproduce that defect. This establishes
the ordering bug, not the original session's natural interrupt sequence.

Timer coalescing and atomic change groups alone did not serialize writers.
Projection entry points now share view-local ownership, with queued source-backed
intent and terminal cleanup. Agent refreshes rediscover handles, and disclosures
retain desired state rather than raw positions. Nested fontification also needs
its own text buffer, since different views share the ordinary reusable buffer.
Neither fix changes the authoritative transcript or introduces a general event
framework.

### September 2026: standalone audits retain their identity

A captured execution delivery followed by a provider batch-start audit rendered
the audit as `Tool (3 lines)` and exposed its encoded body when expanded.
The transcript grammar correctly classified the audit as ignored, but activity
rendering passed the standalone span to the tool parser. Activity entries now
preserve standalone audit identity. Provider bookkeeping produces no entry;
user-facing hook audits retain source-backed disclosures and prevent grouping
from silently discarding them.

### September 2026: failures remain visible between activity groups

Failed calls previously stayed inside collapsed activity groups and opened
automatically when the group was expanded, with warning coloring across their
entire headers. A user review of a mixed run containing a failed Bash call and
a sandbox refusal showed that this hid which calls failed until the group was
opened, then gave their output disproportionate space and emphasis. Failures
were changed to split their surrounding groups, start collapsed, and highlight
only `!`, including warning-class sandbox disclosures.
Explicit user expansion remains source-backed and survives redraws.

### September 2026: distinguish warnings from failed tool operations

A subsequent live-session inspection found that Grep displayed an unreadable
path as `0 matches`, while Bash described a test runner that exited 1 as
`completed`. Both used the same `!` as warnings. The previous split rule also
prevented otherwise useful warning results from joining activity groups.

Errors now remain separate with a red `×`; warnings stay grouped and mark
only the group's `!`. Shared status dispatch routes failed operations away
from success-only summaries across the tool roster. Execution summaries expose
outcome and exit status. A successful compound program that handled a failed
child warns without changing its execution outcome, while the child remains
an error. These changes distinguish a usable result with caveats from an
operation that failed, without restoring full-line coloring or auto-expansion.

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
