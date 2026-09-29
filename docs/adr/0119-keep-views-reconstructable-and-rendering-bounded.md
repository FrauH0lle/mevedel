# Keep views reconstructable and rendering bounded

Status: accepted

## Current decision

The data buffer and session records own durable conversation and runtime facts.
The view is a reconstructable projection: transcript parsing supplies history,
while session status and interaction descriptors supply managed chrome. Composer
text belongs to the user and every redraw preserves it. Source-backed anchors
identify disclosures and reader positions across projection changes. Reader
anchors distinguish section roles and compound-child discriminators, retaining
ordinals only among runs of the same section identity. Grouped rows reuse their
standalone disclosure roles; neighboring separators cannot shift their anchors.

Streaming updates retain completed semantic units and reconcile the mutable
tail. A tool/reasoning/delivery activity run remains mutable until its surrounding
transcript boundary closes it; an individual completed call can still join a
growing group. Grouped rows retain their individual source identities across
changes in presentation. Nested compound rows retain their own metadata and
depth, so disclosure replacement removes descendants without consuming siblings.
Failed tool calls split activity groups and start
collapsed with a red `×`; warning rows remain groupable and mark their group
with `!`. Only markers receive severity highlighting, and an accompanying
sandbox warning cannot downgrade an error. Full rerender is the correctness
fallback. One scheduler coalesces redraws;
unattended graphical views defer visual work, retaining changed tool IDs rather
than forcing a full rebuild on focus return. Incremental projection also drains
those row updates; full projection subsumes them. Retained-agent metadata
replacements refresh source-backed handles, with full projection as the fallback
for unavailable source or generic rows without retained-agent metadata. Distinct
agent paths share a per-view queue, drained one path per callback; duplicate paths
coalesce and view teardown cancels the queue.
Scheduled settled-history rebuilds retain the old projection while a job
restores source properties, resumes full-context canonical scanning, groups and
annotates turns, and prepares whole-response Markdown and tool entries across
callbacks. The scanner and the synchronous correctness path use the same
classification and repair rules; independent per-chunk classification is not
trusted. Source identity and its full modification tick are checked before
publishing the complete plan. On publication, reader-visible turns and all chat
prompt turns render first, then remaining response turns fill one per callback.
This gives a window moved onto a pending response the correct pinned prompt
and a real prompt header to reveal on click, before its response is rendered.
Each callback preserves fresh reader/composer state and validates its source
generation. Losing focus pauses the job; source changes replace it and release
retained private preparation buffers. Session-level history replacements
(resume, rewind, segment rollover, control-transfer acquire and follow),
directive-request setup rollback, an interrupted side answer, and Source's
refresh after a Session Fork use this scheduler: they only change what is displayed, and nothing after them reads
view positions. In-flight reconciliation, explicit immediate refreshes, callers
that read rendered positions, and other writers' correctness fallbacks remain
synchronous. A large
individual line, final scanner repair, prompt-first publication, grouped tool
insertion, and GC can still make a callback exceed its cooperative target. Large
collapsed tool parsing also has its separate staged preparation lifecycle.
Transcript writers also share per-view mutation ownership: nested projection,
terminal, disclosure, and agent-refresh work coalesces rather than mutating
captured view coordinates recursively. Source replacement retires obsolete
intent, but not required terminal cleanup. Markdown fontification reuses a quiet
hidden buffer for ordinary calls and isolates nested calls. Rendering never
prompts to install missing grammars. The [view manual](../view.md) owns the
detailed rendering and recovery contracts.

Request progress animation is presentation only. The view stream schedules
one timer for visible decorative frames and elapsed metadata, separate from
transcript rendering; it updates registered fragment-owned display spans and
never writes the authoritative transcript. Each one-shot callback rearms its
timer from the present, so a stall neither replays overdue callbacks nor
allocates a new timer at frame rate. Time-based frame selection skips missed
samples and retains phase across power changes. The foreground label
offers shimmer, breathe, bounce, dots, ellipsis, braille, ascii, and static;
pending-tool rows use compact braille, ascii, dots, or static indicators.
Color animations prepare bounded, theme-derived frame banks; each live view
pins at most four banks in addition to a six-bank shared reuse cache, preventing
other views from evicting an active sample. Theme changes and edits to the
spinner, its inherited faces, or the default face invalidate both; observing
resolved-color dependencies matters when a paused, zero-fps view has no timer
to discover updated Customize colors.
Glyphs have a
natural, slower cadence; colorless and low-color terminals use the glyph
fallback cadence rather than waking at the color rate. Even resolvable face
colors cannot distinguish the prepared shades on an eight-color terminal;
terminal palettes below 256 colors therefore fall back to glyphs. Braille and
dots use same-width ASCII substitutes at the same cadence on displays missing
their glyphs. Dots check
only frames showing their target span and cache glyph support until the next
semantic tick (or theme invalidation), not at every decorative frame. A global
reduced-motion switch disables decorative updates, not semantic progress or
elapsed metadata; semantic redraws retain the last displayed sample, and
unchanged tool rows are not rebuilt at their initial frame. Hidden
and vertically or horizontally offscreen indicators suspend decorative wakeups;
a still-visible elapsed suffix independently retains its one-second semantic
refresh even when horizontal scrolling hides its label. When both source-span
endpoints fall outside a narrow viewport, a bounded display-row edge probe
checks for visible ordinary text in the middle; a wrapped window may start
inside the suffix. Window focus and target visibility are checked together: a
visible target in an unfocused frame
cannot borrow attention from another frame where the target is offscreen.
The transcript's separate buffer-wide attention gate remains unchanged.
A theme change repaints visible color labels at their displayed or frozen
phase, even when paused metadata and zero fps remove timers; hidden labels
wait until visible again to repaint. Frozen glyphs and pending-tool spans
also recheck their display fallbacks on theme changes or visibility resume;
each surviving tool keeps its own last displayed phase through lightweight,
incremental, and full projection when motion is disabled. This is
event-driven rather than a decorative polling timer. A move between display
frames likewise repaints the existing sample when the target is attended, using
the destination frame's prepared bank or a portable multi-frame fallback; no
timer or global
cache invalidation is needed. Horizontal visibility checks the
displayed animated portion in hscrolled windows, leaving ordinary leading-glyph
callbacks on the cheap window-boundary path. Trailing ellipsis must check its
last displayed glyph even without hscroll: truncation can hide it beyond the
right edge while a wrapped final row can expose it. Plain elapsed text also
needs a text-area/fringe check without hscroll. Vertical pixel scrolling of a
wrapped label can also conceal the bounded animated prefix while keeping its
source buffer position constant; that path checks the displayed index instead
of taking the unscrolled fast path. Replacement display strings map all their
characters to one buffer span, so looking up buffer positions could leave a
60-Hz timer running after a long label's bounded color prefix was offscreen;
the hscrolled check now reads the visible display-string index instead. It
compares that index with the *changing* color bound recorded once while
preparing the bank, not merely the face-bearing prefix: shimmer and bounce
leave some colored characters unchanged throughout the cycle. An event-driven
theme or display-frame repaint instead considers any visible part of the
colored label; it does not schedule motion for a constant-colored tail. Glyph
indicators similarly exclude their invariant trailing separator from motion
visibility. Since `window-scroll-functions` does not run for horizontal
scrolling, a scoped
`set-window-hscroll` observer rearms explicitly scrolled views; an internal
automatic pan does not call that primitive. Explicit pixel scrolling has a
`set-window-vscroll` observer, while a buffer-local redisplay hook is installed
only during horizontal/pixel suspension and defers one resume probe
until after redisplay, only for a window with the target in its visible rows.
The probe cancels and replaces a pending one-second elapsed timer before taking
the view's timer slot; otherwise cleanup loses track of an extra wakeup. Both
the view's one-shot timer and its deferred probe use the Emacs UI host's
top-level timer list. TRAMP's temporary binding must neither discard their
rearms nor hide an already queued timer from cancellation during view stop.
When a window replaces a view, the same buffer-local window-change hook
reevaluates the departing view as well as resuming the arriving one: frozen
tool-only progress has no timer callback to release its power subscription.
Deleting the last window does not invoke that buffer-local hook, so the shared
power observer briefly installs a window-state callback while subscribed views
exist, then removes it after the last subscription ends.
Package install/uninstall owns the global horizontal-scroll and face-change
observers; neither path changes the broader attention gate for transcript
rendering.

The default `auto` power policy uses the Emacs UI host's battery information:
external power permits the configured 60-fps normal ceiling, and battery,
backup, unknown or stale power information uses the conservative 30-fps
saving ceiling. `full` forces the normal ceiling; `save` forces the saving
ceiling. The lower ceiling never speeds up a naturally slower animation or
changes its cycle duration. A saving ceiling of zero freezes decorative
motion while active elapsed time and status remain current. A shared battery
observer consumes existing notifications and uses a deferred, at-most-minute
fallback query while subscribed views exist; animation callbacks never query
power. TRAMP temporarily binds Emacs's timer list to nil, so a fallback
scheduled on that list can be discarded, while `cancel-timer` there cannot
remove an older timer from the hidden outer list. The observer instead arms
its single poll on the UI host's top-level list and removes it there on final
unsubscription, even inside the temporary binding. The view timer uses the
same list ownership primitive. Callback identity rejects
stale delivery. The fallback query runs outside animation callbacks. Neither
the observer nor the spinner enables battery mode or changes
request execution. When a confirmed external sample expires, a subsequent
unknown battery notification must rearm views even though the fresh-state
comparison already says unknown: their actual timer may still use the old
external-power cadence. Reduced wakeups are an overhead reduction, not a claim
of proportional battery-life improvement.

Projection ownership also inhibits redisplay through queued work. Disclosure
expansion rolls back failed replacement. Reader preservation includes both
selection endpoints, neighboring managed zones, and table cells across wrapping.
Views defer table formatting until visible and idle, processing one complete
table per callback. Interactive visibility reuses Emacs's current window boundary,
including variable-height rows; changed display state requests recomputation.
Semantic marker relocation replaces whole-table text diffing. Restored expanded
audit disclosures use their ordinary toggle renderer, preserving the single
summary and audit styling.
A full or live-turn projection shares disposable boundary indexes and pure audit-decoding
results. Audit readers also reuse bounded pure decoding in the current buffer
across projections, retaining at most 1024 payloads totaling 4 MiB (at most
1 MiB each); callers still establish trust independently. Fork-point
classification scans the requested buffer range without copying the transcript. Boundary misses search only
after the preceding validated block. Whole-buffer normalization is reused until
text or properties change. Tool discovery advances through ordered property
runs and validated blocks, discarding completed prefixes and stopping overlap
searches at the candidate's end. This avoids searching old or later history for
each tool while retaining structural repair of stale properties.
Complete tool cache entries use source-buffer character revisions and explicit
provenance intervals. Observed appends retain preceding tools; suffix edits,
missing hooks, and unobserved character changes retire affected identities.
This allows hits to skip payload copying, hashing, structural parsing, and
repeated request-failure decoding. Partial spans depending on surrounding source
are not cached under their own range alone. A structural scan shares candidate control-line
positions among its existing parsers. Activity classification and insertion
reuse entries within that render, with independent coalescing counts. Settled
batch jobs additionally prepare source-backed entries a few at a time and
reuse them during publication when the reader's disclosure state still matches.
Structural overlays advance through ordered role segments and reuse unchanged
suffixes, restarting at earlier source positions when precedence requires it.
This preserves the existing overlay and tool-metadata containment rules without
repeatedly traversing and copying the complete segment list.
Collapsed activity summaries reuse cached tool names; expanded children recover
their complete arguments and results. Tool-call parsing reads from source-string
offsets rather than copying large result bodies to locate the call's end.
Audit-free results also avoid copying their source just to look for audits, and
the audit stripper returns unchanged text without allocating a duplicate.
Collapsed tool projection requests summary-only renderer work. Bash defers output
cleanup and command-prefix construction; ToolCall defers formatting and splitting
the returned value and constructing nested rows. Headers, outcome/warning status,
visibility and grouping remain available. Live expanded output, explicit expansion
and browser projection request complete rendering. Scheduled projection initially
shows a pending row for large uncached tool spans. A view-owned native thread
runs the canonical parser with explicit waits between stages; main-thread
publication validates source identity and invokes current renderers. The worker
waits on main-thread condition notifications, pauses while unattended or
transport is busy, and is
cancelled on source replacement or teardown. Payloads are released after
publication. Explicit synchronous callers and expansion retain ordinary parsing;
metadata is still decoded before the final summary, and shared GC remains.

## Rationale and alternatives

Durable state in view overlays would make rerender, resume, and multiple
projections disagree. Stable source identity survives a change in display length;
raw view offsets do not. Incremental rendering reduces repeated work without
requiring a second transcript model, because it falls back to complete projection
when source anchors are stale. Session state and interactions can refresh without
parsing the transcript.

Rendering prompt turns immediately reuses the canonical display text and click
target rather than building a second, partial prompt projector for pending
placeholders. It makes initial batch work proportional to the number and size of
chat prompts, even when their responses remain deferred; it does not guarantee
bounded first-paint latency for prompt-heavy histories.

The frozen September 2026 control/root/agent replay exposed long keyboard
stalls before the previous batch's first yield, including while grouping and
fontifying history. Resuming canonical preparation in the view-owned job keeps
the complete parser context without introducing a second grammar, stored
summaries, threads, or an external worker. Synchronous callers still receive
complete source positions before returning.

## Consequences

Rendering caches and fold state are disposable. A missing Markdown grammar gives
plain text until the user installs it. A buffer displayed in differently sized
windows has one table/image layout, determined by the latest realignment.
Observers must not change execution or steal focus; a failed projection warns
and retains the last good display where possible.

## Decision history

### September 2026: separate visible animation from status maintenance

The original braille spinner used a repeating 120-ms timer to preserve and
reconcile view state as well as repaint its glyph. Increasing that whole path
to 60 Hz would multiply composer and zone work and keep unattended laptops
waking for invisible frames. A temporary native animation preview established
the desired text and glyph styles but was not package code or evidence of
production performance. The replacement samples a fixed-time animation only
for visible registered spans; slower glyphs keep their cadence, and metadata
retains its independent update interval. Battery observations are shared,
outside the frame callback, and conservative when unavailable. Settings for
arbitrary frame lists and a fixed interval were removed in favor of styles,
rendering ceilings, and explicit power policy. The view manual describes
the current user-facing controls and fallback behavior. No battery-life
percentage or guaranteed delivered display rate follows from timer ceilings.

### September 2026: include horizontal visibility in animation scheduling

An independent production-view check found that an indicator scrolled entirely
left of a truncated line still received 60-Hz display-property writes: the
window-start/end gate described only vertical visibility. Checking the bounded
animated span in hscrolled windows suspends those writes without adding pixel
positioning to ordinary unscrolled frames. A separate Emacs 31.1 probe found
that `window-scroll-functions` does not run when `set-window-hscroll` changes
the horizontal position; observing that primitive for view windows ensures a
stopped timer resumes after explicit horizontal scrolling. A follow-up probe
found that Emacs automatically pans a truncated line during redisplay without
calling the primitive or window-scroll hook. A redisplay observer installed
only for suspended views sees both the pan away and its return, then defers
one scheduler rearm outside redisplay; ordinary animation frames pay no hook
cost.

### September 2026: prompt-first deferred history

Originally only reader-visible turns and their anchors were rendered before
callbacks; all other turns stayed pending. Scrolling to a pending response
between callbacks could then pin a prompt from an earlier exchange and send a
click to the wrong turn. The prompt-first projection replaces that behavior to
keep window-local navigation correct during the deferred interval.

### September 2026: audit memo capacity during long requests

A captured multi-agent request had 184 distinct encoded audits totaling 956 KB.
The 128-record memo cleared on every full scan, so each subsequent token estimate
again decoded all 184 records. Raising the record cap to 1024 keeps the same
4 MiB total encoded-byte and 1 MiB individual-payload limits. Repeated isolated
estimates fell from about 34 ms and 9.3 MB temporary allocation to 2.2 ms and
2.3 MB, with identical character counts. First decoding still incurs its cost;
working sets beyond either bound can still churn. Fresh provenance checks remain
mandatory, including for cached payloads.


### September 2026: nested compound disclosure ownership

Six toggles of a grouped ToolCall left one, one, two, two, three, then three
copies of each child in a live Emacs replay. Parent metadata stamping overwrote
descendant identities, while replacement deleted only the parent's body.
Stamping now stops at that body's boundary; row depth bounds subtree deletion.
Regression coverage checks repeated toggles, independent child disclosure,
neighboring rows, and composer preservation.

### September 2026: compiled agent observer isolation

A production replay scheduled an agent's transcript into its parent view,
replacing the parent's live response with child content. Source-loaded tests
passed, but compiling the observer module before loading session structs made
its temporary view-pointer binding lexical and unused. The callback then read
the child's permanent parent-view pointer. The module now declares the shared
variable explicitly. A fresh-process compilation regression exercises stream
and tool callbacks with both views open and checks parent text, composer state,
and the child's permanent parent binding. This preserves the observer interface
and closes a compilation-order gap in its implementation and tests.

### September 2026: ordered structural overlays

A follow-up profile put 208 ms of the example root history's 325 ms full rebuild
in source preparation. Applying 428 control ranges repeatedly walked and copied
the role list; overlay calls alone took 94 ms. Advancing through ordered ranges
and sharing untouched suffixes reduced those calls below 1 ms while retaining
precedence, overlap recovery, and contained tool metadata semantics.

Three paired original-history trials then measured worst composer key delay at
247 ms before and 74 ms after, and total batched work at 329 versus 176 ms.
Cold synchronous projection took 322 versus 163 ms; profiler-reported temporary
allocation fell from 82.1 to 60.0 MB and collections from two to one. These are
allocation totals, not retained memory. Every injected key, draft and history
projection matched. The separate stress capture's worst key delay did not
improve (229 versus 240 ms); its individual multi-megabyte decode remains an
atomic cost. The change reduces preparation work without adding another cache
or an asynchronous lifecycle.

### September 2026: full-history parsing and allocation

A second investigation replayed all nine root segments of session
`2026-09-18T13-59-e77a57dec9b5`, a representative child, and a separate 11.5 MB
transcript through 2,287 saved semantic events and 65 full-render checkpoints.
It also compared all 45 child transcript and compaction files from the original
session. These are deterministic saved-history replays, not reconstruction of
the original network chunk timing or tool execution.

The large transcript spent about 800 ms in 25 failed boundary recoveries that
walked backward through already validated blocks. Its unchanged full renders
also normalized the same properties again, invalidating boundary memoization.
Collapsed groups reparsed hidden child results just to count their tool names,
and tool readers copied large results while locating the small leading call.
That pass removed those repeated operations. Full-content keys also allowed
complete cached tools to bypass structural parsing. Failure classification stayed
ahead of lookup: a regression demonstrated that restoring trust properties can
expose a request failure without changing its bytes. The later growing-activity
investigation below replaced that ordering with provenance-aware cache keys.

Three paired graphical runs on Emacs 31.1 with Aporetic Serif Mono and installed
Markdown grammars produced these median scheduled full-rebuild times:

| Transcript | Before | After |
| --- | ---: | ---: |
| Original session's final root segment | 404 ms | 277 ms |
| Separate 11.5 MB history | 2,013 ms | 663 ms |

The larger rebuild collected three times instead of eight. An explicit
collection after rendering remained small; profiler-reported allocation across
five rebuilds fell from 846 MB to 369 MB. Across all 45 child files, the batch
median scheduled rebuild fell from 171 ms to 102 ms. Rendered text hashes match
for every file and all 65 chronological checkpoints. Regression tests cover
property-only invalidation, malformed-tool diagnostics, expanded group children,
and composer preservation.

At the end of that pass, changed-history graphical probes took about 870 ms on
the large capture, with an observed timer delay around 820 ms. A 9.6 MB tool span in that capture
also keeps nearby live updates expensive until its activity group closes.
Full projection and mutable activity runs remain synchronous; these changes do
not establish a 100 ms latency bound. The protocol, source hashes, profiles,
per-event timings, and limits are in
`.scratch/full-history-responsiveness/report.md`.

### September 2026: large tools in growing activity groups

The next investigation isolated a 9,580,671-character tool span and the following
eight saved updates. A three-character append took 427 ms. Sampling found that
request-failure classification decoded the entire tool metadata payload before
looking in the rendering cache; phase timers also found about 130 ms in repeated
canonical scans. Activity grouping computed each tool entry for classification
and again for insertion, repeating full-content hashing.

Tool cache keys now include relevant provenance-property intervals, so an
unchanged key can skip failure decoding without hiding property-only restoration
of a request failure. A disposable control-line index lets the existing parsers
seek candidate markers without repeatedly traversing payload text. It grants no
trust and leaves their boundary and nesting rules in place. A per-activity entry
cache shares classification with insertion; source or session presentation changes
invalidate reuse, and callers receive independent coalescing counts.

Three paired graphical runs against the preceding full-history implementation
on Emacs 31.1, including redisplay, produced these medians:

| Workload | Before | After |
| --- | ---: | ---: |
| Eight updates following the large tool | 448 ms | 105 ms |
| Timer delay during those updates | 438 ms | 94 ms |
| Initial large-tool projection | 780 ms | 406 ms |
| Full history after an append | 885 ms | 377 ms |
| Timer delay during that full rebuild | 839 ms | 328 ms |

The timer probe measures event-loop blocking, not typing latency. Source and
restored-property comparisons agree on 57 frozen files and 1,200 generated
marker cases. The caches expire within their scan/activity; no new mutation
observers or global GC tuning were added. Large first arrivals and full rebuilds
remain synchronous, and some following updates still exceed 100 ms. Protocol,
profiles, correctness checks, and remaining limits:
`.scratch/large-tool-responsiveness/report.md`.

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

### September 2026: source revisions and distinct-agent batching

A subsequent investigation found that even a complete tool cache hit still
copied and hashed the entire source body. Complete ranges now use bounded
source-buffer revision identities and explicit provenance intervals; appends
retain preceding tools, while missed hooks retire identities conservatively.
Character and property changes are separate so normalization's property writes
do not invalidate unchanged payloads. Provenance conses are copied into keys so
an in-place property mutation cannot alter a stored key. Partial spans depending
on enclosing text remain uncached under their own range.

Three paired graphical replays reduced the median update after the 9.6 MB tool
from 98 to 36 ms, with matching history hashes. The eight-update allocation
profile fell from 361 to about 90 MB median (52--90 MB across three after runs);
these are temporary allocations, not retained heap. Changed full rebuilds improved only from 374 to 337 ms. The direct ToolCall
audit correction recorded in ADR 0111 prevents the discovered redundant payload
in new captures. The in-memory transformation retained identical full view and
expanded Eval text, but later validation found that its serialized fixture changed
other history after reload. The derived fixture's latency figures are withdrawn;
original-capture comparisons remain valid. Original captures are unchanged.

Independent per-agent timers also expired together. A per-view queue now drains
one distinct path per callback, coalescing repeated paths and cancelling on view
teardown. In two real-terminal-input runs of three waves of 32 distinct agents,
maximum observed key delay fell from 630--635 to 105--106 ms; completion time
increased from about 1.82 to 1.91 seconds. This does not bound the cost of one row,
source-patching bursts, or GC.

The initial cooperative turn-rendering and immutable subprocess prototypes were experiments.
Native threads alone did not make CPU-heavy classification or the existing save
path responsive. Waiting-thread and timer batches allowed input, while an
asynchronous subprocess isolated parsing and its GC. Exporting the large live
snapshot itself still took about 384 ms. These measurements support bounded
source representations and smaller work units; they do not justify moving live
view mutations or publication transactions into a worker thread wholesale.
At this stage production full rebuilds remained synchronous. Reproducible protocols, actual-input
results, lifecycle probes and limitations are in
`.scratch/bounded-responsiveness/report.md`.

### September 2026: resumable settled-history projection

The turn-batching experiment showed that returning to the command loop between
turns reduced input stalls on the unchanged original captures. Scheduled settled-history refreshes now use canonical
source preparation followed by a reader-prioritized projection and individual
turn callbacks. They retain source coordinates, not rendered payload copies.
One owner controls each mutation, including callbacks queued inside a yielding
writer. Source identity and modification ticks reject obsolete work; focus and
transport gates pause it. Cancellation releases timers and pending markers.

Each callback preserves the current draft, point, selection and window anchors.
Regression coverage includes typing between callbacks, source replacement,
reentrant writers, failure rollback, focus recovery, and expanded disclosures.
That coverage also exposed ordinary manually collapsed turns losing their fold
on a full rebuild. Those summaries now retain source identity, and ordinary turn
folds participate in disclosure-state restoration. Directive folds keep their
existing separate policy.

This does not establish a hard responsiveness bound: source preparation and each
turn remain synchronous. The archived 9.6 MB metadata span still dominates its
callback. Direct synchronous callers and in-flight reconciliation retain the
existing complete-projection contract. The implementation and actual-input
measurements are recorded in `.scratch/bounded-responsiveness/report.md`.
Three actual-input runs reduced the median worst key delay from 618 to 362 ms
on the archived stress capture and from 325 to 255 ms on the example session's
final root history. Elapsed work increased from 618 to 655 ms and from 325 to
343 ms respectively. Final history hashes matched for each capture. These
modest gains on long individual turns motivate dividing work below turn size.

### 2026-09-29: schedule display-only history replacements

Resume, rewind, segment rollover, control-transfer acquire and follow,
directive-request setup rollback, an interrupted side answer, and Source's
refresh after a Session Fork called the synchronous full render although no caller read view positions
afterwards. The compiled replay put a synchronous rebuild of the large root
capture at a 284 ms median worst key delay against 71 ms for the scheduled
job (agent capture: 326 versus 109 ms; small control capture: 141 versus
44 ms). These callers now request the coalesced scheduled render. The old
projection stays visible until the job's reader-anchored turns install, and
tests that inspect the view after these operations flush the scheduled
render synchronously.

A trial that moved chat prompts out of publication into the first callbacks
lowered the warm root replay's worst key delay from 139 to about 100 ms. It
was rejected: the root capture has eight prompts at roughly 3 ms each, so the
gain came mostly from where a collection landed rather than from less work,
and it left a window moved onto a pending response with the previous prompt
pinned until the callbacks caught up.

### September 2026: avoid copying first-arrival payloads before reading them

A profile of the unchanged archived 9.6 MB tool span showed repeated copies
before metadata decoding: the block parser copied and trimmed the entire
serialized plist, and structural recovery copied the source span a second time.
The parser now reads within bounds in the existing string; complete spans reuse
their initial source copy. Partial spans still recover the enclosing block, and
reader validation, metadata ownership and failure classification stay intact.

Three paired first-arrival profiles reduced median rendering from 369 to 241 ms
and temporary allocation from 225 to 177 MB. Three separate terminal-input pairs
reduced median worst key delay from 362 to 234 ms, preserving all 100 keys, the
multiline draft and identical history hashes. These compare against the preceding
turn-batching implementation, not the original baseline. Decoding remains
synchronous; the remaining reader allocation alone was about 88 MB in this
profile. This is an allocation reduction, not general lazy payload rendering.
The protocol and frozen comparison functions are recorded in
`.scratch/bounded-responsiveness/report.md`.

### September 2026: defer body formatting for collapsed tools

Collapsed tool caching previously discarded bodies only after the renderer built
them. A summary-only rendering context now lets Bash and ToolCall avoid that work
until expansion, without changing the renderer's argument or return types.
The built-in renderers keep their header and status logic on the summary path;
live Bash output still supplies its initially expanded body. This avoids a new
lazy-function body type and changes to every body consumer.

Three paired synthetic 3 MB result probes reduced collapsed Bash rendering from
142 to 89 ms and ToolCall from 231 to 89 ms, with identical complete-rendering
hashes. ToolCall temporary allocation fell from 66 to 27 MB. Three actual composer
input pairs on that synthetic ToolCall reduced median worst key delay from 233 to
101 ms. Ordinary pipeline results are capped at 30 KB for these tools: at that
size the measured savings were only about 0.1 ms for Bash and 0.4 ms for ToolCall.
The larger cases test archived/imported or otherwise oversized transcript bodies;
they are not claims about typical new tool results. The archived 9.6 MB span is
mostly metadata, which this change still decodes. Source-range parsing remains
the next boundary for deferring payload work.

### September 2026: targeted subprocess experiment

A follow-up exported only the original 9.6 MB tool span and its provenance,
computed a collapsed rendering in another Emacs, then installed the compact
result before the normal incremental update. The actual composer-input probe
included export, reply decoding and view application. Three-run medians were
240 ms elapsed / 241 ms worst key delay synchronously, 527 / 74 ms with an
initialized prestarted worker, and 1,134 / 70 ms with a cold worker. The worker
added about 196 MiB of resident memory at the measured checkpoint; it exited
after one job, so this is not evidence about a persistent pool's retained heap.

Plain `emacs-mule` export, even with Unix line endings, changed the captured
span's character count. An escaped Lisp-string snapshot preserved its character
count and content hash. Export then took about 49 ms and parent application
about 81 ms; the worker returned roughly 470 bytes. One parent GC remained,
alongside two worker GCs. The experiment rejected replies after in-span text or
provenance edits, retained them across appends outside the span, and cancelled
the worker with snapshot cleanup. History, draft and all keys matched.

This remains a prototype. It establishes a responsiveness/throughput/memory
tradeoff for this captured tool, not a general offload contract for custom
renderers or session-dependent presentation. Production rendering still uses
cooperative batches and source-tracked caches. The protocol and limitations are
recorded in `.scratch/bounded-responsiveness/report.md`.

### September 2026: staged native-thread preparation experiment

A selective decoder that skipped large nested result strings with the native
syntax scanner was slower than the ordinary reader on the archived metadata
(61 versus 31 ms), and accepted six malformed escape forms that the reader
rejected. It was not adopted. Reader validation remains intact.

An alternative kept the existing parser and inserted explicit 1 ms waits
between its stages in a Lisp thread. Three original-span composer trials gave
these medians, including normal view application:

| Experimental path | Elapsed work | Worst key delay |
| --- | ---: | ---: |
| Synchronous | 227 ms | 228 ms |
| Thread without explicit waits | 270 ms | 273 ms |
| Thread with waits between stages | 287 ms | 85 ms |
| Thread parses; main thread invokes renderer | 284 ms | 71 ms |
| Initialized prestarted subprocess | 543 ms | 77 ms |

All injected keys, multiline drafts and history hashes matched. The thread
variants collected twice versus once synchronously; shared-heap collection still
pauses the editor. The parse-only thread's RSS checkpoint was about 185 MiB;
the subprocess checkpoints were about 205 MiB in the parent and 196 MiB in the
worker. These are point measurements, not peak or retained-memory guarantees.
Worker startup is excluded from the prestarted measurement.

Keeping renderer invocation on the main thread preserved a custom renderer in
the caller's local registry. New threads do not inherit dynamic bindings, as
confirmed by the experiment's initially missing local environment switches and
the [Emacs thread contract](https://github.com/emacs-mirror/emacs/blob/emacs-31/doc/lispref/threads.texi).
The parse-only experiment rejected in-span text/provenance changes, accepted an
append outside the fixed span, and cancelled a verified paused thread in about
2 ms. That cancellation measurement starts at a yield point; it does not bound
interruption of an ongoing native operation.

This prototype kept the previous view until parsing finished. It demonstrated
that deliberately staged thread work can improve responsiveness without a second
Emacs process; merely moving the same function to a thread does not. The following
implementation replaced its global instrumentation with owned preparation jobs.

### September 2026: deferred first-arrival preparation

The integrated path shows a pending tool row, parses one large span at a time per
view, and publishes through the existing projection owner. Three paired trials
on the unchanged 9.6 MB archived span measured median initial display at 48 ms,
worst composer key delay at 65 ms versus 233 ms synchronously, and elapsed work
at 392 versus 232 ms. All 100 injected keys, multiline drafts and final history
hashes matched in each trial. This deliberately trades throughput for input
responsiveness. Collections increased from one (34 ms) to three (102 ms);
threads share the heap and do not solve GC pauses.

A lifecycle regression exposed that `sleep-for` can dispatch ordinary timers in
the worker: a separate experiment observed 18 worker-dispatched callbacks. A stale
job could then signal itself before replacing its pending row. Private worker
timer queues initially fixed that timer regression. A subsequent native-compilation
failure showed that `sleep-for` also dispatches process sentinels: the compiler's
sentinel tried to drain a main-thread-owned process from the parser worker and
raised "Attempt to accept output from process ... locked to thread". A real
subprocess regression reproduced the same failure without compiler customization.
Checkpoints now block on a condition variable released by the existing main-thread
advance callback, which checks attention and transport readiness before waking the
worker. This replaces private timer queues and polling waits, keeping both timers
and process sentinels on the main thread without spinning while paused.
Source edits, truncation, provenance changes,
cache eviction, queued jobs, focus loss, buffer death, mode changes, renderer
failure and expanded disclosures have regression coverage. Unrenderable complete
spans also fall back rather than being admitted repeatedly.

Refreshing only the active turn after preparation avoided a redundant full
projection; the first integrated replay had a 129 ms worst key delay before this
change. Historical publication retains reader-preserving batches. The native
reader and individual string operations remain indivisible, and hidden metadata
is still decoded in preparation. A separate summary/payload representation would
be needed to defer all payload decoding until expansion.

Complete paired rebuilds measured 163 versus 175 ms elapsed and 162 versus 73 ms
worst key delay for the example root segment. On the stress capture they measured
465 versus 830 ms elapsed and 464 versus 141 ms worst key delay; collections rose
from four to a median seven. Historical publication currently repeats projection
work, so throughput remains a cost of this responsiveness improvement. All draft,
key and history checks passed. Eight repeated preparations left no live parser
threads or timers; post-collection Lisp object accounting rose by about 18 KB,
with RSS settling near 206 MiB. These limited observations are not a leak bound.
Stale-source recovery also respects focus and transport gates; a regression caught
recovery redrawing an unattended view before that ordering was corrected.

### 2026-09-22: reuse display boundaries and ordinary audit expansion

The graphical capture exposed a reminder redraw that inserted its summary twice
and lost its audit face. Generic disclosure restoration retained the summary
while rendering a complete audit block below it. Restoration now uses the audit's
ordinary toggle path. Repeated redraw coverage retains the heading, face, draft
and cursor.

Scrolling profiles also showed table visibility walking display lines on every
idle check. `window-end` with an update request reuses completed redisplay and
handles variable-height rows; batch Emacs retains the line-motion fallback
because it has no glyph matrices. In a separate graphical replay, one check per
scroll fell from about 0.21 ms to 0.008 ms. Whole-scroll timings overlapped after
warm-up, so this is a local improvement, not evidence that all scrolling lag is
fixed. A graphical regression verifies that a tall display row keeps a table
outside the visible range and that scrolling exposes it without another redisplay.

### 2026-09-22: reuse pure audit decoding across projections

The rebuilt graphical capture still allocated about 291 MB in audit decoding.
The projection-local memo expired every redraw, while the existing buffer memo
covered only directive discovery. All audit readers now reuse the same bounded
buffer memo, after current provenance checks. Fork-point classification also uses
the buffer scanner with explicit bounds rather than copying a whole range.

On the captured 574-KB transcript, ten repeated classifications fell from about
40 MB to 7.5 MB of profiler-reported allocation; ten complete redraws fell from
154 MB to 120 MB. Median redraw time fell from 325 to 289 ms for all ten combined.
Rendered text and draft checks matched. These are isolated headless replays,
not a claim that graphical GC pauses or redisplay costs have disappeared.

### 2026-09-23: restore marks without activating editor hooks

The next graphical profile showed spinner preservation repeatedly entering
Evil's mark-activation hook. `set-mark` activates hooks even when restoring an
inactive, unchanged mark. Evil then schedules a distinct post-command callback
for every restoration; while the user is away these accumulate until the next
command. Window-state and managed-zone restoration now move the existing marker
directly and restore activation state explicitly, as logical-zone restoration
already did.

An isolated replay using the installed Evil reproduced 15,000 queued callbacks
and a 1.21-second drain on the next command. The changed preservation path queued
none and its hook drain took under 0.01 ms. Actual spinner and status-update
regressions preserve active/inactive selections, both endpoints and a multiline
draft without activating hooks. The aggregate graphical profile cannot attribute
the exact 1.25-second completion pause to this backlog; this fixes a reproduced
cause of delayed input, with live confirmation still required.
A separate graphical Emacs using the installed Evil took 1.20 seconds to execute
a synthetic keystroke and redisplay after the same buildup, versus about 2 ms
with the fix. This is a controlled reproduction, not the user's full configuration
or a provider-request replay.

### 2026-09-23: distinguish reader sections within shared source coordinates

Reader anchors previously counted every source-property run with the same data
start. A retained-tail render gives a leading separator the activity group's
source start, whereas a complete render gives it the enclosing turn's start.
Switching projections therefore changed which row an ordinal identified. A
deterministic replay reproduced the summary -> first tool -> summary cursor
jump without Evil; disabling retention in the probe removed it.

Anchors now use source start, section role and child discriminator, counting
ordinals only within that identity. Existing disclosure keys supply grouped
rows' standalone roles and compound-child identifiers; content hashes and
temporary in-flight tokens do not participate. Both capture and restoration
respect source, type and disclosure-key boundaries. Tail retention stays in
place. Regressions cover both projection directions, summary and child readers,
selections, window positions and a multiline composer draft.
