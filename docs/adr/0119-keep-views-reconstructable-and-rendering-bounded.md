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

Managed Bash output remains on its initiating source-backed row. Routine
successful empty-input polling rows are suppressed in the view, without removing
model-visible tool records. Durable execution presentation owns a single linked
completion breadcrumb for work that actually yielded, independent of polling
or mailbox delivery. The view resolves its link through the current transcript,
older segments, agent transcripts, or retained evidence; missing evidence is
reported rather than represented by a dead link. The breadcrumb has no duplicate
output body. Explicit output expansion, reader position, selection, and composer
draft survive progress, terminal settlement, and reconstruction. Nested ToolCall
rows follow the same visibility rules. This is a source-backed projection, not
a view-only completion registry or a change to process ownership. An idle
terminal event projects its already-persisted breadcrumb into the open current
view immediately; a closed or historical view reconstructs it through its own
source instead of borrowing the live view's projection.

Streaming updates retain completed semantic units and reconcile the mutable
tail. Every section, hook audits included, renders in its remembered fold
state, so restoration is a fallback that normally toggles nothing. Disclosure
restoration preserves an untouched final unit using its pre-restoration
boundaries and uninterrupted source identity; rewriting that unit or failing
restoration invalidates retention. Expanding an earlier section therefore does
not force complete-turn parsing on each subsequent chunk. Re-rendering the
mutable unit reuses work that its unchanged contents already paid for: tool
parses keyed by source span, archived breadcrumb keys keyed by archive content
identity, execution completions indexed once per transcript change, and path
links resolved once per projection. Boundary bookkeeping steps by property
runs rather than characters. A tool/reasoning/delivery activity run remains mutable until its surrounding
transcript boundary closes it; an individual completed call can still join a
growing group. Grouped rows retain their individual source identities across
changes in presentation. A settled direct ToolCall caches its recognized
specialist as the group presentation name while retaining its canonical
ToolCall child record. Collapsed summaries and expanded rows therefore agree
without reparsing hidden results or changing execution identity. Live/composed
programs and invalid/failed envelopes retain the outer presentation.
Nested compound rows retain their own metadata and
depth, so disclosure replacement removes descendants without consuming siblings.
Failed tool calls split activity groups and start
collapsed with a red `×`; warning rows remain groupable and mark their group
with `!`. Only markers receive severity highlighting, and an accompanying
sandbox warning cannot downgrade an error. Full rerender is the correctness
fallback. One scheduler coalesces redraws;
unattended views (unfocused, or shown in no window of an interactive editor)
arm no render timers and defer visual work, retaining changed tool IDs rather
than forcing a full rebuild on focus return. Incremental projection also drains
those row updates; full projection subsumes them. A settling or stopped turn
projects only its own range, so it keeps them: clearing them there left an older
row, such as a background execution's, showing stale progress. Retained-agent metadata
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
one timer for ordinary visible decorative frames and elapsed metadata, separate from
transcript rendering; it updates registered fragment-owned display spans and
never writes the authoritative transcript. Each one-shot callback rearms its
timer from the present, so a stall neither replays overdue callbacks nor
allocates a new timer at frame rate. Time-based frame selection skips missed
samples and retains phase across power changes. The foreground label
offers shimmer, breathe, bounce, dots, ellipsis, braille, ascii, and static;
pending-tool rows offer shimmer, braille, ascii, dots, or static indicators.
Color animations prepare bounded, theme-derived frame banks; each live view
reserves enough banks for its registered tool rows, overflow row and main label,
with a four-bank minimum, in addition to a six-bank shared reuse cache. The
visible-tool row cap bounds this working set. All eligible banks are prepared
at semantic boundaries, preventing other views from evicting an active sample.
A mixed set of color and portable glyph tool rows keeps both cadences. A frozen
row retains its phase while adopting changed text or a destination display. Theme changes and edits to the
spinner, its inherited faces, or the default face invalidate both; observing
resolved-color dependencies matters when a paused, zero-fps view has no timer
to discover updated Customize colors.
Breathe and bounce retain their 3.6-second cycle. Ordinary text uses the accepted
8-fps ceiling; eligible native surfaces, and ordinary text while PGTK frames
present only what changed, use the configured rendering ceiling.
Glyphs advance every 240 ms (braille/ascii), 480 ms (dots) or 960 ms
(ellipsis); colorless and low-color terminals use the glyph
fallback cadence rather than waking at the color rate. Even resolvable face
colors cannot distinguish the prepared shades on an eight-color terminal;
terminal palettes below 256 colors therefore fall back to glyphs. Braille and
dots use same-width ASCII substitutes at the same cadence on displays missing
their glyphs. Dots check
only frames showing their target span and cache glyph support until the next
semantic tick (or theme invalidation), not at every decorative frame. A global
reduced-motion switch disables decorative updates, not semantic progress or
elapsed metadata; semantic redraws retain the last displayed sample, and
unchanged tool rows are not rebuilt at their initial frame. Hidden and vertically offscreen indicators suspend decorative wakeups. Animation
eligibility uses overlap of a registered span with the completed redisplay's
visible buffer range in an attended window. Partially visible spans are retained;
horizontal clipping and partial pixel scrolling do not refine that decision.
This intentionally permits decorative updates to horizontally clipped labels
on visible rows. Ordinary animation keeps pixel-position queries and
per-frame redisplay hooks out of its hot path. Native presentation additionally
checks geometry at semantic boundaries and guards stale pixels before redisplay;
it never adds a Lisp callback per native frame. The October 2026 measurements
below explain both the ordinary visibility policy and this native exception.

The elapsed suffix independently retains its one-second semantic refresh when
its range is eligible. Window focus and range eligibility must hold together:
a visible target in an unfocused frame cannot borrow attention from another
frame where the target is offscreen. The transcript's separate buffer-wide
attention gate remains unchanged. Theme and display-frame events repaint
eligible labels at their displayed or frozen phase, including zero-fps and
paused labels; offscreen labels wait until they return. Frozen glyphs recheck
display fallbacks without scheduling decorative motion. A move between frames
uses the destination palette or the portable multi-frame fallback.

Periodic housekeeping and metadata share host wakeups. Aligning separate timer
deadlines alone still allowed redisplay between their callbacks on PGTK, so one
UI-host timer now dispatches due callbacks together. GC maintenance, process-exit
recovery and quiet progress retain whole-second updates; telemetry attributes
each callback separately. Its lag heartbeat samples every 100 ms using a
native timed condition wait where supported, so a worker releases the
interpreter lock without dispatching process sentinels or redisplay. Emacs
owns mutex restoration, stack rooting and cancellation. The native boundary
prevents advice, GC and debugger callbacks from consuming its one-shot timeout.
Only PGTK Emacs loads the module for this: elsewhere a timer wakeup that
changes nothing does not redraw. Unavailable native waits or worker creation
fall back to the shared main-thread timer, every 100 ms, or once a second while
a PGTK frame exists; timer callbacks count their own run time as lag. Ordinary
worker sleeps remain unsafe because they dispatch process sentinels.
Metadata-only
views join the whole-second clock. Decorative Lisp animation and output-driven
progress retain their natural deadlines. Missing execution rows trigger recovery
only when their source, or a compound ancestor's source, is in the transcript;
progress remains cached until source arrival and normal boundary projection.
Cancellation removes an owner's work without affecting other owners, and the last cancellation removes the host timer.
Missed periodic observations are skipped after a stall. Batches yield between
callbacks for input or after 25 ms. The host timer stays armed while callbacks
run, so a callback that waits for input or output lets its due siblings run, as
ordinary timers would; a running callback never re-enters itself. Layout hooks avoid scheduling idle callbacks when visible tables
and images are already current.

Focus, window and ordinary buffer-scroll events rearm scheduling. The one-shot
timer uses the Emacs UI host's top-level timer list: TRAMP's temporary binding
neither discards rearms nor hides a queued timer from cancellation. Window
replacement and deletion release power subscriptions, including frozen
tool-only progress with no timer to discover that its window disappeared.
Package install/uninstall owns the global focus and face-change observers.

On Linux PGTK/Wayland the first view makes all of Emacs's frames present only
what changed: the native module has GDK paint each toplevel through GL,
redirects the Emacs executable's own drawing calls to record their damage, cuts
Emacs's whole-frame redraws to it, and keeps GLib dispatch on the main thread,
where the GL context is current. It refuses executables whose imports it cannot
account for. The October 2026 entry below records why it lives in mevedel.

Eligible Linux PGTK/Wayland views optionally use independent native surfaces.
The animation module supplies timed, styled samples; a small C module owns
bounded buffers, GLib scheduling, Pango drawing and Wayland presentation without
calling Emacs APIs from native callbacks. Lisp owns setup, placement and fallback.
The ordinary renderer covers unsupported displays, unavailable build dependencies
and unsuitable geometry. Native presentation coalesces at the parent redisplay
boundary: intermediate row removal/reinsertion never presents, including outside
`inhibit-redisplay`, and unchanged labels reuse surfaces across marker rebuilds.
Creation/movement stays synchronized until the parent's frame callback, then
switches to independent presentation. Position is parent-owned pending state;
an independent child commit before that boundary can flash at `(0, 0)`.

Prompt previews stored for header-line redisplay are limited to 512 characters,
including an ellipsis when truncated, after mailbox filtering and whitespace
normalization. The original prompt and its navigation target remain in the
source-backed transcript. Header width measurement never traverses a complete
multi-kilobyte prompt on each evaluation. The archived-prompt check walks the
window body only when an archived prompt exists and keeps its answer per
window until the window, buffer or invisibility changes: a Claude Code request
spent 13% of its editor CPU in that walk, though no session had been compacted.

The default `auto` power policy uses the Emacs UI host's battery information:
external power permits the configured 30-fps normal ceiling, and battery,
backup, unknown or stale power information uses the conservative 15-fps
saving ceiling. Ordinary decorative frames require full redisplay and the
measured pgtk build presents the whole surface, so the shimmer is cadenced: one one-second sweep
every four seconds, resting in between with no frames scheduled, and
pending-tool rows sweep their verb and tool name in the same frames. The
October 2026 measurement below records why. `full` forces the normal ceiling; `save` forces the saving
ceiling. The lower ceiling never speeds up a naturally slower animation or
changes its cycle duration. A saving ceiling of zero freezes decorative
motion while active elapsed time and status remain current. While the request
waits for user input, every indicator holds its phase (shimmer and breathe at
their rest) and no frame is
scheduled. A shared battery
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

### October 2026: separate grouped presentation from canonical tool identity

A direct Eval row expanded as Eval but collapsed into “ran 1 script” because
its group cache retained the outer ToolCall name. Group summaries now cache the
already-recognized specialist name, while the canonical child stays unchanged.
This fixes the mismatch without adding result parsing to collapsed redraws.

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

Superseded by the October 2026 range-overlap decision below.

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

### Work-oriented execution results

Showing the same completion as a terminal WriteStdin result, an independently
delivered Bash mailbox card, and a changed original row made background work
hard to scan. The original row now owns output; only commands that actually
yielded add a durable, compact completion link. Polling and delivery still
retain their authoritative model-visible records. This supersedes separate
expandable completion-card presentation without changing the mailbox payload
or agent settlement.

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

### 2026-09-29: allocation of a settled refresh

Every compiled replay of a scheduled refresh contained exactly one collection
of 57-72 ms, and wherever it landed was the worst key delay; without it the
slowest step took 56-59 ms. A batched root refresh of the 2 MB capture
allocated 49.7 MB, so at the editor's 64 MB idle-tuned threshold most
refreshes collected. Exclusive allocation attribution found repeated whole
transcript work: each prompt rescanned every audit payload for guest
attribution, each audit lookup copied its payload out of the buffer to key
the decode memo, each decode created a temporary buffer, and every content
segment was cleaned in full only to prove it was not glue. Guest attributions
are now reused per modification tick, payload decodes are found by position
within a tick, decoding reads from the string, and a segment that visibly
begins with content skips the glue test. Batched refresh allocation fell to
40.0 MB for root, from 29.7 to 24.2 MB for the agent capture and from 21.8
to 19.1 MB for control. The replay still collects once per root refresh at
its 32 MB threshold.

### 2026-09-29: subprocess offload re-measured

The subprocess experiment's gain was keeping collections out of the editor.
With collection deferred during typing, the compiled replay's median worst key
delay is 43-45 ms for every scheduled refresh, and the largest remaining step
is the insertion of one rendered turn, which must happen in the editor. A
worker's floor was measured on the same captures with compiled mevedel: 52 ms
to start Emacs, 194 ms to load mevedel, 215 ms to re-derive text properties
the parent already holds (they do not cross a file), and 72 ms to scan and
group the root capture, about 530 ms before it could return a plan, against
709 ms for the whole in-process refresh. The grouped turns also reference
buffers, so a plan needs a translation layer to cross at all. A prestarted
worker would still pay the property derivation and about 196 MiB of memory.
Offload can therefore lower neither the worst key delay nor the settlement
time of these refreshes, and it is not built.

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

### October 2026: remove pixel-level animation visibility probes

The October 3 session CPU profile attributed 52.3% of nonempty CPU samples to
`mevedel-view--animation-span-in-window-p`, including 40.0% under the suspended
animation redisplay hook. Pixel-position queries also reentered expensive
header and mode-line evaluation. Visibility checking accounted for 56.6% of
samples beneath scheduled rendering; that request recorded five 2.57-6.43 second
rendering stalls, four with input pending. The aggregate profile cannot assign
a particular sampled callee to each individual stall.

Buffer-range overlap replaces the precise clipping policy, and its unused
probes, observers and changing-prefix metadata are removed. The accepted cost
is occasional decorative updates to horizontally clipped spans on visible rows.
The previous policy saved those writes but spent substantial CPU deciding to
save them. A 512-character stored prompt preview also bounds the width work
amplified by the position probes. No visibility cache or new scheduler is needed.


### October 2026: preserve intact tails across disclosure restoration

The former conservative fallback invalidated retention whenever restoration
toggled any section. A graphical replay of the October 3 transcript spent about
96 ms per streamed update, and a minimal regression reproduced repeated rendering
of an earlier expanded thinking block. Its final response unit was intact.

The renderer now captures the final unit before restoring disclosure state.
Markers follow shifts caused by earlier sections; a first-character witness
rejects prefix replacements that would otherwise leave a homogeneous suffix.
Retention survives only if the original unit remains one complete
source-property run. A split, merge,
deletion or failed restore still invalidates it. Scanning for the last fragment
only after restoration remains unsafe because it can pair a partial view unit
with the whole source unit, duplicating or dropping content on the next chunk.


An unprofiled graphical replay alternated three before/after pairs on the saved
1.62 MB transcript with eight expanded disclosures and twelve streamed updates
per phase. Median queued-input latency fell from 104.57 ms to 5.92 ms; p95 fell
from 115.19 ms to 97.42 ms, including the initial full rebuild after expansion.
The visibility stress loop fell from 976.91 ms to 0.256 ms for 2,000 checks,
with 5,000 glyph queries reduced to zero. This isolates the changed paths; it
does not reproduce the user's complete configuration or prove each original
multi-second freeze is gone.

### October 2026: keep a long live activity unit cheap to re-render

The visibility fix left the October 3 stalls unexplained under the user's real
session: its replay used a detached transcript, so agent transcripts, archived
segments and session publications were unreachable. A batch replay restored a
copy of that session, loaded its pre-rotation segment as the live transcript,
expanded the live turn's 73 collapsed sections and streamed its final 71 KB
back in 74 property-run chunks. Every update took 0.36 s (median), reading two
or three archived segments.

Four causes compounded. Expanded hook audits rendered collapsed and were
re-expanded by restoration, which invalidated the retained tail, so each update
rebuilt the whole 700 KB turn. With retention working, the retained unit was
still one 40-call activity group covering 464 KB of source: breadcrumb
deduplication re-read and reparsed every older archive for each fresh breadcrumb
(57% of render samples), each Bash row reparsed every audit record in the
transcript, and audited tool spans failed the cache's exact-span test so all
tools were reparsed. Boundary recovery also walked the view a character at a
time even when the retained tail made its result unused.

Hook audits now render their remembered state; archive breadcrumb keys, the
completion index and tool parses are cached as described above; recovery is
skipped on the retained path and steps by property runs. A graphical replay of
the same stream, alternating the committed and changed modules, measured
render plus redisplay per update. With every section expanded the median fell
from 0.36 s to 0.07 s and p95 from 0.40 s to 0.10-0.11 s; queued input
followed the same figures. With the default folds the median fell from 0.10 s
to 0.03 s. Activity grouping is unchanged: a later call can still join the
growing group, so its rows are still re-inserted on each update. Incremental
group insertion was not adopted, because the measured worst case stays below
the 0.15 s render debounce.

### October 2026: cadence the shimmer

On the user's Emacs 31.1 pgtk build (KDE Wayland, 2x scale, a 1536x888
frame), a timer that does nothing but fire ten times a second held `emacs -Q`
at 23-36% CPU and the compositor at about 21%; a 400x300 frame cost 3%.
Every wakeup ends in a redisplay that presents the whole frame surface,
whether or not anything changed, so a frame's cost scales with the window's
area rather than with the animated span: about 2% editor CPU and 1%
compositor per wakeup per second on that frame.

A continuous shimmer at the 60-fps ceiling could not keep up. It reached
about 31 fps and held the editor at 67-82% CPU and the compositor at 30-37%
during an attended request. Measured in the user's editor with the request
held in "Thinking...", the label cost 70% at 60 fps, 62% at 30 and 34% at 12;
a static label cost 8%.

The ceilings were first lowered to 12 and 6 fps. A side-by-side comparison
in the user's own theme showed visible stepping at 12 fps, so that was
reversed. The shimmer now follows Codex's cadence: 0.6 seconds after the
request starts, and every four seconds after that, a cosine band at least
six columns wide crosses the label in one second; the label rests at its
dimmed shade in between and no frame is scheduled (the resting contrast was
subsequently reversed as recorded below). With new 30/15-fps
ceilings (60/30 before), the same label costs 29% (22% at 15 fps). Tool rows used a
continuous braille glyph; they now shimmer their verb and tool name ("Calling
Bash"; the whole row swept too fast on long commands, and "Calling" alone
looked detached), in the same frames as the label, so a pending tool adds no
wakeups of its own. Breathe, bounce
and the glyph styles still pay their cadence throughout (breathe and bounce
64% at 30 fps, braille and ascii about 30%); their cheaper cadences are
recorded below.

### October 2026: arm indicators after their spans are drawn

Animation eligibility uses the completed redisplay's window range. A label
or tool row inserted since, and every `window-scroll-functions` call, still
see the previous range, so a request started in an attended, unscrolled
view never armed its timer and the label stayed frozen, elapsed time
included, until a focus or scroll event. A scroll back to the label froze it
the same way. Both now recheck once after the next redisplay; a span still
outside the window waits for scrolling, as before. Review found that a
resize (`delete-other-windows`, a taller frame) can reveal a span without
running `window-scroll-functions`, so `window-size-change-functions` rechecks
the same way.

The same review measured a disposable editor streaming prose at eight words a
second above the progress row: under both renderers the label was judged
hidden in about half of the samples taken while text arrived, and froze
between semantic renders. Streamed text was inserted at the label's start marker, which did not advance, so the span
absorbed the reply and its start lost the label property. Start markers now
advance past insertions (rewrites of the label recapture them), and the label
animated in every sample while text streamed; the native presenter opened two
surfaces in six seconds instead of tearing its surface down after each
insertion.

Each animation frame rewrites a display property and advances
`buffer-modified-tick`. Header-line caches key on a content tick that
ignores those decorative writes; otherwise the archived-prompt check walked
the window body on every frame. Waiting for user input holds every
indicator still, as the elapsed time already was.


### October 2026: retain every tool shimmer label

Independent review reproduced two failures introduced when tool indicators
changed from a shared glyph prefix to distinct shimmer labels. The scheduler
prepared only the first visible tool, and the old four-bank local limit could
not retain the default five tool labels, overflow row and main label. A display
change or another view evicting shared banks left later rows frozen. Preparation
now covers every eligible label; local retention scales with the bounded row set
and shrinks when rows leave. Color and portable glyph rows contribute separate
cadences, so shimmer's rest cannot stall a glyph in another display.

The overflow row also retained its fragment identity when its count changed.
Restoring its complete previous display string hid the new count indefinitely
at zero fps. Restoration now regenerates changed label text at the retained
phase, preserving reduced motion without suppressing semantic updates. Tests
reproduced both failures before the fixes and cover mixed displays, frozen
tool-only frame changes, cache eviction and composer preservation.


### October 2026: slower continuous styles after visual comparison

A side-by-side comparison in the user's own theme retained the original motion,
12-fps color / half-speed glyph variants, and 8-fps color / third-speed glyph
variants. The user preferred the original appearance but accepted 8 fps for
breathe/bounce and half-speed glyphs as the performance tradeoff; third-speed
glyphs looked laggy. Those accepted rates now define the natural cadences.
The continuous color cycle remains 3.6 seconds, glyph cycles take twice as long,
and shimmer retains its smooth cadenced sweep. No burst-and-pause behavior is
introduced for continuous styles.

Review also reproduced a paused shimmer advancing during visibility refresh:
input pause stopped its timer but did not freeze its sampled phase. Input pause
now participates in the same presentation freeze as reduced motion, including
already-delivered callbacks and palette/display changes. Actual Ask lifecycle
coverage verifies stopping and rearming while preserving a multiline draft.


### October 2026: retain normal animation text contrast

The dimmed resting shade made pending tool labels look washed out next to
ordinary text in the user's light theme. Shimmer now rests at the label's
normal foreground, with only the moving cosine band fading toward the
background. After confirming that appearance, the user requested the same
contrast for the other color styles: breathe starts at normal foreground and
fades out and back, while bounce moves a faded band over normal text. This
reverses the original contrast without changing cadence or frame count. Glyph
styles already retain normal text. Light and dark themes use the same
foreground-to-background fade.


### October 2026: independent native animation surfaces

Reducing Lisp allocation did not remove the dominant cost. At approximately
1545x864 logical pixels and 2x scale on KDE Wayland, a no-op Lisp timer and
prepared text at 30 Hz both used about 55-56% editor CPU. A native GLib timer
without drawing used about 1.3%, but a small GTK drawing widget and Emacs 32
Canvas still paid the expensive parent presentation cost (about 50-55%).
A separate desynchronized Wayland subsurface used about 2% at 30 Hz without an
Emacs patch. The integrated presenter uses the animation module's exact samples
rather than the experiment's approximate gradient.

This changes the earlier decision to express every decorative frame through
ordinary text redisplay. It preserves that renderer as a display/geometry
fallback and keeps request, power and semantic state in Lisp. Native presentation
restores smooth configured rates for continuous color styles; accepted contrast
and glyph cadence remain intact. Initial complete mock-request tests measured
12.75% for native bounce near 30 fps versus 24.5% for ordinary bounce at 8 fps.
A tool workload revealed redundant surface recreation inside temporary row
removal; coalescing at the existing projection boundary removed that per-second
churn. A repeated sleeping-tool workload measured 22-23% with native animation
versus 38.7% ordinary and 18.7% warmed static. Native and static runs had the
same Lisp redisplay count, while native main motion ran at 30 fps instead of
8 fps. The measurements and experiments were machine-local working material
and are not retained.

The user's preview exposed a top-left flash on refocusing that lifecycle counts
had missed. Wayland traces showed child images committed before their parent
position: 17 premature commits in the reproduction. Synchronizing initial and
moved surfaces until the parent frame callback removes this race; subsequent
native frames remain independent. The trace check passes and the user confirmed
the refocus flicker is gone. Pending callbacks are destroyed with their surfaces.


### October 2026: share request wakeups and require recoverable source

Native animation removed per-frame Lisp redisplay, but independent housekeeping,
quiet progress and metadata callbacks still incurred whole-surface presentation.
A bounded callback/redisplay trace showed that equal timer deadlines alone did
not prevent redisplay between callbacks. These owners now share a dispatcher;
missed observation ticks are skipped, individual callback telemetry is retained,
and dispatch yields between callbacks for input or after 25 ms. Output-driven
progress and ordinary decorative animation retain their separate cadences.

Window-change notifications no longer arm an idle Markdown pass when visible
layout is already current. Execution progress still updates its cache immediately,
but a missing tool row is recovered only when its authoritative source exists;
a repeated incremental render cannot reconstruct source that has not arrived.
Normal source-arrival projection, nested source-owning tool IDs and narrowed
transcript buffers remain supported.

A corrected mock with unique tool IDs and matching compiled dependencies measured
real sleeping-Bash requests at 18.17–19.33% editor CPU before these changes and
8.58–10.08% afterward on the same Emacs 31.1 PGTK/Wayland setup, retaining 30-fps
native status motion. The average reduction is about 50%; a provisional strict
10% per-sample target narrowly failed. Those samples ran on the change before it
was combined with the streaming-label review fixes; the combined tree measured
8.25% and 9.92% for the same workload, 7.75% for a silent request (12.0% before)
and 41.5% while prose streamed at eight words a second (44.3% before), all with
native 30-fps motion. Earlier repeated-request mock samples
reused a tool ID and could find a previous source row, so they do not establish
fresh-tool performance. Protocol, source hashes, full-suite results and separate
request/execution teardown checks were machine-local working material and are
not retained.

### October 2026: pace streamed reads

After the wakeup sharing above, streaming was the largest request cost:
prose at eight words a second cost 36% editor CPU with a static label and 32
words a second 59%, against 6% for a silent request. A CPU profile attributed
about 90% of the samples to no Lisp frame: the pgtk presentation after each
redisplay. Emacs redisplays after every non-empty read of process output
(`wait_reading_process_output`), and pgtk queues a whole-widget draw at the end
of every redisplay with no frame parameter to disable it, so the cost scales with
the provider's chunk rate. `process-adaptive-read-buffering` changed nothing
measurable. The stream bridge now stops a local curl process with SIGSTOP after
each read and continues it when the insert batch is due; the server's output
waits in the kernel socket buffer. The batch grew from 0.2 to 0.4 seconds, the
view's stream render delay, and a flushed batch renders from a zero-delay timer
in the same wakeup, so text appears 0-0.4 s after arrival instead of 0.2-0.6 s.
gptel's sentinel treats every call as the end of the request, so stop and
continue events are filtered out before it. gptel settles a request, and runs
its tools, only when curl exits, so pausing after the final read delayed every
response's end by up to the batch; a finished response (a stop reason new to
this tool round, since gptel never clears the previous one, or `data: [DONE]`)
continues at once, even when the read that stopped curl held only part of the
final burst. The isolated loading test caught the delay: its Emacs exited
while curl was still stopped. Review measured two more costs and both were
fixed: pausing after every read capped a 1.1 MB stream at one pipe read per
pause (15.6 s instead of 0.02 s), so only reads under 4 KiB pause (0.42 s);
and a stopped curl outlived `kill-emacs`, so paused processes continued from
`kill-emacs-hook` and on uninstall (the hook was later replaced by a guard
process, below). Pacing applies only to mevedel's own streams. An end-of-stream
experiment (100 small events) now finishes 0.18 s after the unpaced run,
against 0.56 s before. Reading the resumed burst inside
the resume timer saved only 6 of 94 redisplays and was not kept.

The native presenter dominated what remained with animation on: checking each
surface's pixel placement with `posn-at-point` before every redisplay took 30%
of samples while text streamed. The check now runs only when an input that can
move a surface changes (text, window start, point, size, scroll, focus, region,
face remapping, wrapping, line spacing, the frame font or the surfaces), per
window. The pinned-prompt header's on-screen memo keys on the same text-layout
inputs; without them a wrap or text-scale change left a header stale until the
next edit or scroll. A surface whose target a streamed
render replaced and displaced is moved instead of reopened, the presentation's
rearm callback reuses the result it just computed instead of placing again,
and the last eight prepared timelines are reused.

Measured in a disposable editor at 32 words a second, with every streamed word
verified to arrive in order: static label 58.8% to 17.0-17.4%, native bounce
22.0%; at eight words a second 35.6% to 15.6-16.0% and 41.5% to 21.1%. The
layout key uses the frame's character size: `window-font-width` realized faces
on every redisplay and cost 14% of samples. A silent request and
a running Bash command are unaffected (no stream).

The coalesced queue's review also changed: TRAMP's retained-timers wrapper held
a newly armed host timer for the whole remote operation, freezing every shared
callback, including the telemetry heartbeat, which then reported false lag; the
wrapper leaves the host armed. A coalesced timer without a time is rejected
instead of breaking the queue, the heartbeat reads its next due time from its
timer, uninstalling cancels the queue, and a progress interval above one second
keeps at least that spacing while staying on whole-second ticks. A coalesced
callback that waited still delayed its siblings, unlike ordinary timers, until
the entry below.

### October 2026: pace Claude Code streams

A scripted ACP peer that relays a separate producer, as Claude's adapter
relays its CLI, streamed text at 8 and 32 chunks a second: 31.6% and 67.4%
editor CPU after the gptel pacing above, against 19.7% and 22.6% for paced
gptel streams. Each chunk is its own read and redisplay, and acp.el arms a
zero-delay drain per message. `mevedel-acp.el` now stops the adapter after a
small read holding only `agent_message_chunk` or `agent_thought_chunk` frames
and continues it at the next batch: 18.5-20.1% and 22.1-23.6%, with every
streamed word verified in order. The original revision cost 66.8% and 75.4%.

Pausing the adapter can reorder its frames against MCP tool calls, which
reach Emacs on their own socket: the CLI writes its message receipt through
the adapter before calling a tool, and mevedel requires that receipt first.
Only a steady stream pauses, three streamed-text reads in a row; any other frame
keeps the adapter running and restarts the count. A tool phase alternates
short status text with tool frames, and pausing after each text delayed every
step: a scripted memory review with tool batches no longer settled within its
five-second test budget. Every
message to the adapter, the end of a turn and an MCP call continue it, and an
MCP call that finds the adapter paused waits up to 50 ms for its frames;
the continue event alone ends `accept-process-output`, so the wait loops until
output arrives. Pauses are capped at 0.5 s: a test with a 60-second batch
stalled the turn. The pause and resume primitives moved to
`mevedel-transport.el`, shared with the gptel stream bridge.

A real Claude Code request (Sonnet, about 1200 words of prose, no tools)
confirmed the cost and showed that the scripted peer was too simple: the real
adapter sends every delta twice, as an ACP `agent_message_chunk` and as the
SDK's raw `stream_event` `content_block_delta`, about 30 deltas and 38 reads a
second. Counting only ACP chunks as text, pacing never engaged. Treating the
SDK's content deltas as streamed text too (its message receipts are other
types), the same request took 96 reads instead of 733 and 27.5% editor CPU
instead of 74.5%; compositor CPU fell from 36.9% to 13.8%. The same prompt
through gptel's ChatGPT (Codex) backend cost 28.9% unpaced and 25.3% paced: that
endpoint delivers about five reads a second of 2.6 KB, so the mock's 32
chunks a second overstated gptel's real cost and its pacing gain.

### October 2026: continue paced processes after a crash

The exit hook ran only on a normal exit. An Emacs killed by SIGKILL or a
crash left a paused process stopped forever, holding its socket. Emacs 31
starts every child, pipe or pty, in its own session (`setsid`; `ps` shows
PGID = SID = child PID), so the kernel's orphaned-process-group rule, which
sends SIGHUP and SIGCONT only when a group's parent is in the same session,
never applies. Killing a disposable batch Emacs confirmed it: a stopped
`curl` streaming from a local server (still holding its socket) and a stopped
`node` child stayed in state `T`, reparented to the user's subreaper. A pty
child is continued by the terminal hangup, but gptel's curl and acp.el's
adapter both use pipes, and a remote adapter's local process is an ssh client
on a pipe.

`mevedel-transport.el` now starts one guard per Emacs on the first pause: a
`/bin/sh` loop that reads, one line per pause or resume, the IDs paused at
that moment. Whenever Emacs ends, its pipe closes and the guard continues the
last set; continued, curl and the adapter find their pipes closed and exit.
The guard ignores SIGHUP, because a normal exit hangs up every child, and
announces that with a line Emacs waits for: a guard hung up during its first
millisecond died first and left the child stopped in an isolated test. A
process stops only after the guard has its ID, so a failed guard disables
pacing instead of risking a stuck child. Resuming lists only what is still
paused, so a stale ID that the system later reuses is never continued. The
exit hook was redundant and is gone.

Resume now continues a live process regardless of its recorded status. Emacs
records a stop only after its SIGCHLD handling, and a resume in the same Lisp
execution as its pause skipped the continue in 199 of 200 trials, leaving the
child stopped with no pause recorded.

Stopping reading instead (`set-process-filter` to t removes the descriptor
from Emacs's wait set) would have left nothing to continue, but Emacs neither
reads nor drains a process whose filter is t when it exits: a child that
wrote its last line and exited during such a pause lost that line before the
sentinel ran. A stream ends exactly that way. A kernel-side continuation
(`PR_SET_PDEATHSIG`, a shared session) needs control over the spawn, which
gptel and acp.el own.

Pacing is unchanged: a 160-event stream at 32 events a second took 160 reads
unpaced and 19-24 paced, before and after. A pause and resume costs about
50 microseconds instead of about 6 for the two signals; the guard starts in
about 1 ms.

### October 2026: prebuilt native module

Without the native presenter, the final defaults cost 22.7% editor CPU while
waiting, 21.1% with Bash running and 32.9% for a 32-chunk stream, against 6.2%,
7.0% and 21.5% with it, and a failed build was silent. A failure is now
reported once per session with the next step, and `mevedel-view-native-install`
downloads a prebuilt module from the GitHub release for the current source
hash. CI builds it on Ubuntu 22.04 against Emacs 27's `emacs-module.h`: Emacs
only grows the module environment and the module checks for at least the size
it was compiled with, so the older header loads into Emacs 31; the newer
header's module would be refused by older Emacsen, and the module uses only
Emacs 25 functions. A download is loaded only when its SHA-256 matches the pin
CI commits to `native/prebuilt.eld`, so trusting it means trusting a reviewed
commit, not the release assets alone. The download is an explicit command:
the build is first attempted inside animation scheduling, where prompting is
unsafe. A local end-to-end check with compilation disabled installed the
container-built module from a local server and animated a request natively.

### October 2026: a lag heartbeat that does not redisplay (replaced)

This thread-based choice was replaced after the review below reproduced
process-sentinel dispatch on the wrong thread. The measurements here record
why it was originally chosen.

The half-second telemetry heartbeat was the last request-long timer that
existed only to observe. Emacs redisplays after every timer it runs
(`wait_reading_process_output`), and pgtk's `frame_up_to_date` hook flips and
queues a draw of the whole frame after every redisplay; X builds flip only a
frame that needs it. On the desktop (Emacs 31.1 pgtk, KDE Wayland), an
`emacs -Q` frame of 1536x888 logical pixels used 1.7% CPU idle, 15.6% with a
no-op 100 ms timer (216 redisplays in 15 s) and 0.9% with a Lisp thread waking
every 100 ms in `sleep-for` (7 redisplays). A thread wakes without redisplay
because `sleep-for` does not display, and it needs the global lock, which the
main thread releases only while it waits: a busy loop makes it late, exactly
what the heartbeat measures. It now samples every 100 ms instead of 500, so
stalls shorter than the old interval are seen too.

`sleep-for` runs due timers in whichever thread waits, so the heartbeat binds
the timer lists to nil; a test fails without that binding. It records its own
events, so their time is the stall's, and reports input that arrived since the
previous sample rather than calling `input-pending-p`, which would read
display events in that thread. A blocking remote operation waits with the lock
released, so the thread runs on time through it; the outermost operation on
the main thread is counted as late as it took. The timer heartbeat saw only
the part of such a wait during which TRAMP had unbound the timer list.

### October 2026: frozen color styles rest

Freezing for input or a zero saving ceiling kept the last displayed sample of
every style. A shimmer frozen during its one-second sweep therefore held the
faded band, and a breathe its dimmed shade, for the whole pause; the sweep had
been designed to end at the normal foreground. Freezing now moves the label's
and the tool rows' retained phases to the end of the sweep (shimmer) or the
start of the cycle (breathe) and repaints spans already drawn mid-motion.
Glyph styles and bounce keep their phase, which shows a glyph or a band by
design, and so does a shimmer or breathe drawn by its glyph fallback: resting
would move its glyph. Resuming continues from the clock, as before.

### October 2026: deliver siblings while a coalesced callback waits

Dispatch suppressed arming the host timer while any callback ran, so a callback
that waited for input or output held every sibling until it returned; ordinary timers run during such a wait. Dispatch
now tracks the running callbacks instead: the host stays armed while they run,
a tick inside a wait delivers due siblings and skips running callbacks, and the
next arm targets the earliest non-running member. A regression test waits for
process output in one callback while a sibling falls due.

Reusing the host timer object across arms broke yielding. Emacs's timer pass
works on a copy of `timer-list` and reruns any ripe timer in it not marked
triggered; reactivation clears that mark, so a batch that yielded for input
or its 25 ms budget was resumed in the same pass, before input and ordinary
timers. Every arm creates a new host timer again, which no running pass holds.
Compiled, a tick costs 7.2 µs against 6.8 µs with reuse for one member and
the same for five; a regression test delivers through the real timer pass.

### October 2026: other displays need no native presenter

Native surfaces cover pgtk only, so the cost on other displays was measured
before considering another backend. In a headless nested KWin (6.7, a
3200x2000 output at 2x), an `emacs -Q` frame of 1568x884 logical pixels ran
Emacs 31.1 pgtk and the same release built for X with GTK 3 under Xwayland.
Editor CPU, pgtk against X: idle 1.2% and 0.1%; a no-op 10-Hz timer 14.5% and
0.2%; an 8-fps color change on an 11-character label 11.9% and 0.1%; the same
at 30 fps 41.1% and 3.0%, with the compositor at 16.9% and 2.0%. The nested
pgtk figures match the desktop session's (15.6% for the 10-Hz timer). The X
build's `frame_up_to_date` flips only a frame that needs it and draws only
damaged glyphs, so ordinary text animation costs little there. No native
backend is planned for it. The 8-fps breathe/bounce ceiling and half-speed
glyphs, accepted to bound pgtk's cost, also apply under X, where they save
little; raising them there was left out because the measured setups run pgtk.
Measurements of presentation need a presenting compositor: a locked session
turns its outputs off, and the same probes then cost nothing on pgtk too.

### October 2026: native surfaces follow layout, background and their own lifecycle

Several native gaps were found by review rather than reported. Placement was
rechecked only when the character tick, window geometry or remapping changed,
so a disclosure toggled by text properties, an overlay hiding or inserting
text, or a changed invisibility spec left a surface at its old position until
the next edit or scroll. The layout key now also holds the content tick (text
properties without decorative frames), the invisibility spec and the
extent and hiding or inserting properties of overlays between the window start
and the last surface. Overlay edits change no Lisp-visible tick; Emacs's own
overlay counter is internal.

The surface painted the frame's default background through the remapping of
whichever buffer the frame's selected window showed, since selecting a frame
makes that buffer current. It now paints the background redisplay draws at the
label's start: overlay faces for every window or for that window (`hl-line`
among them) by priority, then text faces, then the view's remapped default.
Translucent frames keep ordinary text animation: an opaque box would show over
their translucent text, and a translucent one would show the text it covers.

The module closes a surface itself when its frame unmaps or changes scale, or
drawing fails, and Lisp noticed only at its next semantic update; tool rows
without an elapsed-time label might never have one. The pre-redisplay guard
now asks the module whether each surface is live, which a redisplay after
remapping or rescaling reaches. A Wayland display that closes closes its
surfaces and drops the cached globals, which otherwise pointed into a
disconnected display; GDK emits `closed` before disconnecting, so the
proxies are still valid then. This path is a safeguard, not exercised end to
end: pgtk keeps a display's terminal when its last frame is deleted, a lost
connection exits Emacs, and an Emacs 31.1 pgtk daemon aborted on
`delete-terminal` for a frame that had been displayed or unmapped, with or
without the module. A move while the position
callback of an earlier move was pending kept that callback, which belongs to a
parent commit that may still carry the old position, so samples could run
free there; each move now waits for a callback requested after it. An error or
refused surface for one label closed every surface opened before it, and the
next semantic update reopened them all, about once a second; such a label now
falls back alone. A refused surface is remembered by its signature (label,
style, font, colors and geometry) and tried again once one changes; a label
whose preparation signaled stays with text until it changes or animation
stops. Remembering the label alone, as first written, kept a real Claude Code
turn on text for its whole duration: its label's first placement reported a
height of 0 before the row was laid out, which the module refuses. Such a
placement is now no placement at all, retried at the next update.

A profile of a streamed request (in the harness of the next entries) put
10-13% of samples in native placement: two `posn-at-point` walks from the
window start per check, and a second check in the same pre-redisplay right
after presenting had placed every surface. The check is now skipped after
presenting, and the label's end is measured with one screen line of
`vertical-motion` and `window-text-pixel-size` instead of a second walk, 0.7
ms of a 1.2-ms placement. Placement fell from 160 to 60 of about 1,250
samples; editor CPU from 12.0% to 10.9% at 8 chunks a second and from 13.0%
to 12.0% at 32 (three interleaved rounds each).

These paths had only mocked coverage. An integration test now runs the module
in a pgtk daemon against a headless nested KWin when `kwin_wayland` is
installed (it is skipped elsewhere, including CI). It opens a surface, moves it
twice before its position callback, moves it with text and an invisible
overlay above, closes it behind Lisp's back and requires the next redisplay
to replace it, and checks the Wayland trace for the translated position and
for desynchronizing only after a parent callback requested since the last
move. Restoring the previous move, liveness or overlay behavior each fails it.
`x-focus-frame` also aborted that daemon, so it relies on the compositor
focusing new windows.

### October 2026: one timer pass per streamed batch

A streamed batch still cost three timer passes. Emacs redisplays after each
pass over its due timers, and `timer_check` runs only the timers that existed
when the pass began, so the zero-delay timer that rendered a flushed batch ran
in a second pass after a redisplay; the paced process's resume timer, due at
its own time, added a third. The batch's timer now runs the render itself once
the batch is inserted, through `mevedel-gptel-stream-bridge-after-flush`, and
continues the paced process feeding it (curl or the Claude Code adapter); its
resume timer remains the fallback when nothing is queued. The read that paused
that process started the batch, so the flush continues every process due to
continue within half a batch delay, at least 100 ms to cover the adapter's
50 ms margin. Continuing every paused process instead
read each of N concurrent streams, such as parallel agents, N times per batch
delay; a stream continued early now falls in step with the one flushing, and
the two share their passes from then on. A flush that fails still renders and
continues.

In the headless nested KWin harness (Emacs 31.1 pgtk, a 1536x888 frame, native
bounce label, a mock OpenAI-compatible stream through the real curl pacing),
three interleaved rounds gave, in editor CPU: silent request 4.8% before this
day's changes, 3.7% after (the lag heartbeat); 8 chunks a second 15.9%, 14.5%
and 11.2% with this change; 32 chunks a second 16.7%, 15.7% and 12.9%.
Compositor CPU fell from about 5.2% to 3.5%.

Reading the resumed burst inside the same pass (waiting up to 15 ms for
output) cut redisplays from about 140 to 100 per 15 s but not CPU: 11.5% and
11.6%. Two redisplays within one frame interval share one GTK presentation, so
the burst's own redisplay was already nearly free, as the earlier experiment
found; it was not kept. The rest of a stream's cost is the batch's real
change, rendered and presented about 2.5 times a second.

Real Claude Code turns (Sonnet, about 900 words of prose, no tools) read the
paced adapter about 6.2 times a second: median 700 bytes, 90th percentile
7.5 KB, largest 19 KB after a pause. Text reaches the transcript from acp.el's
drain, a pass after the read, so its batch fell due just after the adapter's
own resume timer and each ran in its own pass; the adapter now stays stopped
50 ms past the batch delay, and its flush continues it (separate resume
passes fell from 13 per 32 batches to 3 per 46). Such a turn cost 16.5-16.8%
editor CPU with native animation; it had run on text animation at
19.6-22.6% while the native failure memory still keyed on the bare label.

### October 2026: PGTK presents only what changed

Every per-wakeup measurement above traced back to one cost: on PGTK each
redisplay presented the whole frame. A perf profile of a no-op 10-Hz timer
in a 1568x884 frame at scale 2 found two causes. `unblock_buffer_flips` calls
the terminal's `buffer_flipping_unblocked_hook` for every frame at the end of
every redisplay, and PGTK's flips and queues a whole-widget draw there,
whether or not anything was drawn (`frame_up_to_date` does the same; X builds
flip only a frame that needs it). And on shared-memory buffers GTK 3 may not
reuse a buffer the compositor still holds; KWin holds the committed one until
the next commit, so every presentation allocated a new whole-window buffer and
backfilled it (`pixman_blt` and page faults), about 11 ms per frame. Damage
tracking alone therefore helped little: an Emacs built with it still used
11.7% for the timer (from 15.1%). GDK's GL presentation (EGL, dmabuf) with
damage-limited redraws took it to 0.4-0.6%.

GL then crashed Emacs: Emacs's `xg_select` and `pgtk_read_socket` dispatch
GLib on whichever thread waits, so a Lisp thread in `sleep-for` (the telemetry
heartbeat) ran GTK's paint, `eglMakeCurrent` failed off the thread holding
the context, and GDK segfaulted. Stock Emacs with `GDK_GL=always` crashes
the same way. Dispatch must stay on the main thread, and the context must not
stay current there: Emacs runs `gtk_main_iteration` itself while waiting for a
frame's map event, so a Lisp thread showing a frame painted with the context
still current on the main thread and segfaulted (exit 139). Every dispatch and
iteration now releases the context, so GDK makes it current on whichever
thread paints; at a 30 Hz change editor CPU was 6.0-6.2% with the release and
6.1-6.2% without. Stock pgtk itself deadlocks when a thread shows a frame, now
and then (1 in 19 runs; 3 in 3 after another thread waited in `sleep-for`): the
thread waits in `gtk_main_iteration` for the GLib context while the main thread
waits in `thread-join`. The module did not change that rate (2 in 28), so the
test showing a frame from a thread runs in a daemon of its own and skips a
timeout; a crash still fails it.

An Emacs patch did all three and passed mevedel's suite, but changes to Emacs
need a rebuild on every machine and distribution, and LLM-written
contributions are not accepted upstream. The native module reaches the same
result in a stock Emacs: `gdk_window_create_gl_context` on each toplevel, and
the executable's GOT entries (all `GLOB_DAT` on Arch's LTO, `-fno-plt`, full
RELRO build) for its drawing calls, `gtk_widget_queue_draw`,
`gdk_window_create_similar_surface`, `cairo_get_target` and GLib dispatch.
Only Emacs's own calls change. Its damage is each drawing call's extents within
the clip; a frame counts as shown once its draw handler painted the newest
surface, so a resize before the flip redraws whole. The visible bell draws on a
copy of the frame's surface, which records no damage, so the module also
redirects `cairo_surface_create_similar` and redraws whole while a copy of a
shown surface lives; scrolling destroys its copy before queueing the draw.
Without that the bell was never shown (no commit in its phase, 15 stock).
Executables importing a drawing call it does not track are refused rather than
left with stale pixels.

Editor CPU in the nested KWin harness, stock Emacs 31.1 without and with the
module: idle 1.1% and 0.0%; a no-op 10-Hz timer 14.1% and 0.3%; a word
inserted 8 times a second 12.6% and 2.1%, 32 times 41.1% and 7.0%; a 30-fps
color label 36.6% and 3.1%; a Lisp thread waking every 100 ms beside 8
insertions a second 2.5% and 0.3%, without a crash. On the desktop session:
the timer 15.3% and 0.4%, insertions 13.0% and 1.9%; KWin's own share fell
from about 6-7% to 1.4%. mevedel streams at 8 and 32 chunks a second cost
9.6% and 11.8% stock with native animation, 2.7% and 4.0% with the module,
and 2.7% and 3.2% with the module and text animation. Waiting with the
default shimmer cost 3.4% stock native, 10.0% stock text, 0.7% module native
and 1.2% module text; with bounce, 4.6% stock native (30 fps), 14.9% stock
text (8 fps), 0.9% module native and 4.5% module text (30 fps). Screenshots after insertions, scrolling, region and mode-line
changes, a translucent frame, a moving child frame and a resize matched both
a full `redraw-frame` and stock Emacs pixel for pixel.

With the module the native presenter saves half a point for the cadenced
shimmer and three and a half for continuous bounce at 30 fps. It stays, and
is to be measured on the other machines. Text breathe/bounce now use the
power-aware ceiling while the module is active, since their 8 fps existed to
bound stock PGTK's per-frame cost: 30-fps text bounce costs what the stock
presenter did.

### October 2026: review of the rendering and wakeup series

A real subprocess regression disproved the safety of the worker heartbeat.
`sleep-for` dispatches process sentinels as well as timers; private timer lists
only excluded the latter. A sentinel draining its main-thread-owned process
from the heartbeat raised "Attempt to accept output ... locked to thread".
Emacs provides no timed condition-variable wait for this worker. The heartbeat
therefore returned to the existing coalesced main-thread timer, preserving
100 ms sampling, remote-operation attribution and post-request tails, while
removing its worker lifecycle. The native-wait follow-up below supersedes
this fallback-only decision. The earlier thread measurements remain history,
not evidence that the unsafe mechanism should stay.

The safety change has a measured cost. Twelve isolated daemon runs on a
headless KWin used compiled code, a 1510x840 frame at scale 1, three interleaved
rounds, and 15-second idle measurement windows after warm-up. Each window
recorded 149 or 150 samples. Median editor CPU with stock PGTK rose from
0.107% for the thread to 3.407% for the timer; with native presentation it
rose from 0.070% to 0.607% (timer range 0.543-0.665%). The shared timer was
chosen for correct process ownership, not as a performance improvement.
These idle measurements do not establish streaming or HiDPI costs; the
non-native fallback retains a material wakeup cost.

Review also found two incomplete geometry assumptions. The pinned-prompt memo
invalidated after text scaling, but its canonical-row walk could still place an
offscreen prompt inside a graphical viewport; graphical displays now use
Emacs's pixel visibility query, with horizontal and pixel scroll in the key.
Native labels now include overlay faces, priorities, window restrictions,
pixel scroll and frame opacity in their cheap layout snapshot. An existing
surface falls back to text when the frame becomes translucent. The nested KWin
fixture verifies changed backgrounds, pixel movement and transparency fallback;
its position oracle now requires the moved child's own parent callback.

Freezing now distinguishes actual color samples from their portable glyph
fallbacks. Only color samples move to the resting phase; the last displayed
glyph remains unchanged even after its window disappears. Native loading
failures remain owned by the loader, so installing after a failed build lets
presentation retry at the next view. Installing with a module already active
does not replace its bindings or process-lifetime hooks. The published binaries
were verified against both checksum pins and require glibc 2.34; the earlier
2.27 claim was contradicted by their imported symbol versions.

The same-pass stream path remains. Its ACP margin now applies only to positive
numeric batching delays, preserving disabled batching. The collection wait
callback checks that its job is still current: transport can restore a timer
cancelled while suspended, and that callback previously rearmed orphan work.
An additional real host-timer test confirms coalesced callbacks still deliver
their siblings during a wait without re-entering themselves.


### October 2026: timed native sampling and teardown verification

The fallback-only heartbeat decision above preserved correctness but imposed a
material, measured CPU cost even while nothing changed. Increasing the sampling
interval or watching only command boundaries would reduce sensitivity to stalls
in process output, redisplay and other C work. Instead, the native module now
adds a timeout to Emacs's existing condition wait. Only the executable's
`pthread_cond_wait` import is redirected; a one-shot thread-local deadline
selects `pthread_cond_timedwait`, and every ordinary wait delegates unchanged.
Emacs still owns interpreter-lock release, stack rooting, mutex restoration and
thread signals. The 100 ms cadence, callback attribution and post-request tails
remain. Unsupported imports, unavailable modules and failed thread creation
retain the safe coalesced-timer fallback, described below.

An adversarial test changed the initial implementation: `funcall` may collect
before entering the condition-wait primitive, and a GC hook waiting on another
condition consumed its deadline, leaving the intended wait blocked indefinitely.
The native boundary now captures the unadvised primitive and guards GC,
debugger entry and quit during the short call setup. Dynamic bindings belong to
the waiting thread; the main thread retains its normal GC settings while it
waits. Tests exercise the actual module in isolated Emacs processes, including
this failure, advice, independent deadlines, notification, recursive mutexes,
ordinary waits, thread cancellation and main-thread process-sentinel ownership.
No caller must arrange these guards or know the redirection mechanism.

Display-close validation also separated module behavior from two Emacs PGTK
bugs. Closing a terminal leaves its Wayland descriptor in the input wait set;
the next event-loop wait aborts even without the module. Directly closing a
terminal with live frames additionally leaves a scale timer referring to freed
display state. The stock regression deletes frames first and reconnects before
waiting, exercising real GTK display closure and fresh Wayland bindings without
those host defects. Removing the module's closed-signal subscription makes the
regression crash. A disposable Emacs build with the descriptor and scale-timer
fixes passed 20 direct live-terminal closures with a 100 ms event-loop wait
after each. No installed editor was modified and no presenter change was needed.

Two interleaved 15-second observations per configuration used frozen bytecode,
real local HTTP/SSE through gptel's curl transport and mevedel's stream bridge,
a 1510x840 logical frame and verified display scales 1 and 2. CPU is a percentage
of one editor core, excluding the compositor, server and startup. At scale 2,
the timer/native-wait medians without native presentation were 12.40/0.46% idle,
24.36/12.96% at 8 chunks per second and 27.64/17.51% at 32. With native
presentation they were 0.92/0.10%, 4.33/3.53% and 9.61/8.43%. The old unsafe
worker's scale-2, non-native 32-chunk median was 17.00%, so these measurements
show the timer regression largely removed, not perfect equivalence to the old
worker. Idle native counters also exposed roughly 150 extra intercepted paint
entries per timer observation, absent with native waiting.

Streaming checks required every numbered chunk to reach both authoritative data
and the projected view in order. Send-to-view-projection latency includes curl
pausing and insertion batching: a token emitted while curl is paused can wait
one 0.4-second interval before being read and another in the next batch. P95
remained around 0.8 seconds across samplers. This is buffer-projection timing,
not compositor scanout, and does not establish physical ARM or battery costs.

The same candidate also compiled without warnings and executed in aarch64
Emacs 31.1 under QEMU, including the guarded-wait and telemetry fixtures,
native drawing, display closure/reconnection and threaded presentation.
QEMU's device translation forced software EGL, so this validates ARM
instructions and lifecycle behavior, not accelerated dmabuf presentation or
physical-device performance.

### October 2026: the PGTK fallback samples once a second

Two reviews replaced the unsafe worker independently. The other proposed a
whole-second heartbeat on the shared clock everywhere, with timer callbacks
timing their own run time: 0.00-0.07% idle editor CPU with native
presentation and 0.27-0.33% without, close to the native wait's 0.10% and
0.18-0.46%. It was not adopted as the only sampler because it sees a
sub-second stall outside timer callbacks only when the stall happens to delay
a timer. Of the 37 recorded sub-second `event-loop-lag` events in local
sessions, 25 were not explained by a callback's own run time.

The native wait therefore stays the sampler, and that design serves only the
fallback. Without the module on PGTK, a 100 ms timer cost 2.8% idle at scale
1 and 12.4% on HiDPI, as presentation itself is stock there. While a PGTK
frame exists the fallback timer runs once a second and callbacks count their
run time since the previous heartbeat. Other displays and frameless Emacs
keep 100 ms, where a wakeup that changes nothing does not redraw. Telemetry
loads the native module only on PGTK, so an X11, terminal or headless Emacs
never compiles it for sampling or reports it missing.
