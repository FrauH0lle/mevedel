# Make Rewind in-place undo

Status: accepted

## Current decision

Rewind truncates the current session and transactionally restores captured files
in its current checkout. It creates no child session or parallel checkout.
Conversation Fork preserves an alternative conversation; Worktree Fork also
provides isolated file coexistence. [Sessions](../sessions.md#rewind) owns the
commands, impact display, restoration procedure, and recovery contract.

Every accepted model turn, including the first, has a pre-turn checkpoint.
Ordinary chat and directive turns share one chronological undo chain even though
directive content is excluded from ordinary-chat model context. Entry points
choose a boundary appropriate to the object the user names:

- Rewind at an assistant response keeps that response's turn.
- The prompt picker returns to before the selected prompt, discarding its answer
  too; selecting the first prompt can empty the session.
- **Rewind before this implementation...** discards the directive attempt and
  every later chat or directive turn.

One candidate state determines the surviving turn count, transcript cutoff,
activity pruning, and file checkpoints. Confirmation names both sides of the
boundary and every known capture gap. Rewind can overwrite external changes to
captured files after approval; it cannot restore uncaptured effects, Git HEAD,
or the index. Failure before commit rolls back the session/file transaction;
rollback failures remain visible for recovery.

Current session configuration and user-authored workspace directive records
survive. Rewind clears live workflow ownership whose tasks, Goals, agents, and
handoffs have no reliable per-turn journal. Discarded directive attempts lose
their answers, feedback, and patches; consumed subdirectives return. Surviving
activity and the current authored request determine the directive's state, and
restored files determine anchor reattachment. A changed request can therefore
leave a directive Ready even when older activity survives. Success starts a
`SessionStart(rewind)` context epoch without ending the live session; cancelled
or rolled-back operations start no epoch.

Portable project sessions expose Redo by republishing a selected immutable head
as a new head. It restores the head's conversation artifacts, sidecar state,
instruction snapshots, retained agent transcripts, persisted tool results, and
captured working-tree files. It neither recovers uncaptured effects nor
resurrects workspace directive records pruned by Rewind. File sessions using
PID locks have no published heads and no Redo.

Redo preparation and confirmation read immutable bytes without reserving the
lease. The reserved transaction then rechecks the current head and confirmed
file plan, backs up and restores files, and commits the replacement head;
pre-commit failure rolls files back. The picker groups heads by settled turn
count and fork-point identity, offering the newest generation of each state and
excluding the current state and incomplete turns.

Publication collection is scheduled at turn settlement and lease-acquiring restore.
It keeps the current head, journal capture pins, settled-turn representatives,
a recent-generation grace window, and
their exact referenced artifact files. A retained artifact does not keep its
original manifest or unrelated files alive. Marking validates one retained
manifest per slice; all obsolete heads are retired before payload deletion.
Deletions are batched into one target control program per slice.
Ordinary transcript readers have no read-pin protocol: an old non-boundary reader outside the grace
window can receive an absence or hash failure.

Resume and settlement schedule collection in bounded idle slices. A scan must
cover all generations before deletion, and a changed head restarts it. Retention
criteria remain unchanged; deletion reserves ownership and is capped per batch.
Repeated saves within a turn reuse checkpoint versions and timestamps. Snapshot
names address raw contents, so equal before/after images across paths and turns
share stored bytes. Immutable published heads retain their own references. PID
stores collect unindexed generated content only after sidecar commit, preserving
the previous durable state when a save fails.

## Rationale and consequences

An in-place operation makes one checkout and its conversation return to one
boundary. Creating an apparently recoverable conversation variant while changing
its shared checkout gave two histories incompatible file states. Directive
patches remain review evidence rather than a selective undo stack, avoiding
restoration that silently clobbers intervening work from other directives.

Rewind and Redo share user vocabulary, but use different evidence: Rewind derives
turn-exact boundaries from prompt/checkpoint history, while Redo selects stored
publication states that may no longer appear in that history. Unifying their
storage machinery would exclude PID-lock sessions, lose boundaries without a
published head, or offer partial streamed responses as complete turns.

Immutable publication makes Redo possible without consuming the state it leaves.
Collection must follow artifact references rather than age alone because new
manifests can retain bytes in older generation directories.

## Decision history

All revisions below belong to ADR 0072. Directive transcript representation was
subsequently replaced by [ADR 0091](0091-render-directive-turns-in-the-shared-session-view.md);
its shared chronology and workspace-owned identity constraints remain.

- **Initial choice:** Rewind was in-place, discarded the selected turn, and
  offered no redo variant. This avoided misleading parallel conversations over
  one rewound checkout. The no-redo decision predated portable publications.
- **Portable Redo, 2026-08-26:** after a Rewind appeared to lose four turns, the
  previous immutable head still held a 530 KB transcript, three retained agent
  transcripts, snapshots, results, and sidecar: all 14 artifacts matched their
  SHA-256 values. This changed the storage constraint behind “no redo.” Redo
  republishes that state, including captured files, within the same coverage
  limits as Rewind.
- **Selection boundary, 2026-08-26:** a user selected response S1 T1 expecting to
  keep it and instead got an empty session. The original uniform “discard the
  selected turn” rule did not match what a response selection meant. Response
  selection now keeps the turn; prompt and directive-before selection discard
  it. Explicit confirmation discloses the boundary.
- **Turn labels and collection, 2026-08-26:** a four-turn session offered about
  100 generation rows in Redo. Grouping by settled turn state removed save-level
  duplication. One measured day held 101 generations and 67 MB, including 54 MB
  of superseded transcripts; reference-aware collection removed 88 generations
  and retained 13. Keeping referenced directories avoided copying unchanged
  artifacts on every publication merely to reach one directory per turn.
- **Complete collection, 2026-08-27:** the initial settlement-only collector
  deleted at most 32 generations, one remote control call per deletion. A
  two-day session generated four to five publications per minute during long
  turns and lost its final settlement on suspend; 421 of 465 generations
  (366 MB of 401 MB) were collectible but unreachable by another settlement.
  Batched deletion removed the reason for the cap, and collection on restore
  covers work whose last turn never settled. Retention criteria stayed intact.
- **Complete-scan cost:** reading 752 uncached generations took 17 seconds in
  one settlement versus 0.12 seconds with immutable generation facts cached.
  Collection still scans every sidecar: coarse target timestamps can put a
  referenced generation outside an arbitrary newest-N window. The original
  manual supplied no date for this measurement.

- **Startup latency, 2026-09-19:** the confirmed resume's cold scan read 279
  manifests and sidecars (346 MB) in 22.1 seconds. A warm cache does not help
  first resume, so synchronous restore collection was replaced by coalesced
  idle slices, cancelled on close or lease loss. Each slice checks ownership
  and publication stability before advancing. Eight-directory deletion batches
  bound cleanup work without imposing a total backlog cap. The same session
  contained 6,680 file-history entries for 51 files: repeated autosaves minted
  new logical backups for one turn. Checkpoint names now remain stable within
  that turn, and obsolete current-manifest names are removed during saving.

- **Snapshot duplication, 2026-09-21:** a current-writer replay alternating two
  128 KiB contents over 12 turns stored 24 files (3 MiB), despite correctly
  reusing versions on repeated saves within each turn. Names now use the full
  content digest, while the checkpoint index retains every turn, path, and
  version. This replaces path/version storage names without changing the reader's
  opaque-name interface or rewriting existing history. PID cleanup follows the
  sidecar commit so shared bytes and failed-save recovery stay valid.

- **Obsolete-name publication, 2026-09-21:** a complete private copy of the
  example archive took 97.2 seconds to close and save, issuing 6,470 control
  programs. Most renewed the lease for tombstones, despite those names causing
  no fixed writes. Skipping those renewals reduced this to 4.8 seconds and 20
  programs, retaining the final ownership proof and manifest commit. Portable
  snapshot writes also omit the unused fixed copy: Rewind and WorktreeFork
  already read immutable publications. Existing archived caches are untouched.
  The remaining profile exposed quadratic artifact deduplication and manifest
  overlay. Hash membership preserves last-write ordering without rescanning
  prior entries. On the same clone, deduplication fell from 2.47 seconds to
  3 ms, generation assembly from 1.25 seconds to 162 ms, and close/save to
  1.16 seconds. All retained checkpoints remain addressable.

- **Collector heap, 2026-09-21:** caching complete manifests for a 335-generation
  capture retained 1.37 million artifact entries and grew the live Lisp heap by
  459 MiB. Collection now caches only generation references, the sidecar locator,
  and transcript size, alongside the existing small sidecar facts. Heap growth
  fell to 5.5 MiB and a warm scan from 1.72 seconds to 45 ms; cold work fell from
  37.7 to 33.3 seconds. Full summary hashes and retention decisions matched.
  These are scan-work measurements, not continuous input stalls: production
  collection remains sliced. Caller-owned reference lists preserve journal pin
  release behavior without allowing destructive retention operations to corrupt
  the cache.

- **File-level collection, 2026-09-21:** whole-directory reference retention
  kept obsolete manifests and sidecars whenever one artifact in their generation
  was still needed. On a private copy of the complete example archive after
  restore, the same head-retention policy needed 11 of 336 heads and 6,794 files.
  Exact-file marking reduced publication storage from 1,139,941,005 to 404,652,906
  bytes; every retained file matched its pre-collection digest. The cold scan
  fell from 31.19 to 1.38 seconds. Collection took 23.92 seconds across 715
  explicitly advanced idle steps, the largest 172 ms; a single manifest read
  can still exceed the nominal slice budget. This replaces directory closure,
  without a manifest format change or rewriting historical artifacts. Retiring
  every obsolete head before sweeping payloads preserves discoverable history
  if deletion is interrupted, including references across generations. New
  journal pins or a changed head invalidate the plan before further deletion.
