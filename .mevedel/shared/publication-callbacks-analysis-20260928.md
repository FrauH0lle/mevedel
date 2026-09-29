# Publication callbacks and legacy storage: production evidence (2026-09-28)

Task: analyze the logs of session `2026-09-27T16-24-5ca69305960c` (spinner Goal,
about 25 h) for the backlog item "Bound remaining publication callbacks and
legacy storage", plus any other issues found. This complements
`spinner-telemetry-analysis-20260928.md` and `spinner-cpu-analysis-20260928.md`
and does not repeat their suspend or C-source-prompt findings. This is an
analysis only: no product code, session data or live state was changed. Live
Emacs was only queried read-only (`emacsclient -e`). It had restarted at about
20:58, so no in-memory collection history survived.

Reproduction scripts: `.scratch/publication-callbacks-analysis-20260928/`
(`lag.py`, `prof.py`, `obs.el`, `ret.el`, `side*.el`). The CPU-profile stacks
come from the earlier decoded
`.scratch/spinner-cpu-analysis-20260928/run-20260927T181246-1b68ad79.json`.
Percentages of **R** use its 21,255,047 displayed counts.

## Priorities

1. **Collection starves for the whole length of a Goal. Storage is unbounded,
   and nothing records it.** The sibling Goal session
   `2026-09-27T19-53-1d10b37d4a5b` (accepted-plan worktree, 23 Goal turns)
   holds **3,070 generations / 3,067 manifests / 4.9 GB** of publications.
   Offline, running the real `generation-summary` and `--retained-heads`
   functions keeps only **25 heads**. The policy is correct; the job just never
   ran to completion.
2. **Write amplification is the main publication cost.** Each generation
   writes a median of 1.34 MB of new bytes. Most of that is whole-file copies of
   data that barely changed. This is the likely source of the GC pauses the
   backlog attributes to "publication writes, sometimes including GC".
3. **Many of the event-loop lags look like GC, and telemetry cannot confirm
   it.** Lag events do not record GC activity, and 43% of them name only
   `anonymous`.
4. **Two synchronous subprocess pollers run on timers**, and one is documented
   as free.
5. **Legacy fixed storage is negligible** (about 12 MB of fixed caches, against
   6.2 GB of publications). Reclaiming it should be deprioritized.

## 1. Collection starvation

Observed:

- In the 19-53 Goal, the gap between one turn's `request-teardown` and the next
  `request-queued` is 0.1–0.2 s. Each Goal turn runs for about 1–2 h with
  about 130 publications. Example: teardown 22:26:44.896 → queued 22:26:44.986.
- `mevedel-session-collection--step` re-arms without doing anything while
  `mevedel--current-request` is set, or while any publication is pending,
  queued or active. The step is an idle timer at `current-idle + 0.2 s`.
  So during a Goal there is effectively no window, and every head change
  restarts the scan anyway.
- After the last turn (teardown 20:21:51) the session stayed open until the
  Emacs exit at 20:57:47 (lease `released`, renewed-at 20:57:47). Nothing was
  deleted. The cause of that final 36-minute miss **cannot be determined**:
  there is no collection telemetry at all (no event types exist). Likely
  candidates are these silent exits:
  - A failed deletion proof clears `:operations`/`:directories`. The job then
    cancels quietly, and a new schedule happens only at the next settlement.
  - A stale job (`:worker` or `:pending` left set) turns every later
    `collection-schedule` into a no-op, because of the `gethash` guard.
  - Warnings go to `*Warnings*` only, and that buffer died with the restart.
- The cost of starting collection grows with the backlog. For this session the
  cold pre-scan (the `generation-observations` child) took **89 s of batch CPU,
  with 8.9 s of GC**. It reads 3,067 manifests (88 MB) and every published
  sidecar (1.39 GB). The child's reply was 6.4 MB against its 8 MiB cap (77%).
  A slightly larger backlog would make the child fail. Its error text would
  read "Journal worker exited with status 0", which is misleading, and
  collection would be cancelled.
- The target session 16-24 (also a Goal) is fine: 4 manifests remain,
  99 MB, all referenced by its head. It was resumed four times (lease
  generation 5), and restore schedules collection. That is the likely reason it
  was collected. The analyzing session 15-18 is at 875 MB / 49 manifests and
  still running.

Suggested direction (not implemented):

- Let collection run between publications while a request is active. Being in
  a root request is not the actual hazard; the pending, queued and active
  publication checks already guard the real races.
- Alternatively, collect only history older than the last settled turn, which
  does not change when the head moves mid-turn.
- Emit `publication-collection` telemetry: start, scan complete, plan (retained
  and candidate counts), each deletion batch, proof failure, cancel reason and
  completion. Collection is currently invisible.
- Make the child's size failure explicit (`reply too large`), or stream or page
  its observations.

## 2. Write amplification per publication (19-53)

New bytes written by each generation, grouped by logical kind:

| Kind | Copies | MB |
|---|---:|---:|
| Agent transcripts (whole file) | 1,625 | 1,685 |
| `session.meta.el` sidecar (every generation) | 3,067 | 1,387 |
| Live segment transcript (whole file) | 1,167 | 1,247 |
| file-history snapshots | 7,055 | 505 |

Peak rate: 407 generations and 739 MB in the 09:00 hour on Sep 28.

The sidecar is about 1 MB, and most of it is redundant:

- `:agent-registry` is 598 KB. There are 40 completed agents, each entry about
  25 KB. The largest field is `:configuration`, about 17 KB (the full agent
  definition including the system prompt). Only **2 distinct configurations**
  exist among the 40.
- `:agent-transcripts` is 420 KB. Of that, **406 KB is per-tool `:activity`
  history** (18–438 items per completed agent). Live metadata keeps only 5
  items (`mevedel-agent-conversation--live-activity-limit`), but the
  persisted copy is unbounded.
- The target 16-24 sidecar shows the same shape: registry 467 KB, transcripts
  263 KB.

Inference: every publication prints, hashes and writes about 1 MB of sidecar
plus 1–2 MB of transcript, synchronously. The cost per write is small in CPU
samples (`publication-publish` is 0.26% of R; leaves are `write-region` and
`secure-hash`). The allocation adds up to GC, though. This matches the backlog's
"publication writes, sometimes including GC", and GC is not attributed to a
caller in the profile (see section 3).

The cheapest local fixes, both inside persistence:

- Store agent configuration once per distinct value, for example keyed by a
  hash.
- Bound or omit `:activity` for settled agents.

Together these would take the sidecar from about 1 MB to roughly 100–200 KB and
remove about 1.2 GB of writes in this session. Transcript whole-file copies
need a structural change: chunked or append-only segment and agent artifacts.
That is the next lever, but larger.

## 3. Event-loop lag: GC-shaped, not publication-shaped

Target session, 8,028 `event-loop-lag` events: 7,413 in 0.5–1 s, 584 in
1–2 s, 31 above 2 s (the large ones are suspend or recovery, as the earlier
report found).

- The slowest timer covers at least 80% of the delay in 4,690 events, and less
  than 20% in 2,486. About 1,600 s of total delay is not explained by any timer.
  That time goes to filters, sentinels, redisplay or GC outside timers.
- The *wall* time of named callbacks is high: medians of about 555–630 ms for
  `flush-scheduled-render`, `settle-main-exit`, `emit-progress` and
  `yield-managed`. Their *CPU* share is tiny: `settle-main-exit` 0.03% of R,
  `flush-scheduled-render` 0.17%.
- For comparison, `gcmh-idle-garbage-collect` itself has a median of 415 ms.
  In the live Emacs, 354 GCs averaged 137 ms each after only 32 minutes.
  Inference: most of these 0.5–1 s lags are one GC landing inside whichever
  callback allocated past the threshold. Treating them as work done by the
  named callback would be misleading.
- Publication and lease callbacks rarely top the list:
  - `lease-renew`: 67 events, maximum 868 ms.
  - `deferred-agent-save`: 63 events, maximum 1,277 ms.
  - `collection--step`: 10 events, maximum 707 ms.

  Publication usually runs *inside* other callbacks (tool settlement,
  `anonymous` closures), so these counts are a lower bound.

Diagnostic gaps to close before restructuring transactions. This is exactly the
backlog's "capture production stalls first":

- Add `gcs-done` / `gc-elapsed` deltas to each `event-loop-lag` event, and to
  the slowest-timer measurement in `mevedel-telemetry--lag-time-callback`.
  This is cheap and settles the GC question.
- Name closures: 3,462 of 8,028 events report only `anonymous`. Publication
  continuations in `mevedel-session-publication.el` are lambdas, and so are
  `mevedel-transport-run-at-time` callers. Record the timer's function name
  when the closure has one, or let callers pass a label.
- Record a publication span (bytes written, artifact count, duration) at
  normal tier. Today only segment rotation has one.

## 4. Synchronous subprocess pollers

Every control-fs program, local sessions included, is a blocking
`process-file bash -p -c <script>` (`mevedel-session-control-fs.el` ~1012).

| Caller | Cadence | CPU (% R) | Lag events |
|---|---|---:|---|
| `mevedel-view--control-transfer-refresh` → `control-transfer-poll` | every 5 s, **per open view** | 525,376 (2.47%), 80% leaf `call-process` | 211, max 3,940 ms |
| `mevedel-session-durability-lease-renew` | 30 s | 87,959 (0.41%) | 67, max 868 ms |

For scale, the whole spinner subtree is 5.03%. The docstring of
`mevedel-view-control-transfer-remote-poll-seconds` says "The poll costs nothing
worth counting locally", and this data contradicts it.

Options:

- Poll only when a transfer is plausible, for example when a mailbox entry
  exists; one cheap `file-attributes` check locally.
- Or share one poll across the views of a session.
- Or read local control records natively rather than through a bash program,
  keeping the target-native path for TRAMP.

## 5. Execution progress re-derives the artifact address

`mevedel-execution--emit-progress` runs every 0.25 s per live execution
(`mevedel-execution-progress-interval`). Each event rebuilds the facts
through this chain:

`--facts` → `--artifact-address` → `mevedel-resource-artifact-address` →
`mevedel-resource-within-root-p`

That last call walks every path component with `file-symlink-p` and
`file-truename`, and binds `remote-file-name-inhibit-cache`. It accounts for
130,222 counts, almost all of the 203,687 inclusive under `emit-progress`.
The spool path is fixed per record. Compute the address once per record, when
it yields, rather than on each tick.

## 6. Legacy storage

Totals across all 30+ sessions:

| Storage | Size |
|---|---:|
| Fixed `file-history/` caches | 7.3 MB |
| Fixed segment copies | 4.7 MB |
| Fixed `agents/` archives | 59 MB |
| Publications | 6.2 GB |
| Telemetry logs | 192 MB |

- Fixed `file-history/` caches exist only in pre-portable August sessions:
  `main-2026-08-31T10-19-2bac` (78 files, 5.7 MB), plus `fix-mention` and
  `fix-rewind`.
- Fixed segment copies are in old sessions only.
- The `agents/` archives are the numbered compaction recovery copies, which
  docs/sessions.md keeps on purpose. They duplicate published bytes: 16 MB in
  16-24.

Reclaiming the historical fixed file-history caches saves at most about 7 MB.
It is not worth the consumer and recovery-path audit the backlog asks for. I
suggest dropping that clause, or rewriting the item around section 1 and
section 2, which are about 700× larger.

## Suggested backlog rewrite (for discussion)

- Replace the "legacy storage" half with the Goal collection starvation and
  its telemetry gap.
- Keep "bound publication callbacks", but first add the GC and closure-name
  instrumentation from section 3.
- Then attack bytes per publication:
  1. sidecar deduplication and activity bounding (small, local);
  2. chunked transcripts (larger).
- The control-transfer poll, lease renewal and progress-address findings are
  independent small items.

## Follow-up measurement: state size, same workload (2026-09-28 22:16–22:53)

Setup:

- **gc-probe** (`2026-09-28T22-15-6e04180d46af`): a Save As copy of the
  spinner session, with a 784 KB sidecar, 40 settled agents and a 99 MB
  publication head.
- **gc-baseline** (`2026-09-28T22-15-03713a4a3367`): a fresh, empty session.
- Both used preset `mevedel-gpt6`, full-auto, best-effort sandbox.
- Each ran `benchmark/session-performance-workload.md` three times,
  alternating sessions, with 150 s between runs so the post-settlement lag
  tails did not overlap. Runs 2 and 3 used renamed agents.
- The live editor was shared with normal use. Lag events carried the new
  `:gc-*` and closure-name fields from commit "Split event-loop lag into
  collection and callback time".
- Driver, prompt and comparison script: `.scratch/gc-probe-run/`.

| | gc-probe | gc-baseline |
|---|---:|---:|
| Tool finishes (3 runs) | 252 | 150 |
| Pauses > 200 ms (request summaries) | 98 | 31 |
| Pauses > 500 ms | 16 (+2 in settle tails) | 0 |
| Worst pause | 1,191 ms | 416 ms |
| Sidecar after runs | 901 KB (+117 KB for 6 agents) | 103 KB |

The model made about 70% more tool calls in the large session, which is a
confound. Per tool call, pauses over 200 ms are still twice as frequent in
the large session, and only it exceeds 500 ms.

Split of the 18 recorded pauses (all in gc-probe):

- Every pause contains at least one collection, usually one GC of about
  150 ms. Collection totals **31% of pause time**; only one pause is
  majority-GC: `realign-markdown` at settlement, 5 GCs / 790 ms.
- In **11 of 18** pauses the slowest timer ran for 53 ms or less. About
  350 ms of each of these ~500 ms pauses therefore happened outside timers and
  outside GC, which points to process filters or sentinels (gptel's curl
  callbacks, where tool results, the pipeline and publication run) or to
  redisplay. The heartbeat does not time these.
- The other 7 pauses sit inside a slow timer that ran about 370–600 ms with
  about 150 ms of GC in it:
  - `settle-main-exit` twice: 562/143 and 367/303 (ms, total/GC);
  - `anonymous` twice (517, 601);
  - `closure:mevedel-transport-nested-p` (542);
  - `realign-markdown` (1,158, of which 790 GC; the majority-GC one).

Conclusions:

- GC contributes, but it is not the main cause.
- The dominant large-state cost is non-GC work that scales with session size.
  Most of it runs outside timer callbacks.
- The next capture should time publication and control-fs programs directly:
  a normal-tier span with duration and bytes, at `publication-publish` and
  `control-fs--run-program`. That tests whether those spans line up with the
  pauses, before any transaction is restructured.
- The remaining `anonymous` labels (8 of 18) are closures whose calls reach
  no named non-primitive function.

Side observations:

- Save As of the large session blocked Emacs for about 10 s (synchronous copy).
- The Save As copy inherited the damaged segment-3 prompt index described in
  `journal-segment-divergence-20260927.md`: cumulative turns 17 and 18 index
  past the 1,305,982-character published artifact. Journal capture now fails
  at every settlement in the original and in every copy of it; the warning is
  shown once per Emacs run. It needs an explicit repair of that index; do not
  clamp. This slightly favours gc-probe, which skips capture work.
- The measurement frame split when `*Warnings*` popped up there, which broke
  the first attempt at baseline run 1. The driver was fixed and the run
  redone.

## Follow-up: persistence spans line up with the pauses (2026-09-28 23:21–23:27)

- Build: `session-save`, `session-publication` and `control-program` events
  (`mevedel-telemetry-measure`), hot-loaded.
- Workload: one more gc-probe run (run 4, agents `_r4`), sidecar about 900 KB.
- Analysis: `.scratch/gc-probe-run/correlate.py`.
- Full suite before the run: 8 unexpected, the same 8 tests as the
  pre-change baseline.

The run had 35 pauses over 200 ms and 3 over 500 ms, the worst at 556 ms. Each
of the three long pauses contains exactly one full save:

| Pause | Deferred GC | Full save | ... of which publication | Save allocation |
|---:|---:|---:|---:|---:|
| 500 ms | 167 ms | 270 ms | 201 ms | 14 MB |
| 519 ms | 165 ms | 287 ms | 213 ms | 18 MB |
| 556 ms | 167 ms | 257 ms | 154 ms | 17 MB |

The save plus its deferred collection accounts for about 85% of each pause. No
collection runs inside the save itself: `mevedel--with-gc-batched` pushes it
to the next allocation.

Totals for the run:

- **12 full saves:** 157–307 ms each, allocating 11–19 MB each.
- **3 agent-registry saves** (sidecar only): 203–331 ms and 9.6–12.5 MB. That
  is as expensive as a full save, so sidecar serialization dominates.
- **54 publications:** 4.4 s in total. The slowest are single-artifact
  sidecar commits of about 950 KB (289 and 338 ms) and five-artifact commits
  of 1.2–1.4 MB (200–233 ms).
- **Control programs of 20 ms or more:** 4.2 s across 128 calls. Of these, 58
  were `target-time` clock reads averaging 36 ms; each is a synchronous
  `bash` spawn.

Conclusion: in a large session, the pauses are the synchronous save
transaction. It costs about 150–300 ms, allocates about 15 MB, and its
collection lands just after it. Sidecar serialization is the largest part.
Next steps, in order:

1. Shrink the sidecar. Store agent configuration once per distinct value;
   bound or omit `:activity` for settled agents.
2. Rerun this probe and compare per-save duration and allocation.
3. Then consider structural work: incremental sidecar or transcript chunks,
   and fewer clock-read spawns.

## Autonomous fix round (2026-09-28 23:10 – 2026-09-29 01:10)

Runs 5–7 use gc-probe (a Save As copy of the spinner session) on
`Codex:gpt-6-luna`, effort `max`, with all model tiers set to Luna.

| Run | Changes active | Pauses > 200 ms | > 500 ms | Worst | Collections |
|---|---|---:|---:|---:|---|
| 1–3 (before) | none | 32–33 per run | 3–7 per run | 1,191 ms | about 1 per 5 s |
| 4 | telemetry only | 35 | 3 | 556 ms | — |
| 5 | sidecar, undo, staging, lease merge, collection | 35 | 0 | 470 ms | 101 in 761 s (16.3 s) |
| 6 | + busy collection threshold | 12 | 0 | 434 ms | 17 in about 450 s (2.9 s) |
| 7 | + prompt previews, journal degrade | 14 | 1\* | 508 ms | 27 in 409 s |

\* The single pause over 500 ms came after settlement: a first catch-up
journal capture of 48 inherited turns collected nine times once the
threshold dropped. Fixed afterwards by a 30 s settlement grace (`ee39dfb6`).

Commits, in order:

- `aa1d57d4` Lag events split into collection time and callback time; closures are named.
- `1080e12a` Saves, publications and control programs are measured.
- `ace922b4` The view's undo list keeps only composer edits (it held 24,000 entries and 5–7 MB per view); generated transcript buffers keep no undo.
- `8095f143` Sidecar shrunk from 941 KB to 259 KB: agent configurations are shared, activity history is no longer persisted, and settled agents release their request payload and decoding memo.
- `6107f591` The execution progress address is memoized.
- `a338cb7a` Local transfer polls skip while the request mailbox is unchanged.
- `9d55c0ca` Collection progresses during Goals, follows head changes, and records telemetry.
- `62317408` Rotation handles a never-saved file-backed segment (was a failing test).
- `3c431efb` Large local control payloads are staged as files instead of base64: a 260 KB write went from 31 to 8.8 ms, a 1.5 MB write takes 8.5 ms.
- `bf9c359d` The final head commit also closes the publication window (one lease write fewer per save).
- `a23f9abe` A busy collection threshold is held while requests run.
- `2c96c8f0` Journal capture names unreachable turns instead of failing the checkpoint.
- `9fa23b66` Prompt previews are found in the buffer instead of copying each prompt.
- `d9d0e175` Save As, fork and rewind hold the busy threshold (Save As: 21 collections, 3.4 s of 7.9 s).
- `f074be8f` The backlog item is rewritten around the measured remaining costs.
- `ee39dfb6` The busy threshold is kept through the settlement tail.

Tests:

- The Eask gptel install was upgraded (2026-08-26 → 2026-09-25), which fixed four stale-dependency failures.
- The plan-handoff dispatch failures came from uncommitted work in `mevedel-plan-handoff.el`. They were fixed there by reading the source buffer's own view instead of asking the interaction router, and by guarding the spinner-owner variable. Left uncommitted with that work.
- The full suite ran with 0 unexpected before the final three commits.

Remaining, recorded in `docs/backlog.md`:

- A synchronous save costs about 150 ms on a large session: 40 ms segment reparse, about 70 ms in five programs, and a whole-segment rewrite.
- Lifecycle copies go through the editor twice.

## Largest remaining factor: mevedel ran interpreted (2026-09-29 01:45)

The user's straight build of mevedel held 211 `.el` symlinks and no `.elc`, and
live reloads loaded sources. Every mevedel function in the editor was an
`interpreted-function`. The spinner session's CPU profile already showed
`#<interpreted-function>` frames.

zenit's rebuild check ignores missing `.elc` files, so nothing rebuilt them.
`load-prefer-newer` is nil, so a persistent `.elc` would shadow later source
edits; the persistent fix is therefore left to the user.

Effect measured in the same Emacs, before and after compiling in memory:

- **Transcript parser:** 67 ms / 6.4 MB interpreted, 19 ms / 1.6 MB compiled.
- **Undo sanitizer:** 3 ms / 1 MB interpreted, 0 ms / 10 KB compiled.
- **Realistic save:** 12.7 MB → 8.5 MB allocated.
- **Run 8 (interpreted) vs run 11 (compiled):** 3.7 → 1.7 MB/s allocation;
  20 collections in 319 s → 13 in 413 s. Run 11 had 10 pauses over 200 ms,
  none over 500 ms, and a worst of 452 ms.

All 6,826 interpreted mevedel functions were byte-compiled in memory for the
current session. `docs/development.md` now documents a compiled hot load.

## Other additions after run 7

- `e33e8db2` The transfer poll rebuilds the interaction zone only when the
  descriptor changes (it allocated about 180 KB per poll).
- `eb75fc4e` The sanitizer drops point-only undo groups.
- `289096ce` Lag events carry `:cpu-ms`, telling a suspend from a busy stall.
- `bbdca6c7` Eask scopes: clean, compile, focused, full, other.

Upstream gptel cost, not changed: building each request's prompt from a
1 MB transcript takes about 90 ms and 18 MB (`gptel--parse-buffer` 8.7 MB,
the prompt-buffer copy 4.6 MB), and it happens on every model round trip.

## Closing both backlog items (2026-09-29 09:00 – 12:10)

All measurements used compiled code: the replay's `elc` tree, and modules
hot-loaded compiled into the live Emacs. Live runs used gpt-6-luna.

### Redraws

| Compiled replay, median worst key delay | Before | After |
|---|---|---|
| root warm | 129-139 ms | 43 ms |
| root cold-scheduled | 62 ms | 45 ms |
| agent cold-scheduled | 107 ms | 43 ms |
| agent warm | 47 ms | 37 ms |
| control warm / cold-scheduled | 41 / 45 ms | 31 / 35 ms |

- Every scheduled refresh contained exactly one 57-72 ms collection, and it
  was always the worst key. Collection is now deferred while typing
  (`mevedel-gc-cons-threshold-while-typing`, 256 MB while input is under a
  second old and a request or history job holds the busy floor). The pending
  collection runs at the next pause (`0c99a8a7`).
- Allocation of a batched root refresh fell from 49.7 to 40.0 MB. It had
  been rescanning every audit payload per prompt, copying payloads to key a
  memo, creating a temporary buffer per decode, and cleaning every content
  segment to prove it was not glue (`fe1f00b7`).
- Display-only synchronous rebuilds (resume, rewind, segment rollover,
  control transfer, directive rollback, side-answer abort, Source after a
  fork) now use the scheduler (`915f6471`). A synchronous rebuild of the
  root capture was 284 ms against 71 ms scheduled.
- A trial that moved prompts out of publication was rejected: its gain came
  from where a collection landed, and it left the pinned prompt stale.
- Subprocess offload was measured, not built. A worker needs about 530 ms
  (start, load, property derivation, scan) before it could return a plan,
  against 709 ms for the whole in-process refresh. Turn insertion must stay
  in the editor, so the worst key could not improve (`64fad163`).
- Structural scans classify control lines once (`972c6d12`, 15-20 % faster
  scans).

### Publication and storage

- ToolCall start checkpoints publish the sidecar instead of running a full
  save. That removed eight saves of 196-274 ms from each probe run
  (`288bb8d9`).
- Sidecar admission shares the publication transaction: 7 programs became 5,
  and 114 ms became about 63-100 ms (`cd4c7aa5`).
- The segment tail count reads the leading drawer directly instead of
  through `org-entry-get` (11 ms per save).
- Save As of the 246 MB session went from 6.0 s and 274 MB allocated to
  3.4 s and 19 MB (7.9 s originally). Hashing is batched through
  `sha256sum`, staging containment is checked lexically, and instruction
  snapshots are publication-only (`1d0ff6f8`). The remainder is about 116
  target programs, dominated by the per-file `ln`/`rm` inside one generation
  program.

### Live runs (same workload)

| Run | Code | >200 ms | >500 ms | Max |
|---|---|---|---|---|
| 12 | before today | 16 | 1 (1.8 s, seven collections) | 1798 ms |
| 13 | file-backed publication | 6 | 0 | 460 ms |
| 16 | + checkpoint, transaction, typing GC | 4 | 1 | 556 ms |
| 17 | + scan and drawer | 8 | 0 | 391 ms |
| 18 | + hashing (auto-compaction ran) | 24 | 2 | 1242 ms |

Run 18 was longer (17 minutes) and crossed the compaction threshold. Its
two pauses of about 1.2 s are compaction's start and end.

### Remaining, measured

- A full save still costs 250-400 ms during a busy run, about four per
  probe run. It is five durable target programs (20-70 ms each under load)
  plus two whole-transcript structural passes (about 90 ms). Incremental
  passes would need dirty-region tracking that silent property writes
  bypass. Faster programs would mean changing ADR 0101's hardened script.
- Compaction pauses: a profiled manual compaction paused 882 ms at its start
  (evidence selection over the whole transcript) and 1030 ms at its end.
  Rotation re-restores the 1.9 MB segment's properties in a copy (about
  300 ms), and the view refresh adds to it. About half of that profile's CPU
  was gptel's auth-source lookup, decrypting `~/.authinfo.gpg` (a
  configuration cost outside mevedel).
- The test suite could hang intermittently: a tree-sitter install prompt
  blocked on stdin (fixed in `d1597dea`).
