# Make rewind in-place undo

Status: accepted (amended 2026-08-26)
Selection boundary: amended below; the selected turn survives a point Rewind.
Directive-event representation: superseded by ADR 0091.
Redo: amended below; conversation-scope restore exists for portable sessions.

Rewind is a true in-place undo: it transactionally truncates the current session and restores captured files in the current checkout without creating a child session, fork lineage, or redo variant. Every accepted model turn, including the first, owns a durable pre-turn checkpoint; Rewind discards the selected turn and everything later, then restores that checkpoint. Ordinary chat and directive turns share this chronological ordering even though directive content is excluded from main-chat conversation context. Conversation Fork and Worktree Fork preserve parallel conversation history, with Worktree Fork providing isolated file coexistence; this avoids the previous state where two persisted conversations appeared recoverable while sharing one checkout whose files had already been rewound. Rewind preserves current session configuration but clears live workflow ownership because tasks, Goals, agents, and handoff state do not have a trustworthy per-turn journal. After an inspectable impact plan and explicit confirmation, Rewind overwrites externally changed captured files so conversation and captured file state return to one coherent point. Directive recovery uses this same linear operation through one **Rewind before this implementation...** action: a directive links to its implementation turn's checkpoint rather than owning recovery history, rewinding before one implementation discards that attempt and every later chat or directive turn, and stored directive patches remain review and revision artifacts rather than a selective undo stack. User-authored workspace directive records and later request edits survive; discarded attempts, answers, feedback, and patches disappear, anchors are resolved against restored files, and each directive derives Ready, Discussed, Implemented, or Failed from its latest surviving activity. A current directive request that differs from the latest surviving attempt's request snapshot instead yields Ready with a request-changed disclosure. Subdirectives consumed by a discarded successful attempt are restored because their consumption belongs to that attempt. Rewind remains available when capture is incomplete; its mandatory impact view distinguishes complete no-change or captured-change outcomes from known coverage gaps and states that uncaptured filesystem effects may remain. A successful Rewind begins `SessionStart(rewind)` as a new context epoch in the same live session without emitting `SessionEnd`; cancelled and rolled-back attempts begin no epoch.

## Amendment: conversation-scope redo for portable sessions (2026-08-26)

"Creates no redo variant" was decided before mevedel had TRAMP and portable
project sessions. The immutable-publication model arrived with that support,
and it changed the trade: every committed head is written once and never
mutated, so the state a Rewind moves away from stays on disk as a complete,
hash-verified generation. A portable Rewind is a head CAS, not a deletion.

The evidence was an incident. A Rewind of a project session left the user with
an empty transcript and the question of whether four turns were gone. They
were not: the superseded head still held the 530 KB transcript, three retained
agent transcripts, the instruction snapshots, the persisted tool results, and
the sidecar -- 14 artifacts, all 14 matching their recorded SHA-256. The
prompt had said "no redo" while the data for one sat one generation back.

Rewind therefore stays a true in-place undo, and `mevedel-redo` republishes a
chosen superseded head as a new committed head. Restoring is an ordinary
publication, not a rollback: the rewound head remains restorable in turn, so
the operation composes both ways rather than consuming history.

The boundary is what a publication contains, and it contains more than the
conversation. Captured file bytes already live in published `file-history`
artifacts indexed by the sidecar's per-turn snapshots; a Rewind trims that
index while republishing the bytes, so the state it restored files *from* is
still committed. Redo therefore restores captured working-tree files in the
same transaction, from the head being restored, and its confirmation names how
many captured files it returns and how many it would overwrite after an
external change. Coverage is Rewind's coverage: uncaptured filesystem effects
remain, and a redo does not resurrect workspace directive records a Rewind
pruned. A PID-lock file session publishes no immutable heads and has no redo;
its Rewind prompt still says so, and the portable prompt promises a redo
instead of none.

File restoration makes redo a two-phase transaction with Rewind's own hazard
shape, so it uses Rewind's ordering: preparation and confirmation read only
immutable published bytes with no lease held, then one reserved-lease
operation rechecks the current head, rechecks the confirmed file plan, backs
up current contents, and restores files; the head CAS follows, and a failure
before it rolls the files back.

This does not make Rewind reversible in general. Redo reaches exactly as far
as capture did: a filesystem effect mevedel never captured was never
restorable by Rewind either, and no publication holds it. That is why the
operation is named for what it restores rather than for undoing a Rewind.

## Amendment: the selection boundary belongs to the surface (2026-08-26)

"Rewind discards the selected turn and everything later" was one rule for two
surfaces that name different objects, and it silently contradicted one of them.
Pointing at an assistant response and asking to rewind reads as "keep this";
the operation instead discarded it. A user rewound to S1 T1 expecting turn 1 to
survive and got an empty session -- the correct outcome under the old rule, and
not the one any label had disclosed.

The rule is now stated on the boundary rather than the turn: Rewind removes
everything after a boundary, and each surface puts the boundary on the side
that matches what it names. Point names a response the user is looking at, so
the boundary falls after it and that turn survives. The picker names one of the
user's own prompts, so the boundary falls before it and the prompt is discarded
with its answer -- which is what makes re-asking possible, and the only reason
to select a prompt rather than a response. The directive **Rewind before this
implementation...** action keeps a `before` boundary because discarding that
attempt is its purpose.

Two properties make the asymmetry safe rather than merely convenient. Coverage
is complementary: the point route reaches every boundary after a response but
cannot empty a session, since no response precedes the first prompt, while the
picker reaches every boundary before a prompt including the first -- the empty
state. And each surface's confirmation now names the boundary and both sides of
it, so the reader is never left to infer whether the named turn survives; the
mismatch this amendment corrects was a disclosure failure before it was a
semantics failure.

Mechanically this is one decision in one place. The candidate session state
decides the surviving turn count, and the transcript cutoff, instruction
pruning, and remote staging pruning derive their answers from it rather than
re-deciding; the file plan reads a turn's pre-turn checkpoint for `before` and
its post-turn checkpoint for `after`. The `after` path is the one Fork already
took, so no new plumbing was required -- only the honesty of naming which
boundary each entry point asks for.

## Amendment: one vocabulary over turns, and collection at settlement (2026-08-26)

Redo's first picker listed publication generations, because that is what the
storage holds. A four-turn session offered a hundred rows: a generation is
written on every committed save, so most of them were snapshots of one
response as it streamed, differing only in how much had arrived. Rewind's
picker listed four. Two pickers over what users think of as the same history
disagreed by a factor of twenty-five.

The two lists were never the same kind of thing. Rewind's target space is the
live prompt index -- semantic, turn-exact -- and it derives its target state by
truncating the transcript and restoring captured files. Redo's is the set of
published generations -- save-exact -- and it replays a stored snapshot. That
difference is load-bearing: redo reaches states the prompt index no longer
names, which is the entire reason it exists, and rewind works on PID-lock
sessions that publish nothing at all.

So the vocabulary is unified rather than the machinery. Every published sidecar
already records its turn count and prompt index, so a generation can be named
by the turn state it restores: heads are grouped by turn count and fork point,
the newest generation of each state stands for all of them, and a head captured
mid-turn -- latest prompt one turn ahead of its count -- is not offered at all.
Both pickers then label rows through the same prompt label. Full machinery
unification was rejected on four counts: PID-lock sessions publish no heads, a
boundary may have no published generation, rewind's file restore is
`file-history`-driven and turn-exact rather than head-driven, and heads can
capture intra-turn state a head-based rewind could not express exactly.

The same turn-state grouping answers storage. Generations were retained until
the session directory was removed, which the deferred-debt note accepted
explicitly; one measured day of one session was 101 generations and 67 MB, 54
of it superseded transcript copies. Settlement is where a turn's intra-turn
generations stop being recovery state, so collection runs there, keeping one
generation per settled turn state plus a recent grace window plus their
reference closure. It cannot be an age cap: manifests carry unchanged entries
forward, so old directories hold bytes new heads still resolve through. The
measured session collects 88 of 101 and keeps 13 -- above the turn count,
because retained blobs keep their directories alive, and driving it to one
directory per turn would mean the per-publish copying that carry-forward exists
to avoid.

Read pins stay deferred. A follower re-reads the owner's current head rather
than pinning it, and the grace window covers that race; a reader holding an
older non-boundary head can still lose its bytes and will see a hash or absence
failure rather than silent corruption.
