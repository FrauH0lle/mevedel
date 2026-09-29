# Journal segment divergence: spinner session

Task: continue investigation of `Journal capture checkpoint failed: Completed
journal turn is outside its durable segment`. This is separate from pipeline
fix commit `285f23c4`. No affected session evidence or application code was
modified during this investigation.

## Verified publication history

Affected session: `2026-09-27T16-24-5ca69305960c`.
Paths below are relative to its `.publications/` directory.
Inspected all 176 retained manifests and their referenced sidecars, then sorted
the relevant states by sidecar `:updated-at` (not directory enumeration order).
Retained manifests establish artifact history; their existence alone does not
prove every intermediate candidate became the authoritative head.

| Sidecar timestamp | Generation | Segment-3 cutoff for indexed turn 17 | Segment-3 artifact |
| --- | --- | --- | --- |
| 20:39:23 | `generation-87b873d1eb1290127324` | 1720429 | `000002.data`, point-max 1720430 |
| 20:41:59 | `generation-741155f2c485a56a0c54` | 1722288 | `000001.data`, point-max 1847918 |
| 20:42:46 | `generation-6fc46f11ab4aff2e752c` | 1722288 | `000001.data`, point-max 1305982 |

The last row is the earliest retained segment-4 sidecar. It keeps the cutoff
but replaces the segment-3 artifact. Later manifests, including
`generation-526245f8cf5d932425de`, reference that same shortened artifact.

Verified the SHA-256 of all three listed segment artifacts against their
manifest entries. Decoded hidden hook-audit records, without evaluating them:

- The first artifact contains fork point `2aca8a10dbb36592004e010b402203c8`
  beginning at 1720188.
- The second contains that same fork point beginning at 1722047, with its closing
  marker ending at the indexed cutoff 1722288 (the following newline advances
  point to 1722289).
- The third contains no record with that ID.

Thus the index previously had corresponding durable bytes. This is not merely
an out-of-range index first introduced by a metadata-only save.

## What changed at rotation

A line-level comparison of the second and third artifacts shows only:

1. Rewritten `GPTEL_BOUNDS` metadata.
2. Added `MEVEDEL_SEGMENT_FINALIZED_AT: 2026-09-27T20-42-46`.
3. Removal of the suffix after the last assistant response.

The previous `GPTEL_BOUNDS` lists its final response as `(1311767 1312063)`.
The shortened artifact ends with the same 296-character response about the
verifier's two spinner edge cases. Its different absolute endpoint also reflects
the header rewrite. The omitted suffix includes subsequent tool/audit history
and the indexed fork-point record, not just an unsent user prompt.

Source at diagnosis supplied a matching mechanism:

- `mevedel-compact-evidence-find-boundary` chooses the end of the last response.
- The automatic pre-send path in `mevedel-compact.el` takes everything after
  that boundary as pending text.
- `mevedel-compact-run--prepare` retains that suffix as `pending-text`.
- `mevedel-session-artifacts-rotate-segment` deletes the matching pending suffix
  before capturing the predecessor's publication text, then restores it to the
  successor's live buffer after publishing.
- Rotation does not refresh the predecessor's prompt index before advancing.

This explains the observed publication shape: an already-persisted completion
record can be treated as pending suffix and removed from its indexed segment.
Exact historical loaded function definitions and the actual pending-text
argument were not captured, so this source-level causal account still needs a
focused regression rather than being treated as a recorded historical trace.

## Safety and remaining work at diagnosis

The original artifact and completion record remain in the immutable publication
store. Do not claim global transcript loss, full recovery, or completeness of a
copied marker in segment 4. Do not clamp cutoffs or suppress journal validation.

No tests were run and no code fix was made in this investigation. Next work:

- Reproduce rotation after a completed tool/error-only suffix followed by a
  new pending request, through the compaction/persistence interface.
- Separate durable predecessor evidence from genuinely unsent pending text;
  also account for position changes from finalized header serialization.
- Verify the predecessor index against the exact artifact actually published.
- Resolve the indexed cumulative-turn versus reserved-turn discrepancy
  independently; it is not needed to prove this artifact replacement.
- Design any repair of this existing session as a separate, explicit operation
  preserving the original evidence. No repair was attempted here.

Unrelated working changes were present in `docs/backlog.md`, `docs/plan-mode.md`,
`mevedel-plan-handoff.el`, and its test file, plus untracked shared working
material. They were left untouched. Spinner Goal prerequisites and profiler
state were not polled or changed.

## Authorized implementation follow-up

User subsequently asked, "Can we fix it?" Implemented the preventive fix in
`mevedel-compact-evidence.el` and `mevedel-session-artifacts.el`, with matching
tests and updates to `docs/compaction.md` and `docs/sessions.md`.

- Compaction now takes the later of the last response end and trusted parsed
  fork-point end. Completed tool/error history is no longer classified as
  pending text merely because no later assistant prose was emitted.
- Finalization rebuilds the predecessor index against the exact finalized text,
  including existing incoming continuations. Missing indexed completion IDs
  fail preparation instead of being silently dropped.
- Portable publication commits matching bytes and index together. File authority
  writes successor, finalized predecessor, then the sidecar commit, followed by
  instructions. Precommit error/quit restores the previous bytes/index;
  committed or pending state remains installed. Upward crash recovery persists
  its refreshed index with its repaired segment counter.
- A proposed equal-counter recovery extension was removed after review found
  it unnecessary for the corrected ordering and not restart-safe. This change
  does not attempt general repair of older inconsistent sessions.

Verification:

- Initial regressions reproduced both the too-early boundary and deletion of
  the predecessor's completion marker: 192/194 tests passed, the two new cases
  failed as expected before the fix.
- Final related Eask run: **765/765 passed** across compaction, artifacts,
  persistence/durability/publication, clear, rewind/fork, history, and journal
  evidence/capture. Its one deliberate publication-failure diagnostic was then
  captured in that test rather than leaked to the run log.
- Final standalone artifacts rerun after that diagnostic fix: **156/156 passed**,
  with no product warnings/messages. No full-repository-suite claim.
- Final Eask compilation: **210 files**, no warnings/errors, followed by Eask
  bytecode cleanup. `git diff --check` passed.
- Independent read-only review initially identified file commit-order and quit
  gaps; both received regressions and fixes. Final scoped review passed.

At the implementation handoff, the affected spinner session's artifacts and
sidecar remained untouched; the changes were neither committed nor hot-loaded.
Existing-session restoration from the retained complete artifact and the separate
cumulative/reserved-turn discrepancy remain outside this preventive patch.

## Commit and hot-load follow-up

User explicitly requested commit and hot load. Committed the six source, test,
and documentation files as `19486cfb`; unrelated edits and this note remain
outside the commit. Reloaded both changed modules in live Emacs through their
Straight source symlinks, verified to resolve to this checkout without shadowing
bytecode. All five inspected entry-point definitions were replaced. Temporary
buffer smoke checks passed for the completed-history boundary and rejection of
a missing indexed completion without index mutation. No affected-session repair
or artifact rewrite was performed.
