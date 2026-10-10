# Migration and session review evidence

Reviewed the complete artifact-store branch changes to migration, session
serialization, resume, fork/Save As, listing and expiry. No real session or
artifact store was modified during this review.

## Fixed

- Completed imports alone receive reusable migration origins. A failed import
  removes only its newly created payload/bookkeeping directories; comments and
  the first version are retried together.
- Fork copies sharing one board retain every migration origin, so retrying
  after that board is edited does not create unintended duplicates.
- Imports and ID allocation account for bookkeeping-only items restored by Git,
  whose empty authored directory is absent. Existing state is never rolled back
  as though it belonged to a failed import.
- Portable comment session names come from verified publications; stale fixed
  sidecars cannot block or misattribute migration.
- Artifact-read failures preserve that whole session unchanged and continue
  processing others. Nested destination paths are refused before writes.
- Migration uses the separate `.state/ID/` bookkeeping layout, preserves ordinary
  legacy filenames and bytes, and stores shared-item revisions in metadata.
- Ordinary chat selection and active-chat fallback share a projection excluding
  dedicated artifact conversations. The internal live-session registry remains
  complete. Listing summaries include attachment IDs.
- Cold abort loads Plan approval handling when an approval is actually pending.
  The isolated pre-review chat test reproduced the missing-load failure.

## Verification

Final isolated Eask migration run: 23 tests, 23 passed, 0 unexpected, 3.179 s.
Log: `/tmp/mevedel-migration-final.log`. The run includes synthetic PID-lock and
portable migrations, unsupported/corrupt-source preservation, interrupted-write
retries, reserved payload filenames, shared fork retries, and Git-restored item
collision safety.

Copied-real-session acceptance used two closed portable backups:

- `2026-09-19T12-48-9c1d83e7f7bd`, v0.5.6, three shared items.
- `2026-10-09T17-14-2e0dc6b24d1a`, v0.5.10, one shared item.

All four imported editor states match their authoritative publications as JSON,
each has one initial version, converted current-schema sidecars retain the
correct attachment IDs, and a second migration reuses the same store IDs.
There were no real PID-lock artifact sessions available; synthetic ERT covers
that authority profile. Comparing normalized JSON avoids Emacs `equal` treating
separately decoded empty JSON-object hash tables as different identities.

All 223 source files in the original backups and all 223 migration input copies
have unchanged SHA-256 hashes after acceptance. SHA-256 of the sorted per-file
hash receipt: `73b1d4a13eda59c4f2c459f1c53b46077c0e4cb64652dd6ac144d738e44493d3`.
Disposable receipt and copies: `/tmp/mevedel-artifact-final-5u6985it`.

The initial broader focused run passed 246/248 tests; the failures were the
cold Plan-abort bug now fixed and project detection polluted by `/tmp/.git`.
The latter also reproduced on untouched source. Root verified the marker holds
only generated excludes. After root moved it aside, a read-only subagent tool
recreated an empty read-only `.git` at 09:52; subagent rename returned EBUSY.
This is sandbox mountpoint pollution, not evidence of a leaking test fixture.
Root runs final verification with TMPDIR outside that ancestry.
