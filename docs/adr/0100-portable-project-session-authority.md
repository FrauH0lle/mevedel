# Use one portable authority for project sessions

Status: accepted

## Decision

Project sessions use one persisted `:authority-mode portable` profile.  The profile is valid for both local and TRAMP access
to the same execution target.  Ownership is a renewable `.lease/`; committed
state is addressed by an immutable publication head.  Project sessions do not
create or interpret `.lock`.

File-workspace sessions retain the separate `:authority-mode pid-lock`
profile and `.lock` authority.  They do not create a portable lease or
publication tree. Both profiles persist and validate a non-empty target
incarnation; the profile chooses ownership mechanics, not whether target
identity is checked.

All authority operations receive the session's explicit profile.  Path
remoteness is only a transport/path concern and cannot choose lock acquisition,
release, renewal, sweeping, cleanup, cold discovery, or mutation admission.
Cold discovery verifies the committed profile before reading state; a mixed
`.lock` and `.lease` directory fails closed.

Archived execution completion uses that same profile: portable sessions read
the authoritative artifact and publish transcript plus sidecar, independent of
path remoteness. PID-lock sessions replace the transcript atomically.

Portable sessions persist a non-empty target-incarnation fingerprint.  Local
and TRAMP probes construct the same canonical boot-id, machine-id, PID-1 start,
and hostname payload, so a replacement target invalidates old authority in
either access mode.  Restore, takeover, unsettled-mutation recovery, and
publication all use that same profile.

A missing or contradictory authority profile is an error. The session codec
accepts one current format without migrations or a dual reader. See
[Sessions](../sessions.md) for the current storage contract.

## Decision history

ADR 0100 introduced the portable profile in session format v0.5.2, replacing
transport-selected ownership for project sessions. It initially described file
sessions as lacking an incarnation; the current shared codec requires one for
both profiles. The original record did not document that later extension's
reason. Keeping the incarnation requirement explicit avoids implying that a
PID lock makes target identity irrelevant.

A later audit found archived execution settlement still selected publication
by path remoteness. Running its poisoned-cache regression through local project
access reproduced a missing-record failure: settlement read the fixed cache
instead of the immutable artifact. The handler now selects storage by the
session's authority profile, completing the transport-independent decision.
