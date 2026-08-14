# Use one portable authority for project sessions

Status: accepted

## Decision

Project sessions use one persisted `:authority-mode portable` profile in
session format `v0.5.2`.  The profile is valid for both local and TRAMP access
to the same execution target.  Ownership is a renewable `.lease/`; committed
state is addressed by an immutable publication head.  Project sessions do not
create or interpret `.lock`.

File-workspace sessions retain the separate `:authority-mode pid-lock`
profile and `.lock` authority.  They do not create a portable lease or
publication tree and do not persist a target incarnation.

All authority operations receive the session's explicit profile.  Path
remoteness is only a transport/path concern and cannot choose lock acquisition,
release, renewal, sweeping, cleanup, cold discovery, or mutation admission.
Cold discovery verifies the committed profile before reading state; a mixed
`.lock` and `.lease` directory fails closed.

Portable sessions persist a non-empty target-incarnation fingerprint.  Local
and TRAMP probes construct the same canonical boot-id, machine-id, PID-1 start,
and hostname payload, so a replacement target invalidates old authority in
either access mode.  Restore, takeover, unsettled-mutation recovery, and
publication all use that same profile.

This is a direct format break.  There is no migration or dual reader for
earlier sidecars, and a missing or contradictory authority profile is an
error.
