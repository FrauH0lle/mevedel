# Full-auto permits destructive operations

Status: accepted

Full Access (`full-auto`) authorizes destructive operations under the target OS
account without heuristic prompts or guardian review, including outside the
workspace. Explicit hard denies, Plan, validation and integrity checks remain.
Users choose Ask or confined Edits when that authority is inappropriate, or
configure deterministic denies. The UI discloses the absence of confinement.

## Decision history

The initial decision covered already-authorized workspace resources and allowed
a guardian veto. The user's September 2026 Full Access requirement deliberately
removed both default resource boundaries and the veto, following evidence of
repeated approvals in the previous full-auto mode.
