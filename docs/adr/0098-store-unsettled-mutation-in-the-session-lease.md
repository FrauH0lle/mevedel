# Store unsettled mutation in the session lease

Status: accepted

## Current decision

A remote mutating execution arms one boolean in the owned portable lease before
launch. Proven settlement clears it only after all armed mutations sharing that
session authority have settled. Renewal, release, and takeover preserve the
latch. Unknown settlement blocks non-read-only operations; lifecycle teardown
waits one bounded proof interval after final KILL before deciding it can clear.

A live session retains the lost execution's process-group identity. Before
refusing a later mutation it asks the target again; only an affirmative `dead`
clears the block. Live, unreachable, ambiguous, or incomplete identity stays
blocked. Busy transport defers proof to a later attempt rather than nesting I/O.
A latch restored after restart carries no process identity and still requires
`mevedel-retry-target-readiness` plus explicit acknowledgement.

## Rationale and consequences

The lease is the crash-safe mutation-authority record already available before
launch. A sidecar written after transport loss would be too late and would
break the completed-turn storage boundary during a shallow first turn. Process
records remain transient; the durable boolean records uncertainty without
claiming that a process stopped. Re-proof uses the original group identity and
the same liveness proof as settlement, so it adds recovery without weakening
admission.

## Decision history

ADR 0098 originally required acknowledgement for every unknown outcome. A live
session then spent half an hour retrying mutating tools while reads worked and
no recovery action was named. Unlike restart recovery, that session still held
the process-group identity needed to ask the target. Automatic re-proof replaced
the dead end for live sessions; every remaining refusal names the explicit
readiness-retry command. The incident's separate date was not recorded.
