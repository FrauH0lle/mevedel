# Store unsettled mutation in the session lease

Status: accepted

A remote mutating execution arms one boolean in the owned portable lease
generation before process launch, and proven settlement clears it only after
all armed mutations sharing that session authority have settled. The lease is
the existing crash-safe mutation-authority record and survives release and
takeover; an incomplete sidecar would be both too late after transport loss and
would violate the completed-turn boundary if written during a shallow first
turn. Process records therefore remain transient, while restore blocks further
mutation until reconnect and explicit acknowledgement durably clear the latch.
The same latch rejects non-read-only tools, and lifecycle teardown waits one
bounded post-KILL interval before clearing it; an unproved survivor remains
unknown.

Acknowledgement is no longer the only way out of an in-session unknown
outcome. A live session that loses a settlement observed the whole block as a
dead end: reads kept working while every mutating tool was refused, nothing
expired, and the refusal named the condition but no recovery, so a turn spent
half an hour retrying before reaching a human. The constraint that made
acknowledgement a judgement call does not hold for that case. A lost
settlement already records the target process-group identity, and the target
can still be asked whether that group exists -- which is exactly the proof the
settlement failed to collect, not a weaker substitute for it. Mutating requests
therefore re-ask before refusing, and an affirmative `dead` settles the outcome
with no human involved. Only an affirmative answer counts: live, unreachable,
ambiguous, and an identity too incomplete to ask about all stay blocked.
Re-proof also stays blocked while the transport is active: starting another
target command from that nested context can consume the pending reply, so the
next mutation attempt retries after the transport becomes idle.

The durable latch keeps its original contract, because it is a boolean and
carries no process identity. Restore across a restart therefore still blocks
until reconnect and explicit acknowledgement, and every refusal now names
`mevedel-retry-target-readiness` so the manual path is visible from inside the
session that needs it.
