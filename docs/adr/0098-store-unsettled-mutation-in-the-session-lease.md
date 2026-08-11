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
