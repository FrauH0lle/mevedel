# Non-Owner Buffers Follow Published State

A session buffer whose lease is held somewhere else keeps up with the owner's
committed publications instead of holding the snapshot it opened with.

Joining a session another client is writing used to be a photograph. The
buffer read the publication that was current at that instant and never looked
again; nothing in the client re-read a later generation. The transfer poll ran
the whole time, but its requester side returns immediately unless a request is
outstanding, so an idle joined client did no target I/O and learned nothing.
The state it showed was correct when it opened and silently wrong from the
first turn the owner finished.

The same applies to the machine that hands control away. Owner and non-owner
are lease positions, not roles: a host that grants a transfer becomes a
read-only buffer of a session being written elsewhere, which is the joined
client's situation exactly. One follow rule serves both, and a rule that only
served the joining side would leave the more surprising case broken.

Granularity is one publication. A non-owner sees what the owner has committed
and never work in progress, so turns appear whole rather than streaming. That
is a consequence of the durability model rather than a target to improve on:
committed state is the only state another client can read consistently.

The cost is bounded by the publication head already carried in the lease
record. An owner that has published nothing new costs one lease observation
per tick and no artifact reads; only an advance pays for the sidecar and the
segment. This is why following is on by default despite adding I/O to a case
that previously had none — the floor is the observation the poll was already
shaped around, and the alternative default is a buffer that lies.

A locally modified buffer is never advanced. Those edits are what the transfer
path already refuses to discard, and a timer must not resolve on the user's
behalf a conflict the interactive path escalates.

## Consequences

- `mevedel-session-follow-published` is read per buffer, so one session can
  opt out without changing the default.
- A follower reloads through the same committed-state path a granted transfer
  ends with, minus the lease acquisition and the write enable. The two paths
  cannot drift apart in what they consider committed state.
- Following does not observe transfer state, so it keeps working while a
  request is in flight: the owner publishes until it drains, and the requester
  watches that happen.
