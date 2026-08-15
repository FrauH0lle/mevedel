# Settle remote stops by zombie-aware probing

Status: accepted

## Decision

Remote process-group liveness is decided by one target-side probe that
ignores zombies, and a stopping execution whose main process exits is
settled by that probe instead of by waiting out the kill grace.

The probe answers with one of three words.  `live`: a non-zombie member
with the group's id remains and the leader's start-time token matches the
one captured at launch -- the group may be signalled.  `dead`: no
non-zombie member remains -- the group is settled, because a zombie can
neither run nor write; it is bookkeeping its holder has not collected.
`ambiguous`: live members exist but the leader identity does not match,
which is the PID-reuse case -- such a group is never signalled and keeps
the unknown-outcome classification, safety over latency.  The probe
replaces the previous pair of round trips (a leader identity check
followed by a `kill -0` on the group) with one process, and it replaces
`kill -0` as the liveness authority because `kill -0` on a process group
succeeds while the group's only remaining members are zombies.

The launcher's own wait loop applies the same rule: it scans `/proc` for
group members and exits once none of them is running, filtering state
`Z`, so the transport process ends when the work ends rather than when
the last zombie is collected.

When the main process exits while a stop is escalating, the record is
settled early if one probe reports the group dead: the pending force
timer is cancelled and the execution finishes at the sentinel.  A live,
ambiguous, or failed probe changes nothing -- the bounded TERM, grace,
KILL, grace escalation stays in charge, and no unknown outcome is
latched from this opportunistic probe, because a transient transport
failure at that moment must not poison a record the escalation could
still settle cleanly.

## Consequences

A stop whose TERM works settles at the sentinel, typically well under a
second, instead of riding up to two grace periods; the grace timers
remain as the backstop for groups that genuinely survive signals.  A
group that ends with a held zombie -- a member whose parent lives
outside the group and has not collected it -- settles instead of being
reported as having survived the final KILL, which previously latched a
false unknown outcome and blocked further durable mutation.

Every remote stop costs one fewer control-connection round trip, and
the test suite no longer needs a shortened grace: the mock-method
binding of the kill delay was removed with no measurable slowdown.

The `ambiguous` answer keeps a rare cost: when the leader PID has been
reused while other members survive, the record still becomes unknown
even though the surviving members are provably ours by group id.  The
group id cannot prove ownership -- a fully-collected group frees its id
for reuse -- so the conservative reading stands.
