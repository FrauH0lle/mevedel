# Allow tree-wide agent communication

Status: accepted

## Current decision

Agents may address any retained agent in the same root session tree by canonical
path. `SendMessage` may target the root or any agent. Agent-originated mail to
`/root` can wake the idle root through the Monitor Background event scheduler;
self-mail never starts a turn. Mail to idle non-root recipients starts their
turn when capacity allows.
`WaitAgent` wakes for mailbox activity from any source. The spawn tree defines
identity, capacity ownership, and automatic child-completion delivery, but does
not restrict explicit communication to parent-child edges.

## Rationale and consequences

Direct peer communication avoids parent relay turns while retaining one session
authority boundary. Idle-root notification follows the session's existing
background-wake holds and current permissions, not sender-controlled dispatch.
Automatic results still go to the spawn parent and can wake it, so a peer that
requests work needs an explicit reply when it needs the result. See
[Agents](../agents.md).

## Decision history

- **Idle-recipient wake (2026-10-19).** SendMessage absorbed FollowupAgent and
  ordinary results now wake non-root parents too. ADR 0035 records the evidence
  and delivery contract; tree-wide addressing is unchanged.
- **Idle-root notification (2026-10).** Root-directed messages and results
  previously stayed queued until another root turn, stranding late reports
  until the user spoke again. The root-only background-wake exception replaces
  the blanket no-start rule; explicit child activation and tree-wide addressing
  are unchanged. [ADR 0035](0035-separate-agent-messages-from-follow-up-work.md)
  owns delivery, workflow-consumption, and recovery semantics.

ADR 0027 restricted messages and follow-ups to tree edges to avoid hidden
cross-branch dependencies and cycles. Its acknowledged cost was relaying through
parents. ADR 0037 replaced that routing restriction with tree-wide addressing;
identity and automatic result ownership still follow the spawn tree. The original
records contain no measurement or incident explaining the reversal beyond this
tradeoff.
