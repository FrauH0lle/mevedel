# Allow tree-wide agent communication

Status: accepted

## Current decision

Agents may address any retained agent in the same root session tree by canonical
path. `SendMessage` may target the root or any agent without starting a turn;
`FollowupAgent` may trigger any non-root agent but may not start the root session.
`WaitAgent` wakes for mailbox activity from any source. The spawn tree defines
identity, capacity ownership, and automatic child-completion delivery, but does
not restrict explicit communication to parent-child edges.

## Rationale and consequences

Direct peer communication avoids parent relay turns while retaining one session
authority boundary. Starting root work remains a user/session responsibility.
Automatic results still go to the spawn parent, so a peer that requests work
needs an explicit reply when it needs the result. See [Agents](../agents.md).

## Decision history

ADR 0027 restricted messages and follow-ups to tree edges to avoid hidden
cross-branch dependencies and cycles. Its acknowledged cost was relaying through
parents. ADR 0037 replaced that routing restriction with tree-wide addressing;
identity and automatic result ownership still follow the spawn tree. The original
records contain no measurement or incident explaining the reversal beyond this
tradeoff.
