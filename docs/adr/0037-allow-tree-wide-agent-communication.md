# Allow tree-wide agent communication

Status: accepted

Agents may address any retained agent in the same root session tree by canonical path. `SendMessage` may target the root or any agent without starting a turn; `FollowupAgent` may trigger any non-root agent but may not start the root session. `WaitAgent` wakes for mailbox activity from any source. The spawn tree still defines identity, capacity ownership, and automatic child-completion delivery, but it is not a communication authority boundary.
