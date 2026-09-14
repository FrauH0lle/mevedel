# Retain settled agents for the session lifetime

Status: accepted

A settled agent remains addressable, with its conversation intact, until its root session is deleted. It consumes no agent-turn capacity while idle. Persisting and resuming the root session therefore preserves its agent tree. There is no explicit close operation or independent agent garbage collector. Forks and Rewind reset retained ownership as described in [ADR 0062](0062-do-not-clone-live-agents-into-session-forks.md).

A follow-up resumes the same conversation; another spawn starts a separate one.
Related work can reuse context, while unrelated or contaminating context calls for
a fresh identity. This is the continuing-conversation decision from ADR 0032.

## Decision history

**ADR 0032 established continuing conversations across follow-ups; ADR 0033
retained idle identities with their session.** Together they separate active-turn
cost from durable context. Idle retention consumes no turn slot and avoids
independent close/collection lifecycle machinery without a demonstrated resource
need. The root session's registry, rather than rendered transcript events, remains
the identity authority under [ADR 0063](0063-persist-the-agent-registry-explicitly.md).
