# Keep Goal authority outside conversation history

Status: accepted

## Current decision

The durable Goal record supplies objective, status, accounting, and accepted-plan
authority. Retained current-context observations expose those facts to the model;
old observations and summaries cannot replace the record. User steering remains
ordinary conversation history. Compaction may retain relevant unresolved work
and completed outcomes, but does not reconstruct Goal state or mechanically carry
every old steering instruction forward.

## Rationale and consequences

Completion decisions must see current authority, and compaction must not
resurrect stale objectives. Updating facts in retained history preserves earlier
request prefixes at the cost of history growth; changed facts supersede prior
observations, and missing selected context causes fresh delivery.

## Decision history

ADR 0068 originally supplied a newly rendered Goal contract in each root request.
[ADR 0115](0115-retain-delivered-conversation-fragments.md) moved changing facts
to retained observations and separated stable execution policy after context
audits found system-prefix changes and repeated instruction overhead. It changed
delivery, while retaining the durable record as the authority source.
