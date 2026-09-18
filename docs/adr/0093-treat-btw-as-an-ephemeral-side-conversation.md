# Treat /btw as an ephemeral side conversation

Status: accepted

`/btw` creates one transient, multi-turn side conversation from an immutable
snapshot of its parent session's effective model-visible context and request
configuration. It may start while the parent is responding; the snapshot
includes the active user prompt and only completed assistant and tool material,
then marks the inherited active turn incomplete. Additional gptel context is
materialized at invocation instead of retaining live files, buffers, overlays,
or media paths. Parent and side proceed independently: later parent state is
not synchronized, side turns are not merged back, and the inherited transcript
is hidden from the side view. The side owns an ephemeral MevView and transient
runtime request context, but no persisted session, checkpoint, input history,
compaction state, workflow authority, or resumable transcript. One parent may
own one non-nested side conversation, which is discarded when closed or when
its parent ends.

Conversation ephemerality does not imply effect ephemerality. Inherently
read-only tools and analyzer-proven read-only Bash run under the inherited
sandbox and absolute denies. ApplyPatch and other mutation-capable operations
cross an invocation-only approval boundary in Ask and Edits, including sides
opened from Plan. Full Access bypasses ordinary asks, but not explicit denies
or integrity checks; side approvals cannot create durable permission rules.
Approved effects occur immediately in the shared workspace and survive side
closure, while normally sanitized security and tool audit remains durable.
Eval, delegation, tasks, Goals, skills, and parent workflow settlement remain
unavailable, and ordinary session lifecycle hooks do not run for side turns.

Permission sources and request settings are frozen when the side is created.
The side owns its permission queue and execution state, but sends only
allowlisted redacted audit metadata to the durable parent telemetry stream.
Mutation review warns while the parent request remains active because both
conversations share the same workspace.

A durable session fork was rejected because lineage, locking, checkpoints,
Rewind, and resume are contrary to an aside. A one-shot modal was rejected
because follow-up questions are central to the intended use. A hard read-only
boundary was rejected because read-only shell inspection and explicitly
requested artifacts are useful. Installing reusable grants from an ephemeral
transcript remains disallowed. Synchronization or automatic merge was rejected
because concurrent histories would become timing-dependent and would
contaminate the parent context.

## Decision history

The original design required human one-shot approval even in Full Auto.
Mode-derived authority replaced that exception: Full Access now consistently
bypasses ordinary asks, and optional automatic review can settle an invocation
in Ask or Edits. Ephemerality still prevents persistent grants, not the effects
the user-authorized permission mode allows.
