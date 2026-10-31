# Treat /btw as an ephemeral side conversation

Status: accepted

`/btw` creates one transient, multi-turn side conversation from an immutable
snapshot of its parent session's effective model-visible context and request
configuration. On gptel it may start while the parent is responding; the snapshot
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

Claude Code sides fork settled native history through ACP rather than copying
the visible transcript. Fork, resume and delete capabilities are all required;
there is no excerpt fallback. Each side turn resumes only the fork, bypassing
normal root history publication and tool refresh. The fork survives successful
answers for multi-turn use and is deleted on close, abort, error or orderly
Emacs exit, after any writer has stopped. Failed cleanup remains visible and
retains the identity for retry on exit. A hard crash cannot guarantee deletion
of adapter-owned files. Native abort/error leaves readable side evidence but
requires a new side for further questions; gptel abort behavior is unchanged.
Provider compaction remains internal to the fork: its notifications do not
rotate mevedel segments, archive the side's tool rows, or publish reminders.

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

Native `/btw` was initially refused because a copied local transcript is not
Claude's complete model history. The installed adapter's native fork/resume/
delete operations provide independent stored history, removing that obstacle.
The initial native boundary is settled history: the adapter forks its latest
stored history, not mevedel's in-flight transcript boundary. Retaining the fork
until the side ends preserves the existing multi-turn capability without adding
durable session lineage or mapping chronological checkpoints.

The original design required human one-shot approval even in Full Auto.
Mode-derived authority replaced that exception: Full Access now consistently
bypasses ordinary asks, and optional automatic review can settle an invocation
in Ask or Edits. Ephemerality still prevents persistent grants, not the effects
the user-authorized permission mode allows.
