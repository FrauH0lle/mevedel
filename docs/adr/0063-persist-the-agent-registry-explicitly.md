# Persist the agent registry explicitly

Status: accepted

The root session persists an explicit agent registry rather than reconstructing identities from rendered root transcript events. Each record owns its opaque storage ID, canonical and parent paths, role and configuration snapshot, current activity, unread mailbox, and session-relative conversation location. Root and child transcript records remain presentation, context, and audit history; compaction or rendering changes cannot alter addressability or topology.


## Live transitions

Published records remain serializable while provider setup yields. A retained
follow-up clears its prior result before entering `starting`, and restores it
on dispatch error or quit. Execution mail delivered during that interval remains
queued independently of the follow-up outcome. Active records never serialize a
previous settled result; validation retains that invariant.

## Conversation residency

Restoring a registry does not load its idle conversations. Follow-up, history
resource access and explicit Emacs inspection hydrate the selected identity through
one persistence entry point. Active abandoned turns still hydrate before recovery
so their partial responses enter the interruption result. Frozen configuration,
mailbox and topology do not depend on buffer residency. Failed hydration does not
publish a partial resident buffer or start a provider request.

Frozen built-in tool references resolve through their owning mevedel registrars
when a cold editor has not loaded those tools yet. Registration reconstructs the
same current schema without executing a tool. Unknown names and foreign-category
paths remain invalid persisted data; they are not substituted with another tool.

## Decision history

On 2026-09-21, a complete archive restored in 4.71 seconds, including 1.58 seconds
hydrating 28 idle conversations. Deferring them reduced restore to 3.19 seconds
and registry restoration to 46 ms, preserving all identities and the rendered root
view. This replaces eager idle hydration while retaining eager abandoned-turn
recovery. Inspection/no-save markers are restored after Org mode setup, and owned
follow-ups acquire writable buffers instead of reusing inspection snapshots.

On 2026-09-23, a graphical multi-agent capture exposed execution completion
during reviewer follow-up startup. The old result remained attached to the
`starting` record until invocation admission, causing an unrelated root mailbox
save to fail with `Invalid live agent registry entry`. Clearing it at startup,
with rollback on dispatch failure, replaces that transient invalid state. A
regression delivers execution mail through registry serialization during setup.

On 2026-10-06, a two-process Claude restart test restored the root but dropped
its retained child because the frozen roster referenced an unloaded `ToolCall`.
The in-process tests had already populated the global gptel registry and missed
this dependency. Cold decoding now initializes missing mevedel built-ins through
the existing registrar owner. Separate-editor deterministic and live tests resume
both native histories, preserve each call ledger and transcript, and retain the
child's frozen model after the parent's selection changes. Opening the session
starts no model work; the saved active Goal restores paused.
