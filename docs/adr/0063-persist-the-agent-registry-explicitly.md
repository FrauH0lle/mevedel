# Persist the agent registry explicitly

Status: accepted

The root session persists an explicit agent registry rather than reconstructing identities from rendered root transcript events. Each record owns its opaque storage ID, canonical and parent paths, role and configuration snapshot, current activity, unread mailbox, and session-relative conversation location. Root and child transcript records remain presentation, context, and audit history; compaction or rendering changes cannot alter addressability or topology.


## Conversation residency

Restoring a registry does not load its idle conversations. Follow-up, history
resource access and explicit Emacs inspection hydrate the selected identity through
one persistence entry point. Active abandoned turns still hydrate before recovery
so their partial responses enter the interruption result. Frozen configuration,
mailbox and topology do not depend on buffer residency. Failed hydration does not
publish a partial resident buffer or start a provider request.

## Decision history

On 2026-09-21, a complete archive restored in 4.71 seconds, including 1.58 seconds
hydrating 28 idle conversations. Deferring them reduced restore to 3.19 seconds
and registry restoration to 46 ms, preserving all identities and the rendered root
view. This replaces eager idle hydration while retaining eager abandoned-turn
recovery. Inspection/no-save markers are restored after Org mode setup, and owned
follow-ups acquire writable buffers instead of reusing inspection snapshots.
