# Persist the agent registry explicitly

Status: accepted

The root session persists an explicit agent registry rather than reconstructing identities from rendered root transcript events. Each record owns its opaque storage ID, canonical and parent paths, role and configuration snapshot, current activity, unread mailbox, and conversation and segment locations. Root and child transcript records remain presentation, context, and audit history; compaction or rendering changes cannot alter addressability or topology.
