# Do not clone live agents into session forks

Status: accepted

A Conversation Fork or Worktree Fork starts with an empty addressable agent
tree. Referenced child transcript files before the selected point may be copied
as read-only historical artifacts, but retained agent identities, mailboxes,
conversations, configuration snapshots, and active turns are not cloned. The
Source session and tree remain unchanged, and canonical task names are
available for fresh agents in the child.

Rewind creates no child session. It clears live agent ownership in the current
session because retained agents have no trustworthy per-turn journal. This
avoids inventing a globally consistent snapshot across asynchronous
conversations or importing agent work performed after the selected point.
