# Do not clone live agents into session forks

Status: accepted

A root-session rewind materializes a new fork with an empty addressable agent tree. Referenced child transcript files before the selected point may be copied as read-only historical artifacts, but retained agent identities, mailboxes, conversations, configuration snapshots, and active turns are not cloned. The original session and tree remain unchanged, and canonical task names are available for fresh agents in the fork. This avoids inventing a globally consistent snapshot across asynchronous conversations or importing agent work performed after the rewind point.
