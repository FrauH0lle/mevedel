---
name: clean-work
description: Review shared working files and remove confirmed obsolete material
context: inline
user-invocable: true
disable-model-invocation: true
argument-hint: "[focus]"
---

Review `work://shared` for obsolete working material, optionally limited by
`$ARGUMENTS`. Search and read candidates before proposing changes. No filenames
or folder structure are required. Shared files are temporary, machine-local
working material; memory consolidation also removes quiet files automatically
after extracting their lasting knowledge. This explicit pass is for cleanup now.

Keep unresolved tasks, user constraints, current decisions, active handoffs, and
files another conversation may still be editing. Check references in relevant
shared files and available session/task context. Files may be used by other
sessions; lack of activity in this session alone is not evidence of abandonment.

Delete duplicates, superseded drafts, completed handoffs, and finished material
whose useful information is preserved elsewhere. When only future usefulness
is doubtful, prefer deletion. Name the evidence and surviving source for each
proposed deletion. Merge useful details into the relevant existing file before
removing a duplicate; do not impose a directory taxonomy. If durable knowledge
needs promotion to memory, report it for `$learn` rather than silently deleting
its only working copy.

Use ordinary ApplyPatch permissions, review and conflict checks. Re-read and
reassess files that change during cleanup. Do not use Bash deletion, recursively
remove directories, delete session plans, or edit journal records. When it is
unclear whether a file is still in active use, leave it and report why.
Conclude with the files removed or merged and anything retained for review.
