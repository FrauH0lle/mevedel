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
or folder structure are required. This is explicit housekeeping, not an age-based
retention policy; old files can still contain current requirements.

Keep unresolved tasks, user constraints, current decisions, active handoffs, and
material whose ownership or relevance is unclear. Check references in relevant
shared files and available session/task context. Files may be used by other
sessions; lack of activity in this session is not evidence of abandonment.

Delete only confirmed duplicates, superseded drafts, or completed handoffs whose
useful information is preserved elsewhere. Name the evidence and surviving
source for each proposed deletion. Merge useful details into the relevant
existing file before removing a duplicate; do not impose a directory taxonomy.
If durable knowledge needs promotion to memory, report it for `/learn` rather
than silently deleting its only working copy.

Use ordinary ApplyPatch permissions, review and conflict checks. Re-read and
reassess files that change during cleanup. Do not use Bash deletion, recursively
remove directories, delete session plans, or edit journal records. When relevance
is uncertain, leave the file and report the uncertainty. Conclude with the files
removed or merged and anything retained for review.
