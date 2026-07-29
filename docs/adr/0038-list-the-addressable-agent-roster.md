# List the addressable agent roster

Status: accepted

`ListAgents` returns every retained agent in the current root session tree,
including `/root`, sorted by canonical path and optionally filtered by path
prefix. Each entry contains only `path`, `role`, and exact current activity
status (`starting`, `running`, `waiting`, `permission-blocked`,
`interaction-blocked`, or `idle`). It excludes opaque IDs, parent fields
already encoded by the path, transcript or final-response content, and
last-turn outcomes already delivered through the mailbox. Listing reflects
addressable identities rather than runtime residency.
