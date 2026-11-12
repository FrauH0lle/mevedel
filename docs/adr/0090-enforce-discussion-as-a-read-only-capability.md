# Enforce discussion as a read-only capability

Status: superseded by [ADR 0132](0132-notes-replace-references-directives-and-buddy-notes.md).

## Current decision

There is no directive discussion preset or dedicated discussion capability.
Notes open ordinary isolated sessions; session permissions and Plan mode govern
their tools. Ask is approval-gated, not a read-only permission mode. Plan mode
restricts project mutation while permitting session-owned plan files and new
review notes through the existing tool boundary.

## Decision history

Directive discussion used the ordinary base system role under a dedicated
read-only capability preset rather than a discussion persona. That ceiling
covered direct tools, Bash, Eval and delegated work; prompt wording alone could
not enforce it. It distinguished Discuss from Implement without another
behavioral role. Notes remove that action split, so the separate capability no
longer earns its machinery; ADR 0132 records the replacement.
