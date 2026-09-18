# Use one canonical permission-mode vocabulary

Status: accepted

Permission modes are named `ask`, `edits`, and `full-auto` in the UI, internal
state, configuration, persisted sessions, tests, and documentation. The middle
mode names the capability it automates instead of overloading `auto`, while one
vocabulary removes translation and search ambiguity. Mevedel accepts no aliases
or legacy persisted values. New sessions inherit the global `ask` default
unless the user deliberately selects another mode.

Edits now includes automatic Bash and batch Eval under required confinement,
alongside native edits. `full-auto` means Full Access under the target OS account,
without confinement or permission prompts. Ask remains conservative. These names
are retained; the UI discloses their authority explicitly.

## Decision history

The September 2026 audit found repeated mode and escalation approvals. The user
chose confined automation for Edits and deliberate Full Access for full-auto,
replacing the earlier native-edits-only and heuristic-prompt-bypass meanings.
