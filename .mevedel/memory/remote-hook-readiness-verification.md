---
name: "Remote hook readiness and stdin timing"
description: "A stalled remote hook command is reproduced by imposing the suspected interleaving, not by raising its timeout; green replays diagnose nothing"
type: project
---

# Remote hook readiness and stdin timing

A remote command hook can block the tool it guards when its stdin handshake stalls. Observed 2026-09-18: a `PreToolUse` hook configured with a 10-second timeout exceeded it and a `Read` never ran. Instrumented checks then passed with both the corrected and the previous handlers, and the final uninstrumented replay passed in the original order. Those passes neither diagnosed the stall nor established a fix. Treat a green replay of a timing-dependent remote failure as evidence in neither direction.

The failure was later reproduced by imposing a reachable write-coalescing schedule: coalescing the final TRAMP exec and JSON writes reproduced it without changing payload bytes or order, and a preceding-shell read-ahead lost stdin on exec. The committed remedy (`9739f7f`, `fix(hooks): Wait for remote readiness before sending stdin`) is a hook-owned readiness acknowledgment plus guarded exactly-once stdin delivery, retaining the existing timeout and cancellation ownership and keeping the payload on stdin rather than in process arguments. `docs/hooks.md` documents the resulting remote-startup acknowledgment; read that for the mechanism and use this note for the verification history and when to apply it.

- Do not answer a stalled remote hook by raising the timeout, or by weakening or deleting the check that caught it. A larger timeout hides the stdin race rather than resolving it.
- Keep a reproduced mechanism separate from the original occurrence. The coalescing schedule demonstrates one way the failure can happen; it is not retrospective proof of what caused the uninstrumented timeout.
- Commit completion, a clean `git status`, and earlier combined-tree test success do not establish isolated-checkout validation or live-runtime deployment; those were not shown.
