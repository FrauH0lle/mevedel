---
name: "Reproduce remote-hook timing before claiming a fix"
description: "Green timing-dependent replays do not diagnose a stall; impose the suspected interleaving without weakening safeguards."
type: feedback
---

# Reproduce remote-hook timing before claiming a fix

**Observed 2026-09-18:** a remote `PreToolUse` hook configured with a 10-second timeout exceeded it, and the guarded Read never ran. Instrumented checks subsequently passed with both the corrected and previous handlers; the final uninstrumented replay also passed in the original order. Those passes neither diagnosed the stall nor established a fix.

A later controlled reproduction imposed a reachable write-coalescing schedule: coalescing the final TRAMP exec and JSON writes reproduced the failure without changing payload bytes or order. A preceding-shell read-ahead lost stdin on exec.

**How to apply:**

- For a timing-dependent remote-hook stall, impose the suspected interleaving and use a comparison that distinguishes the old and changed behavior. A passing replay that does not reach the failure schedule is not evidence that the race was fixed.
- Do not answer the stalled hook by raising its timeout or weakening or deleting the check that caught it. In this investigation, a larger timeout would hide the stdin race rather than resolve it.
- Keep a demonstrated mechanism separate from the original occurrence. The controlled schedule proved one way the failure could happen, not the historical event sequence of the uninstrumented timeout.
- Keep repository validation, isolated-checkout validation, and live-runtime deployment distinct. A completed commit, clean working tree, or earlier combined-tree pass does not establish the other claims.

**When to apply:** remote hook startup, stdin delivery, readiness handshakes, and similar investigations where timing determines whether the guarded operation proceeds. This is not a claim that every remote timeout has the same cause.

Consult `docs/hooks.md` and the relevant source history for the maintained remote-startup mechanism, timeout ownership, and cancellation behavior rather than treating this historical reproduction as a current implementation recipe.
