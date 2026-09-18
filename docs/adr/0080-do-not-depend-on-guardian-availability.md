# Do not depend on guardian availability

Status: accepted

Full Access bypasses the optional approval reviewer entirely, including its
vetoes, latency and availability. In Ask and Edits, unavailable, timed-out,
malformed or uncertain review falls back to human approval before any execution.
A valid denial blocks that invocation; a positive result grants it once after
fresh policy and integrity checks. The harness cancels provider work and ignores
late callbacks. It never invents approval from a timeout.

## Decision history

The former advisory Bash guardian could veto full-auto but an unavailable result
left the unattended path unchanged; Ask/Edits only displayed guidance. The user's
September 2026 Full Access decision removed review from that mode. Optional
exception approval now runs in interactive modes, making human fallback the
availability contract. See ADR 0014 for the authority decision.
