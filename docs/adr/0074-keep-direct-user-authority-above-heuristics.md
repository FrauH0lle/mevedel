# Keep direct user authority above heuristics

Status: accepted

A direct user-authored allow settles the named tool operation without heuristic
prompts or automatic review, including Bash command authorization and
inherently unconfined live Eval. Resource authorization, explicit denies, and
confinement authority remain independent, but once the user has supplied every
required authority, mevedel executes even an operation with catastrophic
potential. Heuristics and the approval reviewer handle missing authority; they do not silently
invalidate deliberate user policy. Direct user denies remain final, and direct
user asks require approval in Ask/Edits. Full Access ignores ordinary asks. Permission prompts may create reusable
authority for dangerous Bash only when the complete command can be stored
without wildcard or dynamic-shell ambiguity; otherwise they offer
invocation-scoped approval only.

For native edits, a direct allow settles both tool authorization and preview
application. An independent filesystem resource grant satisfies only path
authority and never auto-applies an edit by itself.

## Decision history

The September 2026 Full Access contract superseded the former full-auto ask-rule
and guardian-veto exceptions. Explicit hard denies still take precedence.
