# Keep direct user authority above heuristics

Status: accepted

A direct user-authored allow settles the named tool operation without heuristic
prompts or guardian vetoes, including Bash command authorization and
inherently unconfined live Eval. Resource authorization, explicit denies, and
confinement authority remain independent, but once the user has supplied every
required authority, mevedel executes even an operation with catastrophic
potential. Heuristics and the guardian protect defaults; they do not silently
invalidate deliberate user policy. Direct user denies remain final, and direct
user asks prompt even in `full-auto`. Permission prompts may create reusable
authority for dangerous Bash only when the complete command can be stored
without wildcard or dynamic-shell ambiguity; otherwise they offer
invocation-scoped approval only.

For native edits, a direct allow settles both tool authorization and preview
application. An independent filesystem resource grant satisfies only path
authority and never auto-applies an edit by itself.
