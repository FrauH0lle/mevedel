# Delegate invocation approval before human interruption

Status: accepted

The optional permission reviewer uses the existing `guardian` workload before
a human card is admitted in Ask or Edits. `mevedel-permission-reviewer` defaults
to `user`; `auto` may approve the complete invocation once, deny it, or defer.
It does not review ordinary automatically confined work or Full Access.

Actual root user intent and the Goal objective, exact operation, complete
capability request, current authority, target and confinement facts are quoted
data beneath an isolated trusted policy. Delegated tasks and tool content cannot
substitute for user authorization. The harness revalidates deterministic hard
restrictions, evidence and ownership before an approval. A model cannot persist
permissions, select broader resources, change modes, or override Plan.

Errors, timeout, missing evidence and uncertainty display the ordinary card.
Cancellation owns provider cleanup; late/duplicate responses cannot settle twice.
Terminal request state also prevents review startup before queue admission;
late canceller registrations cannot reopen an ended request.
Public abort marks that terminal state even while durable settlement retains the
request. Queue approval checks ownership again after yielding validation and
removes the head from the current queue, preserving newly admitted siblings.
See [the reviewer contract](../guardian-prompts.md) and
[ADR 0080](0080-do-not-depend-on-guardian-availability.md).

## Decision history

ADR 0014 originally required advisory-only Bash risk guidance in Ask/Edits and
allowed a veto in full-auto. It deliberately excluded user intent, so it could
not decide whether a task authorized an exception. The September 2026 audit
recorded 237 matched answers to 239 displayed cards, all approvals. This is
friction evidence, not a safety oracle. The user's accepted design delegates
invocation approval optionally and makes Full Access bypass review entirely.
The old classifier/custom callback API and risk annotation UI were removed.

Adversarial verification cancelled a request from the pre-queue progress callback
and then delivered a late automatic approval. Review and live Eval still started:
the cancellation drain had run before either registered cleanup. Request teardown
now records terminal state before callbacks, immediately cancels late registrations,
and prevents permission admission for that request. Review startup also checks
synchronous cancellation before allocating its timer or preparing evidence.

Follow-up probes found public abort still draining callbacks without marking
cancellation, and directory validation dropping a sibling admitted while it
yielded. Public abort now uses the request owner's cancellation API; queue
settlement refreshes ownership after validation rather than using its old list.
