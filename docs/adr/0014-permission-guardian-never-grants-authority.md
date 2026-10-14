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
The harness may prepare the same directory's tree when confinement cannot
represent an exact directory ([ADR 0086](0086-reuse-approved-execution-permission-profiles.md));
the reviewer judges that disclosed scope. Direct saved rules related to the
operation arrive as candidates with their capability coverage. They inform the
judgment of current intent but grant nothing and do not change deterministic
matching. Live Eval is judged by the visible expression's actual effects:
task-authorized observational inspection of ordinary runtime metadata and
settings may be approved once despite its unconfined execution environment.
Unknown effects, unrelated private data and mutations do not inherit that
authorization from a read-only justification.
Provider evidence and freshness snapshots use one complete, fixed serialization;
callback printer settings and equal-value sharing are not changes in authority.

Errors, timeout, missing evidence and uncertainty display the ordinary card.
The session permission log retains the settled reviewer's bounded explanation
for allow, ask and deny outcomes. It records no raw response or supplied user
evidence, and the explanation is excluded from unified telemetry and forwarded
side audits. It is diagnostic only, never permission authority.
Cancellation owns provider cleanup; late/duplicate responses cannot settle twice.
Terminal request state also prevents review startup before queue admission;
late canceller registrations cannot reopen an ended request.
Public abort marks that terminal state even while durable settlement retains the
request. Queue approval checks ownership again after yielding validation and
removes the head from the current queue, preserving newly admitted siblings.
Natural terminal completion disposes of the isolated review buffer without
aborting the finished engine request. Outstanding work uses the engine-returned
canceller, including synchronous overlong responses and late callbacks.

Shared final permission settlement records accepted reviewer `allow-once`
provenance as display-only metadata. Canonical outcomes retain its bounded reason
independently of later success or failure; pending presentation uses invocation
identity rather than matching command text after approval. Nested calls keep
their own records even when optional bulky render details are omitted. Handler
metadata, human fallback, saved rules, hook approval and mode transitions cannot
claim reviewer approval. Provider messages and ToolCall guest values exclude
these records, and permission policy never reads them as authority.

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

October 2026 completion probes reproduced a false stopped-request notice from
aborting naturally completed reviews, and approval projection probes showed that
accepted decisions needed a common outcome seam to survive failure and nested
execution. Cleanup now distinguishes terminal completion from cancellation;
provenance is recorded only after final approval validation and retained outside
model-visible values. No historical approval is inferred or migrated.

October 2026 offline review probes reproduced false evidence changes under ACP's
callback printer bindings and after equal strings stopped sharing identity.
Freshness and provider input now use fixed printer settings without reference
labels, preserving value changes without rejecting unchanged evidence.
Follow-up numeric probes found two more ambient printer settings: character
notation changed unchanged integer arguments, while a lossy float format merged
distinct values. Serialization now fixes both settings as well, retaining numeric
spelling and full floating-point precision for provider input and freshness.

An October 2026 view-debugging request asked only to read loaded function
metadata and view settings. The reviewer deferred twice, rather than timing out
or failing, but its explanation disappeared with the human card. Policy now
distinguishes an unconfined environment's potential capabilities from a plainly
observational expression's effects, and the permission log retains the bounded
explanation so future deferrals can be diagnosed without guessing.
