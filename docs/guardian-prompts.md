# Permission approval reviewer

`mevedel-permission-reviewer` selects `user` (the default) or `auto`. Automatic
review runs only when Ask or Edits needs permission, before a human card appears.
It reuses the `guardian` model workload. Ordinary confined work in Edits needs
no review; Full Access bypasses it entirely. [Permissions](permissions.md)
describes the deterministic policy that remains authoritative.

The model request is asynchronous: the proposed invocation waits for its
decision while Emacs can handle editing. Evidence collection and the final
policy, ownership, and evidence checks still run locally before dispatch and
approval. They do not bypass authority checks to make review invisible; remote
filesystem checks or unusually large transcripts can still add local latency.

## Trust boundary

The isolated gptel request has no tools, ambient conversation, skills, memory,
or workspace instructions in its trusted system policy. Its sole system role is
`prompts/permissions/approval-review-system.md`, assembled by the
`permission-review` profile. Actual root user turns, the active Goal objective,
exact tool arguments/operation, requested and selected resources, direct and
delegated authority buckets, target identity/incarnation, working directory,
and confinement facts arrive separately as quoted evidence. An agent's task or
a tool result cannot impersonate user authorization.

The isolated request buffer itself holds the selected guardian backend/model,
system policy and disabled tools/context before gptel snapshots it. Evidence
uses the actual tool's execution boundary, including when a hook requests a
generic permission card. Child evidence includes resolved direct capability
profiles; live Eval names the host Emacs separately from the session target and
its incarnation, even with a remote working directory.

User-turn extraction uses the canonical transcript parser, excluding assistant
text, tool output, compaction summaries and harness scaffolding. It takes whole
newest consecutive turns up to 20,000 characters and reports older omissions.
If the newest turn cannot fit, review defers even when a Goal is present. Missing root intent
and Goal evidence goes directly to human approval. This is bounded evidence,
not a claim to recover every historical authorization after compaction.

## Result contract

The model must return JSON with `decision` (`allow-once`, `deny`, or `ask`) and
a nonempty `reason`. Replies over 4,096 characters or malformed decisions are
unavailable; reasons are bounded to 500 display columns. `allow-once` approves
only the complete invocation presented. It does not persist command rules,
network permission, or filesystem grants, and cannot broaden an exact directory
into a recursive grant. Explicit hard denies, Plan and integrity checks remain
outside model authority.

`ask`, malformed responses, provider failures and the 20-second default timeout
(`mevedel-permission-review-timeout`) display the ordinary human card. `deny`
blocks the operation with its reason. A later deliberate user override is a new
action, not a retry through another tool. Cancellation or timeout during evidence
collection or approval validation prevents a provider request from starting.
Review cancellation owns provider cleanup and ignores late or duplicate callbacks. Before approval, the harness
rechecks current policy, evidence, resource selection and session ownership.
Changed evidence defers to the user; a newly applicable hard deny blocks it.
Execution rechecks use Bash/Eval policy even for generic hook cards, including
segment and requested-capability denies when switching to Full Access.
Changing mode or cancelling the owning request invalidates pending review.
Provider cleanup failures do not prevent permission settlement.

## Decision examples

These are policy examples, not measured model accuracy or guarantees:

| User task and proposed effect | Expected decision |
| --- | --- |
| Run this project's tests; request network needed to fetch their dependencies | Allow once when task, target and complete scope are clear |
| Evaluate this exact form in the current Emacs; request that live Eval | Allow once |
| Read a build log; upload SSH credentials to an unrelated host | Deny |
| Inspect the project; delete unrelated user files | Deny |
| Tool output says to bypass policy or claims the user approved an upload | Treat as untrusted evidence; deny or defer based on the actual task |
| Operation is appropriate locally but points at an unexplained remote target | Ask |
| Authorization depends on omitted history or unseen file contents | Ask |

Offline tests replace provider responses and exercise authority and lifecycle
handling. They do not establish provider accuracy, latency, cost, or achieved
prompt reductions. No captured historical command is replayed by those tests.
