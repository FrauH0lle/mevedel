# Address agents by canonical task path

Status: accepted. Incorporates ADR 0045.

## Current decision

Agent accepts a free-form `task_name` and derives its lowercase ASCII path
segment inside the reservation transaction. Characters outside `[a-z0-9_-]`
become hyphens, boundary hyphens are trimmed, and the base is capped at 40
characters, preferring the last hyphen boundary. An empty result becomes `agent`,
and the reserved `root` becomes `agent-root`. Joining it beneath the parent
creates a canonical address such as `/root/review-code-block-copy`. Collisions
with retained or preparing siblings get numeric suffixes starting at `-2`.
The tool returns the resulting canonical path, which callers use for control
and follow-ups rather than treating the supplied name as an address. The retained
identity reserves that path for the lifetime of the root session's agent
registry. Failed unpublished spawns release it;
forks and Rewind clear retained ownership under
[ADR 0062](0062-do-not-clone-live-agents-into-session-forks.md).

Agent optionally accepts `working_directory`, an existing absolute directory or
one relative to its parent's effective directory. Retained identity owns the
canonical selection independently of the root session's directory, and children
inherit it by default. Without a selection anywhere in the ancestor chain, the
existing root-session directory policy is unchanged. Bash/Eval, relative file
tools, permission resource analysis, and environment observations use the same
conversation directory. Save/restore and Save As preserve it; session forks
continue to discard retained agents under ADR 0062.

The directory must resolve on the existing execution target and be inside the
canonical workspace or a currently registered worktree of the same Git
repository. For an external checkout, compare common Git-directory identity and
the registered top-level checkout; names, branch labels and lexical path prefixes
are insufficient. Subdirectories are allowed, but unrelated nested repositories
and submodules outside the workspace do not share that identity. Revalidate an
override before each turn, without falling back when it becomes unavailable.

This is execution context, not a new authority scope. Root-session rules,
resource grants, protected paths, Plan ceilings and sandbox writable roots stay
shared and unchanged. An external linked checkout can be a valid directory
while still requiring resource authority for native writes. Confined execution
retains its existing requirement that cwd lie inside a configured writable root;
an additive grant alone does not replace that boundary.
Logical resources keep their original roots. The selected checkout supplies its
workspace instruction hierarchy without changing the session's workspace or
memory identity.

Targets accept canonical `/root/...` paths or relative descendants beneath the
caller. Ancestors and peers use canonical paths. Empty segments, dot/dot-dot,
malformed names, unknown paths, and opaque invocation IDs are rejected.

Session tasks use these same canonical paths as owners. IDs are allocated
session-wide and never imply ownership: an agent may update its own or its
descendants' tasks and notes, while root may manage the whole tree. Main and
non-path buckets remain shared labels. Explicit creation for another owner and
transfer by an authorized writer remain attribution changes, not dispatch.
TaskWrite validates existing ownership before applying any part of a batch.
Successful terminal settlement closes the owner's open
tasks even though the conversation is retained idle; failed turns leave work
open for recovery. The root task reminder identifies child-owned work and is
throttled to its stale interval.

## Rationale and consequences

One address vocabulary exposes topology without requiring storage identities
or a second lookup scheme. Task labels are creation inputs, not control targets;
normalization and collision handling belong to the harness. Opaque invocation IDs
remain internal persistence identities. Control-specific restrictions still
apply, such as InterruptAgent refusing the root and caller.

## Decision history

**Per-agent directories replace mandatory root-session cwd sharing.** On
2026-10-24 orchestrated worktree tasks depended on every agent remembering
`cd` or `git -C`; a stray Git operation reached the main checkout. A shell-local
directory change never changed the durable conversation directory. Make that
choice an explicit optional Agent input and carry it through the existing
execution and permission boundaries, rather than adding more prompt reminders.
This does not reverse ADR 0062: a session fork does not clone agents.

**Task IDs no longer authorize cross-agent updates.** A later workstream reused
IDs #1-3 belonging to a completed sibling, changing their content and reopening
them without changing their owner. Completion cleanup had already run, leaving
root with repeated stale-task nudges. Keep the existing allocator and successful
settlement cleanup; reject foreign-agent writes atomically and make root's
supervisory reminder explicit and sparse instead of adding prompt bookkeeping.

**Free-form task names replace caller-authored path segments.** Models kept
passing display labels such as `Review code-block copy` despite the explicit
alphabet in the argument description and manual. Rejecting these labels exposed
address bookkeeping to the caller without improving path safety. Creation now
derives a safe unique segment; target parsing remains strict, and the returned
canonical path is still the only control identity. Repeated names create new
conversations; SendMessage continues an existing one.

**Hyphens are accepted in task names.** The original alphabet allowed only
lowercase ASCII letters, digits, and underscores. A saved session showed a
caller retrying a rejected descriptive name as `switch-buffer-error`, only to
receive the same vague path-segment error. Hyphens do not conflict with path
separators or dot segments, so rejecting them added retries without protecting
the address grammar. This alphabet remains the canonical-path grammar; the
later free-form creation rule supersedes the caller-side naming constraint.

**ADR 0034 selected task paths; ADR 0045 closed the targeting vocabulary** to
those paths and relative descendants. The latter removed no independent
capability: it prevents internal IDs or alternate naming rules from becoming a
second model-facing address system. Both decisions are maintained here.
