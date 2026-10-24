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

Targets accept canonical `/root/...` paths or relative descendants beneath the
caller. Ancestors and peers use canonical paths. Empty segments, dot/dot-dot,
malformed names, unknown paths, and opaque invocation IDs are rejected.

## Rationale and consequences

One address vocabulary exposes topology without requiring storage identities
or a second lookup scheme. Task labels are creation inputs, not control targets;
normalization and collision handling belong to the harness. Opaque invocation IDs
remain internal persistence identities. Control-specific restrictions still
apply, such as InterruptAgent refusing the root and caller.

## Decision history

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
