# Address agents by canonical task path

Status: accepted. Incorporates ADR 0045.

## Current decision

Agent requires a caller-supplied `task_name` of lowercase ASCII letters, digits,
and underscores. Joining it beneath the parent creates a canonical address such
as `/root/spec_review`. The retained identity reserves that path for the lifetime
of the root session's agent registry. Failed unpublished spawns release it;
forks and Rewind clear retained ownership under
[ADR 0062](0062-do-not-clone-live-agents-into-session-forks.md).

Targets accept canonical `/root/...` paths or relative descendants beneath the
caller. Ancestors and peers use canonical paths. Empty segments, dot/dot-dot,
malformed names, unknown paths, and opaque invocation IDs are rejected.

## Rationale and consequences

One address vocabulary exposes topology without requiring storage identities,
nicknames, generated names, or a second lookup scheme. Opaque invocation IDs
remain internal persistence identities. Control-specific restrictions still
apply, such as InterruptAgent refusing the root and caller.

## Decision history

**ADR 0034 selected task paths; ADR 0045 closed the targeting vocabulary** to
those paths and relative descendants. The latter removed no independent
capability: it prevents internal IDs or alternate naming rules from becoming a
second model-facing address system. Both decisions are maintained here.
