# Agent tool grants transitive delegation authority

Status: accepted. Incorporates ADRs 0028, 0043, and 0065.

## Current decision

Possession of Agent grants recursive delegation anywhere in the root-session
tree. A named child's tools come from its own role, without intersection with
the parent's direct tools. An untyped default child inherits its delegator's
effective tools. Session permissions, explicit denies, protected resources, and
confinement remain the shared outer authority boundary.

Delegators receive Agent, WaitAgent, and InterruptAgent. Every
built-in role also receives SendMessage and ListAgents. Worker and
explorer can delegate; reviewer and verifier are communicating leaves. Worker
owns an independent implementation toolset, so a directly read-only explorer
can delegate an authorized implementation task to it.

## Rationale and consequences

Delegation is a capability rather than a privileged coordinator or runtime
class. It deliberately does not propagate a monotonic direct-tool ceiling. A
role with Agent may be directly read-only without being team-read-only. Roles
that must remain leaves omit the delegation capability.

The spawn tree owns canonical paths and automatic result delivery. Explicit
communication can cross branches under
[ADR 0037](0037-allow-tree-wide-agent-communication.md).
SendMessage can wake any idle retained recipient without granting authority to
create new agents; see
[ADR 0035](0035-separate-agent-messages-from-follow-up-work.md).

## Decision history

- **ADR 0028 chose nested delegation** over root/coordinator-only spawning so
  specialized roles can delegate without a separate orchestration class.
- **ADR 0030 made that authority transitive**, choosing role-defined child tools
  rather than intersecting each generation with its parent.
- **ADR 0043 split passive observation/communication from active control**, letting
  reviewer and verifier communicate without starting, joining, or interrupting
  other work. The October 2026 root-only background-wake amendment in ADR 0035
  removes the passive qualification for root-directed reports that otherwise
  remained stranded until the next user turn. The later SendMessage merge lets
  these roles also wake idle retained children, but not create new agents.
- **ADR 0065 gave worker an independent implementation toolset**, applying the
  same decision to explorer-to-worker delegation. Default inheritance remains
  separate from named-role specialization.

These records describe one delegation boundary; their individual identities are
retained here while the [agent manual](../agents.md) owns the tool roster.
