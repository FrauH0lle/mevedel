# Store network authority in qualified tool rules

Status: accepted

Reusable network authority in `ask` and `edits` is stored as a session or
workspace-persistent tool allow rule qualified by the network capability. The
rule settles both the matching Bash command or batch-Eval expression and its
network request; an unqualified allow settles only the operation. Network does
not need a parallel resource-grant store, while exact filesystem grants remain
separate because native tools and confined execution share them.

When command and network authority are both unresolved, one combined prompt
presents the operation and requested capability. Invocation approval settles
both checks once; reusable approval stores the corresponding
capability-qualified rule. If command authority already exists, the prompt
presents only the missing capability.

For a reusable outcome, the current invocation receives every approved
requested capability once, while independent toggles choose which capabilities
are remembered with the command. This permits, for example, allowing network
for the current run while persisting only the command. Full execution
escalation remains a separate prompt rather than a toggle because it disables
the complete confinement boundary.

Later requests present existing reusable authority as already granted and
prompt only for missing capabilities. Approval merges selected reusable
authority atomically without duplicating or revoking existing rules or grants;
revocation is an explicit permissions-cockpit action.

The prompt distinguishes network remembered with the command from exact
filesystem authority remembered as a shared session or workspace resource
grant. Shared filesystem grants apply to native tools and other independently
authorized Bash or Eval invocations; they are not stored inside the
capability-qualified command rule.

Additive authority prompts offer invocation-only, session, and persistent
remembering. A remembered write grant promotes an existing read grant for the
same exact path rather than adding overlapping authority.

Persistent prompt authority is limited to the current workspace and is labeled
`Always in this workspace`; prompt actions never create global authority.

One invocation produces one combined authority prompt containing the operation
and every unresolved additive capability. Approval grants the complete request
for the current invocation, while remembering remains independently selectable
per capability. Denial rejects the invocation instead of executing a
lower-authority variant or replaying it.
