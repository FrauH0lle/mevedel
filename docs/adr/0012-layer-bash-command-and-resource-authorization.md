# Layer Bash authority within the selected permission mode

Status: accepted

Ask combines conservative command policy with identified resource authority.
Direct user rules may authorize dangerous or complex syntax; delegated rules
cannot raise confinement authority. A matching direct execution profile supplies
its own filesystem/network scope, while an ordinary command allow supplies none.
Explicit hard denies and Plan restrictions remain effective in every mode.

Edits authorizes arbitrary Bash and batch Eval under required real confinement.
Unknown commands, pipes, redirects and substitutions alone do not require a
permission card. Native edits remain within authorized roots; ordinary native
reads span OS-readable paths except configured inaccessible credential paths.
Read-only protected metadata remains readable. The sandbox enforces child
writes, credential masks and network isolation independently of shell analysis.
Additional authority and live Eval require approval or existing direct grants.

Full Access (`full-auto`) removes default confinement and resource restrictions,
ordinary ask rules and review. It deliberately authorizes the target OS account's
full access, including credential files readable by that account. It cannot
bypass explicit hard denies, Plan, validation or ownership checks.

## Decision history

Originally Bash in every mode needed separately recognized command and literal
resource authority. Recursive grants later supplied directory-tree scope;
ADR 0086 made command profiles self-contained. The September 2026 session audit
found repeated approvals for composed development commands and outside native
reads, while the user explicitly requested Full Access. Edits now relies on
required confinement for ordinary execution, and Full Access is deliberately
unconfined. No package-manager-specific allow rule implements this change.
