# Default to best-effort confinement

Status: accepted

New sessions default to `best-effort` sandbox mode because Bubblewrap is not a
guaranteed dependency or platform capability. This preserves usable child
execution while naming and disclosing the fail-open behavior; users who require
a confinement guarantee select `required` globally or per session.
`Best-effort` selects disclosed direct execution without another prompt when
the initial backend probe is unavailable. A failed confined preparation or
launch returns its refusal without an unrestricted replacement, as decided in
[ADR 0116](0116-return-failed-confined-launches-without-retry.md). The measured
grant-enforcement failures motivating that decision did not change the need
for a usable default on platforms without Bubblewrap.

The first fallback in each live session produces one user-visible warning and
one model-visible note on the affected tool result. Later invocations do not
repeat the warning, but every result retains its actual confinement facts for
the transcript and audit trail. There is no persistent sandbox status-line
item.

Once the selected execution boundary is already unrestricted, an additive
network request changes no capability and therefore creates no authority
prompt. Exact identified filesystem resources still require independent
resource authorization.
