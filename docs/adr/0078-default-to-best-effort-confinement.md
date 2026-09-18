# Default to best-effort confinement

Status: accepted

New sessions default to a `best-effort` sandbox preference because Bubblewrap is not a
guaranteed dependency or platform capability. This preserves usable child
execution in Ask while naming and disclosing the fail-open behavior. Ask uses
the preference; Edits requires confinement regardless of it, and Full Access
runs unrestricted. Users may also select `required` for Ask globally or per session.
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
prompt. In Ask, exact identified filesystem resources still require independent
resource authorization. Full Access bypasses ordinary resource asks, while
explicit denies, Plan, validation and session ownership remain authoritative.

## Decision history

Originally all permission modes used the sandbox preference and retained
independent resource approval even when unrestricted. Mode-derived execution
authority replaced that arrangement because advisory safety classification did
not provide a containment boundary: Edits now means confined execution and
Full Access means unrestricted execution. Ask retains the portable best-effort
default; no mode silently retries a failed confined launch without confinement.
