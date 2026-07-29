# Default to best-effort confinement

Status: accepted

New sessions default to `best-effort` sandbox mode because Bubblewrap is not a
guaranteed dependency or platform capability. This preserves usable child
execution while naming and disclosing the fail-open behavior; users who require
a confinement guarantee select `required` globally or per session.
`Best-effort` falls back to disclosed direct execution without another prompt
only when mevedel can establish that the requested command never started; it
never automatically replays an uncertain or started command.

The first fallback in each live session produces one user-visible warning and
one model-visible note on the affected tool result. Later invocations do not
repeat the warning, but every result retains its actual confinement facts for
the transcript and audit trail. There is no persistent sandbox status-line
item.

Once the selected execution boundary is already unrestricted, an additive
network request changes no capability and therefore creates no authority
prompt. Exact identified filesystem resources still require independent
resource authorization.
