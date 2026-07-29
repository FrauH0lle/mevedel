# Do not depend on guardian availability

Status: accepted

When the opt-in Bash guardian reviews a heuristically suspicious command in
`full-auto`, only a valid `deny` recommendation vetoes execution. An `ask` or
`proceed` recommendation, timeout, request failure, or invalid response leaves
the otherwise authorized unattended path unchanged. The experimental advisory
reviewer must not make `full-auto` execution depend on another model's
availability; explicit denies, resource authority, and confinement still apply.

In `ask` and `edits`, every guardian recommendation remains advisory. Even
`deny` is displayed prominently without removing the user's allow controls;
only `full-auto` interprets it as a veto.
