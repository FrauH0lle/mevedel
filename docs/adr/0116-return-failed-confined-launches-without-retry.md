# Return failed confined launches without retry

Status: accepted

Supersedes [ADR 0015](0015-best-effort-confinement-falls-back-only-before-execution.md).

The permission audit found narrow Git approvals followed by failed confined
operations and requests for complete bypass. Real launch regressions confirmed
that an exact-directory mismatch or mount ordering error can make an approved
scope ineffective. Switching automatically to unrestricted execution would hide
that defect and exceed the boundary the user just reviewed.

Once confined preparation begins, a preparation or launcher failure returns one
refusal. Neither managed Bash nor the one-shot executor starts an unrestricted
replacement, even when a missing start marker proves the command did not run.
An emitted marker, signal, or timeout never causes a replay either. Exact-grant
refusals do not emit the command-start marker. Failed preparations release their
temporary mount targets, and refusal facts do not claim unrestricted execution.

The existing `best-effort` default is retained: an unavailable initial capability
probe may select disclosed direct execution. `required` refuses an unavailable
backend, and `off` deliberately selects direct execution. A failed confined
launcher is reprobed for a later, independently authorized invocation. New
capabilities or complete escalation still require a new explicit request; the
executor does not infer them from failure.

Real disposable-process tests verify a pre-start failure returns its exit code
without creating the original command's mutation marker. The same no-replacement
contract applies to managed and one-shot execution.
