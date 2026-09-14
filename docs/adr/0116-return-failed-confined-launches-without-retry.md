# Return failed confined launches without retry

Status: accepted

## Current decision

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

## Rationale and consequences

Switching automatically to unrestricted execution would hide confinement defects
and exceed the boundary the user reviewed. A failed invocation therefore costs
a new explicit request when more authority is needed, but cannot silently replay
effects or widen its scope. See [Execution](../tools/execution.md).

## Decision history

ADR 0015 allowed disclosed direct execution when an initial capability probe
was unavailable and prohibited replay after the requested process had started:
partial effects could not be excluded. That probe fallback still applies.

ADR 0116 strengthened the no-replacement boundary to the start of confined
preparation, even when a missing start marker proves the command did not run.
The permission audit found narrow Git approvals followed by failed confined
operations and requests for complete bypass. Real launch regressions showed
that exact-directory mismatches and mount ordering could make an approved scope
ineffective. Evidence of no execution did not justify hiding such a defect with
an unrestricted replacement.
