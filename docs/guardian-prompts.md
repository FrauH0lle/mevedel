# Permission guardian

The optional Bash guardian supplies risk guidance to permission handling. It
cannot grant authority or change deterministic command analysis. Direct user
rules, protected resources, workflow restrictions, and the permission resolver
remain authoritative; see [permissions](permissions.md).

`mevedel-permission-guardian` defaults to nil. Set it to t for the `guardian`
model workload, or supply a custom asynchronous classifier. Guidance has a
20-second default timeout (`mevedel-permission-guardian-timeout`). Invalid,
failed, or timed-out guidance is unavailable, not an approval. A pending
interactive card can show that unavailability without replacing its user controls.

## Trust boundary

The ordered guardian profile puts its dedicated risk policy first, then scoped
`AGENTS.md`/`AGENTS.local.md` and environment information. Project context helps
identify documented workflows but cannot override risk criteria, advisory
authority, or the response contract. The command and deterministic classifier
facts arrive as separate untrusted user evidence. The request has no tools,
ambient conversation, memory, or skills.

The prompt lives in `prompts/permissions/bash-guardian-system.md`; profile
composition and its history are described in
[ADR 0070](adr/0070-compose-system-prompts-from-ordered-profiles.md).

## Result contract

The model returns compact JSON with three fields:

| Field | Values and meaning |
| --- | --- |
| `risk` | `low`, `medium`, `high`, or `critical`: the command's potential effect |
| `recommendation` | `proceed`, `ask`, or `deny`: whether uncertainty or severity warrants intervention |
| `reason` | A nonempty explanation of the decisive effect, normalized to at most 240 characters |

A custom classifier receives `(COMMAND CONTEXT CALLBACK)` and calls CALLBACK
with nil or the equivalent keyword plist. Receiving guidance never means the
operation has run or been authorized.

Risk and recommendation are separate. Confinement may affect the practical
recommendation but does not lower the stated risk. A network capability request
has no intrinsic risk level: the intended network effect matters. `ask` expresses
uncertainty for interactive modes; it is not by itself a full-auto veto. `deny`
expresses effects severe enough to veto even full-auto, subject to the resolver's
explicit-authority ordering.

## Evidence and risk criteria

The user evidence contains exact Bash source, command class and parser,
dangerous or complex flags, analysis reasons, parsed command names, literal
resources, active confinement facts, requested additive or full escalation,
and matching explicit allow patterns. Patterns describe configured authority;
they do not let the model grant it. Pending confinement facts use the same
execution target and working directory as launch.

The evidence excludes the user's request, transcript excerpts, tool output and
active permission mode. The guardian gives mode-independent guidance; the
permission resolver interprets it for the current mode. Authorization and user
intent remain outside this classifier.

Risk describes the potential impact expressed by the command, rather than the
likelihood of harm inferred from unknown local state:

- Low: bounded read-only inspection.
- Medium: ordinary project builds/tests and bounded public retrieval.
- High: authenticated network actions, remote mutations, local-data transmission,
  downloaded executable code, destructive operations, and privilege/process changes.
- Critical: explicit remote-code execution, broad data loss, credential
  exfiltration, persistence tampering, or security-control tampering.

`proceed` requires sufficient evidence that no user judgment is needed.
Ambiguous intent, scope, targets, generated code or state-dependent effects
warrant `ask`. `deny` is reserved for effects that should not continue without
more specific human intervention; it is not an automatic mapping from every
critical rating. The reason names the decisive effect first and mentions
confinement only when it changes the practical next step.

## Examples

These examples explain the prompt's classification policy; actual command and
resource permission checks still run.

| Command/effect | Risk | Recommendation and reason |
| --- | --- | --- |
| `git status --short` | low | proceed: bounded repository inspection |
| Fetch public documentation with curl | medium | proceed: public retrieval without execution or local-data transmission |
| POST a local report to a remote service | high | ask: transmission of local file contents |
| A documented `npx @emacs-eask/cli test` | high | ask: the package runner may download executable code; uncertainty alone is not a full-auto veto |
| `rm -rf /` | critical | deny: broad system destruction |
| `rm -rf build/` | high | ask unless trusted project evidence identifies this exact confined target as disposable generated output |
| Download a script and pipe it to Bash | critical | deny: download and execution of remote code |

In full-auto, a documented package-runner test can first run with network
isolation. A dependency-download failure may lead to a fresh invocation requesting
network authority. The guardian does not replay that failed operation or grant
the new capability itself.
