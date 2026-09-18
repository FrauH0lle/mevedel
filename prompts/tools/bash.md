Execute Bash commands for builds, tests, version control, and system operations.

### When to use `Bash`

- A task needs a shell command, executable, or command-line development tool.

### When NOT to use `Bash`

- Ordinary text reads/searches/edits are better served by Read, Grep, Glob,
  and ApplyPatch, which provide integrated results and review.
- Communicating with the user: write the response directly.

### How to use `Bash`

- Commands start in the session working directory. Quote shell arguments
  correctly; command substitution still executes inside double quotes.
- In Edits, commands run automatically with required confinement and no network
  by default. Full Access runs without confinement or permission prompts. Ask
  uses the configured sandbox preference and command policy. Request known
  necessary capabilities together; approval is scoped to that invocation.
- Use `sandbox_permissions="with_additional_permissions"`, a `justification`,
  and `additional_permissions` for network or exact absolute filesystem paths.
  Read and write grants are distinct. Ask through these fields, not a separate
  prose approval question. Retry a still-needed sandbox/network failure with
  the specific missing capability; the new call is a distinct invocation.
- `sandbox_permissions="require_escalated"` bypasses all confinement. Request it
  only after a relevant confined failure when additive authority is insufficient,
  with a justification; it does not bypass command approval. Unavailable
  confinement is disclosed in the result.
- For grant shapes, escalation troubleshooting, or interactive process control,
  read `mevedel://tools/execution.md`. If unavailable, use the known contract
  or report missing guidance instead of guessing authority fields.
- After `yield_time_ms` (10 seconds by default), a running command returns an
  `execution_id`. Poll unread output with WriteStdin and empty `chars`;
  ListExecutions lists your commands and StopExecution stops one. Commands have
  no automatic timeout; use the native `timeout` command for a deadline.
- Pipe stdin is closed. Use `tty=true` for interactive input or required terminal
  behavior. Avoid shell backgrounding (`&`); lower `yield_time_ms` to yield early.
- Results preserve exit codes. Simple grep/rg no-match, diff differences, and
  test/[ false are classified as expected command outcomes, not execution failures.

### Examples of good usage

<example>
- Build, then test only if the build succeeds:
Bash(command="make build && make test")
If it yields an execution_id, poll that execution to obtain completion and remaining output.
</example>

### Examples of bad usage

<example>
Bash(command="make test &")
<reasoning>
Shell backgrounding is rejected. Use Bash(command="make test", yield_time_ms=1000)
to yield through the managed execution lifecycle. If it returns an execution_id,
use that returned ID with WriteStdin to observe completion; yielding is not success.
</reasoning>
</example>
