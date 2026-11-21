# Add a persistent sandbox Eval mode

Status: accepted. Extends the Eval execution scope of
[ADR 0024](0024-yield-bash-through-session-owned-executions.md).

## Decision

Eval gains a third mode beside `live` and `fresh`: `sandbox`, a long-lived
child Emacs that keeps its state between calls. It exists so a model can load
code once and probe it step by step (define, call, redefine, retry) without
evaluating in the user's Emacs and without rebuilding every fixture per call.

- **Process.** The sandbox is `emacs -Q --batch` with the current `load-path`,
  running a small request loop. Requests and replies travel as framed,
  `read`-safe forms with request IDs over the child's stdin and stdout.
  Features are loaded on demand by the evaluated forms; the user's init is
  never loaded, because it loads niwa itself and can start collaboration
  rooms, sleep inhibitors, servers or timers.
- **Service child.** Managed executions own one child and close its pipe stdin
  at launch, so the sandbox is not an ordinary execution. It is a
  session-owned service child started through a new `niwa-execution` entry:
  writable pipe stdin, no PTY, no spool of its own, and not listed as a
  model-visible execution. The facade owns the binding between the service
  and its calls; callers do not inspect process records.
- **Idle is inert.** Only a call in flight counts as working. An idle sandbox
  does not hold an agent's result, does not hold the sleep inhibitor, does not
  block rewind or transfer, and runs no periodic timer; its death is observed
  through the process sentinel. Rewind, transfer and fork kill it, because its
  definitions may come from discarded turns.
- **Ownership.** One sandbox per session and owner: the root and each
  retained agent that uses the mode get their own process, so owners cannot
  change each other's definitions. A sandbox starts on its owner's first
  sandbox call. A retained agent's sandbox survives between its turns: turn
  finalization stops only a call in flight, as it does for yielded Bash.
- **Busy and reset.** A call to a busy sandbox is refused at once; there is no
  queue. The refusal names the running call's `execution_id` once that call
  has yielded, and otherwise says it is still starting or running in the
  foreground. `reset: true` kills the
  process and starts a new one with the current `load-path`, working directory
  and confinement profile; it never unloads in place, and it is refused while
  a call runs.
- **Calls.** Each call is a childless call record bound to its owner's
  service. It receives only the reply frame for its request ID and settles
  when that frame arrives or the service dies. Calls yield like fresh Eval:
  after `yield_time_ms` they return an `execution_id` that WriteStdin polls,
  ListExecutions lists and StopExecution stops. Stopping a call kills the
  sandbox process, because a running form cannot be interrupted safely in
  place.
- **Lifetime.** Idle means no call in flight.
  `niwa-eval-sandbox-idle-timeout` (seconds; nil disables) is armed when a
  call settles and cancelled when the next call is admitted, so it cannot stop
  a running call. Session close and Emacs exit kill the sandbox, as does a
  change of the owner's working directory, detected on the next call, which
  starts a new sandbox. Retained agents have no separate
  removal event; an agent's idle sandbox ends through these boundaries, the
  rewind, transfer and fork boundaries above, or the idle timeout.
  Results after a reset, stop, crash, idle shutdown or restart state that
  earlier definitions are gone when there were any: a sandbox that never
  answered a call, or a stop during preparation, reports no loss.
- **Results.** Replies use the fresh Eval result format (Result, STDOUT,
  MESSAGES, BACKTRACE) and report whether the sandbox was started or reused
  and its `features` count from the reply.
- **Authority.** The sandbox is always confined, in every permission mode,
  with the default child profile: writable workspace roots and no network.
  Full Access removes approval prompts for sandbox calls, not their
  confinement. When no confinement backend is available, sandbox Eval is
  refused with a pointer to fresh Eval. A sandbox records the writable roots it
  started with; a call whose current roots differ restarts the sandbox, and
  the result says state was lost. A permission-mode change alone does not
  restart it, because its authority does not depend on the mode. A sandbox
  call cannot request additional permissions or escalation; it is refused
  with a pointer to fresh Eval, which keeps per-call authority. Remembered
  grants are not applied. Sessions with a remote execution target refuse
  sandbox Eval, as they refuse fresh Eval. Plan mode denies it with the rest
  of Eval.
- **Review evidence.** Review of a sandbox call states whether earlier
  definitions apply, the call starts a clean sandbox, or `reset` discards
  them, with the live sandbox's age and loaded-feature count. Confinement,
  not per-expression review, bounds what a
  sandbox call can do. Review evidence reports the sandbox's fixed confinement
  rather than deriving it from the permission mode, which would describe a
  Full Access sandbox call as unconfined.
- **Visibility.** The background-work panel and `/ps` show one row per live
  sandbox. A busy sandbox shows its running call as the ordinary Eval
  execution row with a sandbox annotation, not a second row; an idle sandbox
  has one row with its owner, age and last reported feature count, and its
  Stop kills it. An idle sandbox's row changes only on call start, call end
  and process death, so it causes no redraws, timers or polling; a busy
  call's row updates like any running execution.

## Rationale and consequences

Live Eval sees real editor state but runs on the main thread, cannot be
interrupted except by the user's `C-g`, and leaves every side effect in the
user's Emacs. Fresh Eval is isolated but stateless: multi-step work must
re-require libraries and rebuild fixtures in every call, which gives it no
advantage over a script in another language. A persistent confined child
combines isolation with the interactive Lisp workflow. A crash or hang costs a
restart, not the user's session.

An Emacs server reached through `emacsclient` was rejected. A confined child
cannot reach the user's server, because confinement hides `XDG_RUNTIME_DIR`
and host sockets. A server inside the sandbox would add a second transport,
socket authentication and a server lifecycle, while a pipe to the child's
stdin and stdout needs none of these. Re-granting authority to a running
process was rejected because a long-lived process would keep any grant after
the call that justified it; fresh Eval already provides per-call grants.

Fresh Eval remains the clean-room mode. It proves that code loads and runs
from scratch, carries per-call network, path and escalation grants, and needs
no lifecycle. The sandbox trades that guarantee for state: stale definitions
are its main risk, which the feature count and `reset` make visible and
recoverable.

Keeping the sandbox confined in Full Access departs from Bash and fresh Eval,
which run unconfined there. A long-lived process would otherwise keep Full
Access authority after the user leaves that mode, and its earlier definitions
could still run unconfined. Refusing the mode without confinement was also
rejected, because it would make the sandbox unavailable in Full Access, where
users who work without prompts would most want it. Confinement fixed by
design keeps the sandbox's authority independent of the mode, so mode changes
need neither revocation nor restarts.

The complete design was chosen over a smaller first slice (root owner,
foreground calls, no panel row) on 2026-11-17. Its scope, an execution-facade
service entry, idle exclusions in the working, owner-live and session-live
checks, agent lifetimes, call records and panel sources, was estimated at
1,200–1,800 lines across seven to nine files.

Each idle sandbox costs about 62 MB of resident memory (measured 2026-11-17:
`emacs -Q --batch` at 61,876 KB maximum RSS; 63,308 KB with `cl-lib`, `seq`
and `backtrace` loaded), which the per-owner scope and idle shutdown bound.
Running the sandbox locally against TRAMP paths, or on a remote host, is not
part of this decision; it can follow if Elisp development over TRAMP needs it.
