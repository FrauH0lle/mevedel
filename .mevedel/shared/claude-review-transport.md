# Claude engine transport review

Reviewed current tree at `514ae42c0e064180af346a6f6c96223b19168a69`
against `5adfcfb28c4504968bc76b70a8bed18e8038f776`. Read ACP/MCP modules,
Claude launch/setup, pipeline changes, relevant development and tool/authority
contracts, PRD, ACP dependency and installed Claude adapter sources. The review
below records the original defects. The authorized corrections are now
implemented in the working tree; no model calls were needed.

## Correction disposition

All three findings are fixed. A connection-local ACP filter retains its newly
scheduled one-shot timers and their successors using the existing transport's
held-timer ownership. Startup and cancellation watchdogs validate their captured
identity before acting. The turn queue consumes cancelled terminal outcomes for
accounting, without admitting further model work; cancelled queued hooks and
tools receive protocol failures so they cannot block acknowledgement. A terminal
reply already queued behind target transport also retains its admission.

Retained child interruption uses the same bounded acknowledgement. Its runtime
canceller returns `deferred` while the connection owns settlement, and the root
admission hold releases exactly once. Repeated interruption does not duplicate
the native cancel or settlement. Buffer destruction retains immediate teardown.
Asynchronous launch preparation now owns its canceller and overall startup
watchdog; duplicate, late, and reentrant callbacks cannot start a retired process.

Desired-behavior regressions are in the normal ACP, ACP-turn, transport, and
agent-runtime test files. The final focused Eask run, including compaction tests,
passed **107/107** in 10.70 seconds. Log:
`.scratch/claude-review/transport-final-focused.log`. The original scratch
diagnostic deliberately asserts the old defect in its control case; it is
historical evidence, not a current regression suite. Parent owns warning-free
compilation and the full suite.

## P1: ACP drain timers are lost during ordinary TRAMP waits

Location: `mevedel-acp.el:137-150` (client creation/notification subscription;
the connection boundary has no integration for ACP's private drain timer).
`mevedel-acp-turn.el:169-174` queues events only after ACP dispatch, which is too
late to protect the dependency's timer. Loaded dependency
`.eask/31.1/elpa/acp-20260914.1318/acp.el:223-228` sets
`message-queue-busy` and schedules the drain with plain `run-at-time`.

When model output arrives while an ordinary TRAMP file operation has suspended
Emacs timers, that new drain lands on the temporary timer list. Returning from
the operation drops it, and later output cannot schedule another drain because
the busy flag remains true. Root/agent requests stop receiving streaming,
compaction receipts, and terminal replies; the turn waits indefinitely or
cancellation eventually closes it as a timeout.

Proof uses an actual ACP subprocess, the real `with-tramp-suspended-timers`
macro, and mevedel's production `mevedel-transport-call-as-remote-operation`
wrapper. Receiving one real response inside that scope leaves the connection
`prompting` with no completion after returning and servicing ordinary output.
Restoring only the dropped drain timer immediately completes the request.
Production remote-operation cleanup does not rescue it: it reactivates only
mevedel's `--pending` and `--held-timers` registries. The separate
`mevedel-transport-with-exclusive-connection` wrapper restores newly scheduled
timers and is not the affected path.

Small remedy: integrate ACP's drain scheduling at the connection boundary with
transport-owned held timers, using a dependency scheduling hook or a narrow
connection-scoped bridge. Protect the queue before callbacks reach the turn
runner; retaining the current callback-level enqueue alone cannot fix it.

## P1: A cancelled startup watchdog can terminate a healthy conversation

Location: `mevedel-acp.el:164-166`; `mevedel-acp--cancel-timer:24-26` clears the
watchdog slot, but its scheduled callback does not validate current startup
state or timer ownership.

When the startup handshake completes during a TRAMP wait, its callback can
cancel the original watchdog while the original timer list is temporarily
hidden. `cancel-timer` cannot remove the hidden timer. On returning from TRAMP,
Emacs restores it; thirty seconds after launch its unconditional callback closes
the now healthy connection. A long initial prompt can therefore fail with
"ACP startup timed out" despite successful admission and ongoing model work.

A real-peer regression reduces the watchdog to 0.5 seconds, completes startup
inside the production remote-operation wrapper, checks ready/idle/no startup
failure, then services normal output beyond the deadline. The connection becomes
closed and the expected-idle assertion fails. No dependency timer has to be
lost for this second failure.

Small remedy: capture timer ownership and check that the connection is still
starting and that the callback still owns its watchdog before failing it.
Apply the same ownership principle to the cancellation watchdog at lines
220-223, which can likewise outlive an acknowledged cancellation.

## Reproduction

Disposable file: `.scratch/claude-review/transport-repro.el`.

```sh
npx @emacs-eask/cli test ert .scratch/claude-review/transport-repro.el
```

Actual result: three tests, one passing control that demonstrates manual
reactivation repairs the dropped drain, two failed expected-behavior regressions:

- `review-acp-remote-arrival-regression`: `(eq success nil)`.
- `review-acp-startup-watchdog-after-remote-ready`: `(eq idle closed)`.

Tests close connections and delete temporary directories on failure. Parent
performed required bytecode cleanup before these focused Eask runs.

The parent independently reran this file together with
`.scratch/claude-review/cancel-usage-repro.el` after compilation and cleanup.
The combined run had one passing control and three failed expected-behavior
regressions, including cancellation accounting. Its output is retained at
`.scratch/claude-review/transport-confirmation.log`.

Loaded-source evidence:

- ACP 0.15.2 source SHA256:
  `9c1c957d90c918a6d1966a2672b3f4c53af223d0200660980a8ee4ca9c17775f`.
- Installed Claude adapter 0.86.0 `dist/acp-agent.js` SHA256:
  `fc5b393d5b5f5b17dc796275581dd00eef60dd6b660c4892f067c19ac4565141`.

## P2: Public cancellation discards available final native usage

Location: `mevedel-acp-turn.el:153-161` and `:175-185`. The prompt completion
callback enqueues the acknowledged terminal outcome, but the queue skips all
operations once the owner is cancelled and substitutes a fresh
`(:status interrupted)` outcome. That discards the normalized final counters
and never invokes `:complete-prompt` to merge the prompt total.

Reproduction uses an actual admitted `mevedel-acp-turn-start`, real ACP
subprocess, real streamed SDK usage, the production Claude normalizer and
completion callback, and public `mevedel-abort`. The scratch peer reports an
initial sample snapshot with input 10/output 1, then acknowledges cancellation
with native totals input 40/cache-write 5/output 20. The normalizer receives
input 45/output 20 and interrupted status. The turn settles aborted exactly once,
but its request retains input 10/output 1. The expected-final-input assertion
fails `(= 45 10)`. This loses known terminal usage; it is independent of any
unknown-counter fallback.

Installed adapter 0.86.0 matches this fixture contract: `dist/acp-agent.js`
`cancelledOutcome` (around lines 7640-7647) calls `turnOutcome`, which includes
`sessionUsage`, or preserves the already recorded result's usage.

Small remedy: permit the owned terminal acknowledgement to run accounting and
terminal settlement after cancellation, while continuing to reject queued text,
hooks, tools, and continuation prompts. Keep interrupted status and known
terminal usage through the same completion path rather than creating an empty
replacement outcome.

```sh
npx @emacs-eask/cli test ert .scratch/claude-review/cancel-usage-repro.el
```

The disposable peer is `.scratch/claude-review/cancel-peer.py`, copied from the
existing deterministic peer with only streamed-snapshot and cancelled-final
usage added. The expected-behavior test failed in 0.49 seconds with resources
cleaned. No production files changed.
