# Project backlog

Canonical home for project notes, todos, feature ideas, fixes, and
explicitly deferred work. Read this before planning work in any listed
area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

/goal Resolve the tickets in .scratch/guardian-prompts-and-plan-revision/tickets.md. Use $implement to do that. Review after each ticket and run
  $ponytail:ponytail-review
    and $thermo-nuclear-code-quality-review additionally. Resolve issues found by the reviews and review again. Repeat if required.

## Inbox

- Add a memory-verification slash command or skill that consolidates project
  memories and checks whether they are still accurate; explore whether a
  weekly automated check is useful.
- Rename permission modes: `default` -> `ask`, `accept-edits` -> `edits`, and
  `trust-all` -> `auto`. Thus, UI and internal naming conventions are the same.
  - Or find better names, also fine.
- Pause the \"working...\" timer while user input is pending.
- Two message queues: Send as soon as possible (right now) or send automatically after request completed
  - Requires to send keybinds: C-c RET for current way, C-c TAB for after request queue?
- Warnings in Emacs are quite intrusive. Consider making warnings in mevedel target
  the messages buffer ([mevedel] Something happened, can be colored, see corfu)
  and display the warning also in the view buffer (but not permanent via the data
  buffer).
- Check what we can steal from https://github.com/karthink/gptel-agent/commit/5eb9fac975b65df04cf62e2eeffaa79273fbf965
- Should WriteStdin prompt for permission? If yes, how?
- Consider removing the timeout option from Bash. yield_time already unblocks the session at some point and /ps + /stop allows the user (and also the LLM) to kill stuck processes
- Consider making mevedel's data buffers hidden 
- Find a better folder for the tool description markdown files
- Ensure all tools have the examples and their descriptions in markdown files
- Revisit provider prompt-cache optimization after ordered prompt profiles have
  usage data; avoid hard-coding unstable pricing assumptions.
- Glob and Grep have issues in agents with bwrap sandbox


## Entry format

Each entry records its source, owed change, reason for deferral, current
status, and blast radius. Keep entries terse and remove them when they
become implemented, obsolete, or unjustified.

## Agents

### Add task-focused summary context for agent forks

- **Source:** `mevedel-agent-control.el`, `mevedel-plan-handoff.el`; agent
  context-fork discussion on 2026-07-24.
- **What's owed:** If real usage shows self-contained Agent messages are
  insufficient, add an explicit `fork_turns="summary"` mode. It should cost one
  disclosed additional model request, use the child task to produce focused
  background, never treat prior requests as child assignments, inject the
  result as labelled context rather than replayed user turns, and leave the
  parent transcript unchanged.
- **Why deferred:** Ordinary delegation should first rely on a complete
  `message`. Generated context adds latency, cost, failure handling, and
  asynchronous preparation to Agent spawning.
- **Status check:** Plan handoff already creates compact implementation
  summaries, but its continuation-oriented prompt preserves unresolved next
  steps and its preparation runs after a completed Plan turn.
- **Blast radius:** Reusing Plan handoff compaction unchanged could revive
  parent obligations in the child and cannot safely mutate or compact a parent
  request that is still active. Reuse lower-level summarization machinery only
  behind a non-mutating, task-focused agent handoff.

## Request lifecycle

### Prevent system sleep during active requests

- **Source:** `mevedel-structs.el` (`mevedel-request-begin`,
  `mevedel-request-push-canceller`); `mevedel-agent-runtime.el`
- **What's owed:** While a top-level or sub-agent request is active, hold an OS
  sleep inhibitor and release it on every completion, failure, abort, and stale
  request replacement path.  On Linux, start `systemd-inhibit --what=sleep` as
  an asynchronous child process and register its teardown as a request
  canceller.  Keep screen blanking and locking unaffected; add other platform
  mechanisms only when they are needed.
- **Why deferred:** Emacs 30 has no portable system-sleep inhibitor, and a
  leaked platform inhibitor could prevent intended suspend indefinitely.
- **Status check:** Request ownership and teardown are already centralized, so
  each request can own its inhibitor without a new session-level reference
  counter.  No inhibitor is currently acquired.
- **Blast radius:** Without this, automatic suspend interrupts long-running
  model, tool, and agent work.  Incorrect cleanup can drain a laptop battery or
  block explicit suspend after mevedel becomes idle.

## Tools

### Bedrock backend support for deferred tool loading

- **Source:** `mevedel-tools.el` (`mevedel-tools--handle-deferred-inject`)
- **What's owed:** Read and replace Bedrock tools under
  `(:toolConfig :tools)` when deferred tools are injected or expired.
- **Why deferred:** Bedrock uses a different payload nesting from the other
  supported gptel backends and has not been exercised here.
- **Status check:** The handler explicitly supports only the common `:tools`
  path.
- **Blast radius:** Bedrock sessions cannot use deferred-tool loading
  correctly.
