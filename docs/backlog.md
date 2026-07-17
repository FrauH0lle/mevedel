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
- Pad tool calls and thinking output by 2 chars in view buffer (similar to "* hook context added")
- Pause the "working..." timer while user input is pending.
- Two message queues: Send as soon as possible (right now) or send automatically after request completed
  - Requires to send keybinds: C-c RET for current way, C-c TAB for after request queue?
- Warnings in Emacs are quite intrusive. Consider making warnings in mevedel target
  the messages buffer ([mevedel] Something happened, can be colored, see corfu)
  and display the warning also in the view buffer (but not permanent via the data
  buffer).

- Steal Command-specific argument policies, /home/roland/codex/codex-rs/shell-command/src/command_safety/is_safe_command.rs:67

- Extend mevedel's sandbox beyond Bash/Eval to every tool for which filesystem,
  network, or process confinement is useful.
- Steal Codex's unified execution lifecycle for Bash: yield interactive or
  long-running commands to session-owned process IDs; support optional PTYs,
  input, polling, interrupts, live output, background completion, listing,
  stopping, and teardown.  Keep each process bound to its original owner,
  sandbox, and resource grants.
  ```
Worth stealing:

  1. Optional PTY allocation — tty=true for REPLs, prompts, and programs that buffer differently without a terminal; pipes remain the clean default. Mevedel
     currently always uses a pipe. /home/roland/codex/codex-rs/core/src/tools/handlers/shell_spec.rs:43 mevedel-tool-exec.el:2000

  2. Live output streaming and background completion events — update the tool row while a command runs and report completion without polling being the only
     observer. /home/roland/codex/codex-rs/core/src/unified_exec/async_watcher.rs:37

  3. Session-owned process registry — list, stop, interrupt, and clean up active terminals. This is required infrastructure once processes can outlive their
     original tool call.

  4. Head-and-tail output retention — Codex drops the middle of huge output, preserving both startup context and final errors. Mevedel currently keeps only the
     first 512 KiB, potentially losing the useful failure summary. We should combine head/tail previews with mevedel’s existing full-result persistence. /home/
     roland/codex/codex-rs/core/src/unified_exec/head_tail_buffer.rs:1 mevedel-tool-exec.el:1814

  5. Stable subprocess environment — disable color and pagers and fix the locale (NO_COLOR, PAGER=cat, GIT_PAGER=cat, LC_ALL=C.UTF-8). Small change, fewer hangs
     and less noisy output.

  6. Structured execution results — retain exit_code, running/session state, wall time, and omitted-output size separately from display text. chunk_id is
     unnecessary for mevedel.
  ```
  ```
  1. Bounded live output capture — highest priority. ccs streams stdout/stderr to disk, keeps a small tail for display, persists large results, and enforces a
     disk cap. Mevedel currently accumulates output in an Emacs buffer and only truncates after completion, so a noisy process can consume substantial memory
     before the “hard cap” runs. /home/roland/ccs/src/utils/ShellCommand.ts:291, mevedel-tool-exec.el:1821

  2. Concurrency-aware tool scheduling. ccs runs recognized read-only Bash calls concurrently but serializes everything else. Gptel starts async tool calls
     immediately, so mevedel can currently race multiple mutating Bash calls. Reuse mevedel’s existing Bash classifier: read-only calls may overlap; all others
     get exclusive execution. /home/roland/ccs/src/tools/BashTool/BashTool.tsx:434, /home/roland/ccs/src/services/tools/toolOrchestration.ts:18

  3. Live progress plus structured completion facts. After two seconds, ccs exposes the output tail, elapsed time, line/byte counts, task ID, and timeout. This
     belongs in mevedel’s live tool row, while final exit-code, duration, and omitted bytes should remain structured render data rather than formatted prose. /
     home/roland/ccs/src/tools/BashTool/BashTool.tsx:659 This is already partly captured in docs/backlog.md:42.

  4. Noninteractive execution defaults. ccs closes unused stdin and controls editor/pager behavior. Mevedel leaves a pipe open, so commands can wait for input
     until timeout. Ordinary Bash should get EOF plus stable no-color/no-pager/UTF-8 settings; only explicit interactive sessions should retain stdin or a PTY.

  5. Command-specific exit semantics — a small, worthwhile polish. grep/rg exit 1 means no matches, diff means differences, and test means false—not execution
     failure. Use mevedel’s parsed final command rather than copying ccs’s heuristic. /home/roland/ccs/src/tools/BashTool/commandSemantics.ts:31

  I would not steal persistent cd, shell-environment snapshots, Bash-emitted image data URLs, or the sed-as-Edit machinery. Mevedel’s session directory, native
  Read/media path, and Edit preview already cover those more cleanly. Also skip the enormous permission-regex implementation; mine its security regression tests
  instead.

  So: output safety first, concurrency second, progress/metadata third. Those complement the background-process work rather than expanding it sideways.
  ```
- Steal Codex's `write_stdin` tool/pattern: keep interactive or long-running Bash
  processes addressable by session ID so a tool can send input, poll output,
  and interrupt them.
- Keep structured Bash execution facts (`exit-code`, running/process state,
  wall time, and omitted-output size) separate from display text, and give
  child processes a stable no-color, no-pager, UTF-8 environment.
- Change the shared oversized-result preview from prefix-only to head-and-tail
  output while leaving the full persisted file intact.  Apply this to
  persisted previews, no-session fallback truncation, and oversized errors;
  remove tool-local prefix caps that would discard the tail first.

## Entry format

Each entry records its source, owed change, reason for deferral, current
status, and blast radius. Keep entries terse and remove them when they
become implemented, obsolete, or unjustified.

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
