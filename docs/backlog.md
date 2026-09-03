# Project backlog

Canonical home for project notes, todos, feature ideas, fixes, and
explicitly deferred work. Read this before planning work in any listed
area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Add a memory-verification slash command or skill that consolidates project
  memories and checks whether they are still accurate; explore whether a
  weekly automated check is useful. See also "/learn" command

- Consider making mevedel's data buffers hidden

## Entry format

Each entry records its source, owed change, reason for deferral, current
status, and blast radius. Keep entries terse and remove them when they
become implemented, obsolete, or unjustified.

## Sessions

### Surface incompatible persisted sessions

- **Source:** `mevedel-session-persistence.el`; session schema changes during
  `v0.5.0`.
- **What's owed:** Detect session directories that the current picker cannot
  restore and tell the user how many were omitted, why, their age and disk
  usage, and where they live. Decide whether to offer inspection, archival, or
  deletion; keep incompatible sessions unselectable until recovery semantics
  are deliberately defined.
- **Why deferred:** Resume remains safe by accepting only the current sidecar
  shape, and ordinary age-based cleanup can reclaim stale data independently.
- **Status check:** the `mevedel` session chooser currently drops missing,
  unreadable, unsupported, and obsolete sidecars without reporting them.
- **Blast radius:** Silent omission looks like data loss and can leave
  unbounded workspace state without giving users enough information to decide
  whether it is disposable.

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

## Review

### Automatic turn advisor

- **Source:** Design discussion on 2026-08-20, prompted by oh-my-pi's
  watchdog/advisor feature (`WATCHDOG.yml`, `advisor.immuneTurns`).
- **What's owed:** After a successful top-level turn, quietly review it with a
  second model and, when something is wrong, inject one hidden note into the
  next request. Trigger from the existing `Stop` hook event only -- once per
  landed turn, not per tool call and not mid-stream. Hand the reviewer the
  transcript delta since the last check and reuse `mevedel-review.el`'s
  spawn/wait/parse-findings machinery over the `reviewer` role in
  `mevedel-agents.el`. Deliver a flagged finding through
  `mevedel-reminders.el` as a `<system-reminder>` alongside the next user
  message. Build both emission guards on day one, because a second model that
  talks constantly is worse than none: hard dedupe (lowercase, collapse
  punctuation and whitespace, drop anything already said this session, drop
  content-free notes such as "looks good"/"lgtm"/"nothing to add", cap at one
  note per pass) and a 3-turn cooldown after a successful injection, during
  which anything further is demoted to a non-interrupting aside.
- **Deliberate exclusions:** No advisor roster -- one reviewer; several named
  advisors with their own models and prompts answer a prompt problem with
  headcount. No per-advisor tool grants -- the reviewer keeps its read-only
  investigation, never edit or bash. No `/advisor configure` UI -- without a
  roster there is nothing to configure. No advisors on spawned sub-agents --
  root session only, or a tree of five agents becomes ten model streams. Add a
  roster when one reviewer prompt is visibly two unrelated jobs stapled
  together; add tool grants when read/grep/glob demonstrably cannot confirm a
  finding.
- **Why deferred:** The routing is trivial and the guards are the real work;
  the accepted cost is one extra model call per turn on the delta, in money and
  latency, which wants deliberate acceptance rather than a quiet default.
- **Status check:** All three pieces exist -- the `Stop` hook event, the
  reviewer role with `/review`'s spawn-and-parse path, and reminder injection.
  Nothing fires a review automatically and no dedupe or cooldown state exists.
- **Blast radius:** Hook lifecycle, sub-agent spawning and capacity, reminder
  injection, and per-turn cost. Ungated notes train the user to ignore
  `<system-reminder>` blocks, at which point the feature is pure spend.

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
