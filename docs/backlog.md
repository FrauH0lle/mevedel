# Project backlog

Canonical home for project notes, todos, feature ideas, fixes, and
explicitly deferred work. Read this before planning work in any listed
area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- See /home/roland/Projekte/mevedel/.scratch/memory-lifecycle

- Consider making mevedel's data buffers hidden

- There’s also a new Codex feature that I honestly care about more than several of those benchmarks.
  - Astra can keep notes across context windows and search previous windows when it needs to recover something that’s no longer in the active context.
  - So a requirement, bug or test result can fall out of the current window and Astra can still go back and find it instead of relying entirely on a compaction having preserved exactly the right thing.
  - It’s experimental for now, but OpenAI expects this to become the default Astra behavior in Codex over the next few weeks.
  - If it works well, this could matter way more in real-world usage than another five points on some benchmark.
- Notification error: (dbus-error "org.freedesktop.Notifications.Error.ExcessNotificationGeneration" "Created too many similar notifications in quick succession") [3 times]
- Warning: unknown coding system "utf8" [6 times]

- shared editing
  - use comments for sending selections to llm

## Entry format

Each entry records its source, owed change, reason for deferral, current
status, and blast radius. Keep entries terse and remove them when they
become implemented, obsolete, or unjustified.

## Permissions

### Reduce repeated permission prompts and make directory grants effective

- **Source:** User backlog note and session audit on 2026-09-07; local evidence:
  `.scratch/permission-analysis/report.md`. The removed `RequestAccess` tool
  previously supplied directory approval; see ADR 0019. Local spec:
  `.scratch/permission-prompt-friction/PRD.md`.
- **What's owed:** Let the user approve a directory tree from a pending path
  permission prompt, including when the request names an individual file.
  Make the chosen directory, exact versus recursive access, read versus write,
  and invocation/session/workspace lifetime explicit. Use the shared resource
  grants and re-evaluate queued requests covered by the approval. Cover system
  research and skill resources, remembered execution profiles for validation,
  effective confined Git metadata writes, and clear permission-mode reasons.
- **Why deferred:** The spec is ready; implementation remains. Remembering an
  exact grant for a session does not
  broaden it to descendants: one external design folder required 15 session
  approvals. Native reads/searches accounted for 74 of 103 answered full-auto
  requests, although directory approval would not eliminate every one.
- **Status check:** `ready-for-agent`. The user confirmed testing through the
  tool/prompt/file-operation flow plus real confined execution. Verify fewer
  repeated approvals and actual granted effects while preserving policy.
  Retain current ToolScript envelope and execution-profile behavior where the
  historical defects are already fixed. Amend ADRs 0019 and 0086 as implemented.
- **Blast radius:** Permission prompt UI, shared resource grants, queue
  re-evaluation, execution profiles and confinement, skill-resource guidance,
  permission diagnostics/cockpit, and maintained documentation.

## Model-facing instructions

### Fewer competing instructions, guidance when needed

- **Source:** Harness assessment discussion on 2026-09-06; five models have
  described the operating instructions as heavy. Local spec:
  `.scratch/instruction-simplification/PRD.md`.
- **Research direction (2026-09-11):** Target current capable models with less
  prescribed reasoning and orchestration. Retain useful context, precise tool
  contracts, and execution guarantees; evaluate removals against completion
  quality, cost, latency, and unnecessary user interruptions. Source findings
  and proposed experiments:
  `.scratch/harness-guidelines/development-guidelines.md`. This is future work,
  not a description of implemented behavior.
- **What's owed:** Reduce unnecessary and conflicting instructions and reliably
  deliver necessary guidance when relevant. Preserve tool-description structure
  and 1-2 examples, support many models, and maximize useful provider cache reuse.
  Generalize on-demand tool manuals through existing `mevedel://` access while
  keeping ordinary calls and consequential prerequisites clear in descriptions.
  Include skills, reminders, nudges, deferred-tool discovery/expiry, memory,
  workspace/hook guidance, and guardian/compaction prompts. Change enforcement
  where a concrete gap warrants it; reuse existing delivery and gptel seams.
- **Why deferred:** The expanded PRD addresses the two reviews; implementation
  starts with effective instruction, ownership, delivery, and cache evidence.
  Baseline cleanup and delivery improvements now belong to the same effort.
- **Status check:** `ready-for-agent`. Capture the baseline, specify bounded
  implementation slices, and reassess across models with focused checks and
  cache evidence. Acceptance is the user's judgment of fewer competing
  obligations and timely guidance, not token counts or positive model praise.
- **Blast radius:** Prompt/tool composition, instruction discovery and lifecycle,
  provider cache stability, and relevant workspace documentation. Preserve trust,
  scoped authority, user policy, and truthful verification; distinguish local
  component memoization from provider cache reuse. Amend affected ADRs with
  implemented behavior changes.

## Memory

### Recover root-session evidence across compaction

- **Source:** User discussion of notes and retrieval across context windows.
- **What's owed:** A bounded pre-compaction scratchpad reminder and ordinary
  search over the owning root session's finalized historical segments, with
  source locators and the existing resource permission boundary.
- **Why deferred:** This follows the journal work; digests and raw historical
  evidence have different retention and retrieval contracts.
- **Status check:** `history://root` already reads the current root transcript,
  but it does not traverse pre-compaction archives or support Grep. Journal
  search retrieves published digests, not those original segments.
- **Blast radius:** Historical retrieval must preserve session scope, bounded
  output, and hidden-audit exclusion. Necessary continuation context cannot
  be replaced by archive pointers while this capability remains unavailable.

## Request lifecycle

### Keep the agent injection marker at the complete transcript end

- **Source:** Full memory-lifecycle regression run and an unchanged master
  (`13e12e6`) reproduction against the same installed dependencies.
- **What's owed:** Diagnose the final newline/marker boundary after closing
  reasoning and injecting consecutive agent messages.
- **Why deferred:** Independent of journal and memory changes; the baseline
  reproduces the identical failure.
- **Status check:** `mevedel-tools--handle-message-inject/test@2` expects marker
  318 but observes 317. Other message-injection cases pass.
- **Blast radius:** Subsequent agent response placement after injected mail.


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
