# Tech debt tracker

Living list of known shortcuts, deferred cleanups, and rough edges in
the codebase. Read this before planning non-trivial work in any of the
listed areas — an entry here may change the scope of what you're about
to do.

Both humans and the agent may consult this file. The agent may **propose**
new entries when it spots debt mid-task, but should flag them in the
conversation first rather than appending silently — the user decides
whether something graduates from "current task" to a tracked entry.

## Tracking flow

```mermaid
flowchart TD
    A[Non-trivial work planned] --> B[Consult tracker]
    B --> C{Relevant debt?}
    C -- Yes --> D[Adjust scope or risk notes]
    C -- No --> E[Proceed normally]
    F[Debt spotted mid-task] --> G[Propose entry in conversation]
    G --> H{User accepts tracking?}
    H -- Yes --> I[Append terse entry]
    H -- No --> J[Keep it in current task only]
```

## Entry format

Each entry uses the structure below. Newest entries on top.

```
### <short title>

- **Location:** `file.el` (`function-name`, or `file.el:LINE`)
- **What's owed:** the actual cleanup or change required
- **Why deferred:** why this wasn't fixed when introduced (scope, risk,
  blocked on X, waiting for an upstream change, etc.)
- **Blast radius:** what code/behavior is affected if this is left
  alone, or what could break when it's finally fixed
- **Added:** YYYY-MM-DD (optional — git blame is authoritative)
```

Keep entries terse. If something needs a paragraph of context, link to
the maintained docs, a design note, or a commit instead.

## Entries

### Markdown view affordances use regex scanning

- **Location:** `mevedel-view.el` (`mevedel-view--decorate-markdown-in-range`)
- **What's owed:** Replace the small regex scanners for fenced blocks,
  pipe tables, local images, and local links if real transcripts show
  false positives or missed Markdown forms.
- **Why deferred:** This pass intentionally stole tiny shell-maker UI
  behaviors without importing a Markdown renderer.
- **Blast radius:** Complex Markdown may not get every copy/table/image/link
  affordance, but raw Markdown still renders as text.
- **Added:** 2026-06-20

### Own token estimation could be replaced by upstream gptel

- **Location:** `mevedel-compact.el`
  (`mevedel--estimate-tokens` and token-baseline helpers)
- **What's owed:** drop the in-tree estimation in favor of gptel's
  native token calculations (gptel commit `4b8894fd`).
- **Why deferred:** gptel only recently added the feature; mevedel's
  homegrown chars/4 estimate predates it.
- **Blast radius:** divergent estimates between mevedel's compaction
  pressure checks and gptel's own UI; small accuracy gap.

### Diff preview overlays leak after multi-edit turns

- **Location:** `mevedel-preview-mode.el` (cleanup paths around
  `mevedel-preview-mode-add-preview` / dismiss)
- **What's owed:** ensure preview buffers/overlays from earlier
  Edit/Write calls in the same turn are reliably reaped when the next
  preview registers or the request finishes.
- **Why deferred:** observed but not yet root-caused.
- **Blast radius:** stale overlays accumulate in the user's buffers
  across long sessions; `mevedel-abort` may not catch the orphans.

### `mevedel-view--tool-status-string` belongs on tool defs

- **Location:** `mevedel-view.el` (status-string lookup) and
  `mevedel-tool-registry.el` (tool struct slots)
- **What's owed:** move the per-tool status-string mapping onto the
  `mevedel-tool` struct (with a reasonable default) instead of
  branching inside the view.
- **Why deferred:** noted in `todo.org`; refactor opportunity, not a
  correctness bug.
- **Blast radius:** new tools currently have no clean place to declare
  their progress label; the view file grows a switch statement.

### Legacy / backwards-compat code from the redesign

- **Location:** TBD — needs an audit pass
- **What's owed:** sweep the redesign branches for compatibility
  shims, removed-feature comments, and dead code paths the new
  architecture no longer needs.
- **Why deferred:** the redesign rolled in incrementally; cleanup was
  postponed to keep diffs small.
- **Blast radius:** dead code obscures the real architecture and
  inflates the agent's reading load.
