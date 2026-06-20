# View Buffer

`mevedel-view.el` renders a compact user-facing projection of the
authoritative gptel data buffer. The data buffer remains the
model-visible transcript; the view buffer owns display, interaction
controls, and the input zone.

## Buffer Roles

- **Data buffer**: org-mode gptel buffer. Holds `mevedel--session`,
  `mevedel--workspace`, tool results, hidden render-data blocks, and
  persisted gptel metadata.
- **View buffer**: `mevedel-view-mode`. Holds `mevedel--data-buffer`,
  compact rendered turns, status and interaction zones, and the input zone.
- **Agent transcript view**: rendered read-only projection of a saved
  sub-agent transcript file. It is opened on demand for terminal agents.

The view is reconstructable from the data buffer. Avoid storing durable
conversation state only in view overlays or text properties.

## Render flow

```mermaid
flowchart TD
    A[Data buffer transcript] --> B[Parse turns and metadata]
    B --> C[Render history region]
    B --> D[Render status zone]
    B --> E[Render interaction zone]
    C --> F[Preserve composer text and point]
    D --> F
    E --> F
    F --> G[View buffer]
    G --> H[User submits composer]
    H --> A
```

## Zones

The view buffer is split into vertically ordered regions. The data buffer
remains the model-visible source of truth; view zones are display and
interaction chrome around that transcript.

```text
+--------------------------------------------------------------+
| Header / mode line chrome                                    |
+--------------------------------------------------------------+
| History region                                               |
|   Rendered user turns, assistant turns, tool summaries,      |
|   inline agent/tool handles, and any in-flight live tail.     |
+------------------------- status marker ----------------------+
| Status zone                                                  |
|   Task overlay and aggregate running/blocked agent rows.      |
+---------------------- interaction marker --------------------+
| Interaction zone                                             |
|   Permission prompts, plan approvals, RequestAccess, Ask,     |
|   queued follow-ups, and preview controls.                   |
+--------------------------------------------------------------+
| Request progress row                                         |
|   Bottom live spinner such as `Working...` or `Compacting...` |
|   while the foreground request is active.                    |
+-------------------------- input marker ----------------------+
| Input zone                                                   |
|   Read-only input prompt, then editable composer body.        |
+--------------------------------------------------------------+
```

Terminology:

- **History region**: rendered transcript above `mevedel-view--status-marker`.
  Pending tool rows like `Calling Read...` are fragment-backed live-tail
  history content, not status-zone content.
- **Status zone**: session status chrome between `mevedel-view--status-marker`
  and `mevedel-view--interaction-marker`; it is for task rows and aggregate
  agent status rows.
- **Interaction zone**: user-action chrome between
  `mevedel-view--interaction-marker` and the request progress row; it is for
  queued prompts and controls that require user response.
- **Request progress row**: the fragment-backed foreground spinner directly
  above the input prompt. It is not part of the history, status, or
  interaction zones.
- **Input zone**: the read-only prompt prefix plus the editable composer.
  **Composer** refers only to the editable unsent input body.

The interaction-zone painter renders descriptor bodies as `interaction`
fragments. Descriptor overlays may still span those fragments as callback
handles for prompt settlement and preview cleanup; they are not independent
renderers. Register controls with `mevedel-view--interaction-register`; do
not direct-insert ad hoc UI near the composer. Registering or rebuilding an
interaction must not auto-focus the prompt or move point out of the composer.
Interaction keybindings are active only when point is on the interaction text;
composer input must never settle or cycle interaction prompts.

The interaction separator is virtual chrome. Task rows, aggregate agent
status rows, interaction bodies, and request progress are view-owned UI
chrome; they do not belong to the model-visible transcript. The input
prompt starts with a read-only blank separator line so status,
interaction, and request-progress rows stay visually distinct from the
composer.

## Fragment-backed chrome

`mevedel-view-fragment.el` provides private primitives for disposable,
identity-backed blocks of view-owned chrome. A fragment is keyed by managed
region identity, namespace, and id. It may also carry priority, label/body
text, keymap/help text, activation metadata, navigation metadata, and a
collapse key. `mevedel-view-fragment--reconcile` owns one namespace inside
an explicit region: it sorts by descending priority and caller order, replaces
stale fragments only for that namespace in that region, and leaves other
regions or namespaces untouched.

Fragment metadata lives in `mevedel-view-fragment-*` text properties. Those
properties are valid for view navigation, activation, collapse, and targeted
refresh decisions, but they are UI cache only. Durable conversation state
continues to live in the data buffer and session structures. Region overlays
in `mevedel-view.el` define managed fragment containers; remaining interaction
or task overlays are compatibility/callback handles, not parallel renderers.

Current fragment namespaces:

- `history-live`: pending tool live-tail rows in the history region, built
  from `mevedel-view--pending-tool-calls`. They are removed and recreated
  from pending state; they must not be preserved as source-backed transcript
  text or deleted by heuristic `Calling ...` line matching.
- `status`: `tasks` and `agents` status-zone blocks. Task and aggregate-agent
  disclosure state is backed by fragment collapse state.
- `interaction`: queued user controls plus a non-navigatable `:separator`
  fragment. Ask, permission, plan, RequestAccess, preview, and queued-message
  callers continue to use the descriptor registry.
- `progress`: the foreground `request` progress row between the interaction
  zone and input prompt.

Source-backed transcript turns, tool summaries, and agent transcript handles
are intentionally outside this chrome-fragment model even when they are
clickable or collapsible. They are projections of the authoritative data
buffer and keep source-coordinate disclosure state. The incremental renderer
(`mevedel-view--render-incremental`) remains the correctness path for streaming
assistant text; fragment updates should not bypass the data-buffer transcript.
Revisit source-backed transcript fragments only as a separate design after a
concrete performance or correctness problem is identified.

High-level zone markers still define layout boundaries. Fragment producers
should mutate only their managed region under the appropriate boundary and
preservation wrapper so status, interaction, progress, and history-live
updates do not become composer text or move point/windows unexpectedly.

## Redraw invariants

Redraw paths must treat the composer as user-owned text. Full rerenders,
interaction rebuilds, status/task rows, spinner ticks, pending-tool live
lines, and targeted agent refreshes should preserve both composer text
and point while suppressing modification hooks for view-owned changes.

`mevedel-view-rerender` is the correctness fallback and is debounced for
bursty updates. Prefer narrower refresh paths when a stable source exists:
agent activity updates refresh source-backed handles and aggregate status
rows, then fall back to a full rerender only when the visible handle is
missing or stale.

Full rerenders rebuild the zone markers from the header, skip leading
compaction summaries, and re-anchor the in-flight assistant turn. Without
a valid in-flight anchor, the next incremental render can erase freshly
rendered history or duplicate a preserved live tail.

Temporary buffers used only to fontify or render view text must suppress
user major-mode hooks and local variables. Use
`mevedel-view--with-render-temp-buffer` rather than raw
`with-temp-buffer` plus mode activation.

Tool-rendering caches are disposable UI caches, not just text caches.
Cache keys must include session-side state that changes visible
headers/status, and collapsed-header cache entries should omit large
bodies so expansion can recompute body content when needed.

Source-backed disclosure state is keyed from data-buffer coordinates and
stable source anchors, not view-buffer positions. Rerenders should capture
and reapply collapse state, including temporary in-flight anchors that later
settle, so expanded tool/response sections do not collapse again during
live refreshes.

Live-tail duplicate detection should compare literal lines while skipping
volatile spinner/tool/agent rows. Avoid building one large regexp from
streamed transcript text; long agent outputs can overflow Emacs regexp
limits.

### Zone mockups

Idle session with no live status or queued controls:

```text
mevedel:main@project

> draft starts here
```

Active request with a pending tool live-tail row and queued follow-up:

```text
mevedel:main@project

You
Please inspect the view layout.

Assistant
I'll inspect the associated files.

Calling Read: mevedel-view.el...

-- 1 queued message pending -----------------------------------

Queued messages
  C-c C-e edit batch; C-c C-q clear
  1. Also check the docs.

Working... · 42s

> editable composer draft
```

Active tasks, agents, and an interaction prompt:

```text
mevedel:main@project

You
Implement the change.

Assistant
I'll work on the changes.

-- tasks -------------------------------------------------------
  Main 1 open
  - Run focused tests

  Agent: verifier -- review spinner layout [running · 1 call]

-- 1 permission prompt pending --------------------------------

Allow Bash?
  npx @emacs-eask/cli test ert test/test-mevedel-view.el

Working... · 1m 08s · 1 agent running

[plan]  >
```

Busy session showing every view-owned zone at once:

```text
mevedel:main@project

You
Update the view docs and verify the spinner layout.

Assistant
I'll update the docs, run the focused checks, and ask before any risky action.

Calling Read: docs/view.md...
Calling Grep: status zone...

-- tasks -------------------------------------------------------
  Main 2 open
  - Update docs with zone mockups
  - Run focused validation

  Agent: explorer -- audit zone terminology [running · 3 calls]
  Agent: verifier -- check spinner ordering [blocked · waiting]

-- 1 question · 1 permission · 2 queued messages pending ------

Ask
  Which validation should run next?
  [focused view tests] [compile] [full suite]

Permission request from verifier--a1b2c3
Allow Bash?
  npx @emacs-eask/cli test ert test/test-mevedel-view.el

Queued messages
  C-c C-e edit batch; C-c C-q clear
  1. Also include a full mockup with agents and permissions.
  2. Keep the request spinner pinned above the composer.

Working... · 2m 14s · 1 agent blocked · 1 agent running

[edits] > I am drafting a follow-up while the request runs.
```

## Input History

`mevedel-view-history.el` provides comint-style input history for the
view input zone:

- `M-p` / `M-n`: previous / next input
- `M-r`: search history
- `C-c C-l`: browse history
- `C-c C-u`: clear current input
- `C-a`: beginning of input line
- `Shift-TAB` / `<backtab>`: cycle the session permission mode

History persists at the workspace level as
`<workspace-root>/.mevedel/input-history.el`, so new and resumed
sessions in the same project share prompt recall. Read-only or
non-persistent sessions keep history in memory only. Rewind keeps the
current buffer-local ring.

The input zone installs slash completion and display-only skill
argument hints. Root slash completion offers local commands and
user-invocable skills and inserts a real space after a completed root
name. Command argument completion is available for commands with useful
candidate sets, such as `/mode` and `/model`. Skill hints are rendered
as a zero-width overlay near point from `argument-hint` or remaining
`arguments` names. They are not buffer text and are never sent to the
model.

Text inserted as a user turn must be plain transcript text. User send,
queued-drain, and synthetic user-role insertion paths strip copied view,
tool, read-only, and `gptel` text properties, then restore only internal
render-data blocks as `'gptel 'ignore`; UI properties copied from the view
must not become model-visible transcript state.

## File Drag/Drop

Interactive view buffers install a buffer-local DND handler for local
`file:` URIs. Dropping regular files inserts visible `@file` mentions in
the composer; paths with whitespace or other token-breaking characters use
the braced `@file:{...}` form. Directory drops are ignored.

Each dropped file also records a pending exact-file grant on the session.
If the next send still contains an `@file` mention for that same expanded
path, the grant becomes an in-memory session-scoped `Read` grant for that
exact path. The grant does not create a directory rule, does not apply to
write tools, and is not persisted with the session.

## Queued Follow-Ups

Plain user input submitted while a request is active is queued on the
session as a transient FIFO and shown in the interaction zone.
`UserPromptSubmit` runs when the prompt is queued; accepted entries store
their prepared model input, display text, hook context, and history
input. Slash commands are not queued; they continue to be rejected until
the active request finishes.

At the next `WAIT` boundary before an HTTP request is sent, all prepared
queued prompts drain as one explicit user-role batch block in FIFO order.
The same batch block is written to the data buffer transcript so the
view/audit log matches the request payload. Direct `WAIT` injection is
rendered live as generated user messages; the system-reminder and
queued-message XML wrappers are display/control wrappers, not prose the
user typed. If no `WAIT` boundary occurs before the active turn reaches
successful `DONE`, the zero-delay post-`DONE` drain sends the queued batch
as the next normal user turn. Aborted and errored turns leave queued prompts
pending for review.

Queued entries that contain `@` mentions, including dropped-file grants,
skip the direct `WAIT` injection path. They drain after the active turn as
a normal send so gptel prompt transforms can expand mentions and attach
media consistently.

Editing queued prompts removes the whole uncommitted batch from the FIFO
and restores a combined draft to the composer, so it cannot be
auto-submitted while being edited.

## Agent Transcript Views

Agent handles and attribution fragments are clickable when a transcript
entry is available. Running agents show status/activity in the main view
and may open a rendered read-only transcript view over the live agent
buffer while that buffer is available. Terminal agents open a rendered
read-only transcript view from the saved transcript file through
`mevedel-view-open-agent-transcript`.

Transcript views restore only the gptel bounds/properties needed for
rendering. They do not restore backend/tool objects or become live agent
buffers themselves.

## Hook Context Display

Model-visible `<hook-context>` blocks are stripped out of the rendered
user message body so injected policy/context does not look like text the
user typed. When such context is present, the view shows a compact
disclosure:

```text
  ◇ hook context added
```

Expanding it shows the hook event and injected context. This keeps
successful context injection quiet by default while still making it
auditable in the transcript view.

Tool calls blocked by `PreToolUse` or `PermissionRequest` stay visible as
normal tool attempts, with a short second line showing which hook blocked
the call and the hook-provided reason.
