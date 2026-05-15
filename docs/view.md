# View Buffer

`mevedel-view.el` renders a compact user-facing projection of the
authoritative gptel data buffer. The data buffer remains the
model-visible transcript; the view buffer owns display, interaction
controls, and the editable input region.

## Buffer Roles

- **Data buffer**: org-mode gptel buffer. Holds `mevedel--session`,
  `mevedel--workspace`, tool results, hidden render-data blocks, and
  persisted gptel metadata.
- **View buffer**: `mevedel-view-mode`. Holds `mevedel--data-buffer`,
  compact rendered turns, status/interaction zones, and editable input.
- **Agent transcript view**: rendered read-only projection of a saved
  sub-agent transcript file. It is opened on demand for terminal agents.

The view is reconstructable from the data buffer. Avoid storing durable
conversation state only in view overlays or text properties.

## Zones

The view has three anchored regions above the editable input:

- **Status zone**: tasks and aggregate agent status.
- **Interaction zone**: queued permission prompts, plan approval prompts,
  RequestAccess, Ask, and other user-response controls.
- **Input zone**: editable user prompt text.

The interaction-zone painter inserts descriptor bodies as real text and
wraps each span in an overlay. Register controls with
`mevedel-view--interaction-register`; do not direct-insert ad hoc UI
near the prompt.

The interaction separator and aggregate status badge are chrome overlays;
their text does not belong to the model-visible transcript.

## Input History

`mevedel-view-history.el` provides comint-style input history for the
view input region:

- `M-p` / `M-n`: previous / next input
- `M-r`: search history
- `C-c C-l`: browse history
- `C-c C-u`: clear current input
- `C-a`: beginning of input line

History persists per materialized session as `input-history.el`.
Read-only or non-persistent sessions keep history in memory only. Forks
copy the parent history sidecar; rewind keeps the current ring.

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
view/audit log matches the request payload. If no `WAIT` boundary occurs
before the active turn reaches successful `DONE`, the zero-delay
post-`DONE` drain sends the queued batch as the next normal user turn.
Aborted and errored turns leave queued prompts pending for review.

Editing queued prompts removes the whole uncommitted batch from the FIFO
and restores a combined draft to the composer, so it cannot be
auto-submitted while being edited.

## Agent Transcript Views

Agent handles and attribution fragments are clickable when a transcript
entry is available. Running agents show status/activity in the main
view. Terminal agents open a rendered read-only transcript view through
`mevedel-view-open-agent-transcript`.

The transcript view restores only gptel bounds/properties needed for
rendering. It does not restore backend/tool objects or become a live
agent buffer.

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
