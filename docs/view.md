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
- **Interaction zone**: queued permission prompts, PresentPlan prompts,
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

## Agent Transcript Views

Agent handles and attribution fragments are clickable when a transcript
entry is available. Running agents show status/activity in the main
view. Terminal agents open a rendered read-only transcript view through
`mevedel-view-open-agent-transcript`.

The transcript view restores only gptel bounds/properties needed for
rendering. It does not restore backend/tool objects or become a live
agent buffer.
