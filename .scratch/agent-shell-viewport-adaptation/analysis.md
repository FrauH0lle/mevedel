---
title: Agent-shell viewport ideas for mevedel
status: exploratory
source: read-only comparison of ~/agent-shell and mevedel view architecture
---

# Agent-shell viewport ideas for mevedel

This note summarizes read-only analysis of `~/agent-shell/` and compares it with mevedel's current view architecture. The focus is on ideas that could be adapted to mevedel, especially around viewport/view rendering.

## Summary

Agent-shell has useful ideas, but its viewport should not be ported wholesale. Mevedel already has the same broad architectural shape: an authoritative transcript/data buffer plus an ephemeral user-facing view buffer.

Relevant mevedel references:

- `docs/view.md:3` documents the dual-buffer model.
- `docs/view.md:18` says durable conversation state should not live only in view overlays/text properties.
- `mevedel-view.el:5` and `mevedel-view.el:10` describe the data buffer as authoritative and the view buffer as user-facing/ephemeral.

The strongest adaptation opportunity is narrower:

> Extract a small reusable fragment/block UI layer for mevedel's status, interaction, tool/live-tail, permission, and agent-rendering regions, inspired by `agent-shell-ui.el`.

That would address a real mevedel pain point: `mevedel-view.el` currently owns many separate marker/insertion/update paths, and those are fragile around composer preservation.

## Agent-shell ideas worth adapting

### 1. Fragment/block UI model for interaction/status/live regions

Agent-shell has a reusable fragment model:

- Fragment fields like `:namespace-id`, `:block-id`, `:label-left`, `:label-right`, and `:body`: `/home/roland/agent-shell/agent-shell-ui.el:45`.
- Rendered state stored in text properties such as `agent-shell-ui-state` and `agent-shell-ui-section`: `/home/roland/agent-shell/agent-shell-ui.el:569`.
- Label/body sections tagged in text properties: `/home/roland/agent-shell/agent-shell-ui.el:477`, `/home/roland/agent-shell/agent-shell-ui.el:512`, `/home/roland/agent-shell/agent-shell-ui.el:550`.

Mevedel currently has separate implementations for several related UI areas:

- Task status overlays: `mevedel-tool-task.el:1089`.
- Agent status rendering: `mevedel-view.el:10311`.
- Interaction descriptors: `mevedel-view.el:11196`.
- Request progress rows: `mevedel-view.el:5471`.
- Live-tail insertion and recovery: `mevedel-view.el:5446`.

A `mevedel-view-fragment.el` or similar module could provide a common primitive:

```elisp
(:namespace interaction
 :id permission-123
 :priority 100
 :label-left "Permission"
 :label-right "Bash"
 :body ...
 :keymap ...
 :navigatable t
 :collapsed nil)
```

Then status, interaction, tool, and agent UI could update by stable identity rather than through ad hoc marker juggling.

This is the most directly valuable idea.

### 2. Incremental streaming updates by section

Agent-shell updates an existing fragment by section and appends only new body chunks, specifically to keep streaming updates cheap and avoid re-rendering stable regions:

- `/home/roland/agent-shell/agent-shell-ui.el:61`
- `/home/roland/agent-shell/agent-shell-ui.el:75`

Mevedel has a correctness-first full rerender path in `mevedel-view.el:8245`, plus debounced rerenders in `mevedel-view.el:8114`. That is robust, but it increases the surface area for composer corruption.

A section-based update API could help with:

- pending tool live tails
- streaming assistant text
- agent status rows
- task overlay refreshes
- interaction-zone changes

The invariant should remain: the data buffer is authoritative, and view fragments are disposable UI cache. That matches mevedel's contract in `docs/view.md:18`.

### 3. Reduce marker insertion-type complexity

Mevedel's current zone model relies heavily on markers:

- input marker: `mevedel-view.el:755`
- status marker: `mevedel-view.el:762`
- interaction marker: `mevedel-view.el:767`

The view implementation has multiple manual insertion-type flips and repair paths, for example:

- `mevedel-view.el:1019`
- `mevedel-view.el:1047`
- `mevedel-view.el:8625`
- `mevedel-tool-task.el:1131`

Agent-shell avoids some of that by storing per-block identity and section boundaries in text properties and updating those regions directly.

Mevedel should probably keep its high-level zone markers. They are useful for layout. The adaptation would be to move subregions inside each zone from marker-managed ad hoc regions to fragment-managed blocks.

A good first target is the interaction zone, because it already has a descriptor registry at `mevedel-view.el:11423`.

### 4. Explicit reusable collapse/navigation behavior

Agent-shell blocks support:

- per-fragment fold state: `/home/roland/agent-shell/agent-shell-ui.el:667`
- next/previous navigation by text property: `/home/roland/agent-shell/agent-shell-ui.el:886`
- global fold toggle: `/home/roland/agent-shell/agent-shell-ui.el:791`
- isearch expanding collapsed matches: `/home/roland/agent-shell/agent-shell-ui.el:964`

This could map well to mevedel's rendered tool calls, agent transcript blocks, permission blocks, and task overlays.

Mevedel already has rendered agent transcript views via `mevedel-view.el:9849`, and clickable handles/status via `mevedel-view.el:9731`. A shared block navigation/collapse layer would make those surfaces more consistent.

### 5. Header/status as data model plus renderer

Agent-shell builds a header model first, then renders it as text/SVG depending on configuration:

- `/home/roland/agent-shell/agent-shell.el:3773`
- `/home/roland/agent-shell/agent-shell.el:3808`

Mevedel's status surface is spread across task status, agent status, request progress, interaction descriptors, and composer prompt refresh. A model-first renderer could make this easier to reason about.

This is lower priority than fragments, but still useful:

```elisp
(mevedel-view--status-model session)
(mevedel-view--render-status model)
```

That would also make tests easier because the model can be asserted without inspecting all buffer text and overlays.

### 6. Inline permission/action buttons as ordinary propertized text

Agent-shell permission buttons are propertized text with keymaps/actions:

- `/home/roland/agent-shell/agent-shell.el:6809`
- navigation by text property: `/home/roland/agent-shell/agent-shell.el:6931`

Mevedel already has permission and ask flows in the interaction zone. The useful part to adapt is not the button idea itself, but standardizing actionable UI elements under the same fragment/block system.

That could make `Ask`, permission, plan approval, preview accept/reject, and queued-message UI behave consistently.

## Things not to copy directly

### Do not replace mevedel's dual-buffer architecture

Agent-shell's viewport is also a projection buffer over a canonical shell/comint buffer: `/home/roland/agent-shell/agent-shell-viewport.el:23`.

Mevedel already has that model:

- documented in `docs/view.md:3`
- implemented in `mevedel-view.el:5`
- data buffer remains authoritative per `docs/view.md:18`

The valuable move is internal modularization, not architectural replacement.

### Do not adopt name-suffix buffer association as-is

Agent-shell maps shell buffer to viewport buffer with a suffix `" [viewport]"`: `/home/roland/agent-shell/agent-shell-viewport.el:608`.

Mevedel already tracks session, workspace, and view relationships more explicitly. A suffix-only relation would be too weak for mevedel's session persistence, agent transcript views, and side-window behavior.

### Be cautious with explicit edit/view major modes

Agent-shell has separate edit/view major modes:

- `/home/roland/agent-shell/agent-shell-viewport.el:1385`
- `/home/roland/agent-shell/agent-shell-viewport.el:1399`

Mevedel has a richer single view mode with zones, composer preservation, mentions, drag/drop grants, preview, queues, and agent transcript variants. Splitting into major modes could be disruptive.

A safer adaptation would be internal state-specific keymaps or minor modes for transcript inspection, permission navigation, and block navigation, not a wholesale mode split.

## Suggested implementation path

### 1. Add a tiny fragment abstraction

Create an internal module, perhaps `mevedel-view-fragment.el`.

Initial capabilities:

- stable fragment identity
- label/body sections
- priority/order
- keymap/help-echo support
- collapsed/navigatable flags
- update-by-identity operation

No behavior changes initially.

### 2. Port only the interaction-zone painter first

The interaction zone already has descriptors at `mevedel-view.el:11423`. It is bounded and high-value.

Preserve the current descriptor API while rendering through fragments.

### 3. Add regression tests for composer preservation

Any status, agent, task, interaction, or async redraw change should test active composer drafts, especially multiline drafts whose first editable character is `>`.

This follows existing project guidance and the known regression class around async view/status redraws.

### 4. Then consider task and agent status

Task status currently materializes text/overlay from `mevedel-tool-task.el:1089`.

Agent status is built around `mevedel-view.el:10311`.

Both could become fragment producers once the primitive is proven in the interaction zone.

### 5. Only after that, consider live-tail/tool rendering

This has higher streaming and scroll risk. The payoff is bigger, but it should come after the fragment primitive is proven.

## Bottom line

The best idea from agent-shell is not "add a viewport"; mevedel already has one in spirit. The best idea is:

> Use a small, text-property-backed fragment/block layer inside the mevedel view, so status, interaction, tool, and agent UI can update by stable identity and section instead of each subsystem manually manipulating markers, overlays, and insertion types.

That would directly target mevedel's current fragility around async redraws and composer preservation while preserving its existing data-buffer-first architecture.
