# Architecture decisions

ADRs explain why the current architecture takes its shape. The manual pages
linked from the [documentation map](../index.md#documentation-map) describe
behavior and operation.

## Maintaining decisions

Keep one record per coherent decision with a current decision, rationale and
consequences, and a decision history when the choice has changed. Fold amendments
into the current explanation. Preserve the prior choice, reason for changing it,
replacement, and original IDs in history; do not make readers follow obsolete
records to understand today's design. Keep independently meaningful decisions
separate. Reference-only details belong in the manual.

Update the record with its implementation. A reversal needs explicit historical
rationale, not a plan presented as current behavior. State missing evidence
honestly. When consolidating records, update the index and all in-repository
references before removing redundant files. New decisions receive new IDs;
existing records keep theirs.

## Decision index

Every original ID has a destination below. A historical destination preserves
that record's rationale inside the current decision. Other rows lead directly
to the decision bearing that ID.

| Original ID | Decision | Current location |
| --- | --- | --- |
| 0001 | Worktree command and skill | [ADR 0001](0001-worktree-command-and-skill.md) |
| 0002 | Agent Resource Roots | [ADR 0002](0002-agents-resource-roots.md) |
| 0003 | Plugin Activation Enables Implemented Components | [ADR 0003](0003-plugin-activation-enables-components.md) |
| 0004 | Session cockpit owns view status | [ADR 0004](0004-session-cockpit-owns-view-status.md) |
| 0005 | Pending Plugin Hook Consent Interrupts Interactive Startup | [ADR 0005](0005-pending-plugin-hook-consent.md) |
| 0006 | Tabulated Cockpit Surfaces Share Table Plumbing | [ADR 0105 — history](0105-cockpit-surfaces-follow-three-archetypes.md#decision-history) |
| 0007 | Worktree Has A Status Menu And List Surface | [ADR 0007](0007-worktree-has-status-menu-and-list-surface.md) |
| 0008 | Skill Roster Is Prompt Context | [ADR 0008](0008-skill-roster-is-prompt-context.md) |
| 0009 | Hook Audit Surfaces | [ADR 0009](0009-hook-audit-surfaces.md) |
| 0010 | Worktree session owns plan execution | [ADR 0010](0010-worktree-session-owns-plan-execution.md) |
| 0011 | Repair model tool input before pipeline execution | [ADR 0011](0011-repair-model-tool-input-before-pipeline.md) |
| 0012 | Layer Bash authority within the selected permission mode | [ADR 0012](0012-layer-bash-command-and-resource-authorization.md) |
| 0013 | Full-auto includes unconfined live Eval | [ADR 0081 — history](0081-full-auto-authorizes-live-eval.md#decision-history) |
| 0014 | Delegate invocation approval before human interruption | [ADR 0014](0014-permission-guardian-never-grants-authority.md) |
| 0015 | Best-effort confinement falls back only before execution | [ADR 0116 — history](0116-return-failed-confined-launches-without-retry.md#decision-history) |
| 0016 | Deny network by default and escalate additively | [ADR 0016](0016-deny-network-by-default-and-escalate-additively.md) |
| 0017 | Confine protected paths by default | [ADR 0017](0017-confine-protected-paths-by-default.md) |
| 0018 | Reuse Codex execution escalation vocabulary | [ADR 0018](0018-reuse-codex-execution-escalation-vocabulary.md) |
| 0019 | Remove model-visible RequestAccess | [ADR 0019](0019-remove-model-visible-request-access.md) |
| 0020 | Run Bash through a login shell | [ADR 0020](0020-run-bash-through-a-login-shell.md) |
| 0021 | Isolate guardian prompt boundaries | [ADR 0070 — history](0070-compose-system-prompts-from-ordered-profiles.md#decision-history) |
| 0022 | Bound automatic plan revision | [ADR 0067 — history](0067-separate-planning-from-goal-execution.md#decision-history) |
| 0023 | Goal outcomes outrank plan details | [ADR 0023](0023-goal-outcomes-outrank-plan-details.md) |
| 0024 | Yield Bash through session-owned executions | [ADR 0024](0024-yield-bash-through-session-owned-executions.md) |
| 0025 | Agents execute asynchronously | [ADR 0025](0025-agents-execute-asynchronously.md) |
| 0026 | Remove the privileged coordinator | [ADR 0026](0026-remove-the-privileged-coordinator.md) |
| 0027 | Restrict agent messaging to tree edges | [ADR 0037 — history](0037-allow-tree-wide-agent-communication.md#decision-history) |
| 0028 | Support nested agent delegation | [ADR 0030 — history](0030-agent-tool-grants-transitive-delegation-authority.md#decision-history) |
| 0029 | Limit active agent turns per session tree | [ADR 0029](0029-limit-active-agent-turns-per-session-tree.md) |
| 0030 | Agent tool grants transitive delegation authority | [ADR 0030](0030-agent-tool-grants-transitive-delegation-authority.md) |
| 0031 | Agent roles are optional overlays | [ADR 0031](0031-agent-roles-are-optional-overlays.md) |
| 0032 | Agent follow-ups retain conversation context | [ADR 0033 — history](0033-retain-settled-agents-for-the-session-lifetime.md#decision-history) |
| 0033 | Retain settled agents for the session lifetime | [ADR 0033](0033-retain-settled-agents-for-the-session-lifetime.md) |
| 0034 | Address agents by canonical task path | [ADR 0034](0034-address-agents-by-canonical-task-path.md) |
| 0035 | Separate agent messages from follow-up work | [ADR 0035](0035-separate-agent-messages-from-follow-up-work.md) |
| 0036 | Wait for agents explicitly | [ADR 0036](0036-wait-for-agents-explicitly.md) |
| 0037 | Allow tree-wide agent communication | [ADR 0037](0037-allow-tree-wide-agent-communication.md) |
| 0038 | List the addressable agent roster | [ADR 0038](0038-list-the-addressable-agent-roster.md) |
| 0039 | Interrupt agent turns without removing agents | [ADR 0039](0039-interrupt-agent-turns-without-removing-agents.md) |
| 0040 | Fork an explicit parent context snapshot | [ADR 0095 — history](0095-select-child-agent-context-explicitly.md#decision-history) |
| 0041 | Allow explicit agent model overrides | [ADR 0041](0041-allow-explicit-agent-model-overrides.md) |
| 0042 | Use one minimal agent spawn schema | [ADR 0095 — history](0095-select-child-agent-context-explicitly.md#decision-history) |
| 0043 | Split agent observation from control tools | [ADR 0030 — history](0030-agent-tool-grants-transitive-delegation-authority.md#decision-history) |
| 0044 | Bound agent waits with a successful timeout | [ADR 0036 — history](0036-wait-for-agents-explicitly.md#decision-history) |
| 0045 | Address agent tools only by path | [ADR 0034 — history](0034-address-agents-by-canonical-task-path.md#decision-history) |
| 0046 | Use plain-text agent messages | [ADR 0035 — history](0035-separate-agent-messages-from-follow-up-work.md#decision-history) |
| 0047 | Keep agent command acknowledgements minimal | [ADR 0035 — history](0035-separate-agent-messages-from-follow-up-work.md#decision-history) |
| 0048 | Deliver agent mail separately from wait results | [ADR 0035 — history](0035-separate-agent-messages-from-follow-up-work.md#decision-history) |
| 0049 | Persist unread agent mail | [ADR 0035 — history](0035-separate-agent-messages-from-follow-up-work.md#decision-history) |
| 0050 | Return every agent turn result to the spawn parent | [ADR 0035 — history](0035-separate-agent-messages-from-follow-up-work.md#decision-history) |
| 0051 | Use one agent result envelope | [ADR 0035 — history](0035-separate-agent-messages-from-follow-up-work.md#decision-history) |
| 0052 | Bound inline agent results | [ADR 0035 — history](0035-separate-agent-messages-from-follow-up-work.md#decision-history) |
| 0053 | Limit automatic agent roster context | [ADR 0053](0053-limit-automatic-agent-roster-context.md) |
| 0054 | Render agent activity from canonical events | [ADR 0054](0054-render-agent-activity-from-canonical-events.md) |
| 0055 | Charge capacity only for new follow-up turns | [ADR 0029 — history](0029-limit-active-agent-turns-per-session-tree.md#decision-history) |
| 0056 | Fork effective post-compaction context | [ADR 0095 — history](0095-select-child-agent-context-explicitly.md#decision-history) |
| 0057 | Compact agent conversations independently | [ADR 0057](0057-compact-agent-conversations-independently.md) |
| 0058 | Freeze agent role configuration at spawn | [ADR 0058](0058-freeze-agent-role-configuration-at-spawn.md) |
| 0059 | Centralize agent permissions in the root session | [ADR 0059](0059-centralize-agent-permissions-in-the-root-session.md) |
| 0060 | Centralize agent interactions in the root session | [ADR 0060](0060-centralize-agent-interactions-in-the-root-session.md) |
| 0061 | Recover abandoned agent turns as interrupted | [ADR 0061](0061-recover-abandoned-agent-turns-as-interrupted.md) |
| 0062 | Do not clone live agents into session forks | [ADR 0062](0062-do-not-clone-live-agents-into-session-forks.md) |
| 0063 | Persist the agent registry explicitly | [ADR 0063](0063-persist-the-agent-registry-explicitly.md) |
| 0064 | Publish agent spawns atomically | [ADR 0064](0064-publish-agent-spawns-atomically.md) |
| 0065 | Give worker an independent implementation toolset | [ADR 0030 — history](0030-agent-tool-grants-transitive-delegation-authority.md#decision-history) |
| 0066 | Hooks Follow Lifecycle Boundaries, Not Model Requests | [ADR 0066](0066-hooks-follow-lifecycle-boundaries.md) |
| 0067 | Separate planning from Goal execution | [ADR 0067](0067-separate-planning-from-goal-execution.md) |
| 0068 | Keep Goal authority outside conversation history | [ADR 0068](0068-keep-goal-authority-outside-conversation-history.md) |
| 0069 | Drive Goals with idle continuation | [ADR 0069](0069-drive-goals-with-idle-continuation.md) |
| 0070 | Compose system prompts from ordered profiles | [ADR 0070](0070-compose-system-prompts-from-ordered-profiles.md) |
| 0071 | Isolate agent tasks by default | [ADR 0095 — history](0095-select-child-agent-context-explicitly.md#decision-history) |
| 0072 | Make rewind in-place undo | [ADR 0072](0072-make-rewind-in-place-undo.md) |
| 0073 | Identify fork points independently of turn numbers | [ADR 0073](0073-identify-fork-points-stably.md) |
| 0074 | Keep direct user authority above heuristics | [ADR 0074](0074-keep-direct-user-authority-above-heuristics.md) |
| 0075 | Use one canonical permission-mode vocabulary | [ADR 0075](0075-use-one-canonical-permission-mode-vocabulary.md) |
| 0076 | Name fail-open confinement best-effort | [ADR 0076](0076-name-fail-open-confinement-best-effort.md) |
| 0077 | Scope sandbox policy to the session | [ADR 0077](0077-scope-sandbox-policy-to-the-session.md) |
| 0078 | Default to best-effort confinement | [ADR 0078](0078-default-to-best-effort-confinement.md) |
| 0079 | Require an explicit escalation retry | [ADR 0086 — history](0086-reuse-approved-execution-permission-profiles.md#decision-history) |
| 0080 | Do not depend on guardian availability | [ADR 0080](0080-do-not-depend-on-guardian-availability.md) |
| 0081 | Full-auto authorizes live Eval | [ADR 0081](0081-full-auto-authorizes-live-eval.md) |
| 0082 | Full-auto authorizes additive network access | [ADR 0086 — history](0086-reuse-approved-execution-permission-profiles.md#decision-history) |
| 0083 | Store network authority in qualified tool rules | [ADR 0086 — history](0086-reuse-approved-execution-permission-profiles.md#decision-history) |
| 0084 | Limit reusable full escalation to literal operations | [ADR 0084](0084-limit-reusable-full-escalation-to-literal-operations.md) |
| 0085 | Full-auto permits destructive operations | [ADR 0085](0085-full-auto-permits-destructive-workspace-writes.md) |
| 0086 | Reuse approved execution permission profiles | [ADR 0086](0086-reuse-approved-execution-permission-profiles.md) |
| 0087 | Keep directive identity outside source overlays | [ADR 0087](0087-keep-directive-identity-outside-source-overlays.md) |
| 0088 | Keep directive activity outside main chat | [ADR 0091 — history](0091-render-directive-turns-in-the-shared-session-view.md#decision-history) |
| 0089 | Make revision state-dependent implementation | [ADR 0089](0089-make-revision-state-dependent-implementation.md) |
| 0090 | Enforce discussion as a read-only capability | [ADR 0090](0090-enforce-discussion-as-a-read-only-capability.md) |
| 0091 | Render directive turns in the shared session view | [ADR 0091](0091-render-directive-turns-in-the-shared-session-view.md) |
| 0092 | Use one patch-oriented file mutation tool | [ADR 0092](0092-use-one-patch-oriented-file-mutation-tool.md) |
| 0093 | Treat /btw as an ephemeral side conversation | [ADR 0093](0093-treat-btw-as-an-ephemeral-side-conversation.md) |
| 0094 | Centralize context-summary generation without centralizing workflows | [ADR 0094](0094-centralize-context-summary-generation.md) |
| 0095 | Select child-agent context explicitly | [ADR 0095](0095-select-child-agent-context-explicitly.md) |
| 0096 | Bind each session to one execution target | [ADR 0096](0096-bind-each-session-to-one-execution-target.md) |
| 0097 | Store durable sessions with their execution target | [ADR 0097](0097-store-durable-sessions-with-their-execution-target.md) |
| 0098 | Store unsettled mutation in the session lease | [ADR 0098](0098-store-unsettled-mutation-in-the-session-lease.md) |
| 0099 | Project live collaboration from host-authoritative state | [ADR 0099](0099-project-live-collaboration-from-host-authoritative-state.md) |
| 0100 | Use one portable authority for project sessions | [ADR 0100](0100-portable-project-session-authority.md) |
| 0101 | Carry control operations as one pinned program | [ADR 0101](0101-carry-control-operations-as-one-pinned-program.md) |
| 0102 | Settle remote stops by zombie-aware probing | [ADR 0102](0102-settle-remote-stops-by-zombie-aware-probing.md) |
| 0103 | Spawn remote Bash on a private channel | [ADR 0103](0103-spawn-remote-bash-on-a-private-channel.md) |
| 0104 | Keep resource addresses closed and capability-neutral | [ADR 0104](0104-keep-resource-addresses-closed-and-capability-neutral.md) |
| 0105 | Cockpit Surfaces Follow Three Archetypes | [ADR 0105](0105-cockpit-surfaces-follow-three-archetypes.md) |
| 0106 | The Directive Frame Is A Child Frame, Not An Overlay | [ADR 0106](0106-directive-frame-is-a-child-frame.md) |
| 0107 | Non-Owner Buffers Follow Published State | [ADR 0107](0107-non-owner-buffers-follow-published-state.md) |
| 0108 | Buddy notes are not instructions | [ADR 0108](0108-buddy-notes-are-not-instructions.md) |
| 0109 | Edit patch changes side by side | [ADR 0109](0109-edit-patch-changes-side-by-side.md) |
| 0110 | Request config lives in the sidecar, not Org properties | [ADR 0110](0110-request-config-lives-in-the-sidecar-not-org-properties.md) |
| 0111 | Run programmatic tool calls in a closed machine | [ADR 0111](0111-run-programmatic-tool-calls-in-a-closed-machine.md) |
| 0112 | Debounce observational agent persists | [ADR 0112](0112-debounce-observational-agent-persists.md) |
| 0113 | Parent Ask Sample Frames To The Top-Level Frame | [ADR 0113](0113-parent-ask-sample-frames-to-the-top-level-frame.md) |
| 0114 | Tie collaboration room lifetime to the host share | [ADR 0114](0114-tie-collaboration-room-lifetime-to-host-share.md) |
| 0115 | Retain delivered reminders and completed provider response fragments | [ADR 0115](0115-retain-delivered-conversation-fragments.md) |
| 0116 | Return failed confined launches without retry | [ADR 0116](0116-return-failed-confined-launches-without-retry.md) |
| 0117 | Publish journal results from fenced outcomes | [ADR 0117](0117-publish-journal-results-from-fenced-outcomes.md) |

## Additional current records

- [ADR 0118: Keep diagnostics observational and bounded](0118-keep-diagnostics-observational-and-bounded.md)
  gathers existing rationale from the telemetry manual; no original ADR is replaced.

- [ADR 0119: Keep views reconstructable and rendering bounded](0119-keep-views-reconstructable-and-rendering-bounded.md).

- [ADR 0120: Edit shared content through the session host](0120-edit-shared-content-through-the-session-host.md).

- [ADR 0121: Store whiteboards as Excalidraw elements](0121-store-whiteboards-as-excalidraw-elements.md)
  amends ADR 0120's whiteboard schema, image transforms and connector rendering.

- [ADR 0122: Let full links change project files directly](0122-let-full-links-change-project-files-directly.md).

- [ADR 0123: Keep turn authority in mevedel across model engines](0123-keep-turn-authority-in-mevedel.md)
  also owns the native context-receipt decision; ADR 0115 keeps the gptel payload
  rule. No original ADR is replaced.

- [ADR 0124: Keep artifacts in a workspace store that sessions attach to](0124-keep-artifacts-in-a-workspace-store.md)
  amends ADR 0099's published-record authority, ADR 0120's per-session storage
  and queue, and ADR 0122's lobby with an Artifacts tab.
