# Module layer map

Memory and journal modules below implement the [curated-memory](memory-curated.md),
[review](memory-review.md), [decision/application/recovery](memory-recovery.md),
[shared-note lifecycle](memory-notes.md) and [journal](memory-journal.md) contracts.

```
Entry point
  niwa.el                  foundational data, command autoloads, install/uninstall, notes
  niwa-evil.el             opt-in Evil states, surface bindings, visual selection and replacement scope bridge

Data model
  niwa-structs.el          passive workspace/session/request/task data shapes, invariants and shared buffer-local state
  niwa-instruction-edit.el line-mapped visited source buffer synchronization
  niwa-instruction-undo.el source geometry and deletion undo/redo without activity rewind
  niwa-turn.el             request admission/cancellation and terminal settlement
  niwa-workspace.el        workspace detection, registry, and state lookup
  niwa-workspace-identity.el project-owned durable workspace identity
  niwa-journal-worker.el   isolated maintenance, consolidation storage and cold publication observations
  niwa-journal-store.el     immutable workspace digest, review, and decision publication
  niwa-journal-index.el     disposable journal discovery and bounded main prompt map
  niwa-journal-claim.el     bounded journal work ownership and durable outcomes
  niwa-journal-gc.el        bounded coordination collection and retained proof references
  niwa-journal-cleanup.el   accepted expiry manifests and retained turn coverage
  niwa-state-cleanup.el     reference-aware generated media and review-package collection
  niwa-journal-capture.el   completed-turn checkpoints and lifecycle sealing
  niwa-journal-process.el   bounded digest requests, outcome recovery, and scheduling
  niwa-journal-idle.el      idle sealing, session-open and idle maintenance opportunities
  niwa-journal-recovery.el  abandoned checkpoint recovery through source authority
  niwa-journal-discard.el   accepted omissions and recoverable source-pin release
  niwa-journal-jobs.el      pending journal inspection, retry, and discard commands
  niwa-journal-evidence.el  frozen completed transcript and bounded session working notes
  niwa-journal-pins.el      source-session and publication retention for captures
  niwa-models.el           model tier/provider resolution, context budget
  niwa-implementation-policy.el strict serializable implementation record, exact model resolution, current root policy
  niwa-memory-proposal.el  bounded consolidation reply and captured-scope validation
  niwa-memory-scope.el     bounded memory/instruction before-state and original target checks
  niwa-memory-reference.el bounded, dated workspace-path reference observations
  niwa-memory-investigation.el request-local Read/Glob/Grep scope, budgets, and cancellation
  niwa-memory-review.el    bounded sessionless consolidation request and validated reply
  niwa-memory-cycle.el     target-clock finite shared-note cohort admission and authenticated attempt recovery
  niwa-memory-pass.el      consolidation gates, admission, settlement, and checked auto apply
  niwa-working-note-files.el bounded shared-note observations, mtime eligibility, and heading discovery
  niwa-memory-notes.el     shared working-note capture, housekeeping, and checked permanent disposal
  niwa-memory-store.el     private pass evidence, accepted proposals, and review publication
  niwa-memory-cleanup.el   completed-review retention dependencies and expiry candidates
  niwa-memory-decision.el  owned proposal transactions, publication, inspection, and recovery
  niwa-memory-apply.el     complete topic/index and instruction transaction preparation
  niwa-memory-write.el     exact before/after row classification and checked target batches
  niwa-memory-library.el   current topic inventory and structured user deletion
  niwa-memory-list.el      candidates/memories/history cockpit, diffs, decisions, recovery
  niwa-hooks.el            project/user/skill/agent hook loading + runner
  niwa-prompt-submission.el accepted prompt + lifecycle-context transaction
  niwa-bash-analysis.el    conservative shell parsing and normalized command facts
  niwa-bash-policy.el      Bash classification and reusable rules
  niwa-bash-git.el         argument-aware Git inspection and hardened launch argv
  niwa-transport.el        remote reentrancy detection and idle-transport deferral
  niwa-sleep.el            OS sleep inhibitor while turns, commands or shares are active
  niwa-execution-target.el immutable local/TRAMP target, path domains, readiness
  niwa-execution-grants.el protected-boundary-aware child authority coverage
  niwa-execution.el        managed execution registry, admission, and facade
  niwa-execution-process.el opaque child, process-group, and spool lifecycle
  niwa-execution-spool.el  asynchronous retired-output compression and disposable logical resource views
  niwa-execution-transcript.el durable execution render data and archive reconciliation
  niwa-execution-scheduler.el fair session-scoped Bash admission
  niwa-execution-telemetry.el safe execution facts and profiler adaptation
  niwa-eval-capture.el     shared Eval value, output, message and backtrace capture
  niwa-eval-sandbox.el     persistent owner-scoped confined Eval protocol and idle expiry
  niwa-sandbox.el          optional Bubblewrap child-process confinement
  niwa-sandbox-grants.el   FD-backed grant mounts and the protected-path mount plan
  niwa-pin.el              symlink-refusing opens and writes of authorized tool paths
  niwa-telemetry.el        append-only lifecycle events and profiler capture
  niwa-telemetry-usage.el  frozen per-provider-call identity and normalized usage
  niwa-plan.el             lifecycle-neutral plan data and artifacts
  niwa-plan-handoff.el     durable accepted-plan preparation and kickoff
  niwa-permission-mode.el  mode normalization, effective request/invocation projection, session transitions
  niwa-permission-rules.el rule parsing, matching, buckets, resource grants
  niwa-permission-origin.el workspace-relative grant origin, containment and usable-authority projection
  niwa-permission-persistence.el target-aware authority store codec
  niwa-permissions.el      permission preflight and 8-step decision facade
  niwa-tool-permission.el permission-step orchestration, hooks, prompts, logging
  niwa-pipeline.el         tool context, standard steps, sequencing, ordering
  niwa-ptc-checkpoint.el  conversation-owned ToolCall audit checkpoints and restart recovery
  niwa-ptc-driver.el      ToolCall orchestration, nested calls, progress
  niwa-ptc-interpreter.el closed programmatic-tool-call evaluator and machine
  niwa-resource.el         resource-address grammar and attempt lifecycle
  niwa-resource-capf.el    resource-address completion
  niwa-permission-log.el   durable permission decision log
  niwa-tool-media.el       tool media storage, scrubbing, provider payloads
  niwa-tool-render-data.el render-data codec, provider scrubber, transcript mutation
  niwa-tool-registry.el    niwa-tool struct, niwa-define-tool macro
  niwa-tool-repair.el      structured validation and atomic input repair
  niwa-tool-repair-gptel.el  lossless gptel argument decoding bridge
  niwa-tool-repair-diagnostics.el  repair audit and telemetry
  niwa-queue.el            session lookup and interaction entry metadata
  niwa-permission-queue.el permission/Bash/Eval/execution-authority queue
  niwa-permission-review.el optional invocation approval before human queue admission
  niwa-reminders.el        shared reminder collection, staged delivery and firing policy
  niwa-history.el          retained reminders and provider response reconstruction
  niwa-history-search.el   cooperative saved-transcript discovery, filtering and search
  niwa-edit-diagnostics.el post-edit Flymake/Flycheck report state machine
  niwa-plugin-registry.el  plugin manifests, activation state, hook consent
  niwa-plugin-lifecycle.el managed Git install, update, and removal
  niwa-plugin-ui.el        plugin cockpit and /plugin command
  niwa-plugins.el          narrow session/workspace plugin facade
  niwa-skills-core.el      skill model, discovery, state, reload
  niwa-mention-bindings.el shared atomic mention validation and edit lifecycle
  niwa-skills-preparation.el argument substitution and body injection execution
  niwa-skills-syntax.el      shared authored Markdown and dependency syntax
  niwa-skills-invoke.el    request context, invocation, fork dispatch, model tools
  niwa-skills-input.el     user token binding, raw dispatch, inline projection
  niwa-skills-plan.el      deterministic user invocation planning and preparation
  niwa-skills-prompt.el    model-visible roster, reminders, activation
  niwa-skills-ui.el        slash commands, cockpit, completion, font-lock

Chat / view
  niwa-chat.el             session lifecycle
  niwa-session-rewind-merge.el staged smerge resolutions for atomic Rewind
  niwa-side-conversation.el  ephemeral /btw conversation lifecycle
  niwa-note-frame.el  note-anchored conversation child frame and right-hand fallback
  niwa-transcript.el       transcript span classification for view/persistence/compaction
  niwa-transcript-source.el per-consumer hook-proven transcript change frontiers
  niwa-transcript-audit.el hidden audit record encoding and structural parsing
  niwa-transcript-restore.el  transcript property restoration via the canonical grammar
  niwa-context-summary.el  stateless validated continuation/handoff/digest summary generation
  niwa-view.el             view mode, zones, and session coordination
  niwa-view-agent.el       agent transcript inspection, status rows, refresh
  niwa-view-execution.el   background-work panel: agent and execution rows, inline inspection, actions
  niwa-view-composer.el    composer geometry, submission, root dispatch, fork/send flow
  niwa-view-pair.el        transcript/composer window lookup, command rules and deferred repair
  niwa-view-input-files.el local file drops and clipboard-image input
  niwa-view-animation.el   prepared status frames, styles, and bounded color cache
  niwa-view-native.el      optional native presentation, placement, fallback and teardown
  native/niwa-view-native.c timed Wayland surfaces without Lisp frame callbacks
  niwa-present.el          once-per-session switch to cheaper PGTK presentation
  native/niwa-present.c    GL presentation, damage-only redraws, main-thread GLib dispatch
  niwa-native.el           native module build, prebuilt download and loading
  native/niwa-native.c     module entry and executable import redirection
  native/niwa-wait.c       guarded timed condition waits for telemetry sampling
  niwa-view-power.el       shared battery observations and effective FPS policy
  niwa-pending-inputs.el   pending queue, steering, delivery, and cockpit
  niwa-patch-review.el     staged ApplyPatch review UI
  niwa-plan-mode.el        Plan conversations and proposal approval UI
  niwa-view-interaction.el interaction registration, ordering, callback overlays, redraw
  niwa-view-control-transfer.el cooperative transfer polling, presentation, and commands
  niwa-view-disclosure.el  source-backed transcript disclosure state and actions
  niwa-view-prepare.el     deferred large-tool parsing, source validation, job lifecycle
  niwa-activity-group.el   activity-group rule shared by the view and browser
  niwa-view-render.el      transcript projection, source mapping, live navigation
  niwa-view-segments.el    historical session segment projection and navigation
  niwa-view-stream.el      request progress, view animation timer, streaming redraws
  niwa-gptel-stream-bridge.el private gptel stream compatibility advice
  niwa-view-audit.el       audit disclosure rendering
  niwa-view-zone.el        managed view-zone lifecycle + fragments
  niwa-view-history.el     view input history ring and persistence
  niwa-view-fontify.el     quiet generic and reusable Markdown fontification
  niwa-collaboration.el    live browser room and lifecycle facade
  niwa-collaboration-guest.el  untrusted guest protocol and input handling
  niwa-collaboration-owner.el owner-link permission and session authorities
  niwa-collaboration-lobby.el  per-workspace lobby: persistent link and restart, session list, open
  niwa-collaboration-artifact-room.el artifact rooms: one shared artifact behind persistent view/edit links
  niwa-collaboration-agent.el  browser agent roster and transcript fetch
  niwa-collaboration-background.el native-ordered work frames, scoped stop and source lookup
  niwa-collaboration-goal.el   read-only durable Goal status and accounting
  niwa-collaboration-artifact-projection.el ApplyPatch artifact projection
  niwa-collaboration-artifact.el browser artifact fetch and notifications
  niwa-collaboration-artifact-history.el bounded read-only HTML discussion answers from canonical history
  niwa-collaboration-files.el  browser project files: list, read, upload, trash
  niwa-collaboration-discussions.el discussions about store items: actions, routing, requests, auto-resolve
  niwa-collaboration-history.el archived browser segments and artifact metadata
  niwa-collaboration-editing.el browser shared editing and selection questions
  niwa-collaboration-presence.el who else is on each guest's page; lobby counts
  niwa-shared-conversation.el item-scoped request history from canonical live/archive turns
  niwa-artifact-backend.el adapter registry, content calls, catalog and change events for items
  niwa-remote-artifact.el  remote artifacts: attach/detach links, the remote adapter, the package bridge
  niwa-shared-editing.el   private editor engine queue, leased store commits, item creation
  niwa-shared-library.el   host whiteboard element library and public Excalidraw collection
  niwa-collaboration-projection.el canonical, retained browser transcript projection
  niwa-collaboration-task.el browser task projection and publication
  niwa-collaboration-share.el bearer-link and QR presentation surface
  niwa-collaboration-transport.el sealed relay WebSocket client
  niwa-view-markdown.el    Markdown links, images, paths, source panels
  niwa-view-path.el        deferred target path verification and memoization
  niwa-view-table.el       rendered pipe tables and window realignment
  niwa-cockpit.el          shared tabulated cockpit surface plumbing
  niwa-window.el           temporary surface display, origin and exact frame restoration
  niwa-report.el           read-only sections, memory navigation, report windows
  niwa-subscription-usage.el session-owned quota report, freshness and cancellation
  niwa-subscription-usage-provider.el isolated Codex HTTP and native Claude ACP quota retrieval, as display text or plain readings
  niwa-menu.el             session cockpit transient and model selection
  niwa-gptel-bridge.el     view-launched gptel menu, restoration, and steering routing
  niwa-background-list.el  background-work cockpit: agents and all live executions, user controls
  niwa-artifacts-list.el   artifacts cockpit: list, create, open, attach, versions, duplicate, delete
  niwa-permissions-list.el remembered authority cockpit and per-row revoke
  niwa-worktree.el         Git worktrees, status/list surfaces, fork plumbing
  niwa-instruction-registry.el workspace instruction buckets, IDs, links
  niwa-overlays.el         instruction geometry, tags, context, prompts
  niwa-note-source.el note anchor/presentation lifecycle and command-scoped replacement
  niwa-overlay-ui.el       instruction overlay actions and rendering
  niwa-notes.el            source note actions, labels and entry hints; owns the note lifecycle transitions (make, mark-sent, hold-change, record-applied, mark-stale, set-settings, keep), the note Rewind rule and the open-note queries
  niwa-notes-activation.el `niwa-notes-global-mode` file-visit restoration and roots-only remote note discovery
  niwa-note-conversation.el isolated durable note send/batch/reopen, composer controls and sent-input inspection
  niwa-note-settings.el    persisted unsent settings and live session settings through the chat model picker
  niwa-note-change.el      nonblocking prepared source changes, checked apply, detailed review and actual per-turn application history
  niwa-note-merge.el       separate localized three-way smerge editor with checked explicit confirmation
  niwa-notes-list.el       workspace notes grouped by attention, visit/edit/tag/send/close/restore actions
  niwa-note-mentions.el    explicit note number/tag attachments, current code and UUID lookup
  niwa-note-resource.el    read-only workspace note documents for Read, Glob and Grep
  niwa-note-write.el       model note mutations, read-only authority and batched store sync
  niwa-mentions.el         @note, @ref and @file mention expansion
  niwa-persistence.el      workspace note store, export/import, restore and Rewind timing and stored-data pruning
  niwa-session-codec.el    closed session sidecar codec and validation
  niwa-session-checkpoint.el committed root projection, conversation consumption, and deferred ToolCall retirement
  niwa-session-artifacts.el  paths, artifacts, snapshots, and segment writes
  niwa-session-bounds.el   writer-scoped incremental gptel response bounds
  niwa-artifact-store.el   workspace artifact store: metadata, versions, attachment, conversations
  niwa-artifact-lease.el   cross-Emacs leases on store whiteboards and documents
  niwa-session-durability.el lease and storage primitives
  niwa-session-recovery.el  specialized recovery protocol and markers
  niwa-session-transfer.el  durable cooperative control transfer protocol
  niwa-session-publication.el immutable publication (async jobs stage, digest and select), retention/deletion, diagnostics
  niwa-session-collection.el  coalesced idle generation scans and cleanup lifecycle
  niwa-session-save-as.el portable Save As transaction and adoption
  niwa-session-persistence.el  lifecycle/resume/listing/locking/cleanup facade
  niwa-session-rewind.el   restore plans, transactional Rewind, published-head redo
  niwa-session-fork.el     Fork/Worktree projection and publication
  niwa-session-naming.el   metadata-only rename and bounded background session titles
  niwa-session-control-fs.el   pinned target-side session control filesystem
  niwa-session-control-transfer.el  control-transfer state, drains, descriptors
  niwa-compact-estimation.el compaction token accounting and admission
  niwa-compact-evidence.el transcript evidence and tool-safe truncation
  niwa-compact-target.el   root/agent archive and application transactions
  niwa-compact-run.el      async compaction retry/cancel/settlement
  niwa-compact.el          public compaction command and gptel gate

Prompt / presets / agents
  niwa-system.el           system prompt assembly
  niwa-context-delivery.el retained dynamic context and selected-history acknowledgement
  niwa-presets.el          gptel presets and request-time FSM assembly
  niwa-gptel-retry.el      admitted native gptel sample retries across backends, conservative error classification, small Responses normalization, backoff and bounded diagnostics
  niwa-agents.el           worker/explorer/verifier/reviewer definitions
  niwa-agent-conversation.el  retained conversation buffers, activity, and saves
  niwa-agent-directory.el  same-target workspace/worktree cwd validation without authority grants
  niwa-agent-control.el    retained-agent tree addressing, turns, mail, waits
  niwa-agent-exec.el       sub-agent request runner and FSM handlers
  niwa-agent-persistence.el durable agent registry codec and cold hydration
  niwa-agent-runtime.el    retained agent request lifecycle and settlement
  niwa-goal.el             phase-free Goal continuation controller
  niwa-schedule.el         session wake-ups: timing, store, one-timer scheduler, mailbox firing
  niwa-workspace-schedule.el workspace schedules: locked store, exclusive run claims, headless run sessions
  niwa-workspace-schedule-runs.el   run outcomes, failure notices and standing-job pause/resume
  niwa-schedules-list.el   standing jobs cockpit and linked run history
  niwa-review.el           /review picker, reviewer output parsing, parent transcript injection

Provider readiness and recovery (all providers)
  niwa-recovery.el         durable operational issues and unsubmitted input DTOs
  niwa-auth.el             asynchronous credential renewal and owner login challenges
  niwa-readiness.el        pre-commit provider checks and recovery wakeups

Turn engines (Claude Code through ACP and MCP)
  niwa-collaboration-recovery.el owner-only browser recovery actions and state
  niwa-claude-code-maintenance.el staged stable updates, validation, retention and rollback, and the bounded status-command runner shared with launch preflight
  niwa-engine.el           shared turn-context access for requests and native gptel FSMs
  niwa-acp.el              ACP conversation startup, prompt, cancellation and process lifecycle
  niwa-acp-text.el         isolated streamed background requests with buffer-owned teardown
  niwa-acp-workload.el     scoped background MCP tools and native batch-boundary guards
  niwa-acp-turn.el         admitted ACP turns, MCP tool scope, streaming and shared settlement
  niwa-acp-compaction.el   native summary lifecycle, transcript rotation and retained tails
  niwa-claude-code-backend.el provider type, model catalog, installation settings (executable/adapter/directory), minimum CLI/adapter versions and version parser, autoloaded engine dispatch
  niwa-claude-code.el      guided setup, launch preflight and isolated ACP launch
  niwa-claude-code-session.el root/note and shared-item sends, historical turn exclusion and external conversation references
  niwa-claude-code-btw.el  ephemeral native history fork, side dispatch and deletion lifecycle
  niwa-claude-code-agent.el retained-child subscription dispatch, sample limits and native history references
  niwa-claude-code-history.el native history state, divergence and excerpt recovery
  niwa-claude-code-context.el native context receipts, compaction restoration and full-context continuation
  niwa-claude-code-usage.el per-sample native usage, duplicate snapshots and cumulative accounting across prompt continuations
  niwa-mcp.el              private asynchronous MCP transport and stdio bridge configuration
  niwa-mcp-tools.el        turn-owned MCP schema export and pipeline result projection

Tools (each dispatches through niwa-pipeline)
  niwa-tool-ptc.el        ToolCall roster, request adapter, registration
  niwa-tool-fs.el          filesystem tool registration and shared path/resource primitives
  niwa-tool-fs-read.el     Read text/media decoding and bounded output
  niwa-tool-fs-search.el   Glob/Grep execution and resource-output privacy
  niwa-tool-patch-registration.el ApplyPatch catalog and callback autoloads
  niwa-tool-patch.el       ApplyPatch parse/match/apply engine and rendering
  niwa-tool-note.el        NoteWrite catalog and handler autoloads
  niwa-tool-code.el        XrefReferences, XrefDefinitions, Imenu, Treesitter
  niwa-tool-exec-permission.el Bash/Eval authority and prompt adapters
  niwa-tool-exec-registration.el Bash/Eval/execution-control catalog and callback autoloads
  niwa-tool-exec.el        Bash/Eval lifecycle and rendering
  niwa-tool-web-registration.el WebSearch/WebFetch catalog and callback autoloads
  niwa-tool-web.el         WebSearch/WebFetch execution and rendering
  niwa-interaction-prompt.el  shared interaction overlay lifecycle
  niwa-permission-prompt.el   generic, Bash, Eval, and execution-authority prompt UI
  niwa-tool-ask.el         Ask handler, result renderer, registration
  niwa-tool-ask-ui.el      Ask form state, controllers, and presentation
  niwa-tool-ui.el          Agent/InterruptAgent/ToolSearch/SendMessage assembly
  niwa-tool-task.el        TaskWrite/TaskList + overlay
  niwa-tool-goal.el        CreateGoal/GetGoal/UpdateGoal tools and root-request authority checks
  niwa-tool-schedule.el    Schedule tool: root-only wake-ups and approval-carded workspace schedules
  niwa-tool-editing.el     SharedCreate/Edit/SaveImage tools, shared:// views
  niwa-tool-skills.el      Skill and ListSkills tool schemas
  niwa-tool-introspect.el  EmacsDescribe/EmacsFind/EmacsManual introspection
  niwa-tool-usage.el       SubscriptionUsage: plain quota readings across subscription backends
  niwa-buddy.el            edit recording, diff assembly, briefs, review requests
  niwa-buddy-feed.el       /buddy: feed a session's plan, Goal, or exchange to Buddy
  niwa-buddy-note.el       Buddy-authored workspace notes and review-local tools
  niwa-tools.el            complete tool registration + stable discovery catalog
  niwa-tools-list.el       native tools cockpit list

Support
  niwa-change.el           recorded byte-change preparation, hunk conflicts, strict rollback
  niwa-file-state.el       LRU file cache
  niwa-diff-apply.el       transactional unified diff staging/application
  niwa-theme-faces.el      active-theme-derived face registration and refresh
  niwa-utilities.el        shared text/path/timer helpers + nonblocking socket cleanup
  niwa-init.el             repository guidance bootstrap command
```
