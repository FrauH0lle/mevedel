# Module layer map

```
Entry point
  mevedel.el                  foundational data, command autoloads, install/uninstall, directives

Data model
  mevedel-structs.el          passive workspace/session/request/task data shapes, invariants and shared buffer-local state
  mevedel-directive.el        directive mutation, lifecycle, plan invalidation, rewind
  mevedel-turn.el             request admission/cancellation and terminal settlement
  mevedel-workspace.el        workspace detection, registry, and state lookup
  mevedel-workspace-identity.el project-owned durable workspace identity
  mevedel-journal-worker.el   isolated maintenance, consolidation storage and cold publication observations
  mevedel-journal-store.el     immutable workspace digest, review, and decision publication
  mevedel-journal-index.el     disposable journal discovery and bounded main prompt map
  mevedel-journal-claim.el     bounded journal work ownership and durable outcomes
  mevedel-journal-gc.el        bounded coordination collection and retained proof references
  mevedel-journal-cleanup.el   accepted expiry manifests and retained turn coverage
  mevedel-state-cleanup.el     reference-aware generated media and review-package collection
  mevedel-journal-capture.el   completed-turn checkpoints and lifecycle sealing
  mevedel-journal-process.el   bounded digest requests, outcome recovery, and scheduling
  mevedel-journal-idle.el      idle sealing, session-open and idle maintenance opportunities
  mevedel-journal-recovery.el  abandoned checkpoint recovery through source authority
  mevedel-journal-discard.el   accepted omissions and recoverable source-pin release
  mevedel-journal-jobs.el      pending journal inspection, retry, and discard commands
  mevedel-journal-evidence.el  frozen completed transcript and bounded shared/session working notes
  mevedel-journal-pins.el      source-session and publication retention for captures
  mevedel-models.el           model tier/provider resolution, context budget
  mevedel-memory-proposal.el  bounded consolidation reply and captured-scope validation
  mevedel-memory-scope.el     bounded memory/instruction before-state and original target checks
  mevedel-memory-reference.el bounded, dated workspace-path reference observations
  mevedel-memory-investigation.el request-local Read/Glob/Grep scope, budgets, and cancellation
  mevedel-memory-review.el    bounded sessionless consolidation request and validated reply
  mevedel-memory-pass.el      consolidation gates, admission, settlement, and checked auto apply
  mevedel-memory-store.el     private pass evidence, accepted proposals, and review publication
  mevedel-memory-cleanup.el   completed-review retention dependencies and expiry candidates
  mevedel-memory-decision.el  immutable decisions, application, activation recovery, and rejection evidence
  mevedel-memory-apply.el     complete topic/index and instruction transaction preparation
  mevedel-memory-write.el     durable before/after intents, shared-root ownership, and checked rollback
  mevedel-memory-library.el   current topic inventory and structured user deletion
  mevedel-memory-list.el      candidates/memories/history cockpit, diffs, decisions, recovery
  mevedel-hooks.el            project/user/skill/agent hook loading + runner
  mevedel-prompt-submission.el accepted prompt + lifecycle-context transaction
  mevedel-bash-analysis.el    conservative shell parsing and normalized command facts
  mevedel-bash-policy.el      Bash classification and reusable rules
  mevedel-bash-git.el         argument-aware Git inspection and hardened launch argv
  mevedel-transport.el        remote reentrancy detection and idle-transport deferral
  mevedel-execution-target.el immutable local/TRAMP target, path domains, readiness
  mevedel-execution-grants.el protected-boundary-aware child authority coverage
  mevedel-execution.el        managed execution registry, admission, and facade
  mevedel-execution-process.el opaque child, process-group, and spool lifecycle
  mevedel-execution-transcript.el durable execution render data and archive reconciliation
  mevedel-execution-scheduler.el fair session-scoped Bash admission
  mevedel-execution-telemetry.el safe execution facts and profiler adaptation
  mevedel-sandbox.el          optional Bubblewrap child-process confinement
  mevedel-sandbox-grants.el   FD-backed grant mounts and the protected-path mount plan
  mevedel-telemetry.el        append-only lifecycle events and profiler capture
  mevedel-telemetry-usage.el  frozen per-provider-call identity and normalized usage
  mevedel-plan.el             lifecycle-neutral plan data and artifacts
  mevedel-plan-handoff.el     durable accepted-plan preparation and kickoff
  mevedel-permission-mode.el  mode normalization, session scoping, lifecycle
  mevedel-permission-rules.el rule parsing, matching, buckets, resource grants
  mevedel-permission-persistence.el target-aware authority store codec
  mevedel-permissions.el      permission preflight and 8-step decision facade
  mevedel-tool-permission.el permission-step orchestration, hooks, prompts, logging
  mevedel-pipeline.el         tool context, standard steps, sequencing, ordering
  mevedel-ptc-checkpoint.el  durable ToolCall audit settlement across restart
  mevedel-ptc-driver.el      ToolCall orchestration, nested calls, progress
  mevedel-ptc-interpreter.el closed programmatic-tool-call evaluator and machine
  mevedel-resource.el         resource-address grammar and attempt lifecycle
  mevedel-resource-capf.el    resource-address completion
  mevedel-permission-log.el   durable permission decision log
  mevedel-tool-media.el       tool media storage, scrubbing, provider payloads
  mevedel-tool-render-data.el render-data codec, provider scrubber, transcript mutation
  mevedel-tool-registry.el    mevedel-tool struct, mevedel-define-tool macro
  mevedel-tool-repair.el      structured validation and atomic input repair
  mevedel-tool-repair-gptel.el  lossless gptel argument decoding bridge
  mevedel-tool-repair-diagnostics.el  repair audit and telemetry
  mevedel-queue.el            session lookup and interaction entry metadata
  mevedel-permission-queue.el permission/Bash/Eval/execution-authority queue
  mevedel-permission-review.el optional invocation approval before human queue admission
  mevedel-reminders.el        shared reminder collection, staged delivery and firing policy
  mevedel-history.el          retained reminders and provider response reconstruction
  mevedel-history-search.el   cooperative saved-transcript discovery, filtering and search
  mevedel-edit-diagnostics.el post-edit Flymake/Flycheck report state machine
  mevedel-plugin-registry.el  plugin manifests, activation state, hook consent
  mevedel-plugin-lifecycle.el managed Git install, update, and removal
  mevedel-plugin-ui.el        plugin cockpit and /plugin command
  mevedel-plugins.el          narrow session/workspace plugin facade
  mevedel-skills-core.el      skill model, discovery, state, reload
  mevedel-mention-bindings.el shared atomic mention validation and edit lifecycle
  mevedel-skills-preparation.el argument substitution and body injection execution
  mevedel-skills-syntax.el      shared authored Markdown and dependency syntax
  mevedel-skills-invoke.el    request context, invocation, fork dispatch, model tools
  mevedel-skills-input.el     user token binding, raw dispatch, inline projection
  mevedel-skills-plan.el      deterministic user invocation planning and preparation
  mevedel-skills-prompt.el    model-visible roster, reminders, activation
  mevedel-skills-ui.el        slash commands, cockpit, completion, font-lock

Chat / view
  mevedel-chat.el             session lifecycle
  mevedel-directive-request.el  directive prompts, dispatch, and settlement
  mevedel-side-conversation.el  ephemeral /btw conversation lifecycle
  mevedel-directive-activity.el  read-only workspace directive inspector
  mevedel-directive-frame.el  directive-anchored child frame and transcript filter
  mevedel-directive-plan.el   directive-owned planning and approval workflow
  mevedel-transcript.el       transcript span classification for view/persistence/compaction
  mevedel-transcript-audit.el hidden audit record encoding and structural parsing
  mevedel-transcript-restore.el  transcript property restoration via the canonical grammar
  mevedel-context-summary.el  stateless validated continuation/handoff/digest summary generation
  mevedel-view.el             view mode, zones, and session coordination
  mevedel-view-agent.el       agent transcript inspection, status rows, refresh
  mevedel-view-composer.el    composer geometry, submission, root dispatch, fork/send flow
  mevedel-view-input-files.el local file drops and clipboard-image input
  mevedel-view-animation.el   prepared status frames, styles, and bounded color cache
  mevedel-view-native.el      optional native presentation, placement, fallback and teardown
  native/mevedel-view-native.c timed Wayland surfaces without Lisp frame callbacks
  mevedel-view-power.el       shared battery observations and effective FPS policy
  mevedel-pending-inputs.el   pending queue, steering, delivery, and cockpit
  mevedel-patch-review.el     staged ApplyPatch review UI
  mevedel-plan-mode.el        Plan conversations and proposal approval UI
  mevedel-view-interaction.el interaction registration, ordering, callback overlays, redraw
  mevedel-view-control-transfer.el cooperative transfer polling, presentation, and commands
  mevedel-view-disclosure.el  source-backed transcript disclosure state and actions
  mevedel-view-prepare.el     deferred large-tool parsing, source validation, job lifecycle
  mevedel-view-render.el      transcript projection, source mapping, live navigation
  mevedel-view-segments.el    historical session segment projection and navigation
  mevedel-view-stream.el      request progress, view animation timer, streaming redraws
  mevedel-gptel-stream-bridge.el private gptel stream compatibility advice
  mevedel-view-audit.el       audit disclosure rendering
  mevedel-view-zone.el        managed view-zone lifecycle + fragments
  mevedel-view-history.el     view input history ring and persistence
  mevedel-view-fontify.el     quiet generic and reusable Markdown fontification
  mevedel-collaboration.el    live browser room and lifecycle facade
  mevedel-collaboration-guest.el  untrusted guest protocol and input handling
  mevedel-collaboration-owner.el owner-link permission and session authorities
  mevedel-collaboration-lobby.el  per-workspace lobby: persistent link and restart, session list, open
  mevedel-collaboration-agent.el  browser agent roster and transcript fetch
  mevedel-collaboration-artifact-projection.el ApplyPatch artifact projection
  mevedel-collaboration-artifact.el browser artifact fetch and notifications
  mevedel-collaboration-files.el  browser project files: list, read, upload, trash
  mevedel-collaboration-artifact-comments.el artifact comment store, threads and conversations
  mevedel-collaboration-history.el archived browser segments and artifact metadata
  mevedel-collaboration-editing.el browser shared editing and selection questions
  mevedel-collaboration-presence.el who else is on each guest's page; lobby counts
  mevedel-shared-conversation.el item-scoped request history from canonical live/archive turns
  mevedel-shared-editing.el   private editor engine queue and leased store commits
  mevedel-shared-library.el   host whiteboard element library and public Excalidraw collection
  mevedel-collaboration-projection.el canonical, retained browser transcript projection
  mevedel-collaboration-task.el browser task projection and publication
  mevedel-collaboration-share.el bearer-link and QR presentation surface
  mevedel-collaboration-transport.el sealed relay WebSocket client
  mevedel-view-markdown.el    Markdown links, images, paths, source panels
  mevedel-view-path.el        deferred target path verification and memoization
  mevedel-view-table.el       rendered pipe tables and window realignment
  mevedel-cockpit.el          shared tabulated cockpit surface plumbing
  mevedel-report.el           read-only sections, memory navigation, report windows
  mevedel-subscription-usage.el session-owned quota report, freshness and cancellation
  mevedel-subscription-usage-provider.el isolated Codex HTTP and native Claude ACP quota retrieval
  mevedel-menu.el             session cockpit transient and model selection
  mevedel-gptel-bridge.el     view-launched gptel menu, restoration, and steering routing
  mevedel-executions-list.el  session-wide live execution cockpit and user controls
  mevedel-artifacts-list.el   artifacts cockpit: list, open, attach, versions, duplicate, delete
  mevedel-permissions-list.el remembered authority cockpit and per-row revoke
  mevedel-worktree.el         Git worktrees, status/list surfaces, fork plumbing
  mevedel-instruction-registry.el workspace instruction buckets, IDs, links
  mevedel-overlays.el         instruction geometry, tags, context, prompts
  mevedel-directive-source.el directive anchor/presentation lifecycle
  mevedel-overlay-ui.el       instruction overlay actions and rendering
  mevedel-mentions.el         @ref and @file mention expansion
  mevedel-directive-persistence.el  workspace directive record codec
  mevedel-persistence.el      save/load instructions
  mevedel-session-codec.el    closed session sidecar codec and validation
  mevedel-session-artifacts.el  paths, artifacts, snapshots, and segment writes
  mevedel-artifact-store.el   workspace artifact store: metadata, versions, attachment, conversations
  mevedel-artifact-lease.el   cross-Emacs leases on store whiteboards and documents
  mevedel-session-durability.el lease and storage primitives
  mevedel-session-recovery.el  specialized recovery protocol and markers
  mevedel-session-transfer.el  durable cooperative control transfer protocol
  mevedel-session-publication.el immutable publication, retention/deletion, diagnostics
  mevedel-session-collection.el  coalesced idle generation scans and cleanup lifecycle
  mevedel-session-save-as.el portable Save As transaction and adoption
  mevedel-session-persistence.el  lifecycle/resume/listing/locking/cleanup facade
  mevedel-session-rewind.el   restore plans, transactional Rewind, published-head redo
  mevedel-session-fork.el     Fork/Worktree projection and publication
  mevedel-session-naming.el   metadata-only rename and bounded background session titles
  mevedel-session-control-fs.el   pinned target-side session control filesystem
  mevedel-session-control-transfer.el  control-transfer state, drains, descriptors
  mevedel-compact-estimation.el compaction token accounting and admission
  mevedel-compact-evidence.el transcript evidence and tool-safe truncation
  mevedel-compact-target.el   root/agent archive and application transactions
  mevedel-compact-run.el      async compaction retry/cancel/settlement
  mevedel-compact.el          public compaction command and gptel gate

Prompt / presets / agents
  mevedel-system.el           system prompt assembly
  mevedel-context-delivery.el retained dynamic context and selected-history acknowledgement
  mevedel-presets.el          gptel presets and request-time FSM assembly
  mevedel-agents.el           worker/explorer/verifier/reviewer definitions
  mevedel-agent-conversation.el  retained conversation buffers, activity, and saves
  mevedel-agent-control.el    retained-agent tree addressing, turns, mail, waits
  mevedel-agent-exec.el       sub-agent request runner and FSM handlers
  mevedel-agent-persistence.el durable agent registry codec and cold hydration
  mevedel-agent-runtime.el    retained agent request lifecycle and settlement
  mevedel-goal.el             phase-free Goal continuation controller
  mevedel-review.el           /review picker, reviewer output parsing, parent transcript injection

Provider readiness and recovery (all providers)
  mevedel-recovery.el         durable operational issues and unsubmitted input DTOs
  mevedel-auth.el             asynchronous credential renewal and owner login challenges
  mevedel-readiness.el        pre-commit provider checks and recovery wakeups

Turn engines (Claude Code through ACP and MCP)
  mevedel-collaboration-recovery.el owner-only browser recovery actions and state
  mevedel-claude-code-maintenance.el staged stable updates, validation, retention and rollback, and the bounded status-command runner shared with launch preflight
  mevedel-engine.el           shared turn-context access for requests and native gptel FSMs
  mevedel-acp.el              ACP conversation startup, prompt, cancellation and process lifecycle
  mevedel-acp-text.el         isolated streamed background requests with buffer-owned teardown
  mevedel-acp-workload.el     scoped background MCP tools and native batch-boundary guards
  mevedel-acp-turn.el         admitted ACP turns, MCP tool scope, streaming and shared settlement
  mevedel-acp-compaction.el   native summary lifecycle, transcript rotation and retained tails
  mevedel-claude-code-backend.el provider type, model catalog, installation settings (executable/adapter/directory), minimum CLI/adapter versions and version parser, autoloaded engine dispatch
  mevedel-claude-code.el      guided setup, launch preflight and isolated ACP launch
  mevedel-claude-code-session.el root, directive and shared-item sends and persisted external conversation references
  mevedel-claude-code-agent.el retained-child subscription dispatch, sample limits and native history references
  mevedel-claude-code-history.el native history state, divergence and excerpt recovery
  mevedel-claude-code-context.el native context receipts, compaction restoration and full-context continuation
  mevedel-claude-code-usage.el per-sample native usage, duplicate snapshots and cumulative accounting across prompt continuations
  mevedel-mcp.el              private asynchronous MCP transport and stdio bridge configuration
  mevedel-mcp-tools.el        turn-owned MCP schema export and pipeline result projection

Tools (each dispatches through mevedel-pipeline)
  mevedel-tool-ptc.el        ToolCall roster, request adapter, registration
  mevedel-tool-fs.el          filesystem tool registration and shared path/resource primitives
  mevedel-tool-fs-read.el     Read text/media decoding and bounded output
  mevedel-tool-fs-search.el   Glob/Grep execution and resource-output privacy
  mevedel-tool-patch-registration.el ApplyPatch catalog and callback autoloads
  mevedel-tool-patch.el       ApplyPatch parse/match/apply engine and rendering
  mevedel-tool-code.el        XrefReferences, XrefDefinitions, Imenu, Treesitter
  mevedel-tool-exec-permission.el Bash/Eval authority and prompt adapters
  mevedel-tool-exec-registration.el Bash/Eval/execution-control catalog and callback autoloads
  mevedel-tool-exec.el        Bash/Eval lifecycle and rendering
  mevedel-tool-web-registration.el WebSearch/WebFetch catalog and callback autoloads
  mevedel-tool-web.el         WebSearch/WebFetch execution and rendering
  mevedel-interaction-prompt.el  shared interaction overlay lifecycle
  mevedel-permission-prompt.el   generic, Bash, Eval, and execution-authority prompt UI
  mevedel-tool-ask.el         Ask handler, result renderer, registration
  mevedel-tool-ask-ui.el      Ask form state, controllers, and presentation
  mevedel-tool-ui.el          Agent/InterruptAgent/ToolSearch/SendMessage assembly
  mevedel-tool-task.el        TaskCreate/Update/List/Get + overlay
  mevedel-tool-goal.el        CreateGoal/GetGoal/UpdateGoal tools and root-request authority checks
  mevedel-tool-editing.el     SharedCreate/Edit tools, shared:// views
  mevedel-tool-skills.el      Skill and ListSkills tool schemas
  mevedel-tool-introspect.el  native Emacs introspection tools
  mevedel-buddy.el            edit recording, diff assembly, review requests
  mevedel-buddy-note.el       ephemeral note overlays and their model tools
  mevedel-tools.el            complete tool registration + stable discovery catalog
  mevedel-tools-list.el       native tools cockpit list

Support
  mevedel-file-state.el       LRU file cache
  mevedel-diff-apply.el       transactional unified diff staging/application
  mevedel-theme-faces.el      active-theme-derived face registration and refresh
  mevedel-utilities.el        package version + shared helpers + coalesced UI timers
  mevedel-init.el             repository guidance bootstrap command
```
