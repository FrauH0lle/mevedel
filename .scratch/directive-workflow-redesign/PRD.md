# Directive workflow and durable activity

Type: PRD
Status: ready-for-agent

## Problem Statement

Mevedel's directive workflow has a strong core: an instruction is anchored in
source, receives reference-derived context, runs independently of the main chat
transcript, and captures the resulting repository patch. The current action and
storage model no longer expresses that core clearly.

Implement, Discuss, and Revise appear as peer actions even though Revise is
really another implementation attempt supplied with history. Revision feedback
is conflated with the original directive request, the latest patch can be
replaced or taken from a shared display buffer, successful directives lose
useful next actions, and discussion has no explicit isolated continuation path.
The nominally read-only discussion preset also exposes mutation paths through
shells, evaluation, and delegated agents.

The directive overlay is simultaneously treated as the identity, anchor,
status display, answer link, and patch store. Deleting its entire region makes
the overlay evaporate without an undo-aware lifecycle event. Deleting its file
can eventually discard persisted instruction state. These failures are most
damaging after implementation, when the directive is the user's entry point to
the work, discussion, captured changes, and recovery action.

Patch-backed reversal and session Rewind also risk becoming competing history
systems. Patch reversal could target one directive but would be conflict-prone
after repository drift and would leave later transcript history describing a
world that no longer exists. Rewind is already chronological and transactional,
but it currently lacks the pre-turn checkpoint and directive linkage needed to
serve as the single implementation undo. Serial directives make the constraint
unavoidable: undoing an earlier implementation cannot retain later session
turns that were produced from the state being discarded.

Users need one comprehensible directive lifecycle, durable access after source
deletion, explicit discussion continuity, honest patch semantics, and one
linear recovery model. The redesign must preserve isolated requests,
reference-derived context, hard read-only discussion, implementation authority,
and inspection of the changes actually produced.

## Solution

Use two initial intents: **Discuss** and **Implement**. Remove Revise as a
user-facing action and behavioral preset. Once an implementation attempt
exists, the state-dependent implementation action is **Request changes** after
success or **Retry** after failure or abort. There is no fresh re-run from the
already modified repository; a clean restart is **Rewind before this
implementation...** followed by Implement.

Make the directive a durable workspace record and treat its overlay as one
presentation of its source anchor. The record owns the current request,
execution-session binding, anchor state, activity, attempts, feedback,
subdirective relationship, and archive state. Each directive request is also a
first-class turn in its bound execution session: it streams and renders in the
ordinary MevView with the same tools, permissions, agents, tasks, interactions,
folding, and Rewind machinery as an ordinary chat turn. The canonical
transcript retains its normal user, response, and tool roles; durable directive
boundaries exclude the whole turn only when ordinary-chat provider context is
assembled.

Use the shared session composer for follow-ups. Entering a directive action
switches it into a loud, sticky directive scope that names the directive and
states that main chat is excluded. Scope survives repeated sends, carries with
queued input, preserves a separate draft, and ends only through an explicit
Back to chat action. For durable access after compaction, source deletion, or
archive, an explicit read-only directive inspector renders the workspace
record. The inspector replaces the displayed view, never auto-splits, and owns
no composer, streaming destination, or interaction registry.

Each accepted implementation, failed implementation, or aborted implementation
creates an immutable attempt record. An attempt retains the exact submitted
request, response or error, outcome, generated patch, patch-capture
completeness, and its session turn checkpoint. Attempts are chronological,
start from the repository's current state, and are not selectable variants.

Request changes builds a new isolated implementation request from the current
directive request, freshly resolved references, new feedback and/or newly
authored subdirectives, and the immediately preceding attempt's answer and
patch. The preceding material is labeled historical; current repository state
is authoritative. Retry follows the same principle but frames the prior error
and any partial captured change as diagnostic context. Older activity remains
inspectable and is not automatically sent.

Discuss uses the ordinary base system role with a dedicated hard read-only
capability preset. The restriction covers direct tools, shell and evaluation
behavior, and delegated work. Discussion is continued through
directive-scoped turns in the shared session view. **Implement this** converts
the current directive-local discussion into a new implementation attempt
without importing main-chat history. After an attempt, **Discuss result** starts
or continues a read-only conversation with that attempt attached.

Rewind is the only undo mechanism. Every accepted model turn has a durable
pre-turn checkpoint. **Rewind before this implementation...** invokes the
ordinary chronological Rewind transaction at the attempt's turn. It previews
all affected files, ordinary chat turns, directive attempts, and known capture
gaps. Confirming it discards that attempt and every later turn in the same
session. Files are restored transactionally where captured, discarded model
activity disappears, user-authored directive records and request edits survive,
and directive state is recomputed from the latest surviving activity. There is
no redo and no reverse application of stored patches.

The source presentation is deliberately compact:

```text
 36 | (defun previous-command ()
 37 |   ...)
 38 |
    | ◆ Detached · Ready      Implement help command      M-m
    | ◆ Detached · Discussed  Implement move command      M-m
    | ◆ Detached · Failed     Implement revert command    M-m
 39 | (defun next-command ()
```

Attached directives retain their region tint and show only a short status
label. Deleting an entire region leaves a zero-width detached anchor and a
one-line row. Co-located detached directives form one stable, source-ordered
block with independent rows. A missing source file removes the source
presentation but not the record; the directive remains available in the
workspace directive list.

A settled directive turn folds to a one-line summary but remains the actual
turn, never a compact proxy. The transcript owns presentation and chronological
placement; the workspace record deliberately retains the exact submitted
request, terminal answer or error, patch, capture metadata, and checkpoint for
follow-up construction and durable inspection.

The principal lifecycle is:

```text
Ready
  Discuss   -> Discussing -> Discussed
  Implement -> Implementing -> Implemented | Failed | Aborted

Discussed
  Continue discussion -> Discussing -> Discussed
  Implement this      -> Implementing -> Implemented | Failed | Aborted

Implemented
  View changes
  Discuss result
  Request changes -> Implementing -> Implemented | Failed | Aborted
  Rewind before this implementation... -> earlier surviving state

Failed / Aborted
  Discuss result
  Retry [optional guidance] -> Implementing -> Implemented | Failed | Aborted
  Rewind before this implementation... -> earlier surviving state
```

## User Stories

1. As a user, I want a new directive to offer Discuss and Implement, so that I choose intent without understanding internal presets.
2. As a user, I want Revise removed as a peer action, so that I do not have to distinguish revision from implementation given history.
3. As a user, I want a successful directive to offer Request changes, so that the next action says what I am asking for.
4. As a user, I want a failed or aborted directive to offer one Retry action, so that guided and unguided recovery do not become separate menu concepts.
5. As a user, I want Retry guidance to be optional, so that I can retry a transient failure without inventing feedback.
6. As a user, I want Request changes to collect multiline feedback, so that substantive revisions are not forced through a minibuffer-sized interaction.
7. As a user, I want newly added subdirectives to count as change instructions, so that I can express local corrections directly in source.
8. As a user, I want Request changes feedback to be optional when unconsumed subdirectives already explain the change, so that I do not repeat myself.
9. As a user, I want Request changes to require feedback when no new subdirective supplies it, so that an accidental empty revision is not submitted.
10. As a user, I want the original directive request preserved across feedback and retries, so that historical intent remains auditable.
11. As a user, I want editing the current directive request to preserve old activity, so that improving the request does not erase what happened.
12. As a user, I want an edited request to return the directive to Ready, so that the next implementation is clearly a first attempt for the new request.
13. As a user, I want Request changes unavailable until the edited request has an implementation attempt, so that history from a different request is not mislabeled as the preceding attempt.
14. As a user, I want every accepted implementation attempt retained, so that later attempts do not overwrite earlier outcomes.
15. As a user, I want successful, failed, and aborted attempts represented in activity, so that partial or interrupted work remains explainable.
16. As a user, I want each attempt to retain its exact submitted request, so that later request edits do not rewrite history.
17. As a user, I want each attempt to retain its answer or error, so that I can understand the result without searching the main chat.
18. As a user, I want each attempt to retain the patch actually observed at completion, so that I can inspect what changed rather than trust the model's description.
19. As a user, I want an attempt to distinguish no filesystem changes from captured changes, so that an empty patch is not mistaken for missing capture.
20. As a user, I want incomplete capture labeled with the known covered files and gaps, so that the activity does not claim certainty it lacks.
21. As a user, I want attempts ordered chronologically rather than treated as switchable variants, so that the displayed history matches the repository states on which they ran.
22. As a user, I want every new attempt to start from the current repository, so that Mevedel does not silently reconstruct an obsolete branch of work.
23. As a user, I want the current repository declared authoritative during Request changes and Retry, so that a stale patch cannot override newer edits.
24. As a user, I want the immediately preceding answer and patch included as historical context for Request changes, so that short feedback has a clear referent.
25. As a user, I want the immediately preceding error and partial change included for Retry, so that the model can repair the failed attempt.
26. As a user, I want references re-resolved for every attempt, so that an implementation sees the current referenced source.
27. As a user, I want older attempts inspectable but omitted from automatic prompt context, so that requests remain focused and bounded.
28. As a user, I want to preview the complete isolated request before submission, including action-specific historical context, so that the model input is auditable.
29. As a user, I want directive requests to ignore the main-chat transcript, so that unrelated conversation cannot silently affect implementation.
30. As a user, I want directive activity excluded from future main-chat model context, so that isolation works in both directions.
31. As a user, I want every directive request rendered as a complete turn in its execution session, so that chronology, tools, interactions, and Rewind use one familiar surface.
32. As a user, I want settled directive turns foldable to one-line summaries, so that full responses remain available without making the transcript unnecessarily dense.
33. As a user, I want an explicit read-only inspector backed by the durable directive record, so that activity remains reachable after compaction, archive, or source loss without creating a second live conversation surface.
34. As a user, I want the shared composer to enter a loud, sticky directive scope with a separate draft and explicit exit, so that follow-up questions remain isolated without a second composer.
35. As a user, I want Implement this after discussion, so that the complete local discussion can become implementation feedback.
36. As a user, I want Discuss result after an implementation attempt, so that I can analyze the result without granting write authority.
37. As a user, I want Discuss enforced as read-only across direct tools, shell commands, evaluation, and delegated agents, so that prompt wording is not the safety boundary.
38. As a user, I want Discuss and Implement to share the ordinary assistant role, so that capability rather than persona explains their difference.
39. As a user, I want Request changes and Retry to use ordinary implementation authority, so that a special revision identity is unnecessary.
40. As a user, I want each directive to retain its chosen model and reasoning effort behavior, so that the redesign does not remove current control.
41. As a user, I want the first directive request to bind an execution session, so that later activity has one chronological recovery history.
42. As a user, I want later actions to reuse that execution session, so that selecting another workspace chat does not silently split directive history.
43. As a user, I want a closed persisted execution session resumed on demand, so that old directives remain usable after restart.
44. As a user, I want an unavailable execution session to require an explicit warned rebind, so that history is never silently reassigned.
45. As a user, I want earlier attempts to retain their original unavailable checkpoint links after rebinding, so that historical provenance remains honest.
46. As a user, I want one Rewind before this implementation action rather than separate undo mechanisms, so that recovery has one meaning.
47. As a user, I want Rewind to preview the complete chronological impact, so that I see later chat turns, directives, and files that will be discarded.
48. As a user, I want Rewind to require confirmation, so that destructive history truncation is deliberate.
49. As a user, I want Rewind to restore the pre-turn state of the first model turn as well as later turns, so that the earliest implementation is undoable.
50. As a user, I want undoing directive B to preserve earlier directive A, so that serial implementations can be unwound from newest to oldest.
51. As a user, I want undoing earlier directive A to discard later directive B and intervening chat turns from the same session, so that history never retains work based on a removed state.
52. As a user, I want directive B itself to survive as Ready when its implementation attempt is discarded, so that authored instructions are not confused with model history.
53. As a user, I want user-authored request edits to survive Rewind, so that undo targets model activity rather than my current workspace instructions.
54. As a user, I want consumed subdirectives restored when the successful attempt that consumed them is rewound, so that its detailed instructions are available again.
55. As a user, I want directive state recomputed from surviving activity after Rewind, so that labels never describe discarded attempts.
56. As a user, I want no surviving activity to yield Ready, a surviving discussion to yield Discussed, a surviving success to yield Implemented, and a surviving failure to yield Failed, so that state is derived rather than patched manually.
57. As a user, I want a request differing from the latest surviving attempt labeled Ready · request changed, so that old activity is visible without governing the new request.
58. As a user, I want Rewind available even when capture is incomplete, so that a known gap does not hide the only recovery path.
59. As a user, I want incomplete recovery coverage shown prominently before confirmation, so that Rewind never promises a complete restoration it cannot provide.
60. As a user, I want stored patches excluded from undo, so that repository drift cannot make reversal silently corrupt later work.
61. As a user, I want no redo initially, so that the product has one simple linear history instead of a second branch system.
62. As a user, I want an attached directive rendered as its tinted region plus a short state label, so that source remains readable.
63. As a user, I want complete deletion of an anchored region to detach rather than destroy its directive, so that the activity and recovery entry point survive.
64. As a user, I want a detached directive represented by a compact zero-width source row, so that it stays discoverable without inserting source text.
65. As a user, I want multiple directives detached at one location shown as stable source-ordered rows, so that each remains independently actionable.
66. As a user, I want detached rows initially unfolded, so that no hidden-group interaction is added before density proves it necessary.
67. As a user, I want a deleted source file to mark its directive Source missing rather than delete it, so that an implementation which deletes its own file remains inspectable and rewindable.
68. As a user, I want source-missing directives available in the workspace directive list, so that no live source buffer is required to reach them.
69. As a user, I want a returning source file to reattach only on one exact unambiguous anchor match, so that Mevedel does not guess incorrectly.
70. As a user, I want ambiguous or absent matches to require explicit reattachment, so that source association remains under my control.
71. As a user, I want partial source edits to resize the attached overlay normally, so that ordinary editing does not create a noisy stale-anchor state.
72. As a user, I want references to remain ephemeral and source-bound, so that durability is reserved for records with activity and recovery value.
73. As a user, I want nested directives to remain details owned by their topmost parent, so that they do not create independent activity histories.
74. As a user, I want parent submission to include all nested directives as hints or corrections, so that their existing purpose is preserved.
75. As a user, I want successful parent implementation to consume submitted subdirectives, so that completed detail instructions leave the source.
76. As a user, I want failure or abort to leave submitted subdirectives in place, so that unfinished detail instructions are not lost.
77. As a user, I want a directive with activity archived rather than permanently deleted, so that its attempts and checkpoint links remain valid.
78. As a user, I want Archive to hide the source presentation and active-list entry, so that completed directives need not clutter normal work.
79. As a user, I want a directive without activity removable outright, so that accidental or unused instructions remain cheap to discard.
80. As a user, I want View changes to show the selected attempt's patch in a reusable diff buffer, so that display storage is not mistaken for history ownership.
81. As a user, I want the shared patch viewer excluded from prompt construction, so that one directive can never inherit another directive's patch.
82. As a user, I want batch processing to implement Ready directives and Implement this for Discussed directives, so that discussion can prepare a batch item.
83. As a user, I want batch processing to skip every directive with a prior implementation attempt, so that it never infers Request changes or Retry.
84. As a user, I want detached and source-missing Ready directives eligible for batch only when their context is sufficient, so that anchor loss alone does not destroy useful work.
85. As a user, I want batch processing to stop on the first failure or abort, so that later directives do not build on an uncertain repository state.
86. As a user, I want processing directives to expose Abort and activity inspection without conflicting submission actions, so that in-flight state is unambiguous.
87. As a maintainer, I want obsolete revision behavior removed rather than preserved behind aliases or migrations, so that the active-development codebase has one workflow.
88. As a maintainer, I want directive identity and activity to have one authoritative owner, so that overlays, sessions, and patch viewers cannot diverge.
89. As a maintainer, I want lifecycle state derived from durable activity and the current request, so that every UI surface projects the same truth.
90. As a maintainer, I want tutor behavior left unchanged, so that this redesign does not absorb a genuinely distinct workflow.

## Implementation Decisions

- Make a durable workspace directive record the single owner of directive
  identity, current request, anchor description, execution-session binding,
  activity, archive state, and parent-owned subdirectives. The source overlay
  stores or resolves only enough identity to render and dispatch that record.
- Do not add a generic workflow framework, event-sourcing layer, alternate
  attempt graph, or secondary undo stack. A directive record, a chronological
  attempt list, existing session turns, and existing overlay/view mechanisms
  are sufficient.
- Keep request isolation strict. Directive prompt assembly starts from the
  current directive record, fresh references, action-local feedback or
  discussion, and permitted immediately preceding attempt material. It neither
  reads nor contributes conversational main-chat context.
- Keep each directive exchange as a first-class turn in the execution session's
  canonical transcript with ordinary user, response, and tool roles. Mark its
  extent with durable structural boundaries. Before gptel parses an ordinary
  chat request, a synchronous prompt-copy transform marks every enclosed
  directive body `gptel 'ignore`; do not overwrite canonical rendering roles or
  maintain a compact event proxy.
- Reserve one canonical session turn identity when an accepted request starts,
  store it on the request, and commit the same identity at terminal settlement.
  Use it for pre-turn snapshots, transcript metadata, prompt/Rewind indexing,
  and directive checkpoint links; no callback may independently predict
  `turn-count + 1`.
- Bind a directive to the session used for its first model request. Reuse that
  session for every subsequent directive request. Resume a closed persisted
  session on demand; require explicit user confirmation to rebind if the
  session cannot be recovered.
- Represent implementation activity as immutable chronological attempts. Each
  attempt records its action framing, exact request snapshot, feedback or
  discussion supplied, result or error, terminal outcome, patch, capture
  completeness, captured-file coverage, and session/checkpoint identity.
- Derive visible lifecycle state from the current request and surviving
  activity. Do not persist multiple manually synchronized status authorities.
- Treat an exact current-request mismatch with the latest surviving attempt as
  Ready with a request-changed qualifier. Preserve all prior activity but do
  not offer state-dependent Request changes for that older request.
- Replace the Revise action with Request changes after a successful attempt.
  Build the request from the current request, fresh references, new feedback
  and/or unconsumed subdirectives, plus the immediately preceding answer and
  patch labeled as historical context.
- Replace failed-state action variants with one Retry. Include the immediately
  preceding error, observed partial patch, and optional new guidance. An empty
  patch under complete capture is explicitly different from incomplete
  capture.
- Do not offer Implement again or Run fresh after an implementation attempt.
  Starting again from a clean pre-attempt state is Rewind followed by
  Implement; starting from current state with knowledge of the attempt is
  Request changes or Retry.
- Keep older attempts out of automatic prompt context. They remain available
  in folded transcript turns and the read-only directive inspector.
- Keep the ordinary implementation preset for Implement, Implement this,
  Request changes, and Retry. Their differences belong in request content and
  lifecycle state rather than separate system identities.
- Keep a dedicated discussion preset solely as a capability boundary. Use the
  base system role and remove every mutation path, including mutation-capable
  shell/evaluation policies and delegated agents.
- Remove the revision preset, revision system profile, revision role prompt,
  shared-patch fallback, and all superseded action/menu paths. Do not add
  compatibility aliases, old persisted-state readers, or migrations.
- Keep tutor separate and unchanged.
- Render live directive work in the existing shared session view and route its
  tools, permissions, Ask prompts, agents, tasks, progress, and streaming
  through the existing machinery. A settled directive turn folds to one line
  but is never replaced by a proxy.
- Give the shared composer an explicit directive scope for Continue discussion,
  Implement this, Discuss result, Request changes, and Retry. Scope is loud,
  sticky across sends, carried by queued inputs, and exited explicitly; chat and
  directive scopes retain separate drafts. Resume starts in chat scope.
- Keep durable off-transcript access in an explicit read-only directive
  inspector rendered from the workspace record. It replaces the current
  displayed view, never auto-splits, and owns no composer, streaming target, or
  interaction registry.
- Keep source rendering intentionally terse. Attached anchors retain tint and
  show a short state label. Detached anchors render one zero-width row.
  Co-located detached anchors render independent rows in stable former source
  order without grouping controls or folding.
- Stop evaporating durable directive overlays. Full region deletion converts
  an Attached anchor to Detached at the deletion position. Partial deletion
  follows normal overlay boundary movement and does not create a stale state.
- Keep references evaporating and source-bound because they own no durable
  activity or recovery affordance.
- When a directive's file is unavailable, set its anchor to Source missing and
  retain the workspace record. Automatic reattachment requires one exact,
  unambiguous match using the existing anchor evidence; all other cases require
  explicit user placement.
- Preserve the existing parent/subdirective semantics. Actions resolve to the
  topmost parent; nested directives enrich its request, have no independent
  activity, are consumed only by success, survive failure/abort, and return
  when their consuming attempt is discarded by Rewind.
- Keep one reusable diff-mode patch viewer as presentation only. Patch truth
  lives on its attempt. Never recover revision context from the viewer or a
  workspace-global patch value.
- Use Rewind as the sole implementation undo. Extend the existing session
  checkpoint contract so every accepted model turn, including the first, owns
  durable pre-turn file state and an impact description.
- Rewinding to an attempt truncates that session immediately before the
  attempt. Discard its model activity and every later session turn, restore
  covered files transactionally, retain user-authored workspace directive
  records and current request edits, restore subdirectives consumed by
  discarded successes, and recompute every affected directive's state.
- Include ordinary chat and all directive turns in the same chronological
  Rewind suffix. Serial directives therefore undo newest-first unless the user
  explicitly accepts discarding every later dependent turn.
- Show capture and checkpoint gaps in the Rewind impact preview. Keep the
  action available, but do not claim complete restoration for uncaptured paths.
- Do not reverse-apply patches, synthesize inverse patches, preserve discarded
  model attempts as redo data, or let Emacs buffer undo stand in for session
  recovery.
- Let directives with activity be Archived but not permanently deleted in the
  initial design. Archive hides source and active-list presentation while
  preserving activity and checkpoint links. Directives without activity may be
  removed normally.
- Batch only first implementations. Ready items use Implement; Discussed items
  use Implement this; any item with an implementation attempt is skipped.
  Never infer Request changes or Retry, and stop after the first failure or
  abort.

## Testing Decisions

- Test user-observable directive behavior through the existing directive
  request lifecycle. Use real temporary workspace files and buffers, replacing
  only the remote model transport so requests can be settled deterministically.
- At the primary seam, assert prompt isolation, fresh reference expansion,
  action-specific context, session binding, first-class directive turns,
  immutable attempt capture, patch ownership, capture completeness, derived
  lifecycle state, and available actions after success, failure, and abort.
- At the real gptel parse seam, assert that an ordinary request excludes each
  directive prompt, response, and tool span while the canonical transcript
  retains normal response and `(tool . id)` properties for MevView. Repeat after
  save/resume, compaction of the containing segment, and session fork.
- Mix chat and directive requests and assert that each request's reserved turn
  identity equals its snapshot key, transcript metadata, prompt-index entry,
  and directive checkpoint link without stubbing prompt candidates.
- Cover Discuss with the real capability-policy assembly. Assert that direct
  write tools, mutating shell/evaluation behavior, and mutation-capable
  delegation are absent or denied while read-only inspection remains usable.
- Assert that Implement this receives the complete directive-local discussion
  but no main-chat transcript, and that Discuss result receives the selected
  attempt without gaining write authority.
- Assert that Request changes uses fresh references, current request, new
  feedback and/or subdirectives, and only the immediately preceding historical
  answer and patch. Assert that Retry uses the prior error and partial capture.
- Test current-request edits as a state boundary: prior activity remains,
  visible state becomes Ready with the request-changed qualifier, and the old
  attempt is not used as Request changes context.
- Use the real session persistence and Rewind path with temporary files. Create
  serial directive attempts and intervening ordinary chat turns, invoke Rewind
  at a selected attempt, and assert restored files, discarded chronological
  suffix, retained authored directive records, restored consumed
  subdirectives, directive-turn removal, and state recomputation.
- Cover the first-turn pre-checkpoint, deleted files, newly created files,
  modified files, multiple serial directives, a later directive whose attempt
  disappears but whose authored record returns Ready, and explicit impact
  confirmation.
- Cover incomplete capture/checkpoint reporting separately from an observed
  empty patch. Assert that the Rewind action remains reachable and the preview
  identifies every known gap without promising complete restoration.
- Extend persistence round-trip tests for durable directive records, immutable
  attempts, execution-session bindings, archive state, Attached/Detached/Source
  missing anchors, missing execution sessions, and historical unavailable
  checkpoint links.
- Extend anchor tests with real buffers and files. Cover partial edits, complete
  region deletion, several co-located detached directives in stable order,
  buffer kill, file deletion, exact unique reattachment, ambiguous matches, and
  explicit reattachment.
- Add small rendering/action tests for attached, detached, source-missing,
  processing, discussed, implemented, failed, aborted, request-changed, and
  archived states. Assert only compact labels and state-correct menus at the
  source seam; substantive content belongs in directive-turn and inspector
  tests.
- Test shared-view rendering, one-line folding, and directive composer scope
  using existing view-zone conventions. Scope remains sticky across sends,
  queued inputs retain it, explicit exit restores the stashed chat draft, and
  resume starts in chat scope. Preserve the established invariant that
  asynchronous status, agent, task, or activity redraw does not alter an active
  multiline scoped draft, including a draft whose first editable character is
  `>`.
- Test the directive inspector after source deletion and compaction. It is
  read-only, never streams or owns a composer, and dispatches actions back to
  the directive record or execution session.
- Cover parent/subdirective behavior through the existing topmost-directive
  submission seam: request inclusion, no independent activity, success
  consumption, failure/abort retention, new post-success corrections, and
  Rewind restoration.
- Extend batch tests for Ready, Discussed, previously attempted, detached, and
  source-missing directives and for stopping after failure/abort. Batch tests
  must assert that Request changes and Retry are never inferred.
- Assert that the patch viewer displays the chosen attempt but cannot become
  revision context or overwrite attempt history.
- Remove or rewrite tests that encode Revise, overlay-owned identity,
  compact directive events, a second live activity surface or composer, shared
  patch fallback, evaporating durable directives, or post-turn-only checkpoint
  semantics.
- Follow the repository's one-test-per-function convention and prefer adding
  cases to existing lifecycle, overlay, preset, persistence, Rewind, and view
  tests. Add no test-only state machine, second fixture format, mock filesystem,
  or alternate persistence harness.

## Out of Scope

- Redo, attempt switching, alternate implementation branches, or restoring a
  later discarded model attempt.
- Selective patch reversal that preserves later turns, inverse-patch
  generation, patch conflict resolution, or a second directive-specific undo
  stack.
- Automatic mechanical stale-patch classification, content hashes as a
  revision gate, or blocking Request changes because repository files drifted.
- Automatically sending the entire attempt history, summarizing directive
  history, or adding a directive-specific compaction system.
- Feeding directive activity into ordinary-chat conversational context, adding
  a second live directive surface or composer, or filtering the shared
  chronology into a hidden directive-only lens.
- A standalone directive chat session independent of the execution session's
  chronology and checkpoints.
- Permanent deletion of directive records that have activity, checkpoint-link
  tombstones, or resurrection rules for deleted records.
- Folding, grouping controls, or garbage collection for stacked detached
  directives before real density makes them necessary.
- Heuristic automatic reattachment when an exact unique source match does not
  exist.
- Making references durable after their source disappears.
- Independent activity or actions for nested subdirectives.
- Changes to the tutor workflow.
- Compatibility wrappers, command aliases, dual persisted formats, migrations,
  or version gates for the superseded directive representation.
- An implementation plan, staged rollout, or speculative framework for future
  workflow types.

## Further Notes

- Source inspection confirmed that nested directives currently act as
  parent-owned detail: processing resolves to the topmost directive, prompt
  assembly includes contained directives as hints or corrections, and parent
  success removes them. The redesigned lifecycle preserves and makes this
  behavior explicit.
- The patch remains valuable as an attempt artifact, a diff-view source, and
  concise historical context for the immediately following change request. It
  is memory, not repository authority and not an undo format.
- The directive record survives Rewind because it is user-authored workspace
  state. Its model-produced activity follows session chronology and is
  discarded with the rewound suffix. This is what allows directive B to return
  to Ready when its implementation disappears without inventing redo.
- “Rewind before this implementation...” is intentionally the only undo label.
  “Undo implementation...” is not offered because it would imply surgical
  reversal of one patch while preserving later dependent history.
- This is a breaking redesign. Existing Revise commands, revision presets,
  revision prompt profiles, overlay-only directive state, and old persisted
  directive data may stop working or be discarded. The project explicitly has
  no backward-compatibility contract, so the implementation should remove the
  superseded paths rather than carry both designs.
- The deliberate simplifications are linear history, latest-attempt request
  context, no redo, no permanent deletion after activity, and no detached-row
  folding. Revisit them only when observed use demonstrates a concrete need.
