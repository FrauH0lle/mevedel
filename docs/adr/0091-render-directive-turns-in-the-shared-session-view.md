# Render directive turns in the shared session view

Status: accepted. Incorporates ADR 0088; directive-specific behavior is replaced
by [ADR 0132](0132-notes-replace-references-directives-and-buddy-notes.md).

## Current decision

An isolated note conversation is an ordinary durable session, not a filtered
scope in another session's transcript. Its own view renders prompts, answers,
tools, permissions, agents and interactions, with the ordinary composer and one
turn identity per accepted request. The note frame displays that real view;
there is no second renderer, execution-session rebinding or sticky directive
composer scope. Notes retain only conversation links in the workspace store.

Each conversation owns its provider history. Claude Code note sessions follow
the ordinary resumable session path, not a fresh isolated native invocation for
every note request. Follow-ups use that session's history and permissions.
Old saved directive turns resume as ordinary turns, visible in chat and model
context; no marker-based exclusion, directive fold or legacy inspector remains.

The rejected-session objection below depended on whole-file snapshot restores.
[ADR 0131](0131-rewind-reverts-recorded-changes.md)'s checked reversal preserves
disjoint later edits and refuses conflicts, allowing independent note sessions
without silently overwriting each other's work. ADR 0132 owns their lifecycle;
the [view manual](../view.md) owns presentation and actions.

## Decision history

### Original shared-session directive decision

The following describes the retired design. Sharing one renderer removed
duplicated streaming and interaction ownership, but directive-local context,
binding and follow-up assembly remained special mechanisms. Ordinary note
sessions now provide that isolation structurally, enabled by checked Rewind.

Directive requests remain conversationally isolated while rendering their full
prompt, answer, tools, permissions, agents, and interactions as ordinary turns
in the bound execution session's view. The canonical transcript retains normal
user, response, and tool roles. Durable directive boundaries let request-time
projection exclude these turns from ordinary-chat provider context; directive
prompts use the workspace record and directive-local history.

For Claude Code, each directive request starts an isolated ACP conversation from
that selected prompt. It does not resume previous hidden native directive
history. The root retains its own native conversation. Directive provider
selection remains request-local, and directive turns do not establish root
model history. Native and ACP completion share the final patch, terminal
callback, and publication sequence. An ACP directive turn persists no native
identity ([ADR 0123](0123-keep-turn-authority-in-mevedel.md)); the preceding
complete transcript is published before its boundary opens, and the new
directive frame at its terminal boundary.

One accepted request reserves one session turn identity for snapshots, attempt
links, transcript metadata, and Rewind. The first accepted request binds the
workspace-owned directive to an execution session, reused on later requests.
A closed persisted session resumes on demand. An unavailable session requires
explicit warned rebinding for future activity; earlier attempt links keep their
historical identity.

Follow-ups use a visible, sticky directive scope in the shared composer. Implement
this combines the directive request, freshly resolved references, and its complete
local discussion into a new implementation attempt. Discuss result continues
local read-only discussion with the selected attempt attached. A read-only
inspector replaces the displayed view for durable inspection after compaction
or source loss; it owns neither streaming nor a composer.

Workspace identity and source recovery remain the decision of
[ADR 0087](0087-keep-directive-identity-outside-source-overlays.md).
State-dependent revision remains the decision of
[ADR 0089](0089-make-revision-state-dependent-implementation.md).
The [view manual](../view.md) owns the user actions and presentation details.

### Historical rationale and consequences

A single render and interaction owner keeps directive turns in the session's
chronological undo chain without making them ordinary-chat context. Exact attempt
material is also retained in the workspace record for follow-up construction and
durable inspection; that bounded duplication serves a different owner.

A session per directive was rejected: interleaved filesystem mutations would
produce competing checkpoint histories, allowing Rewind in one session to clobber
later changes from another. Reusing whichever workspace session was most recently
active would silently split a directive's execution history, hence explicit
binding and warned rebinding.

### Earlier decisions, including ADR 0088

**ADR 0088 placed full responses and follow-up discussion in a workspace-owned
activity surface with a local composer.** The execution session kept only a
compact event linking to that surface. It established conversational isolation,
first-request session binding, warned rebinding, and complete local-discussion
feedback for implementation. [ADR 0132](0132-notes-replace-references-directives-and-buddy-notes.md)
replaces this directive workflow with isolated note conversations. The later
removal of old-data compatibility also removes its historical rendering and
context-exclusion path.

**ADR 0091 replaced the separate activity renderer and compact-event proxy with
complete turns in the bound session view.** Streaming into the session and then
deleting/copying results to the workspace activity surface created two renderers,
two interaction owners, and drifting turn clocks. The shared view and one reserved
turn identity remove that coordination. Workspace records retain ownership and
inspection data, while durable prompt projection preserves isolation. This also
replaces only the activity/composer placement in ADRs 0087 and 0089; their identity,
recovery, and revision decisions remain independent.

**ACP integration preserves selected directive evidence with isolated native
requests.** gptel's directive path sends an explicitly selected prompt rather
than the shared transcript. Reusing a native conversation would accumulate
previous attempts beyond that selection. Directive native identities were first
committed to the sidecar before dispatch; none was ever resumed, yet each one
refused history operations for the whole session, so they are no longer stored.
