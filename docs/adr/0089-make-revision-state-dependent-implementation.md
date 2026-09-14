# Make revision state-dependent implementation

Status: accepted. Composer placement follows [ADR 0091](0091-render-directive-turns-in-the-shared-session-view.md).

## Current decision

Discuss and Implement are the initial directive intents. Request changes after
success and Retry after failure/abort are further implementation attempts, using
the ordinary implementation preset. No revision-specific mode, system role, or
profile is needed.

Request changes receives the current directive, freshly resolved references,
the immediately preceding successful answer and captured patch, and new feedback
or unconsumed subdirectives. Feedback is required only when no nested detail
supplies it. Retry receives the preceding error/partial changes and optional
guidance. Older activity remains inspectable rather than being supplied
implicitly. The shared directive-scoped composer accepts multiline feedback.

The preceding patch is historical evidence with capture time and completeness.
Current repository state is authoritative; there is no mechanical stale-patch
gate. Editing the authored directive returns it to Ready while preserving old
activity, and Request changes stays unavailable until that edited request has
an attempt. A clean restart uses Rewind followed by Implement.

Each accepted attempt retains its exact submitted request, outcome, available
answer/error, captured patch, completeness, and execution checkpoint. Capture
distinguishes known no-change, captured changes, and gaps from unsnapshotted
effects. The reusable diff viewer projects one selected attempt and owns neither
history nor revision context. [Directive views](../view.md#directive-turns-and-inspector)
provide the interaction entry points.

Batch processing starts only initial implementations: Ready directives directly,
Discussed directives with matching discussion. It skips directives with an
implementation attempt, never infers revision/retry, and stops at the first
failed or aborted attempt.

## Rationale and consequences

Available actions follow lifecycle state and preserve authored intent. A separate
revision identity duplicated implementation behavior whose only difference was
historical context. Keeping attempts immutable makes failures and partial effects
inspectable instead of overwriting one directive-level answer. A state-dependent
batch cannot silently reinterpret failed work as permission to retry it.

## Decision history

ADR 0089 removed Revise as a peer action/processing mode in favor of Request
changes and Retry. Its separate composer placement was subsequently replaced by
ADR 0091's shared session view; directive-local context remained. Tutor removal
belongs to [ADR 0070](0070-compose-system-prompts-from-ordered-profiles.md#decision-history).
