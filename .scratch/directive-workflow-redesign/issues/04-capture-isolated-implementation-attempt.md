# 04 — Capture an isolated implementation attempt

**What to build:** Make one Implement action produce a durable implementation attempt outside main-chat conversation. The user can inspect its exact request, result, outcome, observed changes, and capture quality from directive activity while the session retains only a compact chronological event.

**Blocked by:** 02 — Checkpoint before every model turn; 03 — Open directives in a dedicated activity surface

**Status:** resolved

- [x] Implement builds an isolated request from the directive and freshly resolved references without inheriting the main-chat transcript.
- [x] The first accepted directive request binds the directive to the execution session and persists that binding.
- [x] The implementation result is stored as one immutable chronological attempt containing the exact submitted request, answer or error, terminal outcome, patch, capture completeness, covered files, and turn-checkpoint link.
- [x] Success, failure, and user abort each settle an inspectable attempt rather than overwriting one directive-level result.
- [x] Complete capture distinguishes observed changes from no filesystem changes; incomplete capture identifies known coverage and gaps.
- [x] Directive activity shows the attempt and can project its patch into the reusable diff viewer.
- [x] The patch viewer is presentation only and cannot become history ownership or prompt fallback state.
- [x] The execution session stores a compact link event for chronology and Rewind while the full directive response remains outside conversational context.
- [x] Source and activity actions reflect Implementing during flight and the derived Implemented, Failed, or Aborted outcome afterward.
- [x] Deleting or losing the source overlay during an in-flight request does not prevent terminal settlement on the durable directive record.
- [x] Restoring the workspace preserves the attempt, execution-session binding, compact event link, and patch metadata.
- [x] Tests drive the existing directive request lifecycle with real temporary files while replacing only remote model transport.

## Answer

Resolved by commit `ba53c75`.
