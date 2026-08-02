# 05 — Continue read-only directive discussions

**What to build:** Let users discuss a directive or an implementation result in its activity surface, continue that local conversation, and turn it into implementation with Implement this. Discussion is isolated from main chat and cannot mutate the workspace through direct or delegated capabilities.

**Blocked by:** 04 — Capture an isolated implementation attempt

**Status:** resolved

- [x] Ready directives offer Discuss and show Discussing then Discussed through the same durable lifecycle projection.
- [x] The directive activity surface provides a local multiline composer for starting and continuing discussion.
- [x] Discussion requests use the base system role and freshly resolved directive context without reading main-chat history.
- [x] Discussion replies and follow-ups remain in directive activity; the execution session receives only compact non-conversational events.
- [x] The discussion capability excludes direct write tools, mutating shell and evaluation behavior, and mutation-capable delegated work while retaining useful read-only inspection.
- [x] Capability enforcement, rather than request wording, prevents discussion from modifying files.
- [x] Continue discussion receives the complete directive-local discussion and no unrelated transcript.
- [x] Discuss result attaches the selected implementation attempt to the same read-only local discussion.
- [x] Implement this starts an ordinary implementation attempt with the directive request, fresh references, and complete local discussion as feedback.
- [x] Subsequent requests reuse the directive’s bound execution session even if another workspace session is currently selected.
- [x] A closed persisted execution session resumes on demand; an unavailable session requires explicit warned rebinding, and historical checkpoint links are not reassigned.
- [x] Activity redraws preserve an active multiline composer draft, including one whose first editable character is `>`.
- [x] Tests cover hard read-only policy, continued discussion, Discuss result, Implement this, session reuse, resume, explicit rebind, and composer-draft preservation.

## Answer

Implemented in `60ae6b3`.
