# 06 — Replace Revise with Request changes and Retry

**What to build:** Replace the ambiguous Revise workflow with state-dependent implementation actions. Successful work can receive Request changes through the activity composer, while failed or aborted work can Retry with optional guidance; both operate from current repository state and focused preceding-attempt context.

**Blocked by:** 05 — Continue read-only directive discussions

**Status:** resolved

- [x] A successful implementation offers Request changes instead of Revise or Implement again.
- [x] Request changes collects multiline feedback and starts a new ordinary implementation attempt.
- [x] Its isolated request contains the current directive request, freshly resolved references, new feedback, and only the immediately preceding attempt’s answer and patch.
- [x] The preceding patch is labeled historical with capture time and completeness, and current repository state is explicitly authoritative.
- [x] Older attempts remain inspectable but are not automatically supplied to the model.
- [x] Failed and aborted implementations offer one Retry action with optional multiline guidance.
- [x] Retry supplies the immediately preceding error and any observed partial changes as diagnostic context.
- [x] Every new success, failure, or abort appends an immutable attempt rather than replacing earlier activity.
- [x] Editing the current directive request preserves activity but yields Ready with a request-changed qualifier and removes Request changes until the edited request has its own attempt.
- [x] A clean restart is represented only by Rewind followed by Implement; no fresh re-run action starts from the already modified repository.
- [x] Request changes and Retry use ordinary implementation authority rather than a revision-specific system role.
- [x] The Revise action, revision preset, revision role prompt, revision profile, shared-patch fallback, and superseded menu paths are removed without aliases or compatibility readers.
- [x] Tutor behavior remains unchanged.
- [x] Tests cover repeated attempts, focused context, repository drift, request editing, action availability, optional Retry guidance, and absence of all superseded revision behavior.

## Answer

Implemented in `78da377`.
